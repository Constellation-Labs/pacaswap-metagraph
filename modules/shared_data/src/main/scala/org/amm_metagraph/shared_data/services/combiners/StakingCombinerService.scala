package org.amm_metagraph.shared_data.services.combiners

import cats.data.EitherT
import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact._
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.{AllowSpend, CurrencyId, SwapAmount}
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hasher, SecurityProvider}

import monocle.syntax.all._
import org.amm_metagraph.shared_data.SpendTransactions.{checkIfSpendActionAcceptedInGl0, generateSpendAction}
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.epochProgress.getConfirmedExpireEpochProgress
import org.amm_metagraph.shared_data.globalSnapshots.{getAllowSpendsGlobalSnapshotsState, logger}
import org.amm_metagraph.shared_data.services.pricing.PricingService
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, StakingUpdate}
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.Staking._
import org.amm_metagraph.shared_data.types.States.StateTransitionType._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.codecs.{HasherSelector, JsonWithBase64BinaryCodec}
import org.amm_metagraph.shared_data.validations.Errors.DuplicatedUpdate
import org.amm_metagraph.shared_data.validations.StakingValidations

trait StakingCombinerService[F[_]] {
  def combineNew(
    signedUpdate: Signed[StakingUpdate],
    oldState: DataState[AmmOnChainState, AmmCalculatedState],
    globalEpochProgress: EpochProgress,
    lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
    currencyId: CurrencyId
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]]

  def combinePendingAllowSpend(
    pendingSignedUpdate: PendingAllowSpend[StakingUpdate],
    oldState: DataState[AmmOnChainState, AmmCalculatedState],
    globalEpochProgress: EpochProgress,
    lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
    currencyId: CurrencyId
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]]

  def combinePendingSpendAction(
    pendingSpendAction: PendingSpendAction[StakingUpdate],
    oldState: DataState[AmmOnChainState, AmmCalculatedState],
    globalEpochProgress: EpochProgress,
    spendActions: List[SpendAction],
    currentSnapshotOrdinal: SnapshotOrdinal,
    currencyId: CurrencyId
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]]

  def cleanupExpiredOperations(
    oldState: DataState[AmmOnChainState, AmmCalculatedState],
    globalEpochProgress: EpochProgress
  ): DataState[AmmOnChainState, AmmCalculatedState]
}

object StakingCombinerService {
  def make[F[_]: Async: HasherSelector: SecurityProvider](
    applicationConfig: ApplicationConfig,
    pricingService: PricingService[F],
    stakingValidations: StakingValidations[F],
    dataUpdateCodec: JsonWithBase64BinaryCodec[F, AmmUpdate]
  ): StakingCombinerService[F] =
    new StakingCombinerService[F] {
      private def getUpdateAllowSpends(
        stakingUpdate: StakingUpdate,
        lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]]
      ) =
        HasherSelector[F].withBrotli { implicit hs =>
          getAllowSpendsGlobalSnapshotsState(
            stakingUpdate.tokenAAllowSpend,
            stakingUpdate.tokenAId,
            stakingUpdate.tokenBAllowSpend,
            stakingUpdate.tokenBId,
            lastGlobalSnapshotsAllowSpends
          )
        }

      private def handleFailedUpdate(
        stakingUpdate: Signed[StakingUpdate],
        acc: DataState[AmmOnChainState, AmmCalculatedState],
        failedCalculatedState: FailedCalculatedState,
        stakingCalculatedState: StakingCalculatedState,
        updateHash: Hash,
        currentState: StateTransitionType
      ) =
        failedCalculatedState.reason match {
          case DuplicatedUpdate(_) => logger.warn("Duplicated data update, ignoring") >> acc.pure
          case _ =>
            val updatedStakingCalculatedState = stakingCalculatedState
              .focus(_.failed)
              .modify(_ + failedCalculatedState)
              .focus(_.pending)
              .modify(_.filter(_.update =!= stakingUpdate))

            val updatedCalculatedState = acc.calculated
              .focus(_.operations)
              .modify(_.updated(OperationType.Staking, updatedStakingCalculatedState))

            Async[F].pure(
              acc
                .focus(_.onChain.updatedStateDataUpdate)
                .modify { current =>
                  current + UpdatedStateDataUpdate(
                    currentState,
                    Failed,
                    OperationType.Staking,
                    stakingUpdate,
                    updateHash,
                    none
                  )
                }
                .focus(_.calculated)
                .replace(updatedCalculatedState)
            )
        }

      private def removePendingAllowSpend(
        stakingCalculatedState: StakingCalculatedState,
        signedStakingUpdate: Signed[StakingUpdate]
      ) =
        stakingCalculatedState.pending.filterNot {
          case PendingAllowSpend(update, _, _) if update === signedStakingUpdate => true
          case _                                                                 => false
        }

      private def removePendingSpendAction(
        stakingCalculatedState: StakingCalculatedState,
        signedStakingUpdate: Signed[StakingUpdate]
      ) =
        stakingCalculatedState.pending.filterNot {
          case PendingSpendAction(update, _, _, _) if update === signedStakingUpdate => true
          case _                                                                     => false
        }

      def combineNew(
        signedUpdate: Signed[StakingUpdate],
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        globalEpochProgress: EpochProgress,
        lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
        currencyId: CurrencyId
      )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
        val stakingCalculatedState = getStakingCalculatedState(oldState.calculated)
        val confirmedStakings = stakingCalculatedState.confirmed.value
          .get(signedUpdate.source)
          .map(_.values)
          .getOrElse(SortedSet.empty[StakingCalculatedStateValue])

        val pendingStakings = stakingCalculatedState.getPendingUpdates
        val pendingAllowSpendsCalculatedState = stakingCalculatedState.pending
        val updateHashedF = HasherSelector[F].withCurrent(implicit hs => signedUpdate.toHashed(dataUpdateCodec.serialize))

        val stakingUpdate = signedUpdate.value
        val combinedState = for {
          _ <- EitherT(
            stakingValidations.l0Validations(
              signedUpdate,
              oldState.calculated,
              globalEpochProgress
            )
          )
          _ <- EitherT.fromEither(
            stakingValidations.newUpdateValidations(
              oldState.calculated,
              signedUpdate,
              globalEpochProgress,
              confirmedStakings,
              pendingStakings
            )
          )
          updateAllowSpends <- EitherT.liftF(getUpdateAllowSpends(stakingUpdate, lastGlobalSnapshotsAllowSpends))
          updateHashed <- EitherT.liftF(updateHashedF)

          response <- updateAllowSpends match {
            case (None, _) | (_, None) =>
              val pendingAllowSpend = PendingAllowSpend(
                signedUpdate,
                updateHashed.hash
              )

              val updatedPendingCalculatedState = pendingAllowSpendsCalculatedState + pendingAllowSpend
              val newStakingState = stakingCalculatedState
                .focus(_.pending)
                .replace(updatedPendingCalculatedState)

              val updatedCalculatedState = oldState.calculated
                .focus(_.operations)
                .modify(_.updated(OperationType.Staking, newStakingState))

              EitherT.rightT[F, FailedCalculatedState](
                oldState
                  .focus(_.onChain.updatedStateDataUpdate)
                  .modify { current =>
                    current + UpdatedStateDataUpdate(
                      NewUpdate,
                      PendingAllowSpends,
                      OperationType.Staking,
                      signedUpdate,
                      updateHashed.hash,
                      Some(pendingAllowSpend.asInstanceOf[PendingAction[AmmUpdate]])
                    )
                  }
                  .focus(_.calculated)
                  .replace(updatedCalculatedState)
              )

            case (Some(_), Some(_)) =>
              EitherT.liftF[F, FailedCalculatedState, DataState[AmmOnChainState, AmmCalculatedState]](
                combinePendingAllowSpend(
                  PendingAllowSpend(signedUpdate, updateHashed.hash),
                  oldState,
                  globalEpochProgress,
                  lastGlobalSnapshotsAllowSpends,
                  currencyId
                )
              )
          }
        } yield response

        combinedState.foldF(
          failed =>
            updateHashedF.flatMap(hashed =>
              handleFailedUpdate(signedUpdate, oldState, failed, stakingCalculatedState, hashed.hash, NewUpdate)
            ),
          success => success.pure[F]
        )
      }

      def combinePendingAllowSpend(
        pendingAllowSpendUpdate: PendingAllowSpend[StakingUpdate],
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        globalEpochProgress: EpochProgress,
        lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
        currencyId: CurrencyId
      )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {

        val stakingCalculatedState = getStakingCalculatedState(oldState.calculated)
        val stakingUpdate = pendingAllowSpendUpdate.update.value

        val combinedState: EitherT[F, FailedCalculatedState, DataState[AmmOnChainState, AmmCalculatedState]] = for {
          updateAllowSpends <- EitherT.liftF(getUpdateAllowSpends(stakingUpdate, lastGlobalSnapshotsAllowSpends))
          result <- updateAllowSpends match {
            case (Some(allowSpendTokenA), Some(allowSpendTokenB)) =>
              for {
                poolId <- EitherT.liftF(buildLiquidityPoolUniqueIdentifier(stakingUpdate.tokenAId, stakingUpdate.tokenBId))
                stakingTokenInfo <- EitherT(pricingService.getStakingTokenInfo(pendingAllowSpendUpdate.update, poolId, globalEpochProgress))
                _ <- EitherT.fromEither[F](
                  stakingValidations.pendingAllowSpendsValidations(
                    pendingAllowSpendUpdate.update,
                    globalEpochProgress,
                    currencyId,
                    stakingTokenInfo,
                    allowSpendTokenA,
                    allowSpendTokenB
                  )
                )
                (amountToSpendA, amountToSpendB) =
                  if (stakingUpdate.tokenAId === stakingTokenInfo.primaryTokenInformation.identifier) {
                    (
                      SwapAmount(stakingTokenInfo.primaryTokenInformation.amount),
                      SwapAmount(stakingTokenInfo.pairTokenInformation.amount)
                    )
                  } else {
                    (
                      SwapAmount(stakingTokenInfo.pairTokenInformation.amount),
                      SwapAmount(stakingTokenInfo.primaryTokenInformation.amount)
                    )
                  }

                spendAction = generateSpendAction(
                  allowSpendTokenA,
                  amountToSpendA,
                  allowSpendTokenB,
                  amountToSpendB
                )

                updatedPendingAllowSpendCalculatedState =
                  removePendingAllowSpend(stakingCalculatedState, pendingAllowSpendUpdate.update)
                pendingSpendAction = PendingSpendAction(
                  pendingAllowSpendUpdate.update,
                  pendingAllowSpendUpdate.updateHash,
                  spendAction
                )

                updatedPendingSpendActionCalculatedState =
                  updatedPendingAllowSpendCalculatedState + pendingSpendAction

                updatedStakingCalculatedState =
                  stakingCalculatedState
                    .focus(_.pending)
                    .replace(updatedPendingSpendActionCalculatedState)

                updatedCalculatedState = oldState.calculated
                  .focus(_.operations)
                  .modify(_.updated(OperationType.Staking, updatedStakingCalculatedState))

              } yield
                oldState
                  .focus(_.onChain.updatedStateDataUpdate)
                  .modify { current =>
                    current + UpdatedStateDataUpdate(
                      PendingAllowSpends,
                      PendingSpendTransactions,
                      OperationType.Staking,
                      pendingAllowSpendUpdate.update,
                      pendingAllowSpendUpdate.updateHash,
                      Some(pendingSpendAction.asInstanceOf[PendingAction[AmmUpdate]])
                    )
                  }
                  .focus(_.calculated)
                  .replace(updatedCalculatedState)
                  .focus(_.sharedArtifacts)
                  .modify(current =>
                    current ++ SortedSet[SharedArtifact](
                      spendAction
                    )
                  )

            case _ =>
              EitherT.rightT[F, FailedCalculatedState](oldState)
          }
        } yield result

        combinedState.foldF(
          failed =>
            handleFailedUpdate(
              pendingAllowSpendUpdate.update,
              oldState,
              failed,
              stakingCalculatedState,
              pendingAllowSpendUpdate.updateHash,
              PendingAllowSpends
            ),
          success => success.pure[F]
        )
      }

      def combinePendingSpendAction(
        pendingSpendAction: PendingSpendAction[StakingUpdate],
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        globalEpochProgress: EpochProgress,
        spendActions: List[SpendAction],
        currentSnapshotOrdinal: SnapshotOrdinal,
        currencyId: CurrencyId
      )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
        val signedStakingUpdate = pendingSpendAction.update
        val liquidityPoolsCalculatedState = getLiquidityPoolCalculatedState(oldState.calculated)
        val stakingCalculatedState = getStakingCalculatedState(oldState.calculated)

        val stakingUpdate = signedStakingUpdate.value

        val combinedState: EitherT[F, FailedCalculatedState, DataState[AmmOnChainState, AmmCalculatedState]] = for {
          _ <- EitherT.fromEither[F](
            stakingValidations.pendingSpendActionsValidation(
              pendingSpendAction.update,
              globalEpochProgress
            )
          )
          metagraphGeneratedSpendActionHash <- EitherT.liftF(
            HasherSelector[F].withCurrent(implicit hs => Hasher[F].hash(pendingSpendAction.generatedSpendAction))
          )
          globalSnapshotsHashes <- EitherT.liftF(
            HasherSelector[F].withCurrent(implicit hs => spendActions.traverse(action => Hasher[F].hash(action)))
          )
          allSpendActionsAccepted <- EitherT.liftF {
            Async[F].pure(checkIfSpendActionAcceptedInGl0(metagraphGeneratedSpendActionHash, globalSnapshotsHashes))
          }
          result <-
            if (!allSpendActionsAccepted) {
              EitherT.rightT[F, FailedCalculatedState](oldState)
            } else {
              for {
                poolId <- EitherT.liftF(buildLiquidityPoolUniqueIdentifier(stakingUpdate.tokenAId, stakingUpdate.tokenBId))
                liquidityPool <- EitherT.liftF(getLiquidityPoolByPoolId(liquidityPoolsCalculatedState.confirmed.value, poolId))
                stakingReference <- EitherT.liftF(
                  HasherSelector[F].withCurrent(implicit hs => StakingReference.of(signedStakingUpdate))
                )
                sourceAddress = signedStakingUpdate.source
                stakingTokenInfo <- EitherT(pricingService.getStakingTokenInfo(signedStakingUpdate, poolId, globalEpochProgress))

                liquidityPoolUpdated <- EitherT.fromEither[F](
                  pricingService.getUpdatedLiquidityPoolDueStaking(
                    liquidityPool,
                    signedStakingUpdate,
                    sourceAddress,
                    stakingTokenInfo,
                    globalEpochProgress
                  )
                )
                stakingCalculatedStateAddress = StakingCalculatedStateAddress(
                  stakingUpdate.tokenAAllowSpend,
                  stakingUpdate.tokenBAllowSpend,
                  stakingTokenInfo.primaryTokenInformation,
                  stakingTokenInfo.pairTokenInformation,
                  stakingReference
                )
                updatedPendingCalculatedState = removePendingSpendAction(stakingCalculatedState, signedStakingUpdate)
                expirationEpochProgress = getConfirmedExpireEpochProgress(applicationConfig, globalEpochProgress)
                stakingCalculatedStateValue = StakingCalculatedStateValue(
                  expirationEpochProgress,
                  stakingCalculatedStateAddress
                )
                updatedStakingCalculatedState = stakingCalculatedState
                  .focus(_.confirmed.value)
                  .modify(_.updatedWith(sourceAddress) {
                    case Some(confirmedSwaps) =>
                      Some(
                        StakingCalculatedStateInfo(
                          stakingReference,
                          confirmedSwaps.values + stakingCalculatedStateValue
                        )
                      )
                    case None =>
                      Some(
                        StakingCalculatedStateInfo(
                          stakingReference,
                          SortedSet(stakingCalculatedStateValue)
                        )
                      )
                  })
                  .focus(_.pending)
                  .replace(updatedPendingCalculatedState)

                updatedLiquidityPool = liquidityPoolsCalculatedState
                  .focus(_.confirmed.value)
                  .modify(_.updated(poolId.value, liquidityPoolUpdated))

                updatedCalculatedState = oldState.calculated
                  .focus(_.operations)
                  .modify(_.updated(OperationType.Staking, updatedStakingCalculatedState))
                  .focus(_.operations)
                  .modify(_.updated(OperationType.LiquidityPool, updatedLiquidityPool))

              } yield
                oldState
                  .focus(_.onChain.updatedStateDataUpdate)
                  .modify { current =>
                    current + UpdatedStateDataUpdate(
                      PendingSpendTransactions,
                      Confirmed,
                      OperationType.Staking,
                      pendingSpendAction.update,
                      pendingSpendAction.updateHash,
                      none
                    )
                  }
                  .focus(_.calculated)
                  .replace(updatedCalculatedState)
            }
        } yield result

        combinedState.foldF(
          failed =>
            handleFailedUpdate(
              pendingSpendAction.update,
              oldState,
              failed,
              stakingCalculatedState,
              pendingSpendAction.updateHash,
              PendingSpendTransactions
            ),
          success => success.pure[F]
        )
      }

      def cleanupExpiredOperations(
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        globalEpochProgress: EpochProgress
      ): DataState[AmmOnChainState, AmmCalculatedState] = {
        val stakingCalculatedState = getStakingCalculatedState(oldState.calculated)
        val unexpiredFailed = stakingCalculatedState.failed.filter(_.expiringEpochProgress > globalEpochProgress)
        val unexpiredConfirmed = stakingCalculatedState.confirmed.value.collect {
          case (address, infos) =>
            address -> infos
              .focus(_.values)
              .modify(_.filter(_.expiringEpochProgress > globalEpochProgress))
        }

        val updatedStakingCalculatedState = stakingCalculatedState
          .focus(_.failed)
          .replace(unexpiredFailed)
          .focus(_.confirmed.value)
          .replace(unexpiredConfirmed)
        oldState
          .focus(_.calculated.operations)
          .modify(_.updated(OperationType.Staking, updatedStakingCalculatedState))
      }
    }
}
