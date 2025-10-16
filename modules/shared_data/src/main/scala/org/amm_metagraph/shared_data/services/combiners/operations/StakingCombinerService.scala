package org.amm_metagraph.shared_data.services.combiners.operations

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
import org.amm_metagraph.shared_data.epochProgress.{getConfirmedExpireEpochProgress, getFailureExpireEpochProgress}
import org.amm_metagraph.shared_data.globalSnapshots.{getAllowSpendsGlobalSnapshotsState, logger}
import org.amm_metagraph.shared_data.services.pricing.PricingService
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, StakingUpdate}
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.Staking._
import org.amm_metagraph.shared_data.types.States.StateTransitionType._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.codecs.{HasherSelector, JsonWithBase64BinaryCodec}
import org.amm_metagraph.shared_data.validations.Errors._
import org.amm_metagraph.shared_data.validations.StakingValidations
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

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
      def logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

      private def getUpdateAllowSpends(
        stakingUpdate: StakingUpdate,
        lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]]
      ): F[(Option[io.constellationnetwork.security.Hashed[AllowSpend]], Option[io.constellationnetwork.security.Hashed[AllowSpend]])] =
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
      ): F[DataState[AmmOnChainState, AmmCalculatedState]] =
        failedCalculatedState.reason match {
          case DuplicatedUpdate(_) =>
            logger.warn("Duplicated data update, ignoring") >> acc.pure[F]
          case _ =>
            val updatedStakingCalculatedState = stakingCalculatedState
              .focus(_.failed)
              .modify(_ + failedCalculatedState)
              .focus(_.pending)
              .modify(_.filter(_.update =!= stakingUpdate))

            val updatedCalculatedState = acc.calculated
              .focus(_.operations)
              .modify(_.updated(OperationType.Staking, updatedStakingCalculatedState))

            val dataUpdate = UpdatedStateDataUpdate(
              currentState,
              Failed,
              OperationType.Staking,
              stakingUpdate.asInstanceOf[Signed[AmmUpdate]],
              updateHash,
              None,
              Some(failedCalculatedState.reason)
            )

            val result = acc
              .focus(_.onChain.updatedStateDataUpdate)
              .modify(_ + dataUpdate)
              .focus(_.calculated)
              .replace(updatedCalculatedState)

            result.pure[F]
        }

      private def removePendingAllowSpend(
        stakingCalculatedState: StakingCalculatedState,
        signedStakingUpdate: Signed[StakingUpdate]
      ): SortedSet[PendingAction[StakingUpdate]] =
        stakingCalculatedState.pending.filterNot {
          case PendingAllowSpend(update, _, _) if update === signedStakingUpdate => true
          case _                                                                 => false
        }

      private def removePendingSpendAction(
        stakingCalculatedState: StakingCalculatedState,
        signedStakingUpdate: Signed[StakingUpdate]
      ): SortedSet[PendingAction[StakingUpdate]] =
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
          _ <- EitherT.liftF[F, FailedCalculatedState, Unit](logger.info(s"Processing new staking update from ${signedUpdate.source}"))

          _ <- EitherT(
            stakingValidations.l0Validations(
              signedUpdate,
              oldState.calculated,
              globalEpochProgress
            )
          ).leftWiden[FailedCalculatedState]
          _ <- EitherT(
            stakingValidations.newUpdateValidations(
              oldState.calculated,
              signedUpdate,
              globalEpochProgress,
              confirmedStakings,
              pendingStakings
            )
          ).leftWiden[FailedCalculatedState]
          updateAllowSpends <- EitherT.liftF[
            F,
            FailedCalculatedState,
            (Option[io.constellationnetwork.security.Hashed[AllowSpend]], Option[io.constellationnetwork.security.Hashed[AllowSpend]])
          ](getUpdateAllowSpends(stakingUpdate, lastGlobalSnapshotsAllowSpends))
          updateHashed <- EitherT.liftF[F, FailedCalculatedState, io.constellationnetwork.security.Hashed[StakingUpdate]](updateHashedF)
          poolId <- EitherT.liftF[F, FailedCalculatedState, PoolId](
            buildLiquidityPoolUniqueIdentifier(stakingUpdate.tokenAId, stakingUpdate.tokenBId)
          )
          stakingTokenInfo <- EitherT(
            pricingService
              .getStakingTokenInfo(signedUpdate, updateHashed.hash, poolId, globalEpochProgress)
          )
          response <- updateAllowSpends match {
            case (None, _) | (_, None) =>
              val pendingAllowSpend = PendingAllowSpend(
                signedUpdate,
                updateHashed.hash,
                Some(stakingTokenInfo)
              )

              val updatedPendingCalculatedState = pendingAllowSpendsCalculatedState + pendingAllowSpend
              val newStakingState = stakingCalculatedState
                .focus(_.pending)
                .replace(updatedPendingCalculatedState)

              val updatedCalculatedState = oldState.calculated
                .focus(_.operations)
                .modify(_.updated(OperationType.Staking, newStakingState))

              val dataUpdate = UpdatedStateDataUpdate(
                NewUpdate,
                PendingAllowSpends,
                OperationType.Staking,
                signedUpdate.asInstanceOf[Signed[AmmUpdate]],
                updateHashed.hash,
                Some(pendingAllowSpend.asInstanceOf[PendingAction[AmmUpdate]])
              )

              EitherT.liftF[F, FailedCalculatedState, DataState[AmmOnChainState, AmmCalculatedState]](
                logger.warn(s"Allow spend not confirmed, storing as pending") >>
                  oldState
                    .focus(_.onChain.updatedStateDataUpdate)
                    .modify(_ + dataUpdate)
                    .focus(_.calculated)
                    .replace(updatedCalculatedState)
                    .pure[F]
              )

            case (Some(_), Some(_)) =>
              EitherT.liftF[F, FailedCalculatedState, DataState[AmmOnChainState, AmmCalculatedState]](
                logger.info(s"Allow spend confirmed, processing immediately") >>
                  combinePendingAllowSpend(
                    PendingAllowSpend(signedUpdate, updateHashed.hash, Some(stakingTokenInfo)),
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
            for {
              _ <- logger.error(s"Failed to process staking update: ${failed.reason}")
              updateHashed <- updateHashedF
              result <- handleFailedUpdate(signedUpdate, oldState, failed, stakingCalculatedState, updateHashed.hash, NewUpdate)
            } yield result,
          success => logger.info("Successfully processed staking update") >> success.pure[F]
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
        val maybeStakingTokenInfo = pendingAllowSpendUpdate.pricingTokenInfo.collect {
          case stakingTokenInfo: StakingTokenInfo => stakingTokenInfo
        }
        val combinedState: EitherT[F, FailedCalculatedState, DataState[AmmOnChainState, AmmCalculatedState]] = for {
          _ <- EitherT.liftF[F, FailedCalculatedState, Unit](logger.info(s"Processing pending allow spend"))
          updateAllowSpends <- EitherT.liftF[
            F,
            FailedCalculatedState,
            (Option[io.constellationnetwork.security.Hashed[AllowSpend]], Option[io.constellationnetwork.security.Hashed[AllowSpend]])
          ](getUpdateAllowSpends(stakingUpdate, lastGlobalSnapshotsAllowSpends))
          result <- updateAllowSpends match {
            case (Some(allowSpendTokenA), Some(allowSpendTokenB)) =>
              for {
                stakingTokenInfo <- EitherT.fromOption[F](
                  maybeStakingTokenInfo,
                  FailedCalculatedState(
                    MissingStakingTokenInfo(),
                    getFailureExpireEpochProgress(applicationConfig, globalEpochProgress),
                    pendingAllowSpendUpdate.updateHash,
                    pendingAllowSpendUpdate.update
                  ): FailedCalculatedState
                )
                _ <- EitherT(
                  stakingValidations.pendingAllowSpendsValidations(
                    pendingAllowSpendUpdate.update,
                    globalEpochProgress,
                    currencyId,
                    stakingTokenInfo,
                    allowSpendTokenA,
                    allowSpendTokenB
                  )
                ).leftWiden[FailedCalculatedState]
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
                  spendAction,
                  pendingAllowSpendUpdate.pricingTokenInfo
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

                dataUpdate = UpdatedStateDataUpdate(
                  PendingAllowSpends,
                  PendingSpendTransactions,
                  OperationType.Staking,
                  pendingAllowSpendUpdate.update.asInstanceOf[Signed[AmmUpdate]],
                  pendingAllowSpendUpdate.updateHash,
                  Some(pendingSpendAction.asInstanceOf[PendingAction[AmmUpdate]])
                )

              } yield
                oldState
                  .focus(_.onChain.updatedStateDataUpdate)
                  .modify(_ + dataUpdate)
                  .focus(_.calculated)
                  .replace(updatedCalculatedState)
                  .focus(_.sharedArtifacts)
                  .modify(current =>
                    current ++ SortedSet[SharedArtifact](
                      spendAction
                    )
                  )

            case _ =>
              if (pendingAllowSpendUpdate.update.value.maxValidGsEpochProgress <= globalEpochProgress) {
                EitherT.leftT[F, DataState[AmmOnChainState, AmmCalculatedState]](
                  FailedCalculatedState(
                    OperationExpired(pendingAllowSpendUpdate.update),
                    getFailureExpireEpochProgress(applicationConfig, globalEpochProgress),
                    pendingAllowSpendUpdate.updateHash,
                    pendingAllowSpendUpdate.update
                  )
                )
              } else {
                EitherT.rightT[F, FailedCalculatedState](oldState)
              }
          }
        } yield result

        combinedState.foldF(
          failed =>
            for {
              _ <- logger.error(s"Failed to process pending allow spend: ${failed.reason}")
              result <- handleFailedUpdate(
                pendingAllowSpendUpdate.update,
                oldState,
                failed,
                stakingCalculatedState,
                pendingAllowSpendUpdate.updateHash,
                PendingAllowSpends
              )
            } yield result,
          success => logger.info("Successfully processed pending allow spend") >> success.pure[F]
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
          metagraphGeneratedSpendActionHash <- EitherT.liftF[F, FailedCalculatedState, Hash](
            HasherSelector[F].withCurrent(implicit hs => Hasher[F].hash(pendingSpendAction.generatedSpendAction))
          )
          globalSnapshotsHashes <- EitherT.liftF[F, FailedCalculatedState, List[Hash]](
            HasherSelector[F].withCurrent(implicit hs => spendActions.traverse(action => Hasher[F].hash(action)))
          )
          allSpendActionsAccepted <- EitherT.liftF[F, FailedCalculatedState, Boolean] {
            checkIfSpendActionAcceptedInGl0(metagraphGeneratedSpendActionHash, globalSnapshotsHashes).pure[F]
          }
          _ <- EitherT(
            stakingValidations.pendingSpendActionsValidation(
              pendingSpendAction.update,
              allSpendActionsAccepted,
              globalEpochProgress
            )
          ).leftWiden[FailedCalculatedState]
          result <-
            if (!allSpendActionsAccepted) {
              if (pendingSpendAction.update.value.maxValidGsEpochProgress <= globalEpochProgress) {
                EitherT.leftT[F, DataState[AmmOnChainState, AmmCalculatedState]](
                  FailedCalculatedState(
                    OperationExpired(pendingSpendAction.update),
                    getFailureExpireEpochProgress(applicationConfig, globalEpochProgress),
                    pendingSpendAction.updateHash,
                    pendingSpendAction.update
                  )
                )
              } else {
                EitherT.rightT[F, FailedCalculatedState](oldState)
              }
            } else {
              for {
                poolId <- EitherT.liftF[F, FailedCalculatedState, PoolId](
                  buildLiquidityPoolUniqueIdentifier(stakingUpdate.tokenAId, stakingUpdate.tokenBId)
                )
                liquidityPool <- EitherT.liftF[F, FailedCalculatedState, LiquidityPool](
                  getLiquidityPoolByPoolId(liquidityPoolsCalculatedState.confirmed.value, poolId)
                )
                stakingReference <- EitherT.liftF[F, FailedCalculatedState, StakingReference](
                  HasherSelector[F].withCurrent(implicit hs => StakingReference.of(signedStakingUpdate))
                )
                sourceAddress = signedStakingUpdate.source
                stakingTokenInfo <- EitherT(
                  pricingService.getStakingTokenInfo(signedStakingUpdate, pendingSpendAction.updateHash, poolId, globalEpochProgress)
                )

                liquidityPoolUpdated <- EitherT(
                  pricingService.getUpdatedLiquidityPoolDueStaking(
                    liquidityPool,
                    signedStakingUpdate,
                    pendingSpendAction.updateHash,
                    sourceAddress,
                    stakingTokenInfo,
                    globalEpochProgress,
                    currentSnapshotOrdinal
                  )
                )

                stakingCalculatedStateAddress = StakingCalculatedStateAddress(
                  stakingUpdate.source,
                  pendingSpendAction.updateHash,
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

                dataUpdate = UpdatedStateDataUpdate(
                  PendingSpendTransactions,
                  Confirmed,
                  OperationType.Staking,
                  pendingSpendAction.update.asInstanceOf[Signed[AmmUpdate]],
                  pendingSpendAction.updateHash,
                  None
                )

              } yield
                oldState
                  .focus(_.onChain.updatedStateDataUpdate)
                  .modify(_ + dataUpdate)
                  .focus(_.calculated)
                  .replace(updatedCalculatedState)
            }
        } yield result

        combinedState.foldF(
          failed =>
            for {
              _ <- logger.error(s"Failed to process pending spend action: ${failed.reason}")
              result <- handleFailedUpdate(
                pendingSpendAction.update,
                oldState,
                failed,
                stakingCalculatedState,
                pendingSpendAction.updateHash,
                PendingSpendTransactions
              )
            } yield result,
          success => logger.info("Successfully processed pending spend action") >> success.pure[F]
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
