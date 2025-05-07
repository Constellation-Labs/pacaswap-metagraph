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
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hashed, Hasher}

import eu.timepit.refined.types.numeric.NonNegLong
import monocle.syntax.all._
import org.amm_metagraph.shared_data.AllowSpends.getAllAllowSpendsInUseFromState
import org.amm_metagraph.shared_data.SpendTransactions.{checkIfSpendActionAcceptedInGl0, generateSpendAction}
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.globalSnapshots.getAllowSpendsGlobalSnapshotsState
import org.amm_metagraph.shared_data.services.pricing.PricingService
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, StakingUpdate}
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.Staking._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.codecs.{HasherSelector, JsonWithBase64BinaryCodec}
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
}

object StakingCombinerService {
  def make[F[_]: Async: HasherSelector](
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
        updates: List[AmmUpdate],
        stakingUpdate: Signed[StakingUpdate],
        acc: DataState[AmmOnChainState, AmmCalculatedState],
        failedCalculatedState: FailedCalculatedState,
        stakingCalculatedState: StakingCalculatedState
      ) = {
        val updatedStakingCalculatedState = stakingCalculatedState
          .focus(_.failed)
          .modify(_ + failedCalculatedState)
          .focus(_.pending)
          .modify(_.filter(_.update =!= stakingUpdate))

        val updatedCalculatedState = acc.calculated
          .focus(_.operations)
          .modify(_.updated(OperationType.Staking, updatedStakingCalculatedState))

        DataState(
          AmmOnChainState(updates),
          updatedCalculatedState
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
        val confirmedStakings = stakingCalculatedState.confirmed.value.getOrElse(signedUpdate.source, Set.empty)
        val pendingStakings = stakingCalculatedState.getPendingUpdates
        val pendingAllowSpendsCalculatedState = stakingCalculatedState.pending

        val stakingUpdate = signedUpdate.value
        val updates = stakingUpdate :: oldState.onChain.updates
        val combinedState = for {
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
          updateHashed <- EitherT.liftF(
            HasherSelector[F].withCurrent(implicit hs => signedUpdate.toHashed(dataUpdateCodec.serialize))
          )

          response <- updateAllowSpends match {
            case (None, _) | (_, None) =>
              val updatedPendingCalculatedState = pendingAllowSpendsCalculatedState + PendingAllowSpend(signedUpdate, updateHashed.hash)
              val newStakingState = stakingCalculatedState
                .focus(_.pending)
                .replace(updatedPendingCalculatedState)

              val updatedCalculatedState = oldState.calculated
                .focus(_.operations)
                .modify(_.updated(OperationType.Staking, newStakingState))

              val result = DataState(
                AmmOnChainState(updates),
                updatedCalculatedState,
                oldState.sharedArtifacts
              )

              EitherT.rightT[F, FailedCalculatedState](result)

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

        combinedState.valueOr { failedCalculatedState =>
          handleFailedUpdate(updates, signedUpdate, oldState, failedCalculatedState, stakingCalculatedState)
        }
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
        val updates = stakingUpdate :: oldState.onChain.updates

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

                updatedPendingSpendActionCalculatedState =
                  updatedPendingAllowSpendCalculatedState + PendingSpendAction(
                    pendingAllowSpendUpdate.update,
                    pendingAllowSpendUpdate.updateHash,
                    spendAction
                  )

                updatedStakingCalculatedState =
                  stakingCalculatedState
                    .focus(_.pending)
                    .replace(updatedPendingSpendActionCalculatedState)

                updatedCalculatedState = oldState.calculated
                  .focus(_.operations)
                  .modify(_.updated(OperationType.Staking, updatedStakingCalculatedState))

                updatedSharedArtifacts = oldState.sharedArtifacts ++ SortedSet[SharedArtifact](spendAction)
              } yield
                DataState(
                  AmmOnChainState(updates),
                  updatedCalculatedState,
                  updatedSharedArtifacts
                )

            case _ =>
              EitherT.rightT[F, FailedCalculatedState](oldState)
          }
        } yield result

        combinedState.valueOr { failedCalculatedState =>
          handleFailedUpdate(updates, pendingAllowSpendUpdate.update, oldState, failedCalculatedState, stakingCalculatedState)
        }
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
        val updates = stakingUpdate :: oldState.onChain.updates

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

                updatedStakingCalculatedState = stakingCalculatedState
                  .focus(_.confirmed.value)
                  .modify(_.updatedWith(sourceAddress) {
                    case Some(confirmedSwaps) => Some(confirmedSwaps + stakingCalculatedStateAddress)
                    case None                 => Some(Set(stakingCalculatedStateAddress))
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
                DataState(
                  AmmOnChainState(updates),
                  updatedCalculatedState,
                  oldState.sharedArtifacts
                )
            }
        } yield result

        combinedState.valueOr { failedCalculatedState =>
          handleFailedUpdate(updates, signedStakingUpdate, oldState, failedCalculatedState, stakingCalculatedState)
        }
      }
    }
}
