package org.amm_metagraph.shared_data.services.combiners

import cats.data.EitherT
import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact.{SharedArtifact, SpendAction}
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.{AllowSpend, CurrencyId}
import io.constellationnetwork.security.Hashed
import io.constellationnetwork.security.signature.Signed

import eu.timepit.refined.cats.refTypeOrder
import eu.timepit.refined.types.numeric.NonNegLong
import monocle.syntax.all._
import org.amm_metagraph.shared_data.AllowSpends.getAllAllowSpendsInUseFromState
import org.amm_metagraph.shared_data.SpendTransactions.generateSpendAction
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.globalSnapshots.getAllowSpendGlobalSnapshotsState
import org.amm_metagraph.shared_data.services.pricing.PricingService
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.Swap._
import org.amm_metagraph.shared_data.types.codecs.{HasherSelector, JsonWithBase64BinaryCodec}
import org.amm_metagraph.shared_data.validations.SharedValidations.validateIfAllowSpendsAreDuplicated
import org.amm_metagraph.shared_data.validations.{StakingValidations, SwapValidations}

trait SwapCombinerService[F[_]] {
  def combineNew(
    signedUpdate: Signed[SwapUpdate],
    oldState: DataState[AmmOnChainState, AmmCalculatedState],
    globalEpochProgress: EpochProgress,
    lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
    currencyId: CurrencyId
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]]

  def combinePendingAllowSpend(
    pendingSignedUpdate: Signed[SwapUpdate],
    oldState: DataState[AmmOnChainState, AmmCalculatedState],
    globalEpochProgress: EpochProgress,
    lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
    currencyId: CurrencyId
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]]

  def combinePendingSpendAction(
    pendingSpendAction: PendingSpendAction[SwapUpdate],
    oldState: DataState[AmmOnChainState, AmmCalculatedState],
    globalEpochProgress: EpochProgress,
    spendActions: List[SpendAction],
    currentSnapshotOrdinal: SnapshotOrdinal,
    currencyId: CurrencyId
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]]
}

object SwapCombinerService {
  def make[F[_]: Async: HasherSelector](
    pricingService: PricingService[F],
    swapValidations: SwapValidations[F],
    dataUpdateCodec: JsonWithBase64BinaryCodec[F, AmmUpdate]
  ): SwapCombinerService[F] =
    new SwapCombinerService[F] {
      private def handleFailedUpdate(
        updates: List[AmmUpdate],
        swapUpdate: Signed[SwapUpdate],
        acc: DataState[AmmOnChainState, AmmCalculatedState],
        failedCalculatedState: FailedCalculatedState,
        swapCalculatedState: SwapCalculatedState
      ) = {
        val updatedSwapCalculatedState = swapCalculatedState
          .focus(_.failed)
          .modify(_ + failedCalculatedState)
          .focus(_.pending)
          .modify(_.filter(_.update =!= swapUpdate))

        val updatedCalculatedState = acc.calculated
          .focus(_.operations)
          .modify(_.updated(OperationType.Swap, updatedSwapCalculatedState))

        DataState(
          AmmOnChainState(updates),
          updatedCalculatedState
        )
      }

      private def getUpdateAllowSpend(
        swapUpdate: SwapUpdate,
        lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]]
      ) =
        HasherSelector[F].withBrotli { implicit hs =>
          getAllowSpendGlobalSnapshotsState(
            swapUpdate.allowSpendReference,
            swapUpdate.swapFromPair,
            lastGlobalSnapshotsAllowSpends
          )
        }

      private def removePendingAllowSpend(
        swapCalculatedState: SwapCalculatedState,
        signedSwapUpdate: Signed[SwapUpdate]
      ) =
        swapCalculatedState.pending.filterNot {
          case PendingAllowSpend(update) if update === signedSwapUpdate => true
          case _                                                        => false
        }

      private def removePendingSpendAction(
        swapCalculatedState: SwapCalculatedState,
        signedSwapUpdate: Signed[SwapUpdate]
      ) =
        swapCalculatedState.pending.filterNot {
          case PendingSpendAction(update, _) if update === signedSwapUpdate => true
          case _                                                            => false
        }

      def combineNew(
        signedUpdate: Signed[SwapUpdate],
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        globalEpochProgress: EpochProgress,
        lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
        currencyId: CurrencyId
      )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
        val swapUpdate = signedUpdate.value
        val swapCalculatedState = getSwapCalculatedState(oldState.calculated)
        val confirmedSwaps = swapCalculatedState.confirmed.value.getOrElse(signedUpdate.source, Set.empty)
        val pendingSwaps = swapCalculatedState.getPendingUpdates
        val swapCalculatedStatePendingAllowSpend = swapCalculatedState.pending

        val updates = swapUpdate :: oldState.onChain.updates

        val combinedState = for {
          _ <- EitherT.fromEither(
            swapValidations.combinerContextualValidations(
              oldState.calculated,
              signedUpdate,
              globalEpochProgress,
              confirmedSwaps,
              pendingSwaps,
              isNewUpdate = true
            )
          )
          updateAllowSpends <- EitherT.liftF(getUpdateAllowSpend(swapUpdate, lastGlobalSnapshotsAllowSpends))
          response <- updateAllowSpends match {
            case None =>
              val updatedPendingSwapsCalculatedState = swapCalculatedStatePendingAllowSpend + PendingAllowSpend(signedUpdate)
              val newSwapState = swapCalculatedState
                .focus(_.pending)
                .replace(updatedPendingSwapsCalculatedState)

              val updatedCalculatedState = oldState.calculated
                .focus(_.operations)
                .modify(_.updated(OperationType.Swap, newSwapState))

              val result = DataState(
                AmmOnChainState(updates),
                updatedCalculatedState,
                oldState.sharedArtifacts
              )
              EitherT.rightT[F, FailedCalculatedState](result)

            case Some(_) =>
              EitherT.liftF[F, FailedCalculatedState, DataState[AmmOnChainState, AmmCalculatedState]](
                combinePendingAllowSpend(
                  signedUpdate,
                  oldState,
                  globalEpochProgress,
                  lastGlobalSnapshotsAllowSpends,
                  currencyId
                )
              )
          }
        } yield response

        combinedState.valueOr { failedCalculatedState =>
          handleFailedUpdate(updates, signedUpdate, oldState, failedCalculatedState, swapCalculatedState)
        }
      }

      def combinePendingAllowSpend(
        signedUpdate: Signed[SwapUpdate],
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        globalEpochProgress: EpochProgress,
        lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
        currencyId: CurrencyId
      )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
        val swapCalculatedState = getSwapCalculatedState(oldState.calculated)
        val swapUpdate = signedUpdate.value
        val updates = swapUpdate :: oldState.onChain.updates

        val combinedState: EitherT[F, FailedCalculatedState, DataState[AmmOnChainState, AmmCalculatedState]] = for {
          _ <- EitherT.fromEither[F](
            swapValidations.combinerContextualValidations(
              oldState.calculated,
              signedUpdate,
              globalEpochProgress,
              Set.empty,
              Set.empty,
              isNewUpdate = false
            )
          )

          updateAllowSpends <- EitherT.liftF(getUpdateAllowSpend(swapUpdate, lastGlobalSnapshotsAllowSpends))

          result <- updateAllowSpends match {
            case Some(allowSpendToken) =>
              for {
                poolId <- EitherT.liftF(buildLiquidityPoolUniqueIdentifier(swapUpdate.swapFromPair, swapUpdate.swapToPair))
                updatedTokenInformation <- EitherT(pricingService.getSwapTokenInfo(signedUpdate, poolId, globalEpochProgress))
                _ <- EitherT.fromEither[F](
                  swapValidations.combinerValidations(
                    oldState.calculated,
                    signedUpdate,
                    globalEpochProgress,
                    Set.empty,
                    Set.empty,
                    isNewUpdate = false,
                    currencyId,
                    updatedTokenInformation,
                    allowSpendToken
                  )
                )

                spendActionToken = generateSpendAction(
                  allowSpendToken,
                  swapUpdate.amountIn,
                  updatedTokenInformation.pairTokenInformation.identifier,
                  updatedTokenInformation.netReceived,
                  currencyId.value
                )

                updatedPendingAllowSpendCalculatedState =
                  removePendingAllowSpend(swapCalculatedState, signedUpdate)

                updatedPendingSpendActionCalculatedState =
                  updatedPendingAllowSpendCalculatedState + PendingSpendAction(signedUpdate, spendActionToken)

                updatedPendingSwapCalculatedState =
                  swapCalculatedState
                    .focus(_.pending)
                    .replace(updatedPendingSpendActionCalculatedState)

                updatedCalculatedState = oldState.calculated
                  .focus(_.operations)
                  .modify(_.updated(OperationType.Swap, updatedPendingSwapCalculatedState))

                updatedSharedArtifacts = oldState.sharedArtifacts ++ SortedSet[SharedArtifact](spendActionToken)
              } yield
                DataState(
                  AmmOnChainState(updates),
                  updatedCalculatedState,
                  updatedSharedArtifacts
                )

            case None =>
              EitherT.rightT[F, FailedCalculatedState](oldState)
          }
        } yield result

        combinedState.valueOr { failedCalculatedState =>
          handleFailedUpdate(updates, signedUpdate, oldState, failedCalculatedState, swapCalculatedState)
        }
      }

      def combinePendingSpendAction(
        pendingSpendAction: PendingSpendAction[SwapUpdate],
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        globalEpochProgress: EpochProgress,
        spendActions: List[SpendAction],
        currentSnapshotOrdinal: SnapshotOrdinal,
        currencyId: CurrencyId
      )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
        val signedSwapUpdate = pendingSpendAction.update
        val liquidityPoolsCalculatedState = getLiquidityPoolCalculatedState(oldState.calculated)
        val swapCalculatedState = getSwapCalculatedState(oldState.calculated)

        val swapUpdate = pendingSpendAction.update.value
        val updates = swapUpdate :: oldState.onChain.updates

        val combinedState: EitherT[F, FailedCalculatedState, DataState[AmmOnChainState, AmmCalculatedState]] = for {
          poolId <- EitherT.liftF(buildLiquidityPoolUniqueIdentifier(swapUpdate.swapFromPair, swapUpdate.swapToPair))
          liquidityPool <- EitherT.liftF(getLiquidityPoolByPoolId(liquidityPoolsCalculatedState.confirmed.value, poolId))
          updatedTokenInformation <- EitherT(pricingService.getSwapTokenInfo(pendingSpendAction.update, poolId, globalEpochProgress))
          swapReference <- EitherT.liftF(HasherSelector[F].withCurrent(implicit hs => SwapReference.of(signedSwapUpdate)))
          swapUpdateHashed <- EitherT.liftF(
            HasherSelector[F].withCurrent(implicit hs => signedSwapUpdate.toHashed(dataUpdateCodec.serialize))
          )
          sourceAddress = signedSwapUpdate.source
          _ <- EitherT.fromEither[F](
            swapValidations.combinerContextualValidations(
              oldState.calculated,
              signedSwapUpdate,
              globalEpochProgress,
              Set.empty,
              Set.empty,
              isNewUpdate = false
            )
          )
          liquidityPoolUpdated <- EitherT.fromEither[F](
            pricingService.getUpdatedLiquidityPoolDueSwap(
              liquidityPool,
              updatedTokenInformation.primaryTokenInformation,
              updatedTokenInformation.pairTokenInformation,
              updatedTokenInformation.grossReceived,
              currencyId
            )
          )
          swapCalculatedStateAddress = SwapCalculatedStateAddress(
            swapUpdateHashed.hash,
            sourceAddress,
            updatedTokenInformation.primaryTokenInformation,
            updatedTokenInformation.pairTokenInformation,
            swapUpdate.allowSpendReference,
            swapUpdate.amountIn,
            updatedTokenInformation.grossReceived,
            updatedTokenInformation.netReceived,
            swapUpdate.amountOutMinimum,
            swapUpdate.maxValidGsEpochProgress,
            poolId.some,
            currentSnapshotOrdinal,
            swapReference
          )

          updatedPendingCalculatedState = removePendingSpendAction(swapCalculatedState, signedSwapUpdate)
          newSwapState = swapCalculatedState
            .focus(_.confirmed.value)
            .modify(current =>
              current.updatedWith(sourceAddress) {
                case Some(confirmedSwaps) => Some(confirmedSwaps + swapCalculatedStateAddress)
                case None                 => Some(Set(swapCalculatedStateAddress))
              }
            )
            .focus(_.pending)
            .replace(updatedPendingCalculatedState)

          newLiquidityPoolState =
            liquidityPoolsCalculatedState
              .focus(_.confirmed.value)
              .modify(_.updated(poolId.value, liquidityPoolUpdated))

          updatedCalculatedState = oldState.calculated
            .focus(_.operations)
            .modify(_.updated(OperationType.Swap, newSwapState))
            .focus(_.operations)
            .modify(_.updated(OperationType.LiquidityPool, newLiquidityPoolState))
        } yield
          DataState(
            AmmOnChainState(updates),
            updatedCalculatedState,
            oldState.sharedArtifacts
          )

        combinedState.valueOr { failedCalculatedState =>
          handleFailedUpdate(updates, signedSwapUpdate, oldState, failedCalculatedState, swapCalculatedState)
        }
      }
    }
}
