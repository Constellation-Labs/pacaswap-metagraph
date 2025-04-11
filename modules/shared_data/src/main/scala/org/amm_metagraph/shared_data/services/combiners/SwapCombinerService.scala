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
import org.amm_metagraph.shared_data.SpendTransactions.generateSpendAction
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.globalSnapshots.getAllowSpendGlobalSnapshotsState
import org.amm_metagraph.shared_data.services.pricing.PricingService
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.Swap._
import org.amm_metagraph.shared_data.types.codecs.{HasherSelector, JsonWithBase64BinaryCodec}

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
    applicationConfig: ApplicationConfig,
    pricingService: PricingService[F],
    dataUpdateCodec: JsonWithBase64BinaryCodec[F, AmmUpdate]
  ): SwapCombinerService[F] =
    new SwapCombinerService[F] {
      private def validateUpdate(
        signedUpdate: Signed[SwapUpdate],
        maybeTokenInformation: Option[SwapTokenInfo],
        maybeAllowSpendToken: Option[Hashed[AllowSpend]],
        lastSyncGlobalEpochProgress: EpochProgress,
        confirmedSwaps: Set[SwapCalculatedStateAddress],
        pendingSwaps: Set[Signed[SwapUpdate]]
      ): Either[FailedCalculatedState, Signed[SwapUpdate]] = {
        val expireEpochProgress = EpochProgress(
          NonNegLong
            .from(
              lastSyncGlobalEpochProgress.value.value + applicationConfig.failedOperationsExpirationEpochProgresses.value.value
            )
            .getOrElse(NonNegLong.MinValue)
        )

        def failWith(reason: FailedCalculatedStateReason): Left[FailedCalculatedState, Signed[SwapUpdate]] =
          Left(FailedCalculatedState(reason, expireEpochProgress, signedUpdate))

        if (signedUpdate.maxValidGsEpochProgress < lastSyncGlobalEpochProgress) {
          failWith(OperationExpired(signedUpdate))
        } else if (
          confirmedSwaps.exists(swap => swap.allowSpendReference === signedUpdate.allowSpendReference) ||
          pendingSwaps.exists(swap => swap.allowSpendReference === signedUpdate.allowSpendReference)
        ) {
          failWith(DuplicatedSwapRequest(signedUpdate))
        } else {
          (maybeTokenInformation, maybeAllowSpendToken) match {
            case (Some(updatedTokenInformation), Some(allowSpend)) =>
              if (signedUpdate.amountIn.value > allowSpend.amount.value) {
                failWith(AmountGreaterThanAllowSpendLimit(allowSpend.signed.value))
              } else if (updatedTokenInformation.netReceived < signedUpdate.amountOutMinimum) {
                failWith(SwapLessThanMinAmount())
              } else if (allowSpend.lastValidEpochProgress < lastSyncGlobalEpochProgress) {
                failWith(AllowSpendExpired(allowSpend.signed.value))
              } else if (
                updatedTokenInformation.primaryTokenInformation.amount < applicationConfig.minTokensLiquidityPool ||
                updatedTokenInformation.pairTokenInformation.amount < applicationConfig.minTokensLiquidityPool
              ) {
                failWith(SwapWouldDrainPoolBalance())
              } else {
                Right(signedUpdate)
              }
            case _ => Right(signedUpdate)
          }
        }
      }

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
            validateUpdate(signedUpdate, none, none, globalEpochProgress, confirmedSwaps, pendingSwaps)
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
            validateUpdate(
              signedUpdate,
              none,
              none,
              globalEpochProgress,
              Set.empty,
              Set.empty
            )
          )

          updateAllowSpends <- EitherT.liftF(getUpdateAllowSpend(swapUpdate, lastGlobalSnapshotsAllowSpends))

          result <- updateAllowSpends match {
            case Some(allowSpendToken) =>
              for {
                poolId <- EitherT.liftF(buildLiquidityPoolUniqueIdentifier(swapUpdate.swapFromPair, swapUpdate.swapToPair))
                updatedTokenInformation <- EitherT(pricingService.getSwapTokenInfo(signedUpdate, poolId, globalEpochProgress))
                _ <- EitherT.fromEither[F](
                  validateUpdate(
                    signedUpdate,
                    updatedTokenInformation.some,
                    allowSpendToken.some,
                    globalEpochProgress,
                    Set.empty,
                    Set.empty
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
            validateUpdate(signedSwapUpdate, updatedTokenInformation.some, none, globalEpochProgress, Set.empty, Set.empty)
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
