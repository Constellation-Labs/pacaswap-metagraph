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
import io.constellationnetwork.security.signature.Signed

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
import org.amm_metagraph.shared_data.validations.Errors._
import org.amm_metagraph.shared_data.validations.SwapValidations

trait SwapCombinerService[F[_]] {
  def combineNew(
    signedUpdate: Signed[SwapUpdate],
    oldState: DataState[AmmOnChainState, AmmCalculatedState],
    globalEpochProgress: EpochProgress,
    lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
    currencyId: CurrencyId
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]]

  def combinePendingAllowSpend(
    pendingSignedUpdate: PendingAllowSpend[SwapUpdate],
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
    swapValidations: SwapValidations[F],
    dataUpdateCodec: JsonWithBase64BinaryCodec[F, AmmUpdate]
  ): SwapCombinerService[F] =
    new SwapCombinerService[F] {
      private def rollbackAmountInLPs(
        pendingAllowSpend: PendingAction[SwapUpdate],
        lastSyncGlobalSnapshotEpoch: EpochProgress,
        maybePricingTokenInfo: Option[PricingTokenInfo],
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        currencyId: CurrencyId
      ): F[DataState[AmmOnChainState, AmmCalculatedState]] = maybePricingTokenInfo match {

        case Some(SwapTokenInfo(primaryTokenInformationUpdated, pairTokenInformationUpdated, amountIn, grossReceived, _)) =>
          (for {
            poolId <- EitherT.liftF(
              buildLiquidityPoolUniqueIdentifier(
                primaryTokenInformationUpdated.identifier,
                pairTokenInformationUpdated.identifier
              )
            )

            liquidityPoolCalculatedState = getLiquidityPoolCalculatedState(oldState.calculated)

            liquidityPool <- EitherT.liftF(
              getLiquidityPoolByPoolId(liquidityPoolCalculatedState.confirmed.value, poolId)
            )

            updatedLiquidityPool <- EitherT.fromEither(
              pricingService.rollbackSwapLiquidityPoolAmounts(
                pendingAllowSpend.update,
                pendingAllowSpend.updateHash,
                lastSyncGlobalSnapshotEpoch,
                liquidityPool,
                amountIn,
                grossReceived,
                currencyId
              )
            )

            updatedState = {
              val newLiquidityPoolState = liquidityPoolCalculatedState
                .focus(_.confirmed.value)
                .modify(_.updated(poolId.value, updatedLiquidityPool))

              val updatedCalculatedState = oldState.calculated
                .focus(_.operations)
                .modify(_.updated(OperationType.LiquidityPool, newLiquidityPoolState))

              oldState.copy(calculated = updatedCalculatedState)
            }
          } yield updatedState).valueOrF(_ => oldState.pure)

        case Some(_: WithdrawalTokenAmounts) => oldState.pure
        case None                            => oldState.pure
      }

      private def handleFailedUpdate(
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

        acc
          .focus(_.onChain.updates)
          .modify(current => current + swapUpdate)
          .focus(_.calculated)
          .replace(updatedCalculatedState)
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
          case PendingAllowSpend(update, _, _) if update === signedSwapUpdate => true
          case _                                                              => false
        }

      private def removePendingSpendAction(
        swapCalculatedState: SwapCalculatedState,
        signedSwapUpdate: Signed[SwapUpdate]
      ) =
        swapCalculatedState.pending.filterNot {
          case PendingSpendAction(update, _, _, _) if update === signedSwapUpdate => true
          case _                                                                  => false
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
        val liquidityPoolsCalculatedState = getLiquidityPoolCalculatedState(oldState.calculated)

        val combinedState = for {
          _ <- EitherT.fromEither(
            swapValidations.newUpdateValidations(
              oldState.calculated,
              signedUpdate,
              globalEpochProgress,
              confirmedSwaps,
              pendingSwaps
            )
          )
          updateAllowSpends <- EitherT.liftF(getUpdateAllowSpend(swapUpdate, lastGlobalSnapshotsAllowSpends))
          poolId <- EitherT.liftF(buildLiquidityPoolUniqueIdentifier(swapUpdate.swapFromPair, swapUpdate.swapToPair))
          liquidityPool <- EitherT.liftF(getLiquidityPoolByPoolId(liquidityPoolsCalculatedState.confirmed.value, poolId))
          updatedTokenInformation <- EitherT(pricingService.getSwapTokenInfo(signedUpdate, poolId, globalEpochProgress))
          updateHashed <- EitherT.liftF(
            HasherSelector[F].withCurrent(implicit hs => signedUpdate.toHashed(dataUpdateCodec.serialize))
          )
          liquidityPoolUpdated <- EitherT.fromEither[F](
            pricingService.getUpdatedLiquidityPoolDueNewSwap(
              updateHashed,
              liquidityPool,
              updatedTokenInformation.primaryTokenInformationUpdated,
              updatedTokenInformation.pairTokenInformationUpdated,
              updatedTokenInformation.grossReceived,
              currencyId
            )
          )
          newLiquidityPoolState =
            liquidityPoolsCalculatedState
              .focus(_.confirmed.value)
              .modify(_.updated(poolId.value, liquidityPoolUpdated))

          updatedCalculatedState = oldState.calculated
            .focus(_.operations)
            .modify(_.updated(OperationType.LiquidityPool, newLiquidityPoolState))

          updatedLpState = oldState
            .focus(_.calculated)
            .replace(updatedCalculatedState)

          response <- updateAllowSpends match {
            case None =>
              val updatedPendingSwapsCalculatedState =
                swapCalculatedStatePendingAllowSpend + PendingAllowSpend(signedUpdate, updateHashed.hash, updatedTokenInformation.some)
              val newSwapState = swapCalculatedState
                .focus(_.pending)
                .replace(updatedPendingSwapsCalculatedState)

              val updatedCalculatedState = updatedLpState.calculated
                .focus(_.operations)
                .modify(_.updated(OperationType.Swap, newSwapState))

              EitherT.rightT[F, FailedCalculatedState](
                oldState
                  .focus(_.onChain.updates)
                  .modify(current => current + swapUpdate)
                  .focus(_.calculated)
                  .replace(updatedCalculatedState)
              )

            case Some(_) =>
              EitherT.liftF[F, FailedCalculatedState, DataState[AmmOnChainState, AmmCalculatedState]](
                combinePendingAllowSpend(
                  PendingAllowSpend(signedUpdate, updateHashed.hash, updatedTokenInformation.some),
                  updatedLpState,
                  globalEpochProgress,
                  lastGlobalSnapshotsAllowSpends,
                  currencyId
                )
              )
          }
        } yield response

        combinedState.valueOr { failedCalculatedState =>
          handleFailedUpdate(signedUpdate, oldState, failedCalculatedState, swapCalculatedState)
        }
      }

      def combinePendingAllowSpend(
        pendingAllowSpend: PendingAllowSpend[SwapUpdate],
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        globalEpochProgress: EpochProgress,
        lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
        currencyId: CurrencyId
      )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
        val swapCalculatedState = getSwapCalculatedState(oldState.calculated)
        val swapUpdate = pendingAllowSpend.update.value

        val combinedState: EitherT[F, FailedCalculatedState, DataState[AmmOnChainState, AmmCalculatedState]] = for {
          updateAllowSpends <- EitherT.liftF(getUpdateAllowSpend(pendingAllowSpend.update, lastGlobalSnapshotsAllowSpends))

          result <- updateAllowSpends match {
            case Some(allowSpendToken) =>
              val maybeSwapTokenInfo = pendingAllowSpend.pricingTokenInfo.collect {
                case swapTokenInfo: SwapTokenInfo => swapTokenInfo
              }

              for {
                updatedTokenInformation <- EitherT.fromOption[F](
                  maybeSwapTokenInfo,
                  FailedCalculatedState(
                    MissingSwapTokenInfo(),
                    getFailureExpireEpochProgress(applicationConfig, globalEpochProgress),
                    pendingAllowSpend.update
                  )
                )

                _ <- EitherT.fromEither[F](
                  swapValidations.pendingAllowSpendsValidations(
                    pendingAllowSpend.update,
                    globalEpochProgress,
                    currencyId,
                    updatedTokenInformation,
                    allowSpendToken
                  )
                )

                spendActionToken = generateSpendAction(
                  allowSpendToken,
                  swapUpdate.amountIn,
                  updatedTokenInformation.pairTokenInformationUpdated.identifier,
                  updatedTokenInformation.netReceived,
                  currencyId.value
                )

                updatedPendingAllowSpendCalculatedState =
                  removePendingAllowSpend(swapCalculatedState, pendingAllowSpend.update)

                updatedPendingSpendActionCalculatedState =
                  updatedPendingAllowSpendCalculatedState + PendingSpendAction(
                    pendingAllowSpend.update,
                    pendingAllowSpend.updateHash,
                    spendActionToken,
                    pendingAllowSpend.pricingTokenInfo
                  )

                updatedPendingSwapCalculatedState =
                  swapCalculatedState
                    .focus(_.pending)
                    .replace(updatedPendingSpendActionCalculatedState)

                updatedCalculatedState = oldState.calculated
                  .focus(_.operations)
                  .modify(_.updated(OperationType.Swap, updatedPendingSwapCalculatedState))

              } yield
                oldState
                  .focus(_.onChain.updates)
                  .modify(current => current + swapUpdate)
                  .focus(_.calculated)
                  .replace(updatedCalculatedState)
                  .focus(_.sharedArtifacts)
                  .modify(current =>
                    current ++ SortedSet[SharedArtifact](
                      spendActionToken
                    )
                  )

            case None =>
              EitherT.rightT[F, FailedCalculatedState](oldState)
          }
        } yield result

        combinedState.valueOrF { failedCalculatedState =>
          rollbackAmountInLPs(
            pendingAllowSpend,
            globalEpochProgress,
            pendingAllowSpend.pricingTokenInfo,
            oldState,
            currencyId
          ).map { rolledBackState =>
            handleFailedUpdate(
              pendingAllowSpend.update,
              rolledBackState,
              failedCalculatedState,
              swapCalculatedState
            )
          }
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
        val swapCalculatedState = getSwapCalculatedState(oldState.calculated)
        val liquidityPoolsCalculatedState = getLiquidityPoolCalculatedState(oldState.calculated)
        val swapUpdate = pendingSpendAction.update.value

        val combinedState: EitherT[F, FailedCalculatedState, DataState[AmmOnChainState, AmmCalculatedState]] = for {
          swapReference <- EitherT.liftF(HasherSelector[F].withCurrent(implicit hs => SwapReference.of(signedSwapUpdate)))
          maybeSwapTokenInfo = pendingSpendAction.pricingTokenInfo.collect {
            case swapTokenInfo: SwapTokenInfo => swapTokenInfo
          }

          updatedTokenInformation <- EitherT.fromOption[F](
            maybeSwapTokenInfo,
            FailedCalculatedState(
              MissingSwapTokenInfo(),
              getFailureExpireEpochProgress(applicationConfig, globalEpochProgress),
              pendingSpendAction.update
            )
          )

          sourceAddress = signedSwapUpdate.source
          _ <- EitherT.fromEither[F](
            swapValidations.pendingSpendActionsValidation(
              signedSwapUpdate,
              globalEpochProgress
            )
          )
          poolId <- EitherT.liftF(buildLiquidityPoolUniqueIdentifier(swapUpdate.swapFromPair, swapUpdate.swapToPair))

          swapCalculatedStateAddress = SwapCalculatedStateAddress(
            pendingSpendAction.updateHash,
            sourceAddress,
            updatedTokenInformation.primaryTokenInformationUpdated,
            updatedTokenInformation.pairTokenInformationUpdated,
            swapUpdate.allowSpendReference,
            swapUpdate.amountIn,
            updatedTokenInformation.grossReceived,
            updatedTokenInformation.netReceived,
            swapUpdate.amountOutMinimum,
            swapUpdate.amountOutMaximum,
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

          poolId <- EitherT.liftF(buildLiquidityPoolUniqueIdentifier(swapUpdate.swapFromPair, swapUpdate.swapToPair))
          liquidityPool <- EitherT.liftF(getLiquidityPoolByPoolId(liquidityPoolsCalculatedState.confirmed.value, poolId))
          liquidityPoolUpdated <- EitherT.fromEither[F](
            pricingService.getUpdatedLiquidityPoolDueConfirmedSwap(
              pendingSpendAction.updateHash,
              liquidityPool
            )
          )

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
          oldState
            .focus(_.onChain.updates)
            .modify(current => current + swapUpdate)
            .focus(_.calculated)
            .replace(updatedCalculatedState)

        combinedState.valueOrF { failedCalculatedState =>
          rollbackAmountInLPs(
            pendingSpendAction,
            globalEpochProgress,
            pendingSpendAction.pricingTokenInfo,
            oldState,
            currencyId
          ).map { rolledBackState =>
            handleFailedUpdate(
              pendingSpendAction.update,
              rolledBackState,
              failedCalculatedState,
              swapCalculatedState
            )
          }
        }
      }
    }

  private def getFailureExpireEpochProgress(
    applicationConfig: ApplicationConfig,
    lastSyncGlobalEpochProgress: EpochProgress
  ) = {
    val expireEpochProgress = EpochProgress(
      NonNegLong
        .from(
          lastSyncGlobalEpochProgress.value.value + applicationConfig.failedOperationsExpirationEpochProgresses.value.value
        )
        .getOrElse(NonNegLong.MinValue)
    )
    expireEpochProgress
  }
}
