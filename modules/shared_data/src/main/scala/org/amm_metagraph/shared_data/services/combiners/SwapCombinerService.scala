package org.amm_metagraph.shared_data.services.combiners

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
import io.constellationnetwork.security.{Hashed, Hasher, SecurityProvider}

import eu.timepit.refined.types.numeric.NonNegLong
import monocle.syntax.all._
import org.amm_metagraph.shared_data.SpendTransactions.{checkIfSpendActionAcceptedInGl0, generateSpendAction}
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.globalSnapshots.getAllowSpendGlobalSnapshotsState
import org.amm_metagraph.shared_data.services.pricing.PricingService
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.Swap._
import org.amm_metagraph.shared_data.types.codecs.HasherSelector

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
    currentSnapshotOrdinal: SnapshotOrdinal
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]]
}

object SwapCombinerService {
  def make[F[_]: Async: HasherSelector: SecurityProvider](
    applicationConfig: ApplicationConfig,
    pricingService: PricingService[F]
  ): F[SwapCombinerService[F]] = Async[F].delay {
    new SwapCombinerService[F] {
      private def updateLiquidityPool(
        liquidityPool: LiquidityPool,
        fromTokenInfo: TokenInformation,
        toTokenInfo: TokenInformation
      ): LiquidityPool = {
        val tokenA = if (liquidityPool.tokenA.identifier == fromTokenInfo.identifier) fromTokenInfo else toTokenInfo
        val tokenB = if (liquidityPool.tokenB.identifier == toTokenInfo.identifier) toTokenInfo else fromTokenInfo

        liquidityPool.copy(
          tokenA = tokenA,
          tokenB = tokenB
        )
      }

      private def validateUpdate(
        signedUpdate: Signed[SwapUpdate],
        maybeTokenInformation: Option[SwapTokenInfo],
        maybeAllowSpendToken: Option[Hashed[AllowSpend]],
        lastSyncGlobalEpochProgress: EpochProgress
      ): Option[FailedCalculatedState] =
        if (signedUpdate.maxValidGsEpochProgress < lastSyncGlobalEpochProgress) {
          FailedCalculatedState(
            OperationExpired(signedUpdate),
            EpochProgress(NonNegLong.unsafeFrom(lastSyncGlobalEpochProgress.value.value + 30L)),
            signedUpdate
          ).some
        } else {
          (maybeTokenInformation, maybeAllowSpendToken).mapN { (updatedTokenInformation, allowSpend) =>
            if (updatedTokenInformation.receivedAmount.value.value < signedUpdate.minAmount.value.value) {
              FailedCalculatedState(
                SwapLessThanMinAmount(),
                EpochProgress(
                  NonNegLong.unsafeFrom(
                    lastSyncGlobalEpochProgress.value.value + applicationConfig.failedOperationsExpirationEpochProgresses.value.value
                  )
                ),
                signedUpdate
              ).some
            } else if (signedUpdate.minPrice.exists(p => updatedTokenInformation.effectivePrice.value.value < p.value)) {
              FailedCalculatedState(
                SwapPriceBelowAcceptableMinPrice(),
                EpochProgress(
                  NonNegLong.unsafeFrom(
                    lastSyncGlobalEpochProgress.value.value + applicationConfig.failedOperationsExpirationEpochProgresses.value.value
                  )
                ),
                signedUpdate
              ).some
            } else if (signedUpdate.maxPrice.exists(p => updatedTokenInformation.effectivePrice.value.value > p.value)) {
              FailedCalculatedState(
                SwapPriceExceedsAcceptableMaxPrice(),
                EpochProgress(
                  NonNegLong.unsafeFrom(
                    lastSyncGlobalEpochProgress.value.value + applicationConfig.failedOperationsExpirationEpochProgresses.value.value
                  )
                ),
                signedUpdate
              ).some
            } else if (allowSpend.lastValidEpochProgress.value.value < lastSyncGlobalEpochProgress.value.value) {
              FailedCalculatedState(
                AllowSpendExpired(allowSpend.signed.value),
                EpochProgress(NonNegLong.unsafeFrom(lastSyncGlobalEpochProgress.value.value + 30L)),
                signedUpdate
              ).some
            } else {
              None
            }
          }.flatten
        }

      private def handleFailedUpdate(
        updates: List[AmmUpdate],
        acc: DataState[AmmOnChainState, AmmCalculatedState],
        failedCalculatedState: FailedCalculatedState,
        swapCalculatedState: SwapCalculatedState
      ) = {
        val updatedSwapCalculatedState = swapCalculatedState
          .focus(_.failed)
          .modify(_ + failedCalculatedState)

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
          case PendingAllowSpend(update) if update == signedSwapUpdate => true
          case _                                                       => false
        }

      private def removePendingSpendAction(
        swapCalculatedState: SwapCalculatedState,
        signedSwapUpdate: Signed[SwapUpdate]
      ) =
        swapCalculatedState.pending.filterNot {
          case PendingSpendAction(update, _) if update == signedSwapUpdate => true
          case _                                                           => false
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
        val swapCalculatedStatePendingAllowSpend = swapCalculatedState.pending

        val updates = swapUpdate :: oldState.onChain.updates
        getUpdateAllowSpend(swapUpdate, lastGlobalSnapshotsAllowSpends).flatMap {
          case None =>
            val updatedPendingSwapsCalculatedState = if (swapUpdate.maxValidGsEpochProgress < globalEpochProgress) {
              removePendingAllowSpend(swapCalculatedState, signedUpdate)
            } else if (!swapCalculatedStatePendingAllowSpend.exists(_.update == signedUpdate)) {
              swapCalculatedStatePendingAllowSpend + PendingAllowSpend(signedUpdate)
            } else {
              swapCalculatedStatePendingAllowSpend
            }

            val newSwapState = swapCalculatedState
              .focus(_.pending)
              .replace(updatedPendingSwapsCalculatedState)

            val updatedCalculatedState = oldState.calculated
              .focus(_.operations)
              .modify(_.updated(OperationType.Swap, newSwapState))

            DataState(
              AmmOnChainState(updates),
              updatedCalculatedState
            ).pure

          case Some(_) =>
            combinePendingAllowSpend(
              signedUpdate,
              oldState,
              globalEpochProgress,
              lastGlobalSnapshotsAllowSpends,
              currencyId
            )
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
        validateUpdate(signedUpdate, none, none, globalEpochProgress) match {
          case Some(failedCalculatedState) => handleFailedUpdate(updates, oldState, failedCalculatedState, swapCalculatedState).pure
          case None =>
            getUpdateAllowSpend(swapUpdate, lastGlobalSnapshotsAllowSpends).flatMap {
              case Some(allowSpendToken) =>
                for {
                  poolId <- buildLiquidityPoolUniqueIdentifier(swapUpdate.swapFromPair, swapUpdate.swapToPair)
                  swapTokenInfo <- pricingService.getSwapTokenInfo(swapUpdate, poolId)
                  response = swapTokenInfo match {
                    case Left(_) => oldState
                    case Right(updatedTokenInformation) =>
                      val maybeFailedUpdate = validateUpdate(
                        signedUpdate,
                        updatedTokenInformation.some,
                        allowSpendToken.some,
                        globalEpochProgress
                      )

                      maybeFailedUpdate match {
                        case Some(failedCalculatedState) =>
                          handleFailedUpdate(updates, oldState, failedCalculatedState, swapCalculatedState)
                        case None =>
                          val spendActionToken = generateSpendAction(
                            allowSpendToken,
                            updatedTokenInformation.pairTokenInformation.identifier,
                            updatedTokenInformation.receivedAmount,
                            currencyId.value
                          )

                          val updatedPendingAllowSpendCalculatedState =
                            removePendingAllowSpend(swapCalculatedState, signedUpdate)
                          val updatedPendingSpendActionCalculatedState = updatedPendingAllowSpendCalculatedState + PendingSpendAction(
                            signedUpdate,
                            spendActionToken
                          )

                          val updatedPendingStakingCalculatedState =
                            swapCalculatedState
                              .focus(_.pending)
                              .replace(updatedPendingSpendActionCalculatedState)

                          val updatedCalculatedState = oldState.calculated
                            .focus(_.operations)
                            .modify(_.updated(OperationType.Staking, updatedPendingStakingCalculatedState))

                          val updatedSharedArtifacts = oldState.sharedArtifacts ++ SortedSet[SharedArtifact](
                            spendActionToken
                          )

                          DataState(
                            AmmOnChainState(updates),
                            updatedCalculatedState,
                            updatedSharedArtifacts
                          )
                      }
                  }
                } yield response
              case _ => oldState.pure
            }
        }
      }

      def combinePendingSpendAction(
        pendingSpendAction: PendingSpendAction[SwapUpdate],
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        globalEpochProgress: EpochProgress,
        spendActions: List[SpendAction],
        currentSnapshotOrdinal: SnapshotOrdinal
      )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
        val liquidityPoolsCalculatedState = getLiquidityPoolCalculatedState(oldState.calculated)
        val swapCalculatedState = getSwapCalculatedState(oldState.calculated)

        val signedSwapUpdate = pendingSpendAction.update
        val swapUpdate = pendingSpendAction.update.value
        val updates = swapUpdate :: oldState.onChain.updates
        validateUpdate(signedSwapUpdate, none, none, globalEpochProgress) match {
          case Some(value) => handleFailedUpdate(updates, oldState, value, swapCalculatedState).pure
          case None =>
            for {
              metagraphGeneratedSpendActionHash <- HasherSelector[F].withCurrent(implicit hs =>
                Hasher[F].hash(pendingSpendAction.generatedSpendAction)
              )
              globalSnapshotsHashes <- HasherSelector[F].withCurrent(implicit hs => spendActions.traverse(action => Hasher[F].hash(action)))
              allSpendActionsAccepted = checkIfSpendActionAcceptedInGl0(metagraphGeneratedSpendActionHash, globalSnapshotsHashes)
              updatedState <-
                if (!allSpendActionsAccepted) {
                  oldState.pure
                } else {
                  for {
                    poolId <- buildLiquidityPoolUniqueIdentifier(swapUpdate.swapFromPair, swapUpdate.swapToPair)
                    liquidityPool <- getLiquidityPoolByPoolId(liquidityPoolsCalculatedState.confirmed.value, poolId)
                    swapTokenInfo <- pricingService.getSwapTokenInfo(swapUpdate, poolId)
                    response = swapTokenInfo match {
                      case Left(_) => oldState
                      case Right(updatedTokenInformation) =>
                        val liquidityPoolUpdated = updateLiquidityPool(
                          liquidityPool,
                          updatedTokenInformation.primaryTokenInformation,
                          updatedTokenInformation.pairTokenInformation
                        )

                        val swapCalculatedStateAddress = SwapCalculatedStateAddress(
                          swapUpdate.sourceAddress,
                          updatedTokenInformation.primaryTokenInformation,
                          updatedTokenInformation.pairTokenInformation,
                          swapUpdate.allowSpendReference,
                          swapUpdate.minAmount,
                          swapUpdate.maxAmount,
                          swapUpdate.maxValidGsEpochProgress,
                          poolId.some,
                          swapUpdate.minPrice,
                          swapUpdate.maxPrice,
                          currentSnapshotOrdinal
                        )

                        val updatedPendingCalculatedState = removePendingSpendAction(swapCalculatedState, signedSwapUpdate)
                        val newSwapState = swapCalculatedState
                          .focus(_.confirmed.value)
                          .modify(current =>
                            current.updatedWith(swapUpdate.sourceAddress) {
                              case Some(confirmedSwaps) => Some(confirmedSwaps + swapCalculatedStateAddress)
                              case None                 => Some(Set(swapCalculatedStateAddress))
                            }
                          )
                          .focus(_.pending)
                          .replace(updatedPendingCalculatedState)

                        val newLiquidityPoolState =
                          liquidityPoolsCalculatedState
                            .focus(_.confirmed.value)
                            .modify(_.updated(poolId.value, liquidityPoolUpdated))

                        val updatedCalculatedState = oldState.calculated
                          .focus(_.operations)
                          .modify(_.updated(OperationType.Swap, newSwapState))
                          .focus(_.operations)
                          .modify(_.updated(OperationType.LiquidityPool, newLiquidityPoolState))

                        DataState(
                          AmmOnChainState(updates),
                          updatedCalculatedState
                        )
                    }

                  } yield response
                }
            } yield updatedState
        }
      }

    }
  }
}
