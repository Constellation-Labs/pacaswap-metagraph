package org.amm_metagraph.shared_data.services.combiners

import cats.data.OptionT
import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.ext.cats.syntax.next.catsSyntaxNext
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact.SpendAction
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.CurrencyId
import io.constellationnetwork.schema.{GlobalSnapshotInfo, SnapshotOrdinal, swap}
import io.constellationnetwork.security.Hasher
import io.constellationnetwork.security.signature.Signed

import monocle.syntax.all._
import org.amm_metagraph.shared_data.globalSnapshots._
import org.amm_metagraph.shared_data.storages.GlobalSnapshotsStorage
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.Staking._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.Swap._
import org.amm_metagraph.shared_data.types.Withdrawal._
import org.amm_metagraph.shared_data.types.codecs.HasherSelector
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

trait L0CombinerService[F[_]] {
  def combine(
    oldState: DataState[AmmOnChainState, AmmCalculatedState],
    updates: List[Signed[AmmUpdate]]
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]]
}

object L0CombinerService {
  def make[F[_]: Async: HasherSelector](
    globalSnapshotsStorage: GlobalSnapshotsStorage[F],
    governanceCombinerService: GovernanceCombinerService[F],
    liquidityPoolCombinerService: LiquidityPoolCombinerService[F],
    stakingCombinerService: StakingCombinerService[F],
    swapCombinerService: SwapCombinerService[F],
    withdrawalCombinerService: WithdrawalCombinerService[F]
  ): L0CombinerService[F] =
    new L0CombinerService[F] {
      def logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("CombinerService")

      def getPendingAllowSpendsUpdates(
        state: AmmCalculatedState
      ): Set[PendingAllowSpend[AmmUpdate]] =
        getPendingAllowSpendsLiquidityPoolUpdates(state) ++
          getPendingAllowSpendsStakingUpdates(state) ++
          getPendingAllowSpendsSwapUpdates(state)

      def getPendingSpendActionsUpdates(
        state: AmmCalculatedState
      ): Set[PendingSpendAction[AmmUpdate]] =
        getPendingSpendActionLiquidityPoolUpdates(state) ++
          getPendingSpendActionStakingUpdates(state) ++
          getPendingSpendActionSwapUpdates(state) ++
          getPendingSpendActionWithdrawalUpdates(state)

      private def combineIncomingUpdates(
        incomingUpdates: List[Signed[AmmUpdate]],
        lastSyncGlobalEpochProgress: EpochProgress,
        globalSnapshotSyncAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[swap.AllowSpend]]]],
        stateCombinedByVotingWeight: DataState[AmmOnChainState, AmmCalculatedState],
        currencyId: CurrencyId
      )(implicit context: L0NodeContext[F]) =
        if (incomingUpdates.isEmpty) {
          logger.info("Snapshot without any updates, updating the state to empty updates").as(stateCombinedByVotingWeight)
        } else {
          logger.info(s"Incoming updates: ${incomingUpdates.size}") >>
            incomingUpdates.foldLeftM(stateCombinedByVotingWeight) { (acc, signedUpdate) =>
              for {
                combinedState <- signedUpdate.value match {
                  case liquidityPoolUpdate: LiquidityPoolUpdate =>
                    logger.info(s"Received liquidity pool update: $liquidityPoolUpdate") >>
                      liquidityPoolCombinerService.combineNew(
                        Signed(liquidityPoolUpdate, signedUpdate.proofs),
                        acc,
                        lastSyncGlobalEpochProgress,
                        globalSnapshotSyncAllowSpends,
                        currencyId
                      )

                  case stakingUpdate: StakingUpdate =>
                    logger.info(s"Received staking update: $stakingUpdate") >>
                      stakingCombinerService.combineNew(
                        Signed(stakingUpdate, signedUpdate.proofs),
                        acc,
                        lastSyncGlobalEpochProgress,
                        globalSnapshotSyncAllowSpends,
                        currencyId
                      )

                  case withdrawalUpdate: WithdrawalUpdate =>
                    logger.info(s"Received withdrawal update: $withdrawalUpdate") >>
                      withdrawalCombinerService.combineNew(
                        Signed(withdrawalUpdate, signedUpdate.proofs),
                        acc,
                        lastSyncGlobalEpochProgress,
                        globalSnapshotSyncAllowSpends,
                        currencyId
                      )

                  case swapUpdate: SwapUpdate =>
                    logger.info(s"Received swap update: $swapUpdate") >>
                      swapCombinerService.combineNew(
                        Signed(swapUpdate, signedUpdate.proofs),
                        acc,
                        lastSyncGlobalEpochProgress,
                        globalSnapshotSyncAllowSpends,
                        currencyId
                      )

                  case rewardAllocationVoteUpdate: RewardAllocationVoteUpdate =>
                    logger.info(s"Received reward allocation vote update: $rewardAllocationVoteUpdate") >>
                      governanceCombinerService.combineNew(
                        Signed(rewardAllocationVoteUpdate, signedUpdate.proofs),
                        acc,
                        lastSyncGlobalEpochProgress,
                        globalSnapshotSyncAllowSpends,
                        currencyId
                      )
                }
              } yield combinedState
            }
        }

      private def combinePendingAllowSpendsUpdates(
        lastSyncGlobalEpochProgress: EpochProgress,
        globalSnapshotSyncAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[swap.AllowSpend]]]],
        pendingAllowSpends: Set[PendingAllowSpend[AmmUpdate]],
        stateCombinedByNewUpdates: DataState[AmmOnChainState, AmmCalculatedState],
        currencyId: CurrencyId
      )(implicit context: L0NodeContext[F]) =
        if (globalSnapshotSyncAllowSpends.isEmpty) {
          stateCombinedByNewUpdates.pure
        } else {
          pendingAllowSpends.toList
            .foldLeftM(stateCombinedByNewUpdates) { (acc, pendingUpdate) =>
              pendingUpdate.update.value match {
                case lpUpdate: LiquidityPoolUpdate =>
                  liquidityPoolCombinerService.combinePendingAllowSpend(
                    PendingAllowSpend(
                      Signed(lpUpdate, pendingUpdate.update.proofs),
                      pendingUpdate.updateHash,
                      pendingUpdate.pricingTokenInfo
                    ),
                    acc,
                    lastSyncGlobalEpochProgress,
                    globalSnapshotSyncAllowSpends,
                    currencyId
                  )

                case stakingUpdate: StakingUpdate =>
                  stakingCombinerService.combinePendingAllowSpend(
                    PendingAllowSpend(
                      Signed(stakingUpdate, pendingUpdate.update.proofs),
                      pendingUpdate.updateHash,
                      pendingUpdate.pricingTokenInfo
                    ),
                    acc,
                    lastSyncGlobalEpochProgress,
                    globalSnapshotSyncAllowSpends,
                    currencyId
                  )

                case swapUpdate: SwapUpdate =>
                  swapCombinerService.combinePendingAllowSpend(
                    PendingAllowSpend(
                      Signed(swapUpdate, pendingUpdate.update.proofs),
                      pendingUpdate.updateHash,
                      pendingUpdate.pricingTokenInfo
                    ),
                    acc,
                    lastSyncGlobalEpochProgress,
                    globalSnapshotSyncAllowSpends,
                    currencyId
                  )
                case _ => acc.pure
              }
            }
        }

      private def combinePendingSpendTransactionsUpdates(
        lastSyncGlobalEpochProgress: EpochProgress,
        globalSnapshotSyncSpendActions: List[SpendAction],
        currencySnapshotOrdinal: SnapshotOrdinal,
        pendingSpendActions: Set[PendingSpendAction[AmmUpdate]],
        stateCombinedByPendingAllowSpends: DataState[AmmOnChainState, AmmCalculatedState],
        currencyId: CurrencyId
      )(implicit context: L0NodeContext[F]) =
        if (globalSnapshotSyncSpendActions.isEmpty) {
          stateCombinedByPendingAllowSpends.pure
        } else {
          pendingSpendActions.toList
            .foldLeftM(stateCombinedByPendingAllowSpends) { (acc, pendingUpdate) =>
              pendingUpdate.update.value match {
                case lpUpdate: LiquidityPoolUpdate =>
                  liquidityPoolCombinerService.combinePendingSpendAction(
                    PendingSpendAction(
                      Signed(lpUpdate, pendingUpdate.update.proofs),
                      pendingUpdate.updateHash,
                      pendingUpdate.generatedSpendAction,
                      pendingUpdate.pricingTokenInfo
                    ),
                    acc,
                    lastSyncGlobalEpochProgress,
                    globalSnapshotSyncSpendActions,
                    currencySnapshotOrdinal,
                    currencyId
                  )
                case stakingUpdate: StakingUpdate =>
                  stakingCombinerService.combinePendingSpendAction(
                    PendingSpendAction(
                      Signed(stakingUpdate, pendingUpdate.update.proofs),
                      pendingUpdate.updateHash,
                      pendingUpdate.generatedSpendAction,
                      pendingUpdate.pricingTokenInfo
                    ),
                    acc,
                    lastSyncGlobalEpochProgress,
                    globalSnapshotSyncSpendActions,
                    currencySnapshotOrdinal,
                    currencyId
                  )
                case withdrawalUpdate: WithdrawalUpdate =>
                  withdrawalCombinerService.combinePendingSpendAction(
                    PendingSpendAction(
                      Signed(withdrawalUpdate, pendingUpdate.update.proofs),
                      pendingUpdate.updateHash,
                      pendingUpdate.generatedSpendAction,
                      pendingUpdate.pricingTokenInfo
                    ),
                    acc,
                    lastSyncGlobalEpochProgress,
                    globalSnapshotSyncSpendActions,
                    currencySnapshotOrdinal
                  )
                case swapUpdate: SwapUpdate =>
                  swapCombinerService.combinePendingSpendAction(
                    PendingSpendAction(
                      Signed(swapUpdate, pendingUpdate.update.proofs),
                      pendingUpdate.updateHash,
                      pendingUpdate.generatedSpendAction,
                      pendingUpdate.pricingTokenInfo
                    ),
                    acc,
                    lastSyncGlobalEpochProgress,
                    globalSnapshotSyncSpendActions,
                    currencySnapshotOrdinal,
                    currencyId
                  )
                case _ => acc.pure
              }
            }
        }

      private def cleanupExpiredOperations(
        stateCombinedByPendingSpendTransactions: DataState[AmmOnChainState, AmmCalculatedState],
        lastSyncGlobalEpochProgress: EpochProgress
      ) = {
        val liquidityPoolCleanupState = liquidityPoolCombinerService.cleanupExpiredOperations(
          stateCombinedByPendingSpendTransactions,
          lastSyncGlobalEpochProgress
        )

        val stakesCleanupState = stakingCombinerService.cleanupExpiredOperations(
          liquidityPoolCleanupState,
          lastSyncGlobalEpochProgress
        )

        val swapsCleanupState = swapCombinerService.cleanupExpiredOperations(
          stakesCleanupState,
          lastSyncGlobalEpochProgress
        )

        withdrawalCombinerService.cleanupExpiredOperations(
          swapsCleanupState,
          lastSyncGlobalEpochProgress
        )
      }

      override def combine(
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        incomingUpdates: List[Signed[AmmUpdate]]
      )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] =
        (for {
          (lastCurrencySnapshot, lastCurrencySnapshotInfo) <- OptionT(context.getLastCurrencySnapshotCombined).getOrRaise(
            new IllegalStateException("lastCurrencySnapshot unavailable")
          )

          lastSyncGlobal = context.getLastSynchronizedGlobalSnapshotCombined
          (lastSyncGlobalEpochProgress, lastSyncGlobalOrdinal, lastSyncState) <- OptionT(lastSyncGlobal).map {
            case (snapshot, info) => (snapshot.epochProgress, snapshot.ordinal, info)
          }.getOrElseF {
            val message = "Could not get last synchronized global snapshot data"
            logger.error(message) >> new Exception(message).raiseError[F, (EpochProgress, SnapshotOrdinal, GlobalSnapshotInfo)]
          }

          _ <- logger.info(s"LAST SYNC GLOBAL SNAPSHOT EPOCH PROGRESS: ${lastSyncGlobalEpochProgress}")
          currentSnapshotOrdinal = lastCurrencySnapshot.ordinal.next
          currentSnapshotEpochProgress = lastCurrencySnapshot.epochProgress.next

          globalSnapshotSyncAllowSpends = getAllowSpendsFromGlobalSnapshotState(lastSyncState)
          globalSnapshotsSyncSpendActions <- getSpendActionsFromGlobalSnapshots(
            oldState.calculated.lastSyncGlobalSnapshotOrdinal,
            lastSyncGlobalOrdinal,
            globalSnapshotsStorage
          )

          /*We should clean up the previous onChain and sharedArtifacts in every combine call, to avoid duplications.
          In other words, we will preserve between the update batches, but we should clean at every call of the combine.
           */
          newState = oldState
            .focus(_.onChain.updates)
            .replace(Set.empty)
            .focus(_.sharedArtifacts)
            .replace(SortedSet.empty)

          pendingAllowSpends = getPendingAllowSpendsUpdates(newState.calculated)
          pendingSpendActions = getPendingSpendActionsUpdates(newState.calculated)

          updatedVotingWeights = governanceCombinerService.updateVotingWeights(
            newState.calculated,
            lastCurrencySnapshotInfo,
            lastSyncGlobalEpochProgress
          )

          updatedVotingWeightState = newState.calculated
            .focus(_.votingWeights)
            .replace(updatedVotingWeights)

          stateCombinedByVotingWeight = newState
            .focus(_.calculated)
            .replace(updatedVotingWeightState)

          currencyId <- context.getCurrencyId

          stateCombinedByNewUpdates <-
            combineIncomingUpdates(
              incomingUpdates,
              lastSyncGlobalEpochProgress,
              globalSnapshotSyncAllowSpends,
              stateCombinedByVotingWeight,
              currencyId
            )

          stateCombinedByPendingAllowSpendUpdates <-
            combinePendingAllowSpendsUpdates(
              lastSyncGlobalEpochProgress,
              globalSnapshotSyncAllowSpends,
              pendingAllowSpends,
              stateCombinedByNewUpdates,
              currencyId
            )

          stateCombinedByPendingSpendActions <-
            combinePendingSpendTransactionsUpdates(
              lastSyncGlobalEpochProgress,
              globalSnapshotsSyncSpendActions,
              currentSnapshotOrdinal,
              pendingSpendActions,
              stateCombinedByPendingAllowSpendUpdates,
              currencyId
            )

          stateCombinedByCleanupOperations = cleanupExpiredOperations(
            stateCombinedByPendingSpendActions,
            lastSyncGlobalEpochProgress
          )

          stateCombinedGovernanceRewards = governanceCombinerService.handleMonthlyGovernanceRewards(
            stateCombinedByCleanupOperations,
            lastSyncGlobalEpochProgress,
            currentSnapshotEpochProgress
          )

          stateUpdatedByLastGlobalSync = stateCombinedGovernanceRewards
            .focus(_.calculated.lastSyncGlobalSnapshotOrdinal)
            .replace(lastSyncGlobalOrdinal)

          _ <- logger.info(s"SpendTxnProduced: ${stateUpdatedByLastGlobalSync.sharedArtifacts}")
        } yield stateUpdatedByLastGlobalSync).handleErrorWith { e =>
          logger.error(s"Error when combining: ${e.getMessage}").as(oldState)
        }
    }
}
