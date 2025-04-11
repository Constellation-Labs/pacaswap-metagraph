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
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

trait L0CombinerService[F[_]] {
  def combine(
    oldState: DataState[AmmOnChainState, AmmCalculatedState],
    updates: List[Signed[AmmUpdate]]
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]]
}

object L0CombinerService {
  def make[F[_]: Async](
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
      ): (
        Set[Signed[LiquidityPoolUpdate]],
        Set[Signed[StakingUpdate]],
        Set[Signed[SwapUpdate]]
      ) =
        (
          getPendingAllowSpendsLiquidityPoolUpdates(state),
          getPendingAllowSpendsStakingUpdates(state),
          getPendingAllowSpendsSwapUpdates(state)
        )

      def getPendingSpendActionsUpdates(
        state: AmmCalculatedState
      ): (
        Set[PendingSpendAction[LiquidityPoolUpdate]],
        Set[PendingSpendAction[StakingUpdate]],
        Set[PendingSpendAction[SwapUpdate]],
        Set[PendingSpendAction[WithdrawalUpdate]]
      ) =
        (
          getPendingSpendActionLiquidityPoolUpdates(state),
          getPendingSpendActionStakingUpdates(state),
          getPendingSpendActionSwapUpdates(state),
          getPendingSpendActionWithdrawalUpdates(state)
        )

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
        pendingAllowSpendLiquidityPool: Set[Signed[LiquidityPoolUpdate]],
        pendingAllowSpendStaking: Set[Signed[StakingUpdate]],
        pendingAllowSpendSwap: Set[Signed[SwapUpdate]],
        stateCombinedByNewUpdates: DataState[AmmOnChainState, AmmCalculatedState],
        currencyId: CurrencyId
      )(implicit context: L0NodeContext[F]) =
        if (globalSnapshotSyncAllowSpends.isEmpty) {
          stateCombinedByNewUpdates.pure
        } else {
          for {
            stateUpdatedByLiquidityPools <- pendingAllowSpendLiquidityPool.toList
              .foldLeftM(stateCombinedByNewUpdates) { (acc, pendingLp) =>
                logger.info(s"Trying to combine pending allow spends LiquidityPool: $pendingLp") >>
                  liquidityPoolCombinerService.combinePendingAllowSpend(
                    pendingLp,
                    acc,
                    lastSyncGlobalEpochProgress,
                    globalSnapshotSyncAllowSpends,
                    currencyId
                  )
              }
            stateUpdatedByStaking <- pendingAllowSpendStaking.toList
              .foldLeftM(stateUpdatedByLiquidityPools) { (acc, pendingStake) =>
                logger.info(s"Trying to combine pending allow spends Stake: $pendingStake") >>
                  stakingCombinerService.combinePendingAllowSpend(
                    pendingStake,
                    acc,
                    lastSyncGlobalEpochProgress,
                    globalSnapshotSyncAllowSpends,
                    currencyId
                  )
              }
            stateUpdatedBySwap <- pendingAllowSpendSwap.toList
              .foldLeftM(stateUpdatedByStaking) { (acc, pendingSwap) =>
                logger.info(s"Trying to combine pending allow spends Swap: $pendingSwap") >>
                  swapCombinerService.combinePendingAllowSpend(
                    pendingSwap,
                    acc,
                    lastSyncGlobalEpochProgress,
                    globalSnapshotSyncAllowSpends,
                    currencyId
                  )
              }
          } yield stateUpdatedBySwap
        }

      private def combinePendingSpendTransactionsUpdates(
        lastSyncGlobalEpochProgress: EpochProgress,
        globalSnapshotSyncSpendActions: List[SpendAction],
        currencySnapshotOrdinal: SnapshotOrdinal,
        pendingSpendActionLiquidityPool: Set[PendingSpendAction[LiquidityPoolUpdate]],
        pendingSpendActionStaking: Set[PendingSpendAction[StakingUpdate]],
        pendingSpendActionWithdrawals: Set[PendingSpendAction[WithdrawalUpdate]],
        pendingSpendActionSwap: Set[PendingSpendAction[SwapUpdate]],
        stateCombinedByPendingAllowSpends: DataState[AmmOnChainState, AmmCalculatedState],
        currencyId: CurrencyId
      )(implicit context: L0NodeContext[F]) =
        if (globalSnapshotSyncSpendActions.isEmpty) {
          stateCombinedByPendingAllowSpends.pure
        } else {
          for {
            stateUpdatedByLiquidityPools <- pendingSpendActionLiquidityPool.toList
              .foldLeftM(stateCombinedByPendingAllowSpends) { (acc, pendingLp) =>
                logger.info(s"Trying to combine pending spend actions LiquidityPool: $pendingLp") >>
                  liquidityPoolCombinerService.combinePendingSpendAction(
                    pendingLp,
                    acc,
                    lastSyncGlobalEpochProgress,
                    globalSnapshotSyncSpendActions,
                    currencySnapshotOrdinal,
                    currencyId
                  )
              }
            stateUpdatedByStaking <- pendingSpendActionStaking.toList
              .foldLeftM(stateUpdatedByLiquidityPools) { (acc, pendingStake) =>
                logger.info(s"Trying to combine pending spend actions Stake: $pendingStake") >>
                  stakingCombinerService.combinePendingSpendAction(
                    pendingStake,
                    acc,
                    lastSyncGlobalEpochProgress,
                    globalSnapshotSyncSpendActions,
                    currencySnapshotOrdinal,
                    currencyId
                  )
              }
            stateUpdatedByWithdrawals <- pendingSpendActionWithdrawals.toList
              .foldLeftM(stateUpdatedByStaking) { (acc, pendingWithdrawal) =>
                logger.info(s"Trying to combine pending spend actions Withdrawal: $pendingWithdrawal") >>
                  withdrawalCombinerService.combinePendingSpendAction(
                    pendingWithdrawal,
                    acc,
                    lastSyncGlobalEpochProgress,
                    globalSnapshotSyncSpendActions,
                    currencySnapshotOrdinal
                  )
              }
            stateUpdatedBySwap <- pendingSpendActionSwap.toList
              .foldLeftM(stateUpdatedByWithdrawals) { (acc, pendingSwap) =>
                logger.info(s"Trying to combine pending spend actions Swap: $pendingSwap") >>
                  swapCombinerService.combinePendingSpendAction(
                    pendingSwap,
                    acc,
                    lastSyncGlobalEpochProgress,
                    globalSnapshotSyncSpendActions,
                    currencySnapshotOrdinal,
                    currencyId
                  )
              }
          } yield stateUpdatedBySwap
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

          currentSnapshotOrdinal = lastCurrencySnapshot.ordinal.next
          currentSnapshotEpochProgress = lastCurrencySnapshot.epochProgress.next

          globalSnapshotSyncAllowSpends = getAllowSpendsFromGlobalSnapshotState(lastSyncState)
          globalSnapshotsSyncSpendActions <- getSpendActionsFromGlobalSnapshots(
            oldState.calculated.lastSyncGlobalSnapshotOrdinal,
            lastSyncGlobalOrdinal,
            globalSnapshotsStorage
          )

          newState =
            DataState(
              AmmOnChainState(List.empty),
              oldState.calculated
            )

          (pendingAllowSpendLiquidityPool, pendingAllowSpendStaking, pendingAllowSpendSwap) =
            getPendingAllowSpendsUpdates(newState.calculated)

          (pendingSpendActionLiquidityPool, pendingSpendActionStaking, pendingSpendActionSwap, pendingSpendActionWithdrawals) =
            getPendingSpendActionsUpdates(newState.calculated)

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
              pendingAllowSpendLiquidityPool,
              pendingAllowSpendStaking,
              pendingAllowSpendSwap,
              stateCombinedByNewUpdates,
              currencyId
            )

          stateCombinedByPendingSpendActions <-
            combinePendingSpendTransactionsUpdates(
              lastSyncGlobalEpochProgress,
              globalSnapshotsSyncSpendActions,
              currentSnapshotOrdinal,
              pendingSpendActionLiquidityPool,
              pendingSpendActionStaking,
              pendingSpendActionWithdrawals,
              pendingSpendActionSwap,
              stateCombinedByPendingAllowSpendUpdates,
              currencyId
            )

          stateCombinedGovernanceRewards = governanceCombinerService.handleMonthlyGovernanceRewards(
            stateCombinedByPendingSpendActions,
            lastSyncGlobalEpochProgress,
            currentSnapshotEpochProgress
          )

          stateUpdatedByLastGlobalSync = stateCombinedGovernanceRewards
            .focus(_.calculated.lastSyncGlobalSnapshotOrdinal)
            .replace(lastSyncGlobalOrdinal)

        } yield stateUpdatedByLastGlobalSync).handleErrorWith { e =>
          logger.error(s"Error when combining: ${e.getMessage}").as(oldState)
        }
    }
}
