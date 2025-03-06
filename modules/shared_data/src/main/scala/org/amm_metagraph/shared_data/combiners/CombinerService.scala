package org.amm_metagraph.shared_data.combiners

import cats.data.OptionT
import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.ext.cats.syntax.next.catsSyntaxNext
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact.SpendAction
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.{GlobalSnapshotInfo, SnapshotOrdinal, swap}
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hasher, SecurityProvider}

import monocle.syntax.all._
import org.amm_metagraph.shared_data.SpendTransactions.getCombinedSpendTransactions
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.combiners.GovernanceCombiner._
import org.amm_metagraph.shared_data.combiners.LiquidityPoolCombiner._
import org.amm_metagraph.shared_data.combiners.StakingCombiner._
import org.amm_metagraph.shared_data.combiners.SwapCombiner._
import org.amm_metagraph.shared_data.combiners.WithdrawalCombiner._
import org.amm_metagraph.shared_data.globalSnapshots._
import org.amm_metagraph.shared_data.storages.GlobalSnapshotsStorage
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.Staking._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.Swap._
import org.amm_metagraph.shared_data.types.codecs.HasherSelector
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

trait CombinerService[F[_]] {
  def combine(
    oldState: DataState[AmmOnChainState, AmmCalculatedState],
    updates: List[Signed[AmmUpdate]]
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]]
}

object CombinerService {
  def make[F[_]: Async: HasherSelector: SecurityProvider](
    applicationConfig: ApplicationConfig,
    globalSnapshotsStorage: GlobalSnapshotsStorage[F]
  ): F[CombinerService[F]] = Async[F].delay {
    new CombinerService[F] {
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
        Set[PendingSpendAction[SwapUpdate]]
      ) =
        (
          getPendingSpendActionLiquidityPoolUpdates(state),
          getPendingSpendActionStakingUpdates(state),
          getPendingSpendActionSwapUpdates(state)
        )

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
            oldState.calculated.lastSyncGlobalSnapshotOrdinal.getOrElse(lastSyncGlobalOrdinal),
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

          (pendingSpendActionLiquidityPool, pendingSpendActionStaking, pendingSpendActionSwap) =
            getPendingSpendActionsUpdates(newState.calculated)

          updatedVotingWeights = updateVotingWeights(
            applicationConfig,
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

          stateCombinedByNewUpdates <-
            combineIncomingUpdates(
              applicationConfig,
              incomingUpdates,
              lastSyncGlobalEpochProgress,
              globalSnapshotSyncAllowSpends,
              stateCombinedByVotingWeight
            )

          stateCombinedByPendingAllowSpendUpdates <-
            combinePendingAllowSpendsUpdates(
              applicationConfig,
              lastSyncGlobalEpochProgress,
              globalSnapshotSyncAllowSpends,
              pendingAllowSpendLiquidityPool,
              pendingAllowSpendStaking,
              pendingAllowSpendSwap,
              stateCombinedByNewUpdates
            )

          stateCombinedByPendingSpendActions <- HasherSelector[F].withCurrent { implicit hasher =>
            combinePendingSpendTransactionsUpdates(
              applicationConfig,
              lastSyncGlobalEpochProgress,
              globalSnapshotsSyncSpendActions,
              currentSnapshotOrdinal,
              pendingSpendActionLiquidityPool,
              pendingSpendActionStaking,
              pendingSpendActionSwap,
              stateCombinedByPendingAllowSpendUpdates
            )
          }

          stateCombinedGovernanceRewards = handleMonthlyGovernanceRewards(
            applicationConfig,
            stateCombinedByPendingSpendActions,
            lastSyncGlobalEpochProgress,
            currentSnapshotEpochProgress
          )

          stateUpdatedByLastGlobalSync = stateCombinedGovernanceRewards
            .focus(_.calculated.lastSyncGlobalSnapshotOrdinal)
            .replace(lastSyncGlobalOrdinal.some)

        } yield stateUpdatedByLastGlobalSync).handleErrorWith { e =>
          logger.error(s"Error when combining: ${e.getMessage}").as(oldState)
        }
    }
  }

  private def combineIncomingUpdates[F[_]: Async: HasherSelector: SecurityProvider](
    applicationConfig: ApplicationConfig,
    incomingUpdates: List[Signed[AmmUpdate]],
    lastSyncGlobalEpochProgress: EpochProgress,
    globalSnapshotSyncAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[swap.AllowSpend]]]],
    stateCombinedByVotingWeight: DataState[AmmOnChainState, AmmCalculatedState]
  ) =
    if (incomingUpdates.isEmpty) {
      logger.info("Snapshot without any updates, updating the state to empty updates").as(stateCombinedByVotingWeight)
    } else {
      logger.info(s"Incoming updates: ${incomingUpdates.size}") >>
        incomingUpdates.foldLeftM(stateCombinedByVotingWeight) { (acc, signedUpdate) =>
          for {
            address <- signedUpdate.proofs.head.id.toAddress

            combinedState <- signedUpdate.value match {
              case stakingUpdate: StakingUpdate =>
                logger.info(s"Received staking update: $stakingUpdate") >>
                  combineNewStaking(
                    applicationConfig,
                    acc,
                    Signed(stakingUpdate, signedUpdate.proofs),
                    lastSyncGlobalEpochProgress,
                    globalSnapshotSyncAllowSpends
                  )

              case withdrawalUpdate: WithdrawalUpdate =>
                logger.info(s"Received withdrawal update: $withdrawalUpdate") >>
                  combineNewWithdraw(
                    acc,
                    Signed(withdrawalUpdate, signedUpdate.proofs),
                    address,
                    lastSyncGlobalEpochProgress
                  )

              case liquidityPoolUpdate: LiquidityPoolUpdate =>
                logger.info(s"Received liquidity pool update: $liquidityPoolUpdate") >>
                  combineNewLiquidityPool(
                    applicationConfig,
                    acc,
                    Signed(liquidityPoolUpdate, signedUpdate.proofs),
                    lastSyncGlobalEpochProgress,
                    globalSnapshotSyncAllowSpends
                  )

              case swapUpdate: SwapUpdate =>
                logger.info(s"Received swap update: $swapUpdate") >>
                  combineNewSwap(
                    applicationConfig,
                    acc,
                    Signed(swapUpdate, signedUpdate.proofs),
                    lastSyncGlobalEpochProgress,
                    globalSnapshotSyncAllowSpends
                  )

              case rewardAllocationVoteUpdate: RewardAllocationVoteUpdate =>
                logger.info(s"Received reward allocation vote update: $rewardAllocationVoteUpdate") >>
                  combineRewardAllocationVoteUpdate(
                    acc,
                    Signed(rewardAllocationVoteUpdate, signedUpdate.proofs),
                    lastSyncGlobalEpochProgress
                  )
            }
          } yield combinedState
        }
    }

  private def combinePendingAllowSpendsUpdates[F[_]: Async: HasherSelector](
    applicationConfig: ApplicationConfig,
    lastSyncGlobalEpochProgress: EpochProgress,
    globalSnapshotSyncAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[swap.AllowSpend]]]],
    pendingAllowSpendLiquidityPool: Set[Signed[LiquidityPoolUpdate]],
    pendingAllowSpendStaking: Set[Signed[StakingUpdate]],
    pendingAllowSpendSwap: Set[Signed[SwapUpdate]],
    stateCombinedByNewUpdates: DataState[AmmOnChainState, AmmCalculatedState]
  ) =
    if (globalSnapshotSyncAllowSpends.isEmpty) {
      stateCombinedByNewUpdates.pure
    } else {
      for {
        _ <- logger.info(s"lastGlobalSnapshotAllowSpends found: $globalSnapshotSyncAllowSpends")
        stateUpdatedByLiquidityPools <- pendingAllowSpendLiquidityPool.toList
          .foldLeftM(stateCombinedByNewUpdates) { (acc, pendingLp) =>
            combinePendingAllowSpendLiquidityPool(
              applicationConfig,
              acc,
              pendingLp,
              lastSyncGlobalEpochProgress,
              globalSnapshotSyncAllowSpends
            )
          }
        stateUpdatedByStaking <- pendingAllowSpendStaking.toList
          .foldLeftM(stateUpdatedByLiquidityPools) { (acc, pendingStake) =>
            combinePendingAllowSpendStaking(
              applicationConfig,
              acc,
              pendingStake,
              lastSyncGlobalEpochProgress,
              globalSnapshotSyncAllowSpends
            )
          }
        stateUpdatedBySwap <- pendingAllowSpendSwap.toList
          .foldLeftM(stateUpdatedByStaking) { (acc, pendingSwap) =>
            combinePendingAllowSpendSwap(
              applicationConfig,
              acc,
              pendingSwap,
              lastSyncGlobalEpochProgress,
              globalSnapshotSyncAllowSpends
            )
          }
      } yield stateUpdatedBySwap
    }

  private def combinePendingSpendTransactionsUpdates[F[_]: Async: Hasher: SecurityProvider](
    applicationConfig: ApplicationConfig,
    lastSyncGlobalEpochProgress: EpochProgress,
    globalSnapshotSyncSpendActions: List[SpendAction],
    currencySnapshotOrdinal: SnapshotOrdinal,
    pendingSpendActionLiquidityPool: Set[PendingSpendAction[LiquidityPoolUpdate]],
    pendingSpendActionStaking: Set[PendingSpendAction[StakingUpdate]],
    pendingSpendActionSwap: Set[PendingSpendAction[SwapUpdate]],
    stateCombinedByPendingAllowSpends: DataState[AmmOnChainState, AmmCalculatedState]
  ) =
    if (globalSnapshotSyncSpendActions.isEmpty) {
      stateCombinedByPendingAllowSpends.pure
    } else {
      for {
        _ <- logger.info(s"lastGlobalSnapshotAllowSpends found: $globalSnapshotSyncSpendActions")
        stateUpdatedByLiquidityPools <- pendingSpendActionLiquidityPool.toList
          .foldLeftM(stateCombinedByPendingAllowSpends) { (acc, pendingLp) =>
            combinePendingSpendActionLiquidityPool(
              applicationConfig,
              acc,
              pendingLp,
              lastSyncGlobalEpochProgress,
              globalSnapshotSyncSpendActions
            )
          }
        stateUpdatedByStaking <- pendingSpendActionStaking.toList
          .foldLeftM(stateUpdatedByLiquidityPools) { (acc, pendingStake) =>
            combinePendingSpendActionStaking(
              applicationConfig,
              acc,
              pendingStake,
              currencySnapshotOrdinal,
              lastSyncGlobalEpochProgress,
              globalSnapshotSyncSpendActions
            )
          }
        stateUpdatedBySwap <- pendingSpendActionSwap.toList
          .foldLeftM(stateUpdatedByStaking) { (acc, pendingSwap) =>
            combinePendingSpendActionSwap(
              applicationConfig,
              acc,
              pendingSwap,
              currencySnapshotOrdinal,
              lastSyncGlobalEpochProgress,
              globalSnapshotSyncSpendActions
            )
          }
      } yield stateUpdatedBySwap
    }
}
