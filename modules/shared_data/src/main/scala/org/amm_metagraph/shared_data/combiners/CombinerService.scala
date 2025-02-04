package org.amm_metagraph.shared_data.combiners

import cats.data.OptionT
import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.ext.cats.syntax.next.catsSyntaxNext
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hasher, SecurityProvider}

import monocle.syntax.all._
import org.amm_metagraph.shared_data.SpendTransactions.getCombinedSpendTransactions
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.combiners.GovernanceCombiner.{
  combineRewardAllocationVoteUpdate,
  handleMonthlyGovernanceRewards,
  updateVotingWeights
}
import org.amm_metagraph.shared_data.combiners.LiquidityPoolCombiner.combineLiquidityPool
import org.amm_metagraph.shared_data.combiners.StakingCombiner.combineStaking
import org.amm_metagraph.shared_data.combiners.SwapCombiner.combineSwap
import org.amm_metagraph.shared_data.combiners.WithdrawalCombiner.combineWithdrawal
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.States._
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

trait CombinerService[F[_]] {
  def combine(
    oldState: DataState[AmmOnChainState, AmmCalculatedState],
    updates: List[Signed[AmmUpdate]]
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]]
}

object CombinerService {
  def make[F[_]: Async: Hasher: SecurityProvider](
    applicationConfig: ApplicationConfig
  ): F[CombinerService[F]] = Async[F].delay {
    new CombinerService[F] {
      def logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("CombinerService")

      override def combine(
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        incomingUpdates: List[Signed[AmmUpdate]]
      )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] =
        (for {
          (lastCurrencySnapshot, lastCurrencySnapshotInfo) <- OptionT(context.getLastCurrencySnapshotCombined).getOrRaise(
            new IllegalStateException("lastCurrencySnapshot unavailable")
          )
          lastSyncGlobalEpochProgress <- OptionT(context.getLastSynchronizedGlobalSnapshot)
            .map(_.epochProgress)
            .getOrElseF {
              val message = "Could not get lastSyncGlobalEpochProgress"
              logger.error(message) >> new Exception(message).raiseError[F, EpochProgress]
            }
          currentSnapshotOrdinal = lastCurrencySnapshot.ordinal.next
          currentSnapshotEpochProgress = lastCurrencySnapshot.epochProgress.next

          newState =
            DataState(
              AmmOnChainState(List.empty),
              oldState.calculated
            )

          pendingUpdates: Set[Signed[AmmUpdate]] = oldState.calculated.operations.values
            .flatMap(_.pending)
            .toSet

          updates = incomingUpdates ++ pendingUpdates

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

          stateCombinedByUpdates <-
            if (updates.isEmpty) {
              logger.info("Snapshot without any updates, updating the state to empty updates").as(stateCombinedByVotingWeight)
            } else {
              logger.info(s"Incoming updates: ${incomingUpdates.size}") >>
                logger.info(s"Pending updates: ${pendingUpdates.size}") >>
                updates.foldLeftM(stateCombinedByVotingWeight) { (acc, signedUpdate) =>
                  for {
                    address <- signedUpdate.proofs.head.id.toAddress

                    combinedState <- signedUpdate.value match {
                      case stakingUpdate: StakingUpdate =>
                        logger.info(s"Received staking update: $stakingUpdate") >>
                          combineStaking(
                            applicationConfig,
                            acc,
                            Signed(stakingUpdate, signedUpdate.proofs),
                            address,
                            currentSnapshotOrdinal,
                            lastSyncGlobalEpochProgress
                          )

                      case withdrawalUpdate: WithdrawalUpdate =>
                        logger.info(s"Received withdrawal update: $withdrawalUpdate") >>
                          combineWithdrawal(acc, Signed(withdrawalUpdate, signedUpdate.proofs), address, currentSnapshotOrdinal)

                      case liquidityPoolUpdate: LiquidityPoolUpdate =>
                        logger.info(s"Received liquidity pool update: $liquidityPoolUpdate") >>
                          combineLiquidityPool(
                            applicationConfig,
                            acc,
                            Signed(liquidityPoolUpdate, signedUpdate.proofs),
                            address,
                            lastSyncGlobalEpochProgress
                          )

                      case swapUpdate: SwapUpdate =>
                        logger.info(s"Received swap update: $swapUpdate") >>
                          combineSwap(acc, Signed(swapUpdate, signedUpdate.proofs), currentSnapshotOrdinal)

                      case rewardAllocationVoteUpdate: RewardAllocationVoteUpdate =>
                        logger.info(s"Received reward allocation vote update: $rewardAllocationVoteUpdate") >>
                          combineRewardAllocationVoteUpdate(
                            acc,
                            Signed(rewardAllocationVoteUpdate, signedUpdate.proofs),
                            lastSyncGlobalEpochProgress
                          )
                    }

                    spendTransactions = getCombinedSpendTransactions(
                      combinedState.sharedArtifacts
                    )

                    updatedState <-
                      combinedState
                        .focus(_.calculated)
                        .modify(_ =>
                          combinedState.calculated
                            .focus(_.spendTransactions)
                            .modify(current => current ++ spendTransactions)
                        )
                        .pure
                  } yield updatedState
                }
            }

          stateCombinedGovernanceRewards = handleMonthlyGovernanceRewards(
            applicationConfig,
            stateCombinedByUpdates,
            lastSyncGlobalEpochProgress,
            currentSnapshotEpochProgress
          )

        } yield stateCombinedGovernanceRewards).handleErrorWith { e =>
          logger.error(s"Error when combining: ${e.getMessage}").as(oldState)
        }
    }
  }
}
