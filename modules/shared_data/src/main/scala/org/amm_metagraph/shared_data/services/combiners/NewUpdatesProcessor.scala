package org.amm_metagraph.shared_data.services.combiners

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.security.signature.Signed

import org.amm_metagraph.shared_data.services.combiners.operations._
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.States.{AmmCalculatedState, AmmOnChainState}
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

trait NewUpdatesProcessor[F[_]] {
  def processIncomingUpdates(
    state: DataState[AmmOnChainState, AmmCalculatedState],
    incomingUpdates: List[Signed[AmmUpdate]],
    context: ProcessingContext
  )(implicit l0Context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]]
}

object NewUpdatesProcessor {
  def make[F[_]: Async](
    liquidityPoolCombinerService: LiquidityPoolCombinerService[F],
    stakingCombinerService: StakingCombinerService[F],
    swapCombinerService: SwapCombinerService[F],
    withdrawalCombinerService: WithdrawalCombinerService[F],
    governanceCombinerService: GovernanceCombinerService[F],
    rewardsWithdrawService: RewardsWithdrawService[F]
  ): NewUpdatesProcessor[F] = new NewUpdatesProcessor[F] {

    val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

    override def processIncomingUpdates(
      state: DataState[AmmOnChainState, AmmCalculatedState],
      incomingUpdates: List[Signed[AmmUpdate]],
      context: ProcessingContext
    )(implicit l0Context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] =
      if (incomingUpdates.isEmpty) {
        logger.info("Snapshot without any updates, updating the state to empty updates").as(state)
      } else {
        logger.info(s"Incoming updates: ${incomingUpdates.size}") >>
          incomingUpdates.foldLeftM(state) { (acc, signedUpdate) =>
            processUpdate(signedUpdate, acc, context)
          }
      }

    private def processUpdate(
      signedUpdate: Signed[AmmUpdate],
      state: DataState[AmmOnChainState, AmmCalculatedState],
      context: ProcessingContext
    )(implicit l0Context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] =
      signedUpdate.value match {
        case liquidityPoolUpdate: LiquidityPoolUpdate =>
          logger.info(s"Received liquidity pool update: $liquidityPoolUpdate") >>
            liquidityPoolCombinerService.combineNew(
              Signed(liquidityPoolUpdate, signedUpdate.proofs),
              state,
              context.lastSyncGlobalEpochProgress,
              context.globalSnapshotSyncAllowSpends,
              context.currencyId
            )

        case stakingUpdate: StakingUpdate =>
          logger.info(s"Received staking update: $stakingUpdate") >>
            stakingCombinerService.combineNew(
              Signed(stakingUpdate, signedUpdate.proofs),
              state,
              context.lastSyncGlobalEpochProgress,
              context.globalSnapshotSyncAllowSpends,
              context.currencyId
            )

        case withdrawalUpdate: WithdrawalUpdate =>
          logger.info(s"Received withdrawal update: $withdrawalUpdate") >>
            withdrawalCombinerService.combineNew(
              Signed(withdrawalUpdate, signedUpdate.proofs),
              state,
              context.lastSyncGlobalEpochProgress,
              context.globalSnapshotSyncAllowSpends,
              context.currentSnapshotOrdinal,
              context.currencyId
            )

        case swapUpdate: SwapUpdate =>
          logger.info(s"Received swap update: $swapUpdate") >>
            swapCombinerService.combineNew(
              Signed(swapUpdate, signedUpdate.proofs),
              state,
              context.lastSyncGlobalEpochProgress,
              context.globalSnapshotSyncAllowSpends,
              context.currentSnapshotOrdinal,
              context.currencyId
            )

        case rewardAllocationVoteUpdate: RewardAllocationVoteUpdate =>
          logger.info(s"Received reward allocation vote update: $rewardAllocationVoteUpdate") >>
            governanceCombinerService.combineNew(
              Signed(rewardAllocationVoteUpdate, signedUpdate.proofs),
              state,
              context.lastSyncGlobalEpochProgress,
              context.currentSnapshotEpochProgress,
              context.globalSnapshotSyncAllowSpends,
              context.currencyId
            )

        case rewardWithdrawUpdate: RewardWithdrawUpdate =>
          logger.info(s"Received reward withdraw update: $rewardWithdrawUpdate") >>
            rewardsWithdrawService.combineNew(
              Signed(rewardWithdrawUpdate, signedUpdate.proofs),
              state,
              context.currentSnapshotEpochProgress,
              context.lastSyncGlobalEpochProgress
            )
      }
  }
}
