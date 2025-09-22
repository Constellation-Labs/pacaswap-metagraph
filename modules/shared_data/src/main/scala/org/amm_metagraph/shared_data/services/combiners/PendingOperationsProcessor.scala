package org.amm_metagraph.shared_data.services.combiners

import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.SortedSet

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.security.signature.Signed

import org.amm_metagraph.shared_data.services.combiners.operations._
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.Staking._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.Swap._
import org.amm_metagraph.shared_data.types.Withdrawal._
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

trait PendingOperationsProcessor[F[_]] {
  def processPendingOperations(
    state: DataState[AmmOnChainState, AmmCalculatedState],
    context: ProcessingContext
  )(implicit l0Context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]]
}

object PendingOperationsProcessor {
  def make[F[_]: Async](
    liquidityPoolCombinerService: LiquidityPoolCombinerService[F],
    stakingCombinerService: StakingCombinerService[F],
    swapCombinerService: SwapCombinerService[F],
    withdrawalCombinerService: WithdrawalCombinerService[F]
  ): PendingOperationsProcessor[F] = new PendingOperationsProcessor[F] {

    val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

    override def processPendingOperations(
      state: DataState[AmmOnChainState, AmmCalculatedState],
      context: ProcessingContext
    )(implicit l0Context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
      val pendingAllowSpends = getPendingAllowSpendsUpdates(state.calculated)
      val pendingSpendActions = getPendingSpendActionsUpdates(state.calculated)
      for {
        stateCombinedByPendingAllowSpends <- combinePendingAllowSpendsUpdates(
          context,
          pendingAllowSpends,
          state
        )

        stateCombinedByPendingSpendActions <- combinePendingSpendTransactionsUpdates(
          context,
          pendingSpendActions,
          stateCombinedByPendingAllowSpends
        )

      } yield stateCombinedByPendingSpendActions
    }

    def getPendingAllowSpendsUpdates(
      state: AmmCalculatedState
    ): SortedSet[PendingAllowSpend[AmmUpdate]] =
      getPendingAllowSpendsLiquidityPoolUpdates(state) ++
        getPendingAllowSpendsStakingUpdates(state) ++
        getPendingAllowSpendsSwapUpdates(state)

    def getPendingSpendActionsUpdates(
      state: AmmCalculatedState
    ): SortedSet[PendingSpendAction[AmmUpdate]] =
      getPendingSpendActionLiquidityPoolUpdates(state) ++
        getPendingSpendActionStakingUpdates(state) ++
        getPendingSpendActionSwapUpdates(state) ++
        getPendingSpendActionWithdrawalUpdates(state)

    private def combinePendingAllowSpendsUpdates(
      context: ProcessingContext,
      pendingAllowSpends: SortedSet[PendingAllowSpend[AmmUpdate]],
      state: DataState[AmmOnChainState, AmmCalculatedState]
    )(implicit l0Context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
      val pendingAllowSpendsToCombine = if (context.globalSnapshotSyncAllowSpends.nonEmpty) {
        pendingAllowSpends
      } else {
        pendingAllowSpends.filter { pending =>
          pending.update.value match {
            case lpUpdate: LiquidityPoolUpdate      => lpUpdate.maxValidGsEpochProgress < context.lastSyncGlobalEpochProgress
            case stakingUpdate: StakingUpdate       => stakingUpdate.maxValidGsEpochProgress < context.lastSyncGlobalEpochProgress
            case withdrawalUpdate: WithdrawalUpdate => withdrawalUpdate.maxValidGsEpochProgress < context.lastSyncGlobalEpochProgress
            case swapUpdate: SwapUpdate             => swapUpdate.maxValidGsEpochProgress < context.lastSyncGlobalEpochProgress
            case _                                  => false
          }
        }
      }

      pendingAllowSpendsToCombine.toList.foldLeftM(state) { (acc, pendingUpdate) =>
        pendingUpdate.update.value match {
          case lpUpdate: LiquidityPoolUpdate =>
            logger.info(s"Processing LP pending allow spend: ${lpUpdate}") >>
              liquidityPoolCombinerService.combinePendingAllowSpend(
                PendingAllowSpend(
                  Signed(lpUpdate, pendingUpdate.update.proofs),
                  pendingUpdate.updateHash,
                  pendingUpdate.pricingTokenInfo
                ),
                acc,
                context.lastSyncGlobalEpochProgress,
                context.globalSnapshotSyncAllowSpends,
                context.currencyId
              )

          case stakingUpdate: StakingUpdate =>
            logger.info(s"Processing staking pending allow spend: ${stakingUpdate}") >>
              stakingCombinerService.combinePendingAllowSpend(
                PendingAllowSpend(
                  Signed(stakingUpdate, pendingUpdate.update.proofs),
                  pendingUpdate.updateHash,
                  pendingUpdate.pricingTokenInfo
                ),
                acc,
                context.lastSyncGlobalEpochProgress,
                context.globalSnapshotSyncAllowSpends,
                context.currencyId
              )

          case swapUpdate: SwapUpdate =>
            logger.info(s"Processing swap pending allow spend: ${swapUpdate}") >>
              swapCombinerService.combinePendingAllowSpend(
                PendingAllowSpend(
                  Signed(swapUpdate, pendingUpdate.update.proofs),
                  pendingUpdate.updateHash,
                  pendingUpdate.pricingTokenInfo
                ),
                acc,
                context.lastSyncGlobalEpochProgress,
                context.globalSnapshotSyncAllowSpends,
                context.currencyId
              )
          case _ => acc.pure
        }
      }
    }

    private def combinePendingSpendTransactionsUpdates(
      context: ProcessingContext,
      pendingSpendActions: SortedSet[PendingSpendAction[AmmUpdate]],
      state: DataState[AmmOnChainState, AmmCalculatedState]
    )(implicit l0Context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
      val pendingSpendActionsToCombine = if (context.globalSnapshotsSyncSpendActions.nonEmpty) {
        pendingSpendActions
      } else {
        pendingSpendActions.filter { pending =>
          pending.update.value match {
            case lpUpdate: LiquidityPoolUpdate      => lpUpdate.maxValidGsEpochProgress < context.lastSyncGlobalEpochProgress
            case stakingUpdate: StakingUpdate       => stakingUpdate.maxValidGsEpochProgress < context.lastSyncGlobalEpochProgress
            case withdrawalUpdate: WithdrawalUpdate => withdrawalUpdate.maxValidGsEpochProgress < context.lastSyncGlobalEpochProgress
            case swapUpdate: SwapUpdate             => swapUpdate.maxValidGsEpochProgress < context.lastSyncGlobalEpochProgress
            case _                                  => false
          }
        }
      }

      pendingSpendActionsToCombine.toList.foldLeftM(state) { (acc, pendingUpdate) =>
        pendingUpdate.update.value match {
          case lpUpdate: LiquidityPoolUpdate =>
            logger.info(s"Processing LP spend action: ${lpUpdate}") >>
              liquidityPoolCombinerService.combinePendingSpendAction(
                PendingSpendAction(
                  Signed(lpUpdate, pendingUpdate.update.proofs),
                  pendingUpdate.updateHash,
                  pendingUpdate.generatedSpendAction,
                  pendingUpdate.pricingTokenInfo
                ),
                acc,
                context.lastSyncGlobalEpochProgress,
                context.globalSnapshotsSyncSpendActions,
                context.currentSnapshotOrdinal,
                context.currencyId
              )
          case stakingUpdate: StakingUpdate =>
            logger.info(s"Processing Staking spend action: ${stakingUpdate}") >>
              stakingCombinerService.combinePendingSpendAction(
                PendingSpendAction(
                  Signed(stakingUpdate, pendingUpdate.update.proofs),
                  pendingUpdate.updateHash,
                  pendingUpdate.generatedSpendAction,
                  pendingUpdate.pricingTokenInfo
                ),
                acc,
                context.lastSyncGlobalEpochProgress,
                context.globalSnapshotsSyncSpendActions,
                context.currentSnapshotOrdinal,
                context.currencyId
              )
          case withdrawalUpdate: WithdrawalUpdate =>
            logger.info(s"Processing Withdrawal spend action: ${withdrawalUpdate}") >>
              withdrawalCombinerService.combinePendingSpendAction(
                PendingSpendAction(
                  Signed(withdrawalUpdate, pendingUpdate.update.proofs),
                  pendingUpdate.updateHash,
                  pendingUpdate.generatedSpendAction,
                  pendingUpdate.pricingTokenInfo
                ),
                acc,
                context.lastSyncGlobalEpochProgress,
                context.globalSnapshotsSyncSpendActions,
                context.currentSnapshotOrdinal
              )
          case swapUpdate: SwapUpdate =>
            logger.info(s"Processing Swap spend action: ${swapUpdate}") >>
              swapCombinerService.combinePendingSpendAction(
                PendingSpendAction(
                  Signed(swapUpdate, pendingUpdate.update.proofs),
                  pendingUpdate.updateHash,
                  pendingUpdate.generatedSpendAction,
                  pendingUpdate.pricingTokenInfo
                ),
                acc,
                context.lastSyncGlobalEpochProgress,
                context.globalSnapshotsSyncSpendActions,
                context.currentSnapshotOrdinal,
                context.currencyId
              )
          case _ => acc.pure
        }
      }
    }
  }
}
