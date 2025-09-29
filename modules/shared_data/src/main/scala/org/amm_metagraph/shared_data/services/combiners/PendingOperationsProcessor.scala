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
    )(implicit l0Context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] =
      for {
        _ <- logger.info(s"Starting pending operations processing with snapshot ordinal: ${context.currentSnapshotOrdinal}")
        _ <- logger.debug(
          s"Processing context - lastSyncGlobalEpochProgress: ${context.lastSyncGlobalEpochProgress}, currencyId: ${context.currencyId}"
        )

        pendingAllowSpends = getPendingAllowSpendsUpdates(state.calculated)
        pendingSpendActions = getPendingSpendActionsUpdates(state.calculated)

        _ <- logger.info(s"Found ${pendingAllowSpends.size} pending allow spends and ${pendingSpendActions.size} pending spend actions")
        _ <- logger.debug(s"Pending allow spends by type: ${getUpdateTypeBreakdown(pendingAllowSpends)}")
        _ <- logger.debug(s"Pending spend actions by type: ${getUpdateTypeBreakdown(pendingSpendActions)}")

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

        _ <- logger.info("Successfully completed pending operations processing")
      } yield stateCombinedByPendingSpendActions

    private def getUpdateTypeBreakdown[T <: PendingAction[AmmUpdate]](
      pendingUpdates: SortedSet[T]
    ): Map[String, Int] =
      pendingUpdates
        .groupBy(_.update.value match {
          case _: LiquidityPoolUpdate => "LiquidityPool"
          case _: StakingUpdate       => "Staking"
          case _: SwapUpdate          => "Swap"
          case _: WithdrawalUpdate    => "Withdrawal"
          case _                      => "Unknown"
        })
        .view
        .mapValues(_.size)
        .toMap

    def getPendingAllowSpendsUpdates(
      state: AmmCalculatedState
    ): SortedSet[PendingAllowSpend[AmmUpdate]] = {
      val lpUpdates = getPendingAllowSpendsLiquidityPoolUpdates(state)
      val stakingUpdates = getPendingAllowSpendsStakingUpdates(state)
      val swapUpdates = getPendingAllowSpendsSwapUpdates(state)

      val result = lpUpdates ++ stakingUpdates ++ swapUpdates
      result
    }

    def getPendingSpendActionsUpdates(
      state: AmmCalculatedState
    ): SortedSet[PendingSpendAction[AmmUpdate]] = {
      val lpUpdates = getPendingSpendActionLiquidityPoolUpdates(state)
      val stakingUpdates = getPendingSpendActionStakingUpdates(state)
      val swapUpdates = getPendingSpendActionSwapUpdates(state)
      val withdrawalUpdates = getPendingSpendActionWithdrawalUpdates(state)

      val result = lpUpdates ++ stakingUpdates ++ swapUpdates ++ withdrawalUpdates
      result
    }

    private def combinePendingAllowSpendsUpdates(
      context: ProcessingContext,
      pendingAllowSpends: SortedSet[PendingAllowSpend[AmmUpdate]],
      state: DataState[AmmOnChainState, AmmCalculatedState]
    )(implicit l0Context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] =
      for {
        _ <- logger.info(s"Starting to combine ${pendingAllowSpends.size} pending allow spends")
        _ <- logger.debug(s"Global snapshot sync allow spends available: ${context.globalSnapshotSyncAllowSpends.nonEmpty}")

        pendingAllowSpendsToCombine =
          if (context.globalSnapshotSyncAllowSpends.nonEmpty) {
            logger.debug("Using all pending allow spends (global snapshots available)")
            pendingAllowSpends
          } else {
            val filtered = pendingAllowSpends.filter { pending =>
              val shouldProcess = pending.update.value match {
                case lpUpdate: LiquidityPoolUpdate      => lpUpdate.maxValidGsEpochProgress < context.lastSyncGlobalEpochProgress
                case stakingUpdate: StakingUpdate       => stakingUpdate.maxValidGsEpochProgress < context.lastSyncGlobalEpochProgress
                case withdrawalUpdate: WithdrawalUpdate => withdrawalUpdate.maxValidGsEpochProgress < context.lastSyncGlobalEpochProgress
                case swapUpdate: SwapUpdate             => swapUpdate.maxValidGsEpochProgress < context.lastSyncGlobalEpochProgress
                case _                                  => false
              }
              if (!shouldProcess) {
                logger.debug(s"Skipping pending allow spend ${pending.updateHash} - epoch progress not reached")
              }
              shouldProcess
            }
            logger.debug(s"Filtered to ${filtered.size} pending allow spends based on epoch progress")
            filtered
          }

        _ <- logger.info(s"Processing ${pendingAllowSpendsToCombine.size} filtered pending allow spends")

        result <- pendingAllowSpendsToCombine.toList.zipWithIndex
          .foldLeftM(state) {
            case (acc, (pendingUpdate, index)) =>
              for {
                _ <- logger.debug(
                  s"Processing pending allow spend ${index + 1}/${pendingAllowSpendsToCombine.size}: ${pendingUpdate.updateHash}"
                )

                processedState <- pendingUpdate.update.value match {
                  case lpUpdate: LiquidityPoolUpdate =>
                    for {
                      _ <- logger.info(s"Processing LP pending allow spend from ${pendingUpdate.update.source}")
                      result <- liquidityPoolCombinerService.combinePendingAllowSpend(
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
                      _ <- logger.debug(s"Successfully processed LP pending allow spend ${pendingUpdate.updateHash}")
                    } yield result

                  case stakingUpdate: StakingUpdate =>
                    for {
                      _ <- logger.info(s"Processing staking pending allow spend from ${pendingUpdate.update.source}")
                      result <- stakingCombinerService.combinePendingAllowSpend(
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
                      _ <- logger.debug(s"Successfully processed staking pending allow spend ${pendingUpdate.updateHash}")
                    } yield result

                  case swapUpdate: SwapUpdate =>
                    for {
                      _ <- logger.info(
                        s"Processing swap pending allow spend from ${pendingUpdate.update.source}: amount=${swapUpdate.amountIn}, from=${swapUpdate.swapFromPair} to=${swapUpdate.swapToPair}"
                      )
                      result <- swapCombinerService.combinePendingAllowSpend(
                        PendingAllowSpend(
                          Signed(swapUpdate, pendingUpdate.update.proofs),
                          pendingUpdate.updateHash,
                          pendingUpdate.pricingTokenInfo
                        ),
                        acc,
                        context.lastSyncGlobalEpochProgress,
                        context.globalSnapshotSyncAllowSpends,
                        context.currentSnapshotOrdinal,
                        context.currencyId
                      )
                      _ <- logger.debug(s"Successfully processed swap pending allow spend ${pendingUpdate.updateHash}")
                    } yield result

                  case unknownUpdate =>
                    logger.warn(s"Unknown update type in pending allow spend: ${unknownUpdate.getClass.getSimpleName}") >>
                      acc.pure[F]
                }
              } yield processedState
          }
          .handleErrorWith { error =>
            logger.error(s"Error processing pending allow spends: ${error.getMessage}") >>
              state.pure[F]
          }

        _ <- logger.info(s"Completed combining pending allow spends")
      } yield result

    private def combinePendingSpendTransactionsUpdates(
      context: ProcessingContext,
      pendingSpendActions: SortedSet[PendingSpendAction[AmmUpdate]],
      state: DataState[AmmOnChainState, AmmCalculatedState]
    )(implicit l0Context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
      for {
        _ <- logger.info(s"Starting to combine ${pendingSpendActions.size} pending spend actions")
        _ <- logger.debug(s"Global snapshot sync spend actions available: ${context.globalSnapshotsSyncSpendActions.nonEmpty}")

        pendingSpendActionsToCombine =
          if (context.globalSnapshotsSyncSpendActions.nonEmpty) {
            logger.debug("Using all pending spend actions (global snapshots available)")
            pendingSpendActions
          } else {
            val filtered = pendingSpendActions.filter { pending =>
              val shouldProcess = pending.update.value match {
                case lpUpdate: LiquidityPoolUpdate      => lpUpdate.maxValidGsEpochProgress < context.lastSyncGlobalEpochProgress
                case stakingUpdate: StakingUpdate       => stakingUpdate.maxValidGsEpochProgress < context.lastSyncGlobalEpochProgress
                case withdrawalUpdate: WithdrawalUpdate => withdrawalUpdate.maxValidGsEpochProgress < context.lastSyncGlobalEpochProgress
                case swapUpdate: SwapUpdate             => swapUpdate.maxValidGsEpochProgress < context.lastSyncGlobalEpochProgress
                case _                                  => false
              }
              if (!shouldProcess) {
                logger.debug(s"Skipping pending spend action ${pending.updateHash} - epoch progress not reached")
              }
              shouldProcess
            }
            logger.debug(s"Filtered to ${filtered.size} pending spend actions based on epoch progress")
            filtered
          }

        _ <- logger.info(s"Processing ${pendingSpendActionsToCombine.size} filtered pending spend actions")

        result <- pendingSpendActionsToCombine.toList.zipWithIndex
          .foldLeftM(state) {
            case (acc, (pendingUpdate, index)) =>
              for {
                _ <- logger.debug(
                  s"Processing pending spend action ${index + 1}/${pendingSpendActionsToCombine.size}: ${pendingUpdate.updateHash}"
                )

                processedState <- pendingUpdate.update.value match {
                  case lpUpdate: LiquidityPoolUpdate =>
                    for {
                      _ <- logger.info(s"Processing LP spend action from ${pendingUpdate.update.source}")
                      result <- liquidityPoolCombinerService.combinePendingSpendAction(
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
                      _ <- logger.debug(s"Successfully processed LP spend action ${pendingUpdate.updateHash}")
                    } yield result

                  case stakingUpdate: StakingUpdate =>
                    for {
                      _ <- logger.info(s"Processing staking spend action from ${pendingUpdate.update.source}")
                      result <- stakingCombinerService.combinePendingSpendAction(
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
                      _ <- logger.debug(s"Successfully processed staking spend action ${pendingUpdate.updateHash}")
                    } yield result

                  case withdrawalUpdate: WithdrawalUpdate =>
                    for {
                      _ <- logger.info(s"Processing withdrawal spend action from ${pendingUpdate.update.source}")
                      result <- withdrawalCombinerService.combinePendingSpendAction(
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
                      _ <- logger.debug(s"Successfully processed withdrawal spend action ${pendingUpdate.updateHash}")
                    } yield result

                  case swapUpdate: SwapUpdate =>
                    for {
                      _ <- logger.info(
                        s"Processing swap spend action from ${pendingUpdate.update.source}: amount=${swapUpdate.amountIn}, from=${swapUpdate.swapFromPair} to=${swapUpdate.swapToPair}"
                      )
                      result <- swapCombinerService.combinePendingSpendAction(
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
                      _ <- logger.debug(s"Successfully processed swap spend action ${pendingUpdate.updateHash}")
                    } yield result

                  case unknownUpdate =>
                    logger.warn(s"Unknown update type in pending spend action: ${unknownUpdate.getClass.getSimpleName}") >>
                      acc.pure[F]
                }
              } yield processedState
          }
          .handleErrorWith { error =>
            logger.error(s"Error processing pending spend actions: ${error.getMessage}") >>
              state.pure[F]
          }

        _ <- logger.info(s"Completed combining pending spend actions")
      } yield result
    }
  }
}
