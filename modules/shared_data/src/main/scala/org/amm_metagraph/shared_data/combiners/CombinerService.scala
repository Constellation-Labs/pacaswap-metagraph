package org.amm_metagraph.shared_data.combiners

import cats.data.OptionT
import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.ext.cats.syntax.next.catsSyntaxNext
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hasher, SecurityProvider}

import monocle.syntax.all._
import org.amm_metagraph.shared_data.SpendTransactions.getCombinedSpendTransactions
import org.amm_metagraph.shared_data.Utils.toAddress
import org.amm_metagraph.shared_data.combiners.LiquidityPoolCombiner.combineLiquidityPool
import org.amm_metagraph.shared_data.combiners.StakingCombiner.combineStaking
import org.amm_metagraph.shared_data.combiners.SwapCombiner.combineSwap
import org.amm_metagraph.shared_data.combiners.WithdrawCombiner.combineWithdraw
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
  def make[F[_]: Async: Hasher: SecurityProvider]: F[CombinerService[F]] = Async[F].delay {
    new CombinerService[F] {
      def logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("CombinerService")

      override def combine(
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        incomingUpdates: List[Signed[AmmUpdate]]
      )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
        val newState =
          DataState(
            AmmOnChainState(List.empty),
            AmmCalculatedState(
              oldState.calculated.confirmedOperations,
              oldState.calculated.pendingUpdates,
              oldState.calculated.spendTransactions
            )
          )

        val pendingUpdates = oldState.calculated.pendingUpdates
        val updates = incomingUpdates ++ pendingUpdates

        if (updates.isEmpty) {
          logger.info("Snapshot without any updates, updating the state to empty updates").as(newState)
        } else {
          logger.info(s"Incoming updates: ${incomingUpdates.size}") >>
            logger.info(s"Pending updates: ${pendingUpdates.size}") >>
            updates.foldLeftM(newState) { (acc, signedUpdate) =>
              for {
                address <- toAddress(signedUpdate.proofs.head)
                currentSnapshotOrdinal <- OptionT(context.getLastCurrencySnapshot)
                  .map(_.ordinal.next)
                  .getOrElseF {
                    val message = "Could not get the ordinal from currency snapshot. lastCurrencySnapshot not found"
                    logger.error(message) >> new Exception(message).raiseError[F, SnapshotOrdinal]
                  }
                combinedState <- signedUpdate.value match {
                  case stakingUpdate: StakingUpdate =>
                    logger.info(s"Received staking update: $stakingUpdate") >>
                      combineStaking(acc, Signed(stakingUpdate, signedUpdate.proofs), address, currentSnapshotOrdinal)

                  case withdrawUpdate: WithdrawUpdate =>
                    logger
                      .info(s"Received withdraw update: $withdrawUpdate")
                      .as(
                        combineWithdraw(acc, Signed(withdrawUpdate, signedUpdate.proofs), address)
                      )

                  case liquidityPoolUpdate: LiquidityPoolUpdate =>
                    logger.info(s"Received liquidity pool update: $liquidityPoolUpdate") >>
                      combineLiquidityPool(acc, Signed(liquidityPoolUpdate, signedUpdate.proofs), address)

                  case swapUpdate: SwapUpdate =>
                    logger.info(s"Received swap update: $swapUpdate") >>
                      combineSwap(acc, Signed(swapUpdate, signedUpdate.proofs), currentSnapshotOrdinal)
                }

                (createdPendingSpendTransactions, createdConcludedSpendTransactions) = getCombinedSpendTransactions(
                  combinedState.sharedArtifacts
                )

                updatedState <-
                  combinedState
                    .focus(_.calculated)
                    .modify(_ =>
                      combinedState.calculated
                        .focus(_.spendTransactions)
                        .modify(current => current ++ createdPendingSpendTransactions ++ createdConcludedSpendTransactions)
                    )
                    .pure
              } yield updatedState
            }
        }
      }
    }
  }
}
