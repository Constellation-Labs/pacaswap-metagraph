package org.amm_metagraph.shared_data.combiners

import cats.data.OptionT
import cats.effect.Async
import cats.syntax.all._
import org.amm_metagraph.shared_data.Utils.toAddress
import org.amm_metagraph.shared_data.combiners.LiquidityPoolCombiner.combineLiquidityPool
import org.amm_metagraph.shared_data.combiners.StakingCombiner.combineStaking
import org.amm_metagraph.shared_data.combiners.WithdrawCombiner.combineWithdraw
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, LiquidityPoolUpdate, StakingUpdate, WithdrawUpdate}
import org.amm_metagraph.shared_data.types.States._
import org.tessellation.currency.dataApplication.{DataState, L0NodeContext}
import org.tessellation.ext.cats.syntax.next.catsSyntaxNext
import org.tessellation.schema.SnapshotOrdinal
import org.tessellation.security.SecurityProvider
import org.tessellation.security.signature.Signed
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger


trait CombinerService[F[_]] {
  def combine(
    oldState      : DataState[AmmOnChainState, AmmCalculatedState],
    updates       : List[Signed[AmmUpdate]],
    maybeL0Context: Option[L0NodeContext[F]]
  ): F[DataState[AmmOnChainState, AmmCalculatedState]]
}

object CombinerService {
  def make[F[_] : Async : SecurityProvider]: F[CombinerService[F]] = Async[F].delay {
    new CombinerService[F] {
      def logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("CombinerService")

      override def combine(
        oldState      : DataState[AmmOnChainState, AmmCalculatedState],
        updates       : List[Signed[AmmUpdate]],
        maybeL0Context: Option[L0NodeContext[F]]
      ): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
        val newState = DataState(AmmOnChainState(List.empty), AmmCalculatedState(oldState.calculated.ammState))
        if (updates.isEmpty) {
          logger.info("Snapshot without any updates, updating the state to empty updates").as(newState)
        } else {
          updates.foldLeftM(newState) { (acc, signedUpdate) =>
            for {
              address <- toAddress(signedUpdate.proofs.head)
              context <- maybeL0Context.toOptionT.getOrRaise(
                new IllegalStateException("Could not get l0 context")
              )
              currentSnapshotOrdinal <- OptionT(context.getLastCurrencySnapshot)
                .map(_.ordinal.next)
                .getOrElseF {
                  val message = "Could not get the ordinal from currency snapshot. lastCurrencySnapshot not found"
                  logger.error(message) >> new Exception(message).raiseError[F, SnapshotOrdinal]
                }
              updatedState <- signedUpdate.value match {
                case stakingUpdate: StakingUpdate =>
                  logger.info(s"Received a new staking update: $stakingUpdate") >>
                    combineStaking(acc, stakingUpdate, address, currentSnapshotOrdinal)

                case withdrawUpdate: WithdrawUpdate =>
                  logger.info(s"Received a new withdraw update: $withdrawUpdate").as(
                    combineWithdraw(acc, withdrawUpdate, address)
                  )

                case liquidityPoolUpdate: LiquidityPoolUpdate =>
                  logger.info(s"Received a new liquidity pool update: $liquidityPoolUpdate") >>
                    combineLiquidityPool(acc, liquidityPoolUpdate, address)
              }
            } yield updatedState
          }
        }
      }
    }

  }
}
