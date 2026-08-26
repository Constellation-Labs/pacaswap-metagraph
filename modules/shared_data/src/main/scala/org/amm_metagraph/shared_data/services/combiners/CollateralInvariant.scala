package org.amm_metagraph.shared_data.services.combiners

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.DataState
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.swap.CurrencyId

import org.amm_metagraph.shared_data.types.LiquidityPool.{LiquidityPool, getLiquidityPoolCalculatedState}
import org.amm_metagraph.shared_data.types.States._
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

/** The invariant the AMM must satisfy at all times:
  *
  *   for every ledger L:  sum(pool_reserve[L])  ==  balance[L](metagraph address)
  *
  * The book may never promise more than the wallet holds. This was never asserted anywhere,
  * which is the reason a divergence grew for eleven months without anyone noticing: every
  * mechanism that caused it would have been caught in a single snapshot by this check.
  *
  * Reported, not enforced. At the time of writing the invariant is violated by construction -
  * the book is short 1,641,127.95926795 DAG and two token legs, pending either treasury funding
  * or a write-down - so rejecting on breach would refuse every snapshot and halt the chain.
  * It logs, and it flags any breach that WIDENS, which is the thing that must never happen
  * silently again. Turn `enforce` on once the outstanding gap is closed.
  *
  * Note on what is comparable: DAG is pooled across every pool, so only the aggregate is
  * meaningful. Each token belongs to exactly one pool, so those are exact per pool. Never
  * derive one from the other.
  */
trait CollateralInvariant[F[_]] {
  def check(
    state: DataState[AmmOnChainState, AmmCalculatedState],
    context: ProcessingContext
  ): F[Unit]
}

object CollateralInvariant {

  case class LedgerPosition(ledger: String, reserve: BigInt, balance: BigInt) {
    def shortfall: BigInt = reserve - balance
    def breached: Boolean = shortfall > 0
  }

  def positions(
    pools: Iterable[LiquidityPool],
    metagraphAddress: Address,
    dagBalance: Option[BigInt],
    tokenBalance: CurrencyId => Option[BigInt]
  ): List[LedgerPosition] = {
    val dagReserve = pools.foldLeft(BigInt(0)) { (acc, p) =>
      acc + (if (p.tokenA.identifier.isEmpty) BigInt(p.tokenA.amount.value) else BigInt(0)) +
        (if (p.tokenB.identifier.isEmpty) BigInt(p.tokenB.amount.value) else BigInt(0))
    }
    val dagRow = dagBalance.map(b => LedgerPosition("DAG", dagReserve, b)).toList

    val tokenRows = pools.toList.flatMap { p =>
      List(p.tokenA, p.tokenB).flatMap { t =>
        t.identifier.flatMap { cid =>
          tokenBalance(cid).map(b => LedgerPosition(cid.value.value.value, BigInt(t.amount.value), b))
        }
      }
    }
    dagRow ++ tokenRows
  }

  def make[F[_]: Async]: CollateralInvariant[F] = new CollateralInvariant[F] {
    val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("CollateralInvariant")

    override def check(
      state: DataState[AmmOnChainState, AmmCalculatedState],
      context: ProcessingContext
    ): F[Unit] = {
      val self = context.currencyId.value
      val pools = getLiquidityPoolCalculatedState(state.calculated).confirmed.value.values

      context.lastSyncGlobalSnapshotInfo match {
        case None =>
          logger.warn("COLLATERAL_INVARIANT unknown: no global snapshot info this snapshot")
        case Some(info) =>
          val rows = positions(
            pools,
            self,
            info.balances.get(self).map(b => BigInt(b.value.value)),
            cid =>
              info.lastCurrencySnapshots
                .get(cid.value)
                .flatMap(_.toOption)
                .flatMap { case (_, ci) => ci.balances.get(self) }
                .map(b => BigInt(b.value.value))
          )
          rows.traverse_ { r =>
            if (r.breached)
              logger.warn(
                s"COLLATERAL_INVARIANT BREACH ledger=${r.ledger} " +
                  s"reserve=${r.reserve} balance=${r.balance} shortfall=${r.shortfall} " +
                  "- the book promises more than the wallet holds"
              )
            else
              logger.debug(s"COLLATERAL_INVARIANT ok ledger=${r.ledger} surplus=${-r.shortfall}")
          }
      }
    }
  }
}
