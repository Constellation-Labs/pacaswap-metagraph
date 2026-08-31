package org.amm_metagraph.shared_data.services.combiners

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.DataState
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact.SpendAction
import io.constellationnetwork.schema.swap.CurrencyId

import org.amm_metagraph.shared_data.ProtocolActivation
import org.amm_metagraph.shared_data.types.LiquidityPool.LiquidityPool
import org.amm_metagraph.shared_data.types.States._
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

/** The invariant the AMM must satisfy at all times.
  *
  * The naive form - sum(reserve) == balance - is WRONG, and would fire on every healthy swap. A pool reserve is credited when the
  * SpendAction is generated, but the wallet only moves when the global layer settles that SpendAction. Between those two moments the book
  * legitimately runs ahead of the wallet on the inbound leg and behind it on the outbound leg. The correct statement has to net that flight
  * out:
  *
  * for every ledger L: sum(pool_reserve[L]) == balance[L] + inflight_in[L] - inflight_out[L]
  *
  * where the in-flight terms come from the metagraph's own pending spend actions - the exact amounts it has committed to move and booked,
  * but which have not settled yet.
  *
  * How each primitive lands in that statement:
  *
  *   - AllowSpend. A user's AllowSpend debits the USER and sits in the global activeAllowSpends until spent. It is never in the metagraph's
  *     balance, and the AMM has already booked it into a reserve at the pending stage, so it must be counted as inflight_in. It is, because
  *     the AMM's own PendingSpendAction carries the inbound leg.
  *
  *   - SpendTransaction. Each leg names source and destination. Legs INTO the metagraph address are inflight_in, legs OUT are inflight_out.
  *     This is the only source used, so the check cannot drift from what the AMM actually committed.
  *
  *   - TokenLock. The metagraph never locks its own funds - no code path creates a TokenLock from the metagraph address, and it holds none
  *     on any ledger. Users' locks are governance voting power and never touch a pool reserve, so they are correctly absent here. If the
  *     metagraph address ever DOES appear in a token lock, the invariant would silently over-report backing, so that case is flagged
  *     separately as an anomaly.
  *
  *   - Rewards. Minted by the currency reward mechanism, never custodied at the metagraph address, so outstanding rewards are not a claim
  *     on the wallet and are not subtracted.
  *
  * Advisory below `ProtocolActivation.collateralInvariantEnforced`, refusing from it onwards. It was written advisory because the book was
  * short 1,641,127.95926795 DAG by construction at the time, pending treasury funding; rejecting then would have refused every snapshot.
  * That funding landed on 2026-08-27 and every ledger read 1:1, but the check was left sampling one ordinal in fifty and only warning. On
  * 2026-08-31 the combine rolled back two already-settled swaps at ordinal 741789 and built the snapshot anyway. The invariant saw it -
  * `COLLATERAL_INVARIANT BREACH ordinal=741800 ledger=DAG` is in the node log - four ordinals too late to prevent anything.
  *
  * DAG is fungible and shared across every pool, so only the aggregate is meaningful. Each token belongs to exactly one pool, so those are
  * exact per pool. Never derive one from the other: that conflation previously produced a 147,940.10 DAG error.
  */
trait CollateralInvariant[F[_]] {
  def check(
    state: DataState[AmmOnChainState, AmmCalculatedState],
    context: ProcessingContext
  ): F[Unit]
}

object CollateralInvariant {

  /** Net value the metagraph has committed to move but which has not settled, per ledger. Positive = owed to the metagraph (inbound),
    * negative = owed by it (outbound). `None` keys the native DAG ledger, mirroring TokenInformation.identifier.
    */
  def inFlight(
    spendActions: Iterable[SpendAction],
    metagraphAddress: Address
  ): Map[Option[CurrencyId], BigInt] =
    spendActions.toList
      .flatMap(_.spendTransactions.toList)
      .foldLeft(Map.empty[Option[CurrencyId], BigInt]) { (acc, tx) =>
        val amount = BigInt(tx.amount.value.value)
        val delta =
          if (tx.destination === metagraphAddress && tx.source =!= metagraphAddress) amount
          else if (tx.source === metagraphAddress && tx.destination =!= metagraphAddress) -amount
          else BigInt(0) // self-transfer, or unrelated to this metagraph: no net effect
        if (delta === BigInt(0)) acc
        else acc.updated(tx.currencyId, acc.getOrElse(tx.currencyId, BigInt(0)) + delta)
      }

  case class LedgerPosition(
    ledger: String,
    reserve: BigInt,
    balance: BigInt,
    inFlightNet: BigInt
  ) {

    /** What the wallet is expected to hold once everything in flight settles. */
    def backing: BigInt = balance + inFlightNet

    /** Positive means the book promises more than the wallet can ever back. */
    def shortfall: BigInt = reserve - backing
    def breached: Boolean = shortfall > 0
  }

  def positions(
    pools: Iterable[LiquidityPool],
    metagraphAddress: Address,
    dagBalance: Option[BigInt],
    tokenBalance: CurrencyId => Option[BigInt],
    pendingSpendActions: Iterable[SpendAction] = Nil
  ): List[LedgerPosition] = {
    val flight = inFlight(pendingSpendActions, metagraphAddress)

    val dagReserve = pools.foldLeft(BigInt(0)) { (acc, p) =>
      acc + List(p.tokenA, p.tokenB).foldLeft(BigInt(0)) { (a, t) =>
        if (t.identifier.isEmpty) a + BigInt(t.amount.value) else a
      }
    }
    val dagRow = dagBalance.map { b =>
      LedgerPosition("DAG", dagReserve, b, flight.getOrElse(None, BigInt(0)))
    }.toList

    val tokenRows = pools.toList.flatMap { p =>
      List(p.tokenA, p.tokenB).flatMap { t =>
        t.identifier.flatMap { cid =>
          tokenBalance(cid).map { b =>
            LedgerPosition(cid.value.value.value, BigInt(t.amount.value), b, flight.getOrElse(cid.some, BigInt(0)))
          }
        }
      }
    }
    dagRow ++ tokenRows
  }

  /** How often the check runs. It sits inside consensus, so it must never become a cost the combine has to pay every snapshot: it reads
    * collections and writes log lines, both of which are real work on the critical path. Sampling keeps drift detection within a couple of
    * minutes while making the amortised cost negligible. It touches no state, so sampling cannot affect consensus.
    */
  val checkEveryNOrdinals: Long = 50L

  def make[F[_]: Async]: CollateralInvariant[F] = new CollateralInvariant[F] {
    val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("CollateralInvariant")

    override def check(
      state: DataState[AmmOnChainState, AmmCalculatedState],
      context: ProcessingContext
    ): F[Unit] = {
      val ordinal = context.currentSnapshotOrdinal.value.value

      // Everything below is inside defer so that nothing - not a missing map key, not a
      // malformed state - can throw while the effect is being BUILT. An eager throw here would
      // escape the caller's error handling and take the whole combine down with it, which is
      // exactly what happened the first time this was written.
      // Sampling only applies while the check is advisory. Once it can refuse a snapshot it has to
      // look at every one: a breach the combine is allowed to skip past is the failure this whole
      // mechanism exists to prevent, and 49 unchecked ordinals in 50 is how 741789 got built.
      val enforced = ProtocolActivation.collateralInvariantEnforcedActive(context.currentSnapshotOrdinal)

      Async[F].defer {
        if (!enforced && ordinal % checkEveryNOrdinals =!= 0L) Async[F].unit
        else {
          val self = context.currencyId.value

          // .get, never Map.apply: this runs before the LiquidityPool entry need exist.
          val pools = state.calculated.operations
            .get(OperationType.LiquidityPool)
            .collect { case lp: LiquidityPoolCalculatedState => lp }
            .fold(Iterable.empty[LiquidityPool])(_.confirmed.value.values)

          val pendingSpendActions = state.calculated.operations.values.toList.flatMap {
            _.pending.toList.collect { case p: PendingSpendAction[_] => p.generatedSpendAction }
          }

          context.lastSyncGlobalSnapshotInfo match {
            case None =>
              logger.warn("COLLATERAL_INVARIANT unknown: no global snapshot info this snapshot")
            case Some(info) =>
              val selfLocked = info.activeTokenLocks.flatMap(_.get(self)).exists(_.nonEmpty)
              val rows = positions(
                pools,
                self,
                info.balances.get(self).map(b => BigInt(b.value.value)),
                cid =>
                  info.lastCurrencySnapshots
                    .get(cid.value)
                    .flatMap(_.toOption)
                    .flatMap { case (_, ci) => ci.balances.get(self) }
                    .map(b => BigInt(b.value.value)),
                pendingSpendActions
              )

              val anomaly: F[Unit] =
                if (selfLocked)
                  logger.warn(
                    "COLLATERAL_INVARIANT anomaly: the metagraph address holds a token lock. " +
                      "Locked value sits outside the balances map, so backing is over-reported."
                  )
                else Async[F].unit

              // Only breaches are logged. A per-row debug line on every ledger every time
              // would put avoidable I/O on the consensus path for no signal.
              val breaches = rows.filter(_.breached)

              val describe = breaches.traverse_ { r =>
                logger.warn(
                  s"COLLATERAL_INVARIANT BREACH ordinal=$ordinal ledger=${r.ledger} " +
                    s"reserve=${r.reserve} balance=${r.balance} inFlightNet=${r.inFlightNet} " +
                    s"backing=${r.backing} shortfall=${r.shortfall} - " +
                    "the book promises more than the wallet can back"
                )
              }

              // Refusing means this combine yields no snapshot. That is the point: the alternative
              // is committing a book the wallet cannot back, which is unrecoverable without a
              // one-time fix. Only raise once the gate is active, so signed history keeps replaying
              // under the advisory behaviour it was produced with.
              val enforce: F[Unit] =
                if (enforced && breaches.nonEmpty)
                  logger.error(
                    s"COLLATERAL_INVARIANT_ENFORCED ordinal=$ordinal: refusing to build this snapshot. " +
                      s"${breaches.size} ledger(s) short: ${breaches.map(_.ledger).mkString(", ")}."
                  ) >> new IllegalStateException(
                    s"Collateral invariant violated at ordinal $ordinal on ${breaches.map(_.ledger).mkString(", ")}"
                  ).raiseError[F, Unit]
                else Async[F].unit

              anomaly >> describe >> enforce
          }
        }
      }
        // A fault in the CHECK must still never take the combine down - that bug was made once
        // already. But a deliberate refusal has to pass through, or enforcement is decorative.
        .handleErrorWith {
          case e: IllegalStateException if enforced => e.raiseError[F, Unit]
          case e                                    => logger.warn(e)("COLLATERAL_INVARIANT check failed; combine unaffected")
        }
    }
  }
}
