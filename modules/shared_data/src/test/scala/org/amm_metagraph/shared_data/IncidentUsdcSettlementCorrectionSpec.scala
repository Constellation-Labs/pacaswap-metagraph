package org.amm_metagraph.shared_data

import scala.collection.immutable.SortedMap

import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.swap.CurrencyId
import io.constellationnetwork.security.hash.Hash

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.{NonNegLong, PosLong}
import monocle.syntax.all._
import org.amm_metagraph.shared_data.services.combiners.L0CombinerService
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States.{AmmCalculatedState, LiquidityPoolCalculatedState, OperationType}
import weaver.SimpleIOSuite

/** The book correction for the USDC.dag pool after the swap reversed at 756063 that the global layer settled at 6887529.
  *
  * The deltas are pinned against the divergence measured on the live chain, not restated from the object under test: the monitor read the
  * book 21,774.19860646 DAG above the wallet and 53.06141748 USDC.dag below it, unchanged to the datum across every hourly sample from 09:04
  * UTC on 2026-09-08. The wallet holds this swap and the book does not, the opposite direction of the 747126 correction.
  */
object IncidentUsdcSettlementCorrectionSpec extends SimpleIOSuite {

  private val USDC = Address("DAG0S16WDgdAvh8VvroR6MWLdjmHYdzAF5S181xh")
  private val owner = Address("DAG62QdFnvW8xX3uGmo6F3yB2CT5i25hZoVmN6za")

  /** Reserves read from the live pool at currency ordinal 747294, after the divergence. */
  private val liveDagLeg = 104712276877445L
  private val liveUsdcLeg = 712488369605L

  /** Book minus wallet, measured by scripts/monitor_collateral.py against the same chain. */
  private val measuredDagExcess = 2177419860646L
  private val measuredUsdcShortfall = 5306141748L

  private def poolAt(dagLeg: Long, usdcLeg: Long): LiquidityPool =
    LiquidityPool(
      updateHash = Hash.empty,
      poolId = PoolId(USDC.value.value),
      tokenA = TokenInformation(None, PosLong.unsafeFrom(dagLeg)),
      tokenB = TokenInformation(Some(CurrencyId(USDC)), PosLong.unsafeFrom(usdcLeg)),
      owner = owner,
      k = BigInt(dagLeg) * BigInt(usdcLeg),
      poolShares = PoolShares(PosLong.unsafeFrom(1L), SortedMap.empty),
      poolFees = FeeDistributor.standard
    )

  private def stateWith(pool: LiquidityPool): AmmCalculatedState =
    AmmCalculatedState().focus(_.operations).modify {
      _.updated(
        OperationType.LiquidityPool,
        LiquidityPoolCalculatedState.empty.focus(_.confirmed.value).modify(_.updated(IncidentUsdcSettlementCorrection.poolId, pool))
      )
    }

  private def legs(s: AmmCalculatedState): (Long, Long, BigInt) = {
    val p = getLiquidityPoolCalculatedState(s).confirmed.value(IncidentUsdcSettlementCorrection.poolId)
    (p.tokenA.amount.value, p.tokenB.amount.value, p.k)
  }

  private val at = ProtocolActivation.usdcSettlementCorrection
  private def ord(o: Long): SnapshotOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(o))

  pureTest("the correction closes exactly the divergence the monitor measured") {
    val before = stateWith(poolAt(liveDagLeg, liveUsdcLeg))
    val after = IncidentUsdcSettlementCorrection.applyTo(before, at).toOption.get
    val (dagLeg, usdcLeg, _) = legs(after)

    expect(liveDagLeg - dagLeg == measuredDagExcess, s"DAG should fall by $measuredDagExcess, fell by ${liveDagLeg - dagLeg}")
      .and(
        expect(
          usdcLeg - liveUsdcLeg == measuredUsdcShortfall,
          s"USDC.dag should rise by $measuredUsdcShortfall, rose by ${usdcLeg - liveUsdcLeg}"
        )
      )
      .and(
        // Independently: the deltas are the two legs of the SpendAction emitted at 747126 for allow-spend 036c98c3.
        expect(IncidentUsdcSettlementCorrection.dagDelta == -2177419860646L, "DAG delta is minus the SpendAction's DAG leg")
      )
      .and(expect(IncidentUsdcSettlementCorrection.usdcDelta == 5306141748L, "USDC.dag delta is the SpendAction's USDC.dag leg"))
  }

  pureTest("k is recomputed from the corrected legs, never carried over") {
    val before = stateWith(poolAt(liveDagLeg, liveUsdcLeg))
    val staleK = legs(before)._3
    val after = IncidentUsdcSettlementCorrection.applyTo(before, at).toOption.get
    val (dagLeg, usdcLeg, k) = legs(after)
    expect(k == BigInt(dagLeg) * BigInt(usdcLeg), "k must be the product of the corrected legs")
      .and(expect(k != staleK, "k must not be the pre-correction product"))
  }

  pureTest("it fires exactly once even when multiple data blocks are combined at the activation ordinal") {
    val before = stateWith(poolAt(liveDagLeg, liveUsdcLeg))
      .focus(_.lastProcessedCurrencyOrdinal)
      .replace(Some(ord(at.value.value - 1L)))
    val one = IncidentUsdcSettlementCorrection.applyTo(before, at).toOption.get
    val finalizedBlockOne = one.focus(_.lastProcessedCurrencyOrdinal).replace(Some(at))
    val secondBlock = IncidentUsdcSettlementCorrection.applyTo(finalizedBlockOne, at).toOption.get
    val later = IncidentUsdcSettlementCorrection.applyTo(finalizedBlockOne, ord(at.value.value + 1L)).toOption.get
    expect(
      legs(IncidentUsdcSettlementCorrection.applyTo(before, ord(at.value.value - 1L)).toOption.get) == legs(before),
      "untouched below activation"
    ).and(expect(legs(secondBlock) == legs(one), "a second block at the activation ordinal must not apply it again"))
      .and(expect(legs(later) == legs(one), "a later ordinal must not apply it again"))
  }

  pureTest("it retries after activation when SDK fallback preserved the pre-correction state") {
    val before = stateWith(poolAt(liveDagLeg, liveUsdcLeg))
      .focus(_.lastProcessedCurrencyOrdinal)
      .replace(Some(ord(at.value.value - 1L)))
    val retried = IncidentUsdcSettlementCorrection.applyTo(before, ord(at.value.value + 1L)).toOption.get
    expect(liveDagLeg - legs(retried)._1 == measuredDagExcess, "DAG leg corrected on retry")
      .and(expect(legs(retried)._2 - liveUsdcLeg == measuredUsdcShortfall, "USDC.dag leg corrected on retry"))
  }

  pureTest("it refuses rather than guessing when the pool is not the shape it expects") {
    val flipped = poolAt(liveDagLeg, liveUsdcLeg)
      .focus(_.tokenA.identifier)
      .replace(Some(CurrencyId(USDC)))
      .focus(_.tokenB.identifier)
      .replace(None)
    expect(IncidentUsdcSettlementCorrection.applyTo(stateWith(flipped), at).isLeft, "sides swapped must be refused")
      .and(expect(IncidentUsdcSettlementCorrection.applyTo(AmmCalculatedState(), at).isLeft, "missing pool must be refused"))
      .and(
        // A DAG leg too small to absorb the reduction must fail, not wrap into a bogus PosLong.
        expect(IncidentUsdcSettlementCorrection.applyTo(stateWith(poolAt(1000L, liveUsdcLeg)), at).isLeft, "leg below delta must be refused")
      )
  }

  pureTest("correction arithmetic rejects Long overflow before narrowing") {
    expect(IncidentUsdcSettlementCorrection.applyTo(stateWith(poolAt(liveDagLeg, Long.MaxValue)), at).isLeft)
  }

  pureTest("the correction stays fail-closed until a finalized state proves it was applied") {
    val before = ord(at.value.value - 1L)
    val after = ord(at.value.value + 1L)
    expect(L0CombinerService.mustFailClosed(Some(at), Some(before), atOneTimeFix = false), "armed at activation")
      .and(expect(L0CombinerService.mustFailClosed(Some(after), Some(before), atOneTimeFix = false), "still armed while unproven"))
      .and(
        expect(!L0CombinerService.mustFailClosed(Some(after), Some(at), atOneTimeFix = false), "disarmed once finalized past activation")
      )
  }

  pureTest("the correction lands after the defect is gated off and before enforcement") {
    expect(
      ProtocolActivation.usdcSettlementCorrection.value.value > ProtocolActivation.settlementDeadlineExpiry.value.value,
      "the settlement-deadline expiry fix must be live before the book is corrected"
    ).and(
      expect(
        ProtocolActivation.usdcSettlementCorrection.value.value < ProtocolActivation.collateralInvariantEnforced.value.value,
        "the book must be right before the invariant may refuse snapshots"
      )
    )
  }
}
