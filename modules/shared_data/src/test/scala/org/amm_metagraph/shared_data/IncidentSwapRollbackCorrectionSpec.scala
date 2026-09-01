package org.amm_metagraph.shared_data

import scala.collection.immutable.SortedMap

import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.swap.CurrencyId
import io.constellationnetwork.security.hash.Hash

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.{NonNegLong, PosLong}
import monocle.syntax.all._
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States.{AmmCalculatedState, LiquidityPoolCalculatedState, OperationType}
import weaver.SimpleIOSuite

/** The book correction for the 741789 rollback.
  *
  * The deltas are pinned against the divergence measured on the live chain, not restated from the object under test: the monitor read the
  * book 4,371.71305445 DAG above the wallet and 19,552.65547325 SWAP below it, unchanged across four samples over forty minutes.
  */
object IncidentSwapRollbackCorrectionSpec extends SimpleIOSuite {

  private val SWAP = Address("DAG7X5idd4aLfp4XC6WQdG1eDfR3LGPVEwtUUB2W")

  /** Reserves read from the live pool at 2026-08-31 21:43Z, before the correction. */
  private val liveSwapLeg = 5111911820916687L
  private val liveDagLeg = 1145998709526253L

  /** Book minus wallet, measured by scripts/monitor_collateral.py against the same chain. */
  private val measuredDagExcess = 437171305445L
  private val measuredSwapShortfall = 1955265547325L

  private def poolAt(swapLeg: Long, dagLeg: Long): LiquidityPool =
    LiquidityPool(
      updateHash = Hash.empty,
      poolId = PoolId(SWAP.value.value),
      tokenA = TokenInformation(Some(CurrencyId(SWAP)), PosLong.unsafeFrom(swapLeg)),
      tokenB = TokenInformation(None, PosLong.unsafeFrom(dagLeg)),
      owner = SWAP,
      k = BigInt(swapLeg) * BigInt(dagLeg),
      poolShares = PoolShares(PosLong.unsafeFrom(1L), SortedMap.empty),
      poolFees = FeeDistributor.standard
    )

  private def stateWith(pool: LiquidityPool): AmmCalculatedState =
    AmmCalculatedState().focus(_.operations).modify {
      _.updated(
        OperationType.LiquidityPool,
        LiquidityPoolCalculatedState.empty.focus(_.confirmed.value).modify(_.updated(IncidentSwapRollbackCorrection.poolId, pool))
      )
    }

  private def legs(s: AmmCalculatedState): (Long, Long, BigInt) = {
    val p = getLiquidityPoolCalculatedState(s).confirmed.value(IncidentSwapRollbackCorrection.poolId)
    (p.tokenA.amount.value, p.tokenB.amount.value, p.k)
  }

  private val at = ProtocolActivation.swapRollbackCorrection
  private def ord(o: Long): SnapshotOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(o))

  pureTest("the correction closes exactly the divergence the monitor measured") {
    val before = stateWith(poolAt(liveSwapLeg, liveDagLeg))
    val after = IncidentSwapRollbackCorrection.applyTo(before, at).toOption.get
    val (swapLeg, dagLeg, _) = legs(after)

    expect.all(
      // The book was short this much SWAP against the wallet, so it goes up by exactly that.
      swapLeg - liveSwapLeg == measuredSwapShortfall,
      // And long this much DAG, so it comes down by exactly that.
      liveDagLeg - dagLeg == measuredDagExcess,
      // Independently: the object's own deltas are the two rollback entries from the node log.
      IncidentSwapRollbackCorrection.swapDelta == 1933266495245L + 21999052080L,
      IncidentSwapRollbackCorrection.dagDelta == -(432254469442L + 4916836003L)
    )
  }

  pureTest("k is recomputed from the corrected legs, never carried over") {
    val before = stateWith(poolAt(liveSwapLeg, liveDagLeg))
    val staleK = legs(before)._3
    val after = IncidentSwapRollbackCorrection.applyTo(before, at).toOption.get
    val (swapLeg, dagLeg, k) = legs(after)
    expect.all(k == BigInt(swapLeg) * BigInt(dagLeg), k != staleK)
  }

  pureTest("it fires exactly once even when multiple data blocks are combined at the activation ordinal") {
    val before = stateWith(poolAt(liveSwapLeg, liveDagLeg))
      .focus(_.lastProcessedCurrencyOrdinal)
      .replace(Some(ord(at.value.value - 1L)))
    val one = IncidentSwapRollbackCorrection.applyTo(before, at).toOption.get
    // Tessellation calls combine once per accepted data block. cleanupAndFinalize records the
    // ordinal after block one, and block two receives that chained state at the same ordinal.
    val finalizedBlockOne = one
      .focus(_.lastProcessedCurrencyOrdinal)
      .replace(Some(at))
    val secondBlock = IncidentSwapRollbackCorrection.applyTo(finalizedBlockOne, at).toOption.get
    val later = IncidentSwapRollbackCorrection.applyTo(finalizedBlockOne, ord(at.value.value + 1L)).toOption.get
    expect.all(
      legs(IncidentSwapRollbackCorrection.applyTo(before, ord(at.value.value - 1L)).toOption.get) == legs(before),
      legs(secondBlock) == legs(one),
      legs(later) == legs(one)
    )
  }

  pureTest("it retries after activation when SDK fallback preserved the pre-correction state") {
    val before = stateWith(poolAt(liveSwapLeg, liveDagLeg))
      .focus(_.lastProcessedCurrencyOrdinal)
      .replace(Some(ord(at.value.value - 1L)))
    val retried = IncidentSwapRollbackCorrection.applyTo(before, ord(at.value.value + 1L)).toOption.get

    expect.all(
      legs(retried)._1 - liveSwapLeg == measuredSwapShortfall,
      liveDagLeg - legs(retried)._2 == measuredDagExcess
    )
  }

  pureTest("it refuses rather than guessing when the pool is not the shape it expects") {
    val flipped = poolAt(liveSwapLeg, liveDagLeg)
      .focus(_.tokenA.identifier)
      .replace(None)
      .focus(_.tokenB.identifier)
      .replace(Some(CurrencyId(SWAP)))
    expect.all(
      IncidentSwapRollbackCorrection.applyTo(stateWith(flipped), at).isLeft,
      IncidentSwapRollbackCorrection.applyTo(AmmCalculatedState(), at).isLeft,
      // A DAG leg too small to absorb the reduction must fail, not wrap into a bogus PosLong.
      IncidentSwapRollbackCorrection.applyTo(stateWith(poolAt(liveSwapLeg, 1000L)), at).isLeft
    )
  }

  pureTest("the correction ordinal is ahead of the enforcement ordinal") {
    // The book has to be right before the invariant starts refusing snapshots on it, or the first
    // enforced ordinal halts the chain on a divergence we already know about.
    expect(
      ProtocolActivation.swapRollbackCorrection.value.value <
        ProtocolActivation.collateralInvariantEnforced.value.value
    )
  }
}
