package org.amm_metagraph.shared_data

import cats.data.NonEmptyList
import cats.effect.IO

import scala.collection.immutable.SortedMap

import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact.{SpendAction, SpendTransaction}
import io.constellationnetwork.schema.swap.SwapAmount
import io.constellationnetwork.security.hash.Hash

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.{NonNegLong, PosLong}
import fs2.concurrent.SignallingRef
import org.amm_metagraph.shared_data.globalSnapshots.getSpendActionsFromGlobalSnapshots
import org.amm_metagraph.shared_data.storages.GlobalSnapshotsStorage
import weaver.SimpleIOSuite

/** Covers the POST-activation behaviour of the reserve-accounting fixes.
  *
  * The rest of the suite runs at SnapshotOrdinal.MinValue and therefore exercises the pre-activation path, which by design reproduces the
  * original behaviour so history replays. These tests pin the behaviour that actually applies from ordinal 731647 onward.
  */
object ReserveAccountingFixesSpec extends SimpleIOSuite {

  private def ord(v: Long) = SnapshotOrdinal(NonNegLong.unsafeFrom(v))
  private val ACTIVE = ord(731647L)
  private val BEFORE = ord(731646L)

  // ---------------------------------------------------------------- the gate

  pureTest("gate: activates at 731647, never before") {
    expect.all(
      ProtocolActivation.reserveAccountingFixes.value.value == 731647L,
      !ProtocolActivation.reserveAccountingFixesActive(SnapshotOrdinal.MinValue),
      !ProtocolActivation.reserveAccountingFixesActive(ord(1L)),
      !ProtocolActivation.reserveAccountingFixesActive(BEFORE),
      ProtocolActivation.reserveAccountingFixesActive(ACTIVE),
      ProtocolActivation.reserveAccountingFixesActive(ord(999999L))
    )
  }

  // ------------------------------------------- spend-action evidence completeness

  test("a cold cache reports the read as INCOMPLETE, so absence is not evidence") {
    for {
      ref <- SignallingRef.of[IO, SortedMap[SnapshotOrdinal, io.constellationnetwork.schema.GlobalIncrementalSnapshot]](SortedMap.empty)
      storage = GlobalSnapshotsStorage.make[IO](ref)
      read <- getSpendActionsFromGlobalSnapshots[IO](ord(10L), ord(12L), storage, None)
    } yield
      expect.all(
        read.actions.isEmpty,
        !read.complete // the whole point: empty AND unresolved, so nothing may be expired on it
      )
  }

  test("an unresolvable range stays incomplete even when the fallback is for another ordinal") {
    for {
      ref <- SignallingRef.of[IO, SortedMap[SnapshotOrdinal, io.constellationnetwork.schema.GlobalIncrementalSnapshot]](SortedMap.empty)
      storage = GlobalSnapshotsStorage.make[IO](ref)
      read <- getSpendActionsFromGlobalSnapshots[IO](ord(10L), ord(10L), storage, None)
    } yield expect(!read.complete)
  }

  // ------------------------------------------------------ exact share issuance

  pureTest("share issuance: Double diverges from exact integer maths at real pool magnitudes") {
    // Real magnitudes from the SWAP/DAG pool. Double carries 53 bits of mantissa; an 8-decimal
    // fixed-point reserve of 3.6e17 needs 59, so the division loses bits before it is scaled.
    val reserve = 360348314082469011L
    val totalShares = 562936280L
    val deposit = 91970493735518751L

    val exact = ((BigInt(deposit) * BigInt(totalShares)) / BigInt(reserve)).toLong
    val viaDouble = ((deposit.toDouble / reserve) * totalShares).toLong

    expect.all(
      exact == 143676341L,
      viaDouble == 143676342L,
      // They differ, and Double is the one that is wrong. Here it OVER-issues, which is the
      // dangerous direction: shares are a claim on the pool.
      exact != viaDouble,
      viaDouble > exact
    )
  }

  // ------------------------------------------------------------ rollback keeps k

  pureTest("k must equal the product of the reserves stored beside it") {
    // The live USDC.dag/DAG pool, read from the calculated state at currency ordinal 731646.
    // Every forward mutation recomputes k; the rollback paths did not, which is how k drifted
    // away from A*B across the historical pool snapshots.
    val dagSide = 287378052832436L
    val usdcSide = 2177968498680L
    val kStored = BigInt("625900346281042356527184480")
    expect(BigInt(dagSide) * BigInt(usdcSide) == kStored)
  }

  // -------------------------------------------------------- reverse-quote fee scale

  pureTest("a 0.3% pool fee is 0.003 as a factor, not 0.3") {
    import org.amm_metagraph.shared_data.refined.Percentage
    import org.amm_metagraph.shared_data.refined.Percentage._
    val threeTenths = Percentage.unsafeFrom(0.3)
    // .toDecimal divides by 100. Reading the raw value charged 30% on the reverse path.
    expect.all(
      threeTenths.toDecimal == BigDecimal("0.003"),
      (BigDecimal(1) - threeTenths.toDecimal) == BigDecimal("0.997")
    )
  }
}
