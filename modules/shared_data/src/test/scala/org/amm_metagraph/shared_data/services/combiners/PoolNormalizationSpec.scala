package org.amm_metagraph.shared_data.services.combiners

import cats.effect.IO

import scala.collection.immutable.SortedMap

import io.constellationnetwork.currency.dataApplication.DataState
import io.constellationnetwork.schema.SnapshotOrdinal

import eu.timepit.refined.types.all.NonNegLong
import fs2.concurrent.SignallingRef
import org.amm_metagraph.shared_data.loaders.LiquidityPoolLoader
import org.amm_metagraph.shared_data.types.States._
import weaver.SimpleIOSuite

/** updated-pools-14.json brings every pool to reserve == wallet, 1:1.
  *
  * Every one of the 48 pool records in the twelve hand-written predecessors carries `k != tokenA * tokenB`, and from the second file onward
  * their share ledgers do not sum to totalShares either. Those inconsistencies are in the live state today. This resource is generated
  * rather than written, and these assertions are what make that worth anything.
  */
object PoolNormalizationSpec extends SimpleIOSuite {

  private def ord(v: Long) = SnapshotOrdinal(NonNegLong.unsafeFrom(v))
  private val pools = LiquidityPoolLoader.loadPools("updated-pools-14.json").get

  pureTest("the resource loads and covers all four pools") {
    expect(pools.size == 4)
  }

  pureTest("k is the exact product of the reserves written beside it") {
    // The defect that runs through every earlier fix file.
    expect(pools.values.forall(p => p.k == BigInt(p.tokenA.amount.value) * BigInt(p.tokenB.amount.value)))
  }

  pureTest("totalShares equals the sum of addressShares, so no pool promises over 100%") {
    expect(pools.values.forall { p =>
      p.poolShares.totalShares.value == p.poolShares.addressShares.values.map(_.value.value.value).sum
    })
  }

  pureTest("totalShares only ever rises, never silently drops") {
    // A drop would mean shares vanished from somebody. Measured live values at ordinal 731646.
    val liveTotals = Map(
      "DAG0CyySf35ftDQDQBnd1bdQ9aPyUdacMghpnCuM" -> 1283806888L,
      "DAG0S16WDgdAvh8VvroR6MWLdjmHYdzAF5S181xh" -> 562936280L,
      "DAG7Ghth1WhWK83SB3MtXnnHYZbCsmiRTwJrgaW1" -> 6789486700L,
      "DAG7X5idd4aLfp4XC6WQdG1eDfR3LGPVEwtUUB2W" -> 3470214074L
    )
    expect(pools.values.forall(p => p.poolShares.totalShares.value >= liveTotals(p.poolId)))
  }

  pureTest("the written totals are exactly the measured sums") {
    val expected = Map(
      "DAG0CyySf35ftDQDQBnd1bdQ9aPyUdacMghpnCuM" -> 1285354640L,
      "DAG0S16WDgdAvh8VvroR6MWLdjmHYdzAF5S181xh" -> 562953484L,
      "DAG7Ghth1WhWK83SB3MtXnnHYZbCsmiRTwJrgaW1" -> 6847659924L,
      "DAG7X5idd4aLfp4XC6WQdG1eDfR3LGPVEwtUUB2W" -> 3477010611L
    )
    expect(pools.values.forall(p => p.poolShares.totalShares.value == expected(p.poolId)))
  }

  pureTest("the Upsider AI book is UNCHANGED - the surplus is swept out, not booked in") {
    // 145,335,256.25433419 UP sits at the custody address and no pool's book claims it. Booking
    // it in would have halved that pool's UP price; sweeping it out at ordinal 731649 leaves the
    // book alone, so the pool reaches reserve == wallet with no price movement. The two are
    // mutually exclusive, and this asserts we did not do both.
    val up = pools.values.find(_.poolId == "DAG7Ghth1WhWK83SB3MtXnnHYZbCsmiRTwJrgaW1").get
    val token = if (up.tokenA.identifier.isDefined) up.tokenA else up.tokenB
    expect.all(
      token.amount.value == 7362903167321414L, // the book, untouched
      token.amount.value != 14533525625433419L // NOT raised to the wallet
    )
  }

  pureTest("the shortfall pools keep their book; those are closed by transfer, not a rewrite") {
    val dor = pools.values.find(_.poolId == "DAG0CyySf35ftDQDQBnd1bdQ9aPyUdacMghpnCuM").get
    val usdc = pools.values.find(_.poolId == "DAG0S16WDgdAvh8VvroR6MWLdjmHYdzAF5S181xh").get
    val dorToken = if (dor.tokenA.identifier.isDefined) dor.tokenA else dor.tokenB
    val usdcToken = if (usdc.tokenA.identifier.isDefined) usdc.tokenA else usdc.tokenB
    expect.all(
      dorToken.amount.value == 2470424645101111L,
      usdcToken.amount.value == 2177968498680L
    )
  }

  pureTest("the DAG side is untouched - it is pooled and closed by the treasury transfer") {
    val dagTotal = pools.values.foldLeft(0L) { (acc, p) =>
      acc + List(p.tokenA, p.tokenB).filter(_.identifier.isEmpty).map(_.amount.value).sum
    }
    // 3,811,901.49344871 + 2,873,780.52832436 + 2,591,738.08118421 + 12,308,553.84768349
    expect(dagTotal == 2158597395064077L)
  }

  test("the handler fires it at 731648 and treats it as fail-closed") {
    for {
      ref <- SignallingRef.of[IO, SnapshotOrdinal](SnapshotOrdinal.MinValue)
      h = OneTimeFixesHandler.make[IO](ref)
    } yield
      expect.all(
        h.isOneTimeFixOrdinal(ord(731648L)),
        h.isOneTimeFixOrdinal(ord(731647L)),
        !h.isOneTimeFixOrdinal(ord(731649L))
      )
  }

  test("a missing resource at 731648 RAISES rather than skipping normalization") {
    // updatePoolsAtOrdinal is shared with thirteen historical replay ordinals and returns the
    // previous state on a load failure, by design, so their signed history still replays. The
    // normalization ordinal opts out of that. Without the opt-in a bad updated-pools-14.json
    // would log, return oldState, and let the snapshot proceed with k still inconsistent and the
    // share ledger still promising over 100% of every pool, while the sweep at 731649 fired
    // anyway. The fail-closed wrapper in L0CombinerService cannot catch it: nothing propagates.
    val empty = DataState(
      AmmOnChainState.empty,
      AmmCalculatedState(SortedMap(OperationType.LiquidityPool -> LiquidityPoolCalculatedState.empty))
    )
    for {
      ref <- SignallingRef.of[IO, SnapshotOrdinal](SnapshotOrdinal.MinValue)
      h = OneTimeFixesHandler.make[IO](ref)
      atNormalization <- h.handleOneTimeFixesOrdinals(empty, ord(731648L)).attempt
      // An ordinary ordinal does nothing at all, so a raise here would be the flag leaking.
      atOrdinary <- h.handleOneTimeFixesOrdinals(empty, ord(731650L)).attempt
    } yield
      expect.all(
        atNormalization.isLeft, // never a silent Some(oldState)
        atOrdinary.isRight, // and the flag is scoped, not blanket
        atOrdinary.exists(_.isEmpty)
      )
  }
}
