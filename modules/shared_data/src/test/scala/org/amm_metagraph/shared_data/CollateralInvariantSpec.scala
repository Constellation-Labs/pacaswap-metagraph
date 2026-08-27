package org.amm_metagraph.shared_data

import cats.data.NonEmptyList

import scala.collection.immutable.SortedMap

import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact.{SpendAction, SpendTransaction}
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.swap.{CurrencyId, SwapAmount}

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.PosLong
import org.amm_metagraph.shared_data.FeeDistributor
import org.amm_metagraph.shared_data.services.combiners.CollateralInvariant
import org.amm_metagraph.shared_data.types.LiquidityPool._
import weaver.SimpleIOSuite

/** The book must always equal the wallet. These use the real mainnet figures at currency ordinal 731646 so the check is pinned against a
  * state we have independently measured.
  */
object CollateralInvariantSpec extends SimpleIOSuite {

  private val AMM = Address("DAG7X5idd4aLfp4XC6WQdG1eDfR3LGPVEwtUUB2W")
  private val DOR = CurrencyId(Address("DAG0CyySf35ftDQDQBnd1bdQ9aPyUdacMghpnCuM"))
  private val UP = CurrencyId(Address("DAG7Ghth1WhWK83SB3MtXnnHYZbCsmiRTwJrgaW1"))

  private def pool(id: String, tokenId: Option[CurrencyId], tokenAmt: Long, dagAmt: Long) =
    LiquidityPool(
      updateHash = io.constellationnetwork.security.hash.Hash.empty,
      tokenA = TokenInformation(tokenId, PosLong.unsafeFrom(tokenAmt)),
      tokenB = TokenInformation(None, PosLong.unsafeFrom(dagAmt)),
      owner = AMM,
      poolId = PoolId(id),
      k = BigInt(tokenAmt) * BigInt(dagAmt),
      poolShares = PoolShares(PosLong.unsafeFrom(1L), SortedMap.empty[Address, ShareAmount]),
      poolFees = FeeDistributor.standard
    )

  pureTest("a fully backed book reports no breach") {
    val p = pool("p", Some(DOR), 1000L, 500L)
    val rows = CollateralInvariant.positions(
      List(p),
      AMM,
      dagBalance = Some(BigInt(500)),
      tokenBalance = _ => Some(BigInt(1000)),
      pendingSpendActions = Nil
    )
    expect.all(rows.size == 2, rows.forall(!_.breached))
  }

  pureTest("the DAG column is summed across pools, never taken per pool") {
    // DAG is fungible and shared, so only the aggregate is meaningful.
    val a = pool("a", Some(DOR), 1000L, 300L)
    val b = pool("b", Some(UP), 2000L, 200L)
    val rows = CollateralInvariant.positions(
      List(a, b),
      AMM,
      Some(BigInt(500)),
      _ => Some(BigInt(999999))
    )
    val dag = rows.find(_.ledger == "DAG").get
    expect.all(dag.reserve == BigInt(500), !dag.breached, rows.count(_.ledger == "DAG") == 1)
  }

  pureTest("a short DAG column is a breach, and the shortfall is exact") {
    // The real numbers: four pools claim 9,463,886.34252019 DAG, the wallet holds
    // 7,822,758.38325224, short 1,641,127.95926795.
    val p = pool("p", Some(DOR), 1000L, 946388634252019L)
    val rows = CollateralInvariant.positions(
      List(p),
      AMM,
      Some(BigInt(782275838325224L)),
      _ => Some(BigInt(1000))
    )
    val dag = rows.find(_.ledger == "DAG").get
    expect.all(dag.breached, dag.shortfall == BigInt(164112795926795L))
  }

  pureTest("a token surplus is not a breach - over-backing is safe") {
    // The UP pool: book 73,629,031.67321414, wallet 145,335,256.25433419.
    val p = pool("p", Some(UP), 7362903167321414L, 100L)
    val rows = CollateralInvariant.positions(
      List(p),
      AMM,
      Some(BigInt(100)),
      _ => Some(BigInt(14533525625433419L))
    )
    val up = rows.find(_.ledger != "DAG").get
    expect.all(!up.breached, up.shortfall == BigInt(-7170622458112005L))
  }

  pureTest("a token column short of its wallet is a breach") {
    // The DOR pool: book 24,704,246.45101111, wallet 22,859,282.41981735.
    val p = pool("p", Some(DOR), 2470424645101111L, 100L)
    val rows = CollateralInvariant.positions(
      List(p),
      AMM,
      Some(BigInt(100)),
      _ => Some(BigInt(2285928241981735L))
    )
    val dor = rows.find(_.ledger != "DAG").get
    expect.all(dor.breached, dor.shortfall == BigInt(184496403119376L))
  }

  pureTest("an unreadable wallet yields no row, never a false pass") {
    val p = pool("p", Some(DOR), 1000L, 500L)
    val rows = CollateralInvariant.positions(List(p), AMM, dagBalance = None, tokenBalance = _ => None, pendingSpendActions = Nil)
    expect(rows.isEmpty)
  }

  // ------------------------------------------------- value in flight

  private val USER = Address("DAG4w5mUqNNxQNS4hgdpx3E8FGgiu2UCRsJxHwhX")
  private def leg(cur: Option[CurrencyId], amt: Long, from: Address, to: Address) =
    SpendTransaction(None, cur, SwapAmount(PosLong.unsafeFrom(amt)), from, to)
  private def action(txs: SpendTransaction*) = SpendAction(NonEmptyList.fromListUnsafe(txs.toList))

  pureTest("a healthy in-flight swap is NOT a breach - this is the false positive to avoid") {
    // The reserve is credited when the SpendAction is generated; the wallet only moves when the
    // global layer settles it. Naively comparing reserve to balance fires on every live swap.
    // DAG in 500, DOR out 1000. Reserve already reflects both; the wallet reflects neither.
    val p = pool("p", Some(DOR), 9000L, 1500L)
    val pending = List(action(leg(None, 500L, USER, AMM), leg(Some(DOR), 1000L, AMM, USER)))
    val rows = CollateralInvariant.positions(
      List(p),
      AMM,
      dagBalance = Some(BigInt(1000)), // 500 not yet received
      tokenBalance = _ => Some(BigInt(10000)), // 1000 not yet paid out
      pendingSpendActions = pending
    )
    val dag = rows.find(_.ledger == "DAG").get
    val dor = rows.find(_.ledger != "DAG").get
    expect.all(
      dag.inFlightNet == BigInt(500),
      dag.backing == BigInt(1500),
      !dag.breached,
      dor.inFlightNet == BigInt(-1000),
      dor.backing == BigInt(9000),
      !dor.breached
    )
  }

  pureTest("an AllowSpend leg destined to the metagraph counts as inbound backing") {
    // An AllowSpend debits the user and sits in the global activeAllowSpends - never in the
    // metagraph balance. The AMM has already booked it, so it must count as in flight.
    val flight = CollateralInvariant.inFlight(List(action(leg(None, 777L, USER, AMM))), AMM)
    expect(flight.getOrElse(None, BigInt(0)) == BigInt(777))
  }

  pureTest("an outbound leg reduces the backing the wallet is expected to keep") {
    val flight = CollateralInvariant.inFlight(List(action(leg(Some(UP), 42L, AMM, USER))), AMM)
    expect(flight.getOrElse(Some(UP), BigInt(0)) == BigInt(-42))
  }

  pureTest("legs unrelated to the metagraph are ignored entirely") {
    val other = Address("DAG8uqhyGtFABWSS5KeVB2ia1R4vXop5AeijXeoU")
    expect(CollateralInvariant.inFlight(List(action(leg(None, 999L, USER, other))), AMM).isEmpty)
  }

  pureTest("inbound and outbound on the same ledger net out") {
    val flight = CollateralInvariant.inFlight(
      List(action(leg(None, 900L, USER, AMM), leg(None, 400L, AMM, USER))),
      AMM
    )
    expect(flight.getOrElse(None, BigInt(0)) == BigInt(500))
  }

  pureTest("in flight cannot mask a genuine shortfall") {
    // Reserve 5000, wallet 1000, only 500 legitimately in flight: still short 3500.
    val p = pool("p", Some(DOR), 10L, 5000L)
    val rows = CollateralInvariant.positions(
      List(p),
      AMM,
      Some(BigInt(1000)),
      _ => Some(BigInt(10)),
      List(action(leg(None, 500L, USER, AMM)))
    )
    val dag = rows.find(_.ledger == "DAG").get
    expect.all(dag.breached, dag.shortfall == BigInt(3500))
  }

  // ------------------------------------------- it must never break the combine

  pureTest("the check is sampled, so it is not a cost paid on every snapshot") {
    // It runs inside consensus. Reading collections and writing log lines is real work on the
    // critical path; a check that slows the combine can itself cause the problem it looks for.
    expect.all(
      CollateralInvariant.checkEveryNOrdinals > 1L,
      731650L % CollateralInvariant.checkEveryNOrdinals == 0L
    )
  }

  // The regression for "the check must never fail the combine" is covered end to end by
  // CombinerTest, which runs the whole combine with the invariant wired in. It is what caught
  // the original break: the first version computed its inputs while BUILDING the effect, so a
  // missing map key threw before any error handling could see it and the combine dropped every
  // update in the batch. Reproducing that here would mean fabricating a Hashed currency
  // snapshot for ProcessingContext, which would test the fixture more than the code.
}
