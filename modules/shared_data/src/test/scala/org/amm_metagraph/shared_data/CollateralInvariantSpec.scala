package org.amm_metagraph.shared_data

import scala.collection.immutable.SortedMap

import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.swap.CurrencyId

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.PosLong
import org.amm_metagraph.shared_data.FeeDistributor
import org.amm_metagraph.shared_data.services.combiners.CollateralInvariant
import org.amm_metagraph.shared_data.types.LiquidityPool._
import weaver.SimpleIOSuite

/** The book must always equal the wallet. These use the real mainnet figures at currency
  * ordinal 731646 so the check is pinned against a state we have independently measured.
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
      List(p), AMM, dagBalance = Some(BigInt(500)), tokenBalance = _ => Some(BigInt(1000))
    )
    expect.all(rows.size == 2, rows.forall(!_.breached))
  }

  pureTest("the DAG column is summed across pools, never taken per pool") {
    // DAG is fungible and shared, so only the aggregate is meaningful.
    val a = pool("a", Some(DOR), 1000L, 300L)
    val b = pool("b", Some(UP), 2000L, 200L)
    val rows = CollateralInvariant.positions(
      List(a, b), AMM, Some(BigInt(500)), _ => Some(BigInt(999999))
    )
    val dag = rows.find(_.ledger == "DAG").get
    expect.all(dag.reserve == BigInt(500), !dag.breached, rows.count(_.ledger == "DAG") == 1)
  }

  pureTest("a short DAG column is a breach, and the shortfall is exact") {
    // The real numbers: four pools claim 9,463,886.34252019 DAG, the wallet holds
    // 7,822,758.38325224, short 1,641,127.95926795.
    val p = pool("p", Some(DOR), 1000L, 946388634252019L)
    val rows = CollateralInvariant.positions(
      List(p), AMM, Some(BigInt(782275838325224L)), _ => Some(BigInt(1000))
    )
    val dag = rows.find(_.ledger == "DAG").get
    expect.all(dag.breached, dag.shortfall == BigInt(164112795926795L))
  }

  pureTest("a token surplus is not a breach - over-backing is safe") {
    // The UP pool: book 73,629,031.67321414, wallet 145,335,256.25433419.
    val p = pool("p", Some(UP), 7362903167321414L, 100L)
    val rows = CollateralInvariant.positions(
      List(p), AMM, Some(BigInt(100)), _ => Some(BigInt(14533525625433419L))
    )
    val up = rows.find(_.ledger != "DAG").get
    expect.all(!up.breached, up.shortfall == BigInt(-7170622458112005L))
  }

  pureTest("a token column short of its wallet is a breach") {
    // The DOR pool: book 24,704,246.45101111, wallet 22,859,282.41981735.
    val p = pool("p", Some(DOR), 2470424645101111L, 100L)
    val rows = CollateralInvariant.positions(
      List(p), AMM, Some(BigInt(100)), _ => Some(BigInt(2285928241981735L))
    )
    val dor = rows.find(_.ledger != "DAG").get
    expect.all(dor.breached, dor.shortfall == BigInt(184496403119376L))
  }

  pureTest("an unreadable wallet yields no row, never a false pass") {
    val p = pool("p", Some(DOR), 1000L, 500L)
    val rows = CollateralInvariant.positions(List(p), AMM, dagBalance = None, tokenBalance = _ => None)
    expect(rows.isEmpty)
  }
}
