package org.amm_metagraph.shared_data.calculated_state

import cats.effect.IO

import scala.collection.immutable.SortedMap

import io.constellationnetwork.schema.swap.CurrencyId

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.PosLong
import io.circe.syntax.EncoderOps
import org.amm_metagraph.shared_data.Shared._
import org.amm_metagraph.shared_data.types.LiquidityPool.TokenInformation
import org.amm_metagraph.shared_data.types.States.OperationType.LiquidityPool
import org.amm_metagraph.shared_data.types.States._
import weaver.SimpleIOSuite

/** D6-04: serialization round-trip — decode(encode(x)) == x for the consensus state types, so a node rebuilding calculated state from
  * snapshots reconstructs byte-identical data.
  *
  * D6-03: the calculated-state proof is deterministic and node-independent — two independent service instances hash the same state to the
  * same value (the property the consensus proof relies on).
  */
object CalculatedStateSerializationSpec extends SimpleIOSuite {

  private val tokenA = TokenInformation(Some(CurrencyId(sourceAddress)), PosLong.unsafeFrom(toFixedPoint(10000.0)))
  private val tokenB = TokenInformation(Some(ammMetagraphIdAsCurrencyId), PosLong.unsafeFrom(toFixedPoint(20000.0)))

  private val (_, lpState) = buildLiquidityPoolCalculatedState(tokenA, tokenB, sourceAddress)

  private val populatedState: AmmCalculatedState =
    AmmCalculatedState(operations = SortedMap[OperationType, AmmOffChainState](LiquidityPool -> lpState))

  pureTest("AmmCalculatedState round-trips through JSON (empty)") {
    val empty = AmmCalculatedState()
    expect(empty.asJson.as[AmmCalculatedState] == Right(empty))
  }

  pureTest("AmmCalculatedState round-trips through JSON (populated with a liquidity pool)") {
    expect(populatedState.asJson.as[AmmCalculatedState] == Right(populatedState))
  }

  pureTest("AmmOnChainState round-trips through JSON (empty)") {
    val empty = AmmOnChainState.empty
    expect(empty.asJson.as[AmmOnChainState] == Right(empty))
  }

  test("calculated-state proof is deterministic across independent service instances (D6-03)") {
    for {
      s1 <- CalculatedStateService.make[IO]
      s2 <- CalculatedStateService.make[IO]
      h1 <- s1.hash(populatedState)
      h2 <- s2.hash(populatedState)
      h1again <- s1.hash(populatedState)
    } yield expect(h1 == h2).and(expect(h1 == h1again))
  }
}
