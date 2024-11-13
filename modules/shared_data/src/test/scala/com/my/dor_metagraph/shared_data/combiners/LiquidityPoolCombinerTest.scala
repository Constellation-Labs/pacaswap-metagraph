package com.my.dor_metagraph.shared_data.combiners

import cats.data.NonEmptySet
import cats.effect.IO
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.DataState
import io.constellationnetwork.schema.ID.Id
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.swap.CurrencyId
import io.constellationnetwork.security.hex.Hex
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.signature.signature.{Signature, SignatureProof}

import eu.timepit.refined.auto._
import org.amm_metagraph.shared_data.Utils.{LongOps, buildLiquidityPoolUniqueIdentifier}
import org.amm_metagraph.shared_data.combiners.LiquidityPoolCombiner.combineLiquidityPool
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._
import weaver.SimpleIOSuite

object LiquidityPoolCombinerTest extends SimpleIOSuite {
  def getFakeSignedUpdate(
    update: LiquidityPoolUpdate
  ): Signed[LiquidityPoolUpdate] =
    Signed(
      update,
      NonEmptySet.one(
        SignatureProof(
          Id(
            Hex(
              "db2faf200159ca3c47924bf5f3bda4f45d681a39f9490053ecf98d788122f7a7973693570bd242e10ab670748e86139847eb682a53c7c5c711b832517ce34860"
            )
          ),
          Signature(
            Hex(
              "3045022100fb26702e976a6569caa3507140756fee96b5ba748719abe1b812b17f7279a3dc0220613db28d5c5a30d7353383358b653aa29772151ccf352a2e67a26a74e49eac57"
            )
          )
        )
      )
    )
  test("Successfully create a liquidity pool") {
    val primaryToken = TokenInformation(CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some, 100L)
    val pairToken = TokenInformation(CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some, 50L)
    val ownerAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Ks")
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(Map.empty)
    val state = DataState(ammOnChainState, ammCalculatedState)
    val liquidityPoolUpdate = getFakeSignedUpdate(
      LiquidityPoolUpdate(
        primaryToken,
        pairToken,
        0.3
      )
    )

    for {
      stakeResponse <- combineLiquidityPool[IO](
        state,
        liquidityPoolUpdate,
        ownerAddress
      )
      poolId <- buildLiquidityPoolUniqueIdentifier(primaryToken.identifier, pairToken.identifier)
      updatedLiquidityPoolCalculatedState = stakeResponse.calculated
        .operations(OperationType.LiquidityPool)
        .asInstanceOf[LiquidityPoolCalculatedState]
      updatedLiquidityPool = updatedLiquidityPoolCalculatedState.liquidityPools(poolId.value)
    } yield
      expect.eql(100L.toTokenAmountFormat, updatedLiquidityPool.tokenA.amount.value) &&
        expect.eql(primaryToken.identifier.get, updatedLiquidityPool.tokenA.identifier.get) &&
        expect.eql(50L.toTokenAmountFormat, updatedLiquidityPool.tokenB.amount.value) &&
        expect.eql(pairToken.identifier.get, updatedLiquidityPool.tokenB.identifier.get) &&
        expect.eql(5000d, updatedLiquidityPool.k)
  }

  test("Successfully create a liquidity pool - L0Token - DAG") {
    val primaryToken = TokenInformation(CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some, 100L)
    val pairToken = TokenInformation(none, 50L)
    val ownerAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Ks")
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(Map.empty)
    val state = DataState(ammOnChainState, ammCalculatedState)
    val liquidityPoolUpdate = getFakeSignedUpdate(
      LiquidityPoolUpdate(
        primaryToken,
        pairToken,
        0.3
      )
    )

    for {
      stakeResponse <- combineLiquidityPool[IO](
        state,
        liquidityPoolUpdate,
        ownerAddress
      )
      poolId <- buildLiquidityPoolUniqueIdentifier(primaryToken.identifier, pairToken.identifier)
      updatedLiquidityPoolCalculatedState = stakeResponse.calculated
        .operations(OperationType.LiquidityPool)
        .asInstanceOf[LiquidityPoolCalculatedState]
      updatedLiquidityPool = updatedLiquidityPoolCalculatedState.liquidityPools(poolId.value)
    } yield
      expect.eql(100L.toTokenAmountFormat, updatedLiquidityPool.tokenA.amount.value) &&
        expect.eql(primaryToken.identifier.get, updatedLiquidityPool.tokenA.identifier.get) &&
        expect.eql(50L.toTokenAmountFormat, updatedLiquidityPool.tokenB.amount.value) &&
        expect.eql(pairToken.identifier.isEmpty, updatedLiquidityPool.tokenB.identifier.isEmpty) &&
        expect.eql(5000d, updatedLiquidityPool.k)
  }
}
