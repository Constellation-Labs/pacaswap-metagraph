package com.my.dor_metagraph.shared_data.combiners

import cats.effect.IO
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.DataState
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.swap.CurrencyId

import eu.timepit.refined.auto._
import org.amm_metagraph.shared_data.Utils.{LongOps, buildLiquidityPoolUniqueIdentifier}
import org.amm_metagraph.shared_data.combiners.LiquidityPoolCombiner.combineLiquidityPool
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._
import weaver.SimpleIOSuite

object LiquidityPoolCombinerTest extends SimpleIOSuite {

  test("Successfully create a liquidity pool") {
    val primaryToken = TokenInformation(CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some, 100L)
    val pairToken = TokenInformation(CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some, 50L)
    val ownerAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Ks")
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(Map.empty)
    val state = DataState(ammOnChainState, ammCalculatedState)
    val stakingUpdate = LiquidityPoolUpdate(
      primaryToken,
      pairToken,
      0.3
    )

    for {
      stakeResponse <- combineLiquidityPool[IO](
        state,
        stakingUpdate,
        ownerAddress
      )
      poolId <- buildLiquidityPoolUniqueIdentifier(primaryToken.identifier, pairToken.identifier)
      updatedLiquidityPoolCalculatedState = stakeResponse.calculated
        .ammState(OperationType.LiquidityPool)
        .asInstanceOf[LiquidityPoolCalculatedState]
      updatedLiquidityPool = updatedLiquidityPoolCalculatedState.liquidityPools(poolId)
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
    val stakingUpdate = LiquidityPoolUpdate(
      primaryToken,
      pairToken,
      0.3
    )

    for {
      stakeResponse <- combineLiquidityPool[IO](
        state,
        stakingUpdate,
        ownerAddress
      )
      poolId <- buildLiquidityPoolUniqueIdentifier(primaryToken.identifier, pairToken.identifier)
      updatedLiquidityPoolCalculatedState = stakeResponse.calculated
        .ammState(OperationType.LiquidityPool)
        .asInstanceOf[LiquidityPoolCalculatedState]
      updatedLiquidityPool = updatedLiquidityPoolCalculatedState.liquidityPools(poolId)
    } yield
      expect.eql(100L.toTokenAmountFormat, updatedLiquidityPool.tokenA.amount.value) &&
        expect.eql(primaryToken.identifier.get, updatedLiquidityPool.tokenA.identifier.get) &&
        expect.eql(50L.toTokenAmountFormat, updatedLiquidityPool.tokenB.amount.value) &&
        expect.eql(pairToken.identifier.isEmpty, updatedLiquidityPool.tokenB.identifier.isEmpty) &&
        expect.eql(5000d, updatedLiquidityPool.k)
  }
}
