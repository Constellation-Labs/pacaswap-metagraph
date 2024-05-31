package com.my.dor_metagraph.shared_data.combiners

import cats.effect.IO
import cats.syntax.option._
import eu.timepit.refined.auto._
import org.amm_metagraph.shared_data.Utils.{PosLongOps, buildLiquidityPoolUniqueIdentifier}
import org.amm_metagraph.shared_data.combiners.LiquidityPoolCombiner.combineLiquidityPool
import org.amm_metagraph.shared_data.combiners.StakingCombiner.combineStaking
import org.amm_metagraph.shared_data.types.DataUpdates.{LiquidityPoolUpdate, StakingUpdate}
import org.amm_metagraph.shared_data.types.LiquidityPool.{LiquidityPool, TokenInformation}
import org.amm_metagraph.shared_data.types.States._
import org.tessellation.currency.dataApplication.DataState
import org.tessellation.schema.SnapshotOrdinal
import org.tessellation.schema.address.Address
import weaver.SimpleIOSuite

object LiquidityPoolCombinerTest extends SimpleIOSuite {

  test("Successfully create a liquidity pool") {
    val primaryToken = TokenInformation(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb").some, 100L)
    val pairToken = TokenInformation(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5").some, 50L)
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
      updatedLiquidityPoolCalculatedState = stakeResponse.calculated.ammState(OperationType.LiquidityPool).asInstanceOf[LiquidityPoolCalculatedState]
      updatedLiquidityPool = updatedLiquidityPoolCalculatedState.liquidityPools(poolId)
    } yield expect.eql(100L, updatedLiquidityPool.tokenA.amount.value) &&
      expect.eql(primaryToken.identifier.get, updatedLiquidityPool.tokenA.identifier.get) &&
      expect.eql(50L, updatedLiquidityPool.tokenB.amount.value) &&
      expect.eql(pairToken.identifier.get, updatedLiquidityPool.tokenB.identifier.get) &&
      expect.eql(5000L, updatedLiquidityPool.k.value)
  }
}