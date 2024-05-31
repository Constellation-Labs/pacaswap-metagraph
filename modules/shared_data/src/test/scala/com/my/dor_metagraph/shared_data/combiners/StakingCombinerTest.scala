package com.my.dor_metagraph.shared_data.combiners

import cats.effect.IO
import cats.syntax.option._
import eu.timepit.refined.auto._
import org.amm_metagraph.shared_data.Utils.{PosLongOps, toTokenAmountFormat}
import org.amm_metagraph.shared_data.combiners.StakingCombiner.combineStaking
import org.amm_metagraph.shared_data.types.DataUpdates.StakingUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool.{LiquidityPool, LiquidityProviders, TokenInformation}
import org.amm_metagraph.shared_data.types.States._
import org.tessellation.currency.dataApplication.DataState
import org.tessellation.schema.SnapshotOrdinal
import org.tessellation.schema.address.Address
import weaver.SimpleIOSuite

object StakingCombinerTest extends SimpleIOSuite {

  def buildLiquidityPoolCalculatedState(
    tokenA: TokenInformation,
    tokenB: TokenInformation,
    owner : Address
  ): (String, LiquidityPoolCalculatedState) = {
    val primaryAddressAsString = tokenA.identifier.fold("")(address => address.value.value)
    val pairAddressAsString = tokenB.identifier.fold("")(address => address.value.value)
    val poolId = s"$primaryAddressAsString-$pairAddressAsString"
    val liquidityPool = LiquidityPool(
      poolId,
      tokenA,
      tokenB,
      owner,
      (tokenA.amount.value * tokenB.amount.value).toPosLongUnsafe,
      0.3,
      toTokenAmountFormat(math.sqrt(tokenA.amount.value.toDouble * tokenB.amount.value.toDouble)),
      LiquidityProviders(Map(owner -> toTokenAmountFormat(math.sqrt(tokenA.amount.value.toDouble * tokenB.amount.value.toDouble))))
    )
    (poolId, LiquidityPoolCalculatedState(Map(poolId -> liquidityPool)))
  }

  test("Test combining successfully when liquidity pool exists") {
    val primaryToken = TokenInformation(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb").some, 100L)
    val pairToken = TokenInformation(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5").some, 50L)
    val ownerAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Ks")

    val (poolId, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)
    val stakingUpdate = StakingUpdate(
      "fake-primary-txn-1",
      "fake-pair-txn-1",
      primaryToken.identifier,
      100L.toPosLongUnsafe,
      pairToken.identifier
    )

    for {
      stakeResponse <- combineStaking[IO](
        state,
        stakingUpdate,
        ownerAddress,
        SnapshotOrdinal.MinValue
      )

      stakingCalculatedState = stakeResponse.calculated.ammState(OperationType.Staking).asInstanceOf[StakingCalculatedState]
      addressStakedResponse = stakingCalculatedState.addresses(ownerAddress)

      oldLiquidityPoolCalculatedState = state.calculated.ammState(OperationType.LiquidityPool).asInstanceOf[LiquidityPoolCalculatedState]
      oldLiquidityPool = oldLiquidityPoolCalculatedState.liquidityPools(poolId)

      updatedLiquidityPoolCalculatedState = stakeResponse.calculated.ammState(OperationType.LiquidityPool).asInstanceOf[LiquidityPoolCalculatedState]
      updatedLiquidityPool = updatedLiquidityPoolCalculatedState.liquidityPools(poolId)
    } yield expect.eql(100L, addressStakedResponse.primaryToken.amount.value) &&
      expect.eql(primaryToken.identifier.get, addressStakedResponse.primaryToken.identifier.get) &&
      expect.eql(50L, addressStakedResponse.pairToken.amount.value) &&
      expect.eql(pairToken.identifier.get, addressStakedResponse.pairToken.identifier.get) &&
      expect.eql(100L, oldLiquidityPool.tokenA.amount.value) &&
      expect.eql(200L, updatedLiquidityPool.tokenA.amount.value) &&
      expect.eql(50L, oldLiquidityPool.tokenB.amount.value) &&
      expect.eql(100L, updatedLiquidityPool.tokenB.amount.value) &&
      expect.eql(5000L, oldLiquidityPool.k.value) &&
      expect.eql(20000L, updatedLiquidityPool.k.value)
  }

  test("Throws error if liquidity pool does not exists") {
    val primaryToken = TokenInformation(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb").some, 100L)
    val pairToken = TokenInformation(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5").some, 50L)
    val ownerAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Ks")

    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(Map.empty)
    val state = DataState(ammOnChainState, ammCalculatedState)
    val stakingUpdate = StakingUpdate(
      "fake-primary-txn-1",
      "fake-pair-txn-1",
      primaryToken.identifier,
      100L.toPosLongUnsafe,
      pairToken.identifier
    )
    combineStaking[IO](
      state,
      stakingUpdate,
      ownerAddress,
      SnapshotOrdinal.MinValue
    ).attempt.map {
      case Left(e: IllegalStateException) =>
        expect(e.getMessage == "Liquidity Pool does not exists")
      case Left(e) =>
        failure(s"Unexpected exception: $e")
      case Right(_) =>
        failure("Expected exception was not thrown")
    }
  }
}