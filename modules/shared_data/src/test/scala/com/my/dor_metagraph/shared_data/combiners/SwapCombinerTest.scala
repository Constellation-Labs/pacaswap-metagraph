package com.my.dor_metagraph.shared_data.combiners

import cats.effect.IO
import cats.syntax.all._
import eu.timepit.refined.auto._
import org.amm_metagraph.shared_data.Utils._
import org.amm_metagraph.shared_data.combiners.SwapCombiner.combineSwap
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._
import org.tessellation.currency.dataApplication.DataState
import org.tessellation.schema.SnapshotOrdinal
import org.tessellation.schema.address.Address
import weaver.SimpleIOSuite

object SwapCombinerTest extends SimpleIOSuite {
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
      tokenA.amount.value.fromTokenAmountFormat * tokenB.amount.value.fromTokenAmountFormat,
      0.003,
      math.sqrt(tokenA.amount.value.toDouble * tokenB.amount.value.toDouble).toTokenAmountFormat,
      LiquidityProviders(Map(owner -> math.sqrt(tokenA.amount.value.toDouble * tokenB.amount.value.toDouble).toTokenAmountFormat))
    )
    (poolId, LiquidityPoolCalculatedState(Map(poolId -> liquidityPool)))
  }

  test("Test swap successfully when liquidity pool exists") {
    val primaryToken = TokenInformation(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb").some, 100L.toTokenAmountFormat.toPosLongUnsafe)
    val pairToken = TokenInformation(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5").some, 50L.toTokenAmountFormat.toPosLongUnsafe)
    val ownerAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Ks")
    val metagraphAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Gs")

    val (poolId, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)
    val stakingUpdate = SwapUpdate(
      primaryToken.identifier,
      pairToken.identifier,
      metagraphAddress,
      0L,
      "",
      "",
      10L.toPosLongUnsafe,
      10L.toPosLongUnsafe,
      SnapshotOrdinal.unsafeApply(20),
      none,
      1L.toPosLongUnsafe,
      10L.toPosLongUnsafe
    )

    for {
      stakeResponse <- combineSwap[IO](
        state,
        stakingUpdate,
        ownerAddress,
        SnapshotOrdinal.MinValue
      )

      swapCalculatedState = stakeResponse.calculated.ammState(OperationType.Swap).asInstanceOf[SwapCalculatedState]
      addressSwapResponse = swapCalculatedState.addresses(ownerAddress)

      oldLiquidityPoolCalculatedState = state.calculated.ammState(OperationType.LiquidityPool).asInstanceOf[LiquidityPoolCalculatedState]
      oldLiquidityPool = oldLiquidityPoolCalculatedState.liquidityPools(poolId)

      updatedLiquidityPoolCalculatedState = stakeResponse.calculated.ammState(OperationType.LiquidityPool).asInstanceOf[LiquidityPoolCalculatedState]
      updatedLiquidityPool = updatedLiquidityPoolCalculatedState.liquidityPools(poolId)
    } yield expect.eql(10997000000L, addressSwapResponse.fromToken.amount.value) &&
      expect.eql(primaryToken.identifier.get, addressSwapResponse.fromToken.identifier.get) &&
      expect.eql(4546694553L, addressSwapResponse.toToken.amount.value)
  }
}