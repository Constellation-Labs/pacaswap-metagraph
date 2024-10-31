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
    owner : Address,
    feeRate: Long
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
      (feeRate.toDouble / 100D),
      math.sqrt(tokenA.amount.value.toDouble * tokenB.amount.value.toDouble).toTokenAmountFormat,
      LiquidityProviders(Map(owner -> math.sqrt(tokenA.amount.value.toDouble * tokenB.amount.value.toDouble).toTokenAmountFormat))
    )
    (poolId, LiquidityPoolCalculatedState(Map(poolId -> liquidityPool)))
  }

  test("Test swap successfully when liquidity pool exists 3% fee") {
    val primaryToken = TokenInformation(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb").some, 1000000L.toTokenAmountFormat.toPosLongUnsafe)
    val pairToken = TokenInformation(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5").some, 2000000L.toTokenAmountFormat.toPosLongUnsafe)
    val ownerAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Ks")
    val metagraphAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Gs")

    val (poolId, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress, 3L)
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
      100000L.toPosLongUnsafe,
      100000L.toPosLongUnsafe,
      SnapshotOrdinal.unsafeApply(20),
      none,
      1L.toPosLongUnsafe,
      20000.toPosLongUnsafe
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
    } yield expect.eql(1100000L, addressSwapResponse.fromToken.amount.value.fromTokenAmountFormat) &&
      expect.eql(primaryToken.identifier.get, addressSwapResponse.fromToken.identifier.get) &&
      expect.eql(1823154.05651777, addressSwapResponse.toToken.amount.value.fromTokenAmountFormat) &&
      expect.eql(1000000L.toTokenAmountFormat, oldLiquidityPool.tokenA.amount.value) &&
      expect.eql(2000000L.toTokenAmountFormat, oldLiquidityPool.tokenB.amount.value) &&
      expect.eql(1100000L.toTokenAmountFormat, updatedLiquidityPool.tokenA.amount.value) &&
      expect.eql(1823154.05651777.toTokenAmountFormat, updatedLiquidityPool.tokenB.amount.value)
  }

  test("Test swap successfully when liquidity pool exists - 10% fee") {
    val primaryToken = TokenInformation(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb").some, 1000000L.toTokenAmountFormat.toPosLongUnsafe)
    val pairToken = TokenInformation(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5").some, 2000000L.toTokenAmountFormat.toPosLongUnsafe)
    val ownerAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Ks")
    val metagraphAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Gs")

    val (poolId, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress, 10L)
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
      100000L.toPosLongUnsafe,
      100000L.toPosLongUnsafe,
      SnapshotOrdinal.unsafeApply(20),
      none,
      1L.toPosLongUnsafe,
      20000.toPosLongUnsafe
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
    } yield expect.eql(1100000L, addressSwapResponse.fromToken.amount.value.fromTokenAmountFormat) &&
      expect.eql(primaryToken.identifier.get, addressSwapResponse.fromToken.identifier.get) &&
      expect.eql(1834862.3853211, addressSwapResponse.toToken.amount.value.fromTokenAmountFormat) &&
      expect.eql(1000000L.toTokenAmountFormat, oldLiquidityPool.tokenA.amount.value) &&
      expect.eql(2000000L.toTokenAmountFormat, oldLiquidityPool.tokenB.amount.value) &&
      expect.eql(1100000L.toTokenAmountFormat, updatedLiquidityPool.tokenA.amount.value) &&
      expect.eql(1834862.3853211.toTokenAmountFormat, updatedLiquidityPool.tokenB.amount.value)
  }
}