package org.amm_metagraph.shared_data.combiners

import cats.effect.Async
import cats.syntax.all._
import org.amm_metagraph.shared_data.Utils.{PosLongOps, buildLiquidityPoolUniqueIdentifier, toTokenAmountFormat}
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, LiquidityPoolUpdate}
import org.amm_metagraph.shared_data.types.LiquidityPool.{LiquidityPool, LiquidityProviders, TokenInformation}
import org.amm_metagraph.shared_data.types.States._
import org.tessellation.currency.dataApplication.DataState
import org.tessellation.schema.address.Address

object LiquidityPoolCombiner {
  def combineLiquidityPool[F[_] : Async](
    acc                : DataState[AmmOnChainState, AmmCalculatedState],
    liquidityPoolUpdate: LiquidityPoolUpdate,
    signerAddress      : Address
  ): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
    val liquidityPools = acc.calculated.ammState.get(OperationType.LiquidityPool).fold(Map.empty[String, LiquidityPool]) {
      case liquidityPoolCalculatedState: LiquidityPoolCalculatedState => liquidityPoolCalculatedState.liquidityPools
      case _ => Map.empty
    }

    for {
      poolId <- buildLiquidityPoolUniqueIdentifier(liquidityPoolUpdate.tokenA.identifier, liquidityPoolUpdate.tokenB.identifier)
      amountA = liquidityPoolUpdate.tokenA.amount.value
      amountB = liquidityPoolUpdate.tokenB.amount.value
      poolTotalLiquidity = toTokenAmountFormat(math.sqrt(amountA.toDouble * amountB.toDouble))

      liquidityPool = LiquidityPool(
        poolId,
        TokenInformation(liquidityPoolUpdate.tokenA.identifier, liquidityPoolUpdate.tokenA.amount),
        TokenInformation(liquidityPoolUpdate.tokenB.identifier, liquidityPoolUpdate.tokenB.amount),
        signerAddress,
        (amountA * amountB).toPosLongUnsafe,
        liquidityPoolUpdate.feeRate,
        poolTotalLiquidity,
        LiquidityProviders(Map(signerAddress -> poolTotalLiquidity))
      )
      updatedLiquidityPoolCalculatedState = LiquidityPoolCalculatedState(liquidityPools.updated(poolId, liquidityPool))
      updatedState = acc.calculated.ammState.updated(OperationType.LiquidityPool, updatedLiquidityPoolCalculatedState)
      updates: List[AmmUpdate] = liquidityPoolUpdate :: acc.onChain.updates
    } yield DataState(
      AmmOnChainState(updates),
      AmmCalculatedState(updatedState)
    )
  }
}