package org.amm_metagraph.shared_data.combiners

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.DataState
import io.constellationnetwork.schema.address.Address

import org.amm_metagraph.shared_data.Utils._
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, LiquidityPoolUpdate}
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._

object LiquidityPoolCombiner {
  def combineLiquidityPool[F[_]: Async](
    acc: DataState[AmmOnChainState, AmmCalculatedState],
    liquidityPoolUpdate: LiquidityPoolUpdate,
    signerAddress: Address
  ): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
    val liquidityPools = getLiquidityPools(acc)

    for {
      poolId <- buildLiquidityPoolUniqueIdentifier(liquidityPoolUpdate.tokenA.identifier, liquidityPoolUpdate.tokenB.identifier)
      amountA = liquidityPoolUpdate.tokenA.amount.value
      amountB = liquidityPoolUpdate.tokenB.amount.value
      poolTotalLiquidity = math.sqrt(amountA.toDouble * amountB.toDouble).toTokenAmountFormat

      liquidityPool = LiquidityPool(
        poolId,
        TokenInformation(
          liquidityPoolUpdate.tokenA.identifier,
          liquidityPoolUpdate.tokenA.amount.value.toTokenAmountFormat.toPosLongUnsafe
        ),
        TokenInformation(
          liquidityPoolUpdate.tokenB.identifier,
          liquidityPoolUpdate.tokenB.amount.value.toTokenAmountFormat.toPosLongUnsafe
        ),
        signerAddress,
        (amountA * amountB).toDouble,
        liquidityPoolUpdate.feeRate,
        poolTotalLiquidity,
        LiquidityProviders(Map(signerAddress -> poolTotalLiquidity))
      )
      updatedLiquidityPoolCalculatedState = LiquidityPoolCalculatedState(liquidityPools.updated(poolId, liquidityPool))
      updatedState = acc.calculated.ammState.updated(OperationType.LiquidityPool, updatedLiquidityPoolCalculatedState)
      updates: List[AmmUpdate] = liquidityPoolUpdate :: acc.onChain.updates
    } yield
      DataState(
        AmmOnChainState(updates),
        AmmCalculatedState(updatedState)
      )
  }
}
