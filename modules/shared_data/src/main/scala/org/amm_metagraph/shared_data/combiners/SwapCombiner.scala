package org.amm_metagraph.shared_data.combiners

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.DataState
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address

import org.amm_metagraph.shared_data.Utils._
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, SwapUpdate}
import org.amm_metagraph.shared_data.types.LiquidityPool.{LiquidityPool, TokenInformation, getLiquidityPools}
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.Swap.{SwapCalculatedStateAddress, SwapCalculatedStateLastReference}

object SwapCombiner {
  private def getUpdatedTokenInformation(
    swapUpdate: SwapUpdate,
    liquidityPool: LiquidityPool
  ): (TokenInformation, TokenInformation) = {
    val fromTokenInfo = if (swapUpdate.swapFromToken == liquidityPool.tokenA.identifier) liquidityPool.tokenA else liquidityPool.tokenB
    val toTokenInfo = if (swapUpdate.swapToToken == liquidityPool.tokenA.identifier) liquidityPool.tokenA else liquidityPool.tokenB

    val swapAmount = swapUpdate.maxAmount.value
    val fee = swapAmount * liquidityPool.feeRate
    val netSwapAmount = swapAmount - fee

    val newFromTokenReserve = fromTokenInfo.amount.value.fromTokenAmountFormat + netSwapAmount
    val finalFromTokenReserve = newFromTokenReserve + fee
    val newToTokenReserve = liquidityPool.k / newFromTokenReserve

    (
      fromTokenInfo.copy(amount = finalFromTokenReserve.toTokenAmountFormat.toPosLongUnsafe),
      toTokenInfo.copy(amount = newToTokenReserve.toTokenAmountFormat.toPosLongUnsafe)
    )
  }

  private def updateLiquidityPool(
    liquidityPool: LiquidityPool,
    fromTokenInfo: TokenInformation,
    toTokenInfo: TokenInformation
  ): LiquidityPool = {
    val tokenA = if (liquidityPool.tokenA.identifier == fromTokenInfo.identifier) fromTokenInfo else toTokenInfo
    val tokenB = if (liquidityPool.tokenB.identifier == toTokenInfo.identifier) toTokenInfo else fromTokenInfo

    liquidityPool.copy(
      tokenA = tokenA,
      tokenB = tokenB
    )
  }

  def combineSwap[F[_]: Async](
    acc: DataState[AmmOnChainState, AmmCalculatedState],
    swapUpdate: SwapUpdate,
    signerAddress: Address,
    currentSnapshotOrdinal: SnapshotOrdinal
  ): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
    val swapCalculatedStateAddresses =
      acc.calculated.ammState.get(OperationType.Swap).fold(Map.empty[Address, SwapCalculatedStateAddress]) {
        case swapCalculatedState: SwapCalculatedState => swapCalculatedState.addresses
        case _                                        => Map.empty
      }

    val liquidityPoolsCalculatedState = getLiquidityPools(acc)

    val maybeLastSwapInfo = swapCalculatedStateAddresses.get(signerAddress) match {
      case Some(swapCalculatedState: SwapCalculatedStateAddress) =>
        SwapCalculatedStateLastReference(
          swapCalculatedState.fromToken,
          swapCalculatedState.toToken,
          swapCalculatedState.fee,
          swapCalculatedState.reference,
          swapCalculatedState.allowSpendReference,
          swapCalculatedState.minAmount,
          swapCalculatedState.maxAmount,
          swapCalculatedState.maxValidGsOrdinal,
          swapCalculatedState.poolId,
          swapCalculatedState.minPrice,
          swapCalculatedState.maxPrice,
          swapCalculatedState.ordinal
        ).some

      case _ => none
    }

    for {
      poolId <- buildLiquidityPoolUniqueIdentifier(swapUpdate.swapFromToken, swapUpdate.swapToToken)
      liquidityPool <- getLiquidityPoolByPoolId(liquidityPoolsCalculatedState, poolId)
      (fromTokenInfo, toTokenInfo) = getUpdatedTokenInformation(swapUpdate, liquidityPool)
      liquidityPoolUpdated = updateLiquidityPool(liquidityPool, fromTokenInfo, toTokenInfo)

      swapCalculatedStateAddress = SwapCalculatedStateAddress(
        fromTokenInfo,
        toTokenInfo,
        swapUpdate.fee,
        swapUpdate.reference,
        swapUpdate.allowSpendReference,
        swapUpdate.minAmount,
        swapUpdate.maxAmount,
        swapUpdate.maxValidGsOrdinal,
        swapUpdate.poolId,
        swapUpdate.minPrice,
        swapUpdate.maxPrice,
        currentSnapshotOrdinal,
        maybeLastSwapInfo
      )
      updatedSwapCalculatedState = SwapCalculatedState(swapCalculatedStateAddresses.updated(signerAddress, swapCalculatedStateAddress))
      updatedLiquidityPool = LiquidityPoolCalculatedState(liquidityPoolsCalculatedState.updated(poolId, liquidityPoolUpdated))

      updates: List[AmmUpdate] = swapUpdate :: acc.onChain.updates
      updatedCalculatedState = acc.calculated.ammState
        .updated(OperationType.Swap, updatedSwapCalculatedState)
        .updated(OperationType.LiquidityPool, updatedLiquidityPool)

    } yield
      DataState(
        AmmOnChainState(updates),
        AmmCalculatedState(updatedCalculatedState)
      )
  }
}
