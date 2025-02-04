package org.amm_metagraph.shared_data.combiners

import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.SortedSet

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.artifact.SharedArtifact
import io.constellationnetwork.security.Hasher
import io.constellationnetwork.security.signature.Signed

import monocle.syntax.all._
import org.amm_metagraph.shared_data.SpendTransactions.generateSpendAction
import org.amm_metagraph.shared_data.globalSnapshots.{getAllowSpendLastSyncGlobalSnapshotState, getLastSyncGlobalIncrementalSnapshot}
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.Swap._

object SwapCombiner {
  private def getUpdatedTokenInformation(
    swapUpdate: SwapUpdate,
    liquidityPool: LiquidityPool
  ): (TokenInformation, TokenInformation) = {

    val fromTokenInfo = if (swapUpdate.swapFromPair == liquidityPool.tokenA.identifier) liquidityPool.tokenA else liquidityPool.tokenB
    val toTokenInfo = if (swapUpdate.swapToPair == liquidityPool.tokenA.identifier) liquidityPool.tokenA else liquidityPool.tokenB

    val swapAmount = swapUpdate.maxAmount.value.value
    val newFromTokenReserve = fromTokenInfo.amount.value + swapAmount
    val newToTokenReserve = (BigDecimal(liquidityPool.k) / BigDecimal(newFromTokenReserve)).toLong

    (
      fromTokenInfo.copy(amount = newFromTokenReserve.toTokenAmountFormat.toPosLongUnsafe),
      toTokenInfo.copy(amount = newToTokenReserve.toPosLongUnsafe)
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

  def combineSwap[F[_]: Async: Hasher](
    acc: DataState[AmmOnChainState, AmmCalculatedState],
    signedSwapUpdate: Signed[SwapUpdate],
    currentSnapshotOrdinal: SnapshotOrdinal
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
    val swapUpdate = signedSwapUpdate.value
    val swapCalculatedState = getSwapCalculatedState(acc.calculated)

    val updates = swapUpdate :: acc.onChain.updates

    getAllowSpendLastSyncGlobalSnapshotState(swapUpdate.allowSpendReference).flatMap {
      case None =>
        for {
          lastSyncHashedGIS <- getLastSyncGlobalIncrementalSnapshot
          gsEpochProgress = lastSyncHashedGIS.epochProgress
          updatedPendingSwapsCalculatedState =
            if (swapUpdate.maxValidGsEpochProgress < gsEpochProgress) {
              swapCalculatedState.pending - signedSwapUpdate
            } else if (!swapCalculatedState.pending.contains(signedSwapUpdate)) {
              swapCalculatedState.pending + signedSwapUpdate
            } else {
              swapCalculatedState.pending
            }

          newSwapState = swapCalculatedState
            .focus(_.pending)
            .replace(updatedPendingSwapsCalculatedState)

          updatedCalculatedState = acc.calculated
            .focus(_.operations)
            .modify(_.updated(OperationType.Swap, newSwapState))

        } yield
          DataState(
            AmmOnChainState(updates),
            updatedCalculatedState
          )

      case Some(hashedAllowSpend) =>
        val liquidityPoolsCalculatedState = getLiquidityPoolCalculatedState(acc.calculated)

        for {
          poolId <- buildLiquidityPoolUniqueIdentifier(swapUpdate.swapFromPair, swapUpdate.swapToPair)
          liquidityPool <- getLiquidityPoolByPoolId(liquidityPoolsCalculatedState.confirmed.value, poolId)
          (fromTokenInfo, toTokenInfo) = getUpdatedTokenInformation(swapUpdate, liquidityPool)
          liquidityPoolUpdated = updateLiquidityPool(liquidityPool, fromTokenInfo, toTokenInfo)

          swapCalculatedStateAddress = SwapCalculatedStateAddress(
            swapUpdate.sourceAddress,
            fromTokenInfo,
            toTokenInfo,
            swapUpdate.allowSpendReference,
            swapUpdate.minAmount,
            swapUpdate.maxAmount,
            swapUpdate.maxValidGsEpochProgress,
            swapUpdate.poolId,
            swapUpdate.minPrice,
            swapUpdate.maxPrice,
            currentSnapshotOrdinal
          )

          updatedPendingCalculatedState = swapCalculatedState.pending - signedSwapUpdate

          newSwapState = swapCalculatedState
            .focus(_.confirmed.value)
            .modify(current =>
              current.updatedWith(swapUpdate.sourceAddress) {
                case Some(confirmedSwaps) => Some(confirmedSwaps + swapCalculatedStateAddress)
                case None                 => Some(Set(swapCalculatedStateAddress))
              }
            )
            .focus(_.pending)
            .replace(updatedPendingCalculatedState)

          newLiquidityPoolState =
            liquidityPoolsCalculatedState
              .focus(_.confirmed.value)
              .modify(_.updated(poolId.value, liquidityPoolUpdated))

          updatedCalculatedState = acc.calculated
            .focus(_.operations)
            .modify(_.updated(OperationType.Swap, newSwapState))
            .focus(_.operations)
            .modify(_.updated(OperationType.LiquidityPool, newLiquidityPoolState))

          spendAction: SharedArtifact = generateSpendAction(hashedAllowSpend)

        } yield
          DataState(
            AmmOnChainState(updates),
            updatedCalculatedState,
            SortedSet(spendAction)
          )
    }
  }
}
