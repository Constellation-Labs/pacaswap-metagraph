package org.amm_metagraph.shared_data.combiners

import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.SortedSet

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact.SharedArtifact
import io.constellationnetwork.security.Hasher
import io.constellationnetwork.security.signature.Signed

import org.amm_metagraph.shared_data.Utils._
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.LiquidityPool.{LiquidityPool, TokenInformation, getLiquidityPools}
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

  def combineSwap[F[_]: Async: Hasher](
    acc: DataState[AmmOnChainState, AmmCalculatedState],
    signedSwapUpdate: Signed[SwapUpdate],
    currentSnapshotOrdinal: SnapshotOrdinal
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
    val swapUpdate = signedSwapUpdate.value
    val (confirmedSwaps, pendingSwaps) = getConfirmedAndPendingSwaps(acc)

    val updates = swapUpdate :: acc.onChain.updates

    getAllowSpendLastSyncGlobalSnapshotState(swapUpdate.allowSpendReference).flatMap {
      case None =>
        for {
          lastSyncHashedGIS <- getLastSyncGlobalIncrementalSnapshot
          gsEpochProgress = lastSyncHashedGIS.epochProgress
          updatedPendingSwaps =
            if (signedSwapUpdate.maxValidGsEpochProgress < gsEpochProgress) {
              pendingSwaps.removed(swapUpdate.sourceAddress)
            } else {
              pendingSwaps.updatedWith(swapUpdate.sourceAddress) {
                case Some(existingUpdates) => Some(existingUpdates + signedSwapUpdate)
                case None                  => Some(Set(signedSwapUpdate))
              }
            }
          newSwapState = SwapCalculatedState(confirmedSwaps, updatedPendingSwaps)
          updatedCalculatedState = acc.calculated.operations.updated(OperationType.Swap, newSwapState)

        } yield
          DataState(
            AmmOnChainState(updates),
            AmmCalculatedState(updatedCalculatedState)
          )

      case Some(hashedAllowSpend) =>
        val liquidityPoolsCalculatedState = getLiquidityPools(acc.calculated)

        for {
          poolId <- buildLiquidityPoolUniqueIdentifier(swapUpdate.swapFromPair, swapUpdate.swapToPair)
          liquidityPool <- getLiquidityPoolByPoolId(liquidityPoolsCalculatedState, poolId)
          (fromTokenInfo, toTokenInfo) = getUpdatedTokenInformation(swapUpdate, liquidityPool)
          liquidityPoolUpdated = updateLiquidityPool(liquidityPool, fromTokenInfo, toTokenInfo)

          swapCalculatedStateAddress = SwapCalculatedStateAddress(
            swapUpdate.sourceAddress,
            fromTokenInfo,
            toTokenInfo,
            swapUpdate.fee,
            swapUpdate.allowSpendReference,
            swapUpdate.minAmount,
            swapUpdate.maxAmount,
            swapUpdate.maxValidGsEpochProgress,
            swapUpdate.poolId,
            swapUpdate.minPrice,
            swapUpdate.maxPrice,
            currentSnapshotOrdinal
          )

          newSwapState = SwapCalculatedState(
            confirmedSwaps.updatedWith(swapUpdate.sourceAddress) {
              case Some(confirmedSwaps) => Some(confirmedSwaps + swapCalculatedStateAddress)
              case None                 => Some(Set(swapCalculatedStateAddress))
            },
            pendingSwaps - swapUpdate.sourceAddress
          )

          newLiquidityPoolState = LiquidityPoolCalculatedState(
            liquidityPoolsCalculatedState.updated(poolId.value, liquidityPoolUpdated)
          )

          updatedCalculatedState = acc.calculated.operations
            .updated(OperationType.Swap, newSwapState)
            .updated(OperationType.LiquidityPool, newLiquidityPoolState)

          spendTransaction: SharedArtifact = generatePendingSpendTransaction(hashedAllowSpend)

        } yield
          DataState(
            AmmOnChainState(updates),
            AmmCalculatedState(updatedCalculatedState),
            SortedSet(spendTransaction)
          )
    }
  }

  private def getConfirmedAndPendingSwaps(
    acc: DataState[AmmOnChainState, AmmCalculatedState]
  ): (Map[Address, Set[SwapCalculatedStateAddress]], Map[Address, Set[Signed[SwapUpdate]]]) = {
    val swapState = getSwapCalculatedState(acc.calculated)
    (swapState.confirmed, swapState.pending)
  }
}
