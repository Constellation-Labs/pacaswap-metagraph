package org.amm_metagraph.shared_data.combiners

import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.SortedSet
import scala.math.BigDecimal.RoundingMode

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.artifact.SharedArtifact
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.AllowSpend
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hashed, Hasher}

import eu.timepit.refined.types.numeric.NonNegLong
import monocle.syntax.all._
import org.amm_metagraph.shared_data.SpendTransactions.generateSpendAction
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.globalSnapshots.{getAllowSpendLastSyncGlobalSnapshotState, getLastSyncGlobalIncrementalSnapshot}
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.Swap._

object SwapCombiner {
  private case class UpdatedTokenInformation(
    primaryTokenInformation: TokenInformation,
    pairTokenInformation: TokenInformation,
    receivedAmount: Amount,
    effectivePrice: Amount
  )

  private def getUpdatedTokenInformation(
    swapUpdate: SwapUpdate,
    liquidityPool: LiquidityPool
  ): UpdatedTokenInformation = {

    val fromTokenInfo = if (swapUpdate.swapFromPair == liquidityPool.tokenA.identifier) liquidityPool.tokenA else liquidityPool.tokenB
    val toTokenInfo = if (swapUpdate.swapToPair == liquidityPool.tokenA.identifier) liquidityPool.tokenA else liquidityPool.tokenB

    val swapAmount = swapUpdate.maxAmount.value.value
    val newFromTokenReserve = BigInt(fromTokenInfo.amount.value) + BigInt(swapAmount)
    val newToTokenReserve = liquidityPool.k / newFromTokenReserve
    val receivedAmount = toTokenInfo.amount.value - newToTokenReserve
    val effectivePrice = ((BigDecimal(receivedAmount) / BigDecimal(swapAmount)) * BigDecimal(1e8)).setScale(0, RoundingMode.HALF_UP).toLong

    UpdatedTokenInformation(
      fromTokenInfo.copy(amount = newFromTokenReserve.toLong.toPosLongUnsafe),
      toTokenInfo.copy(amount = newToTokenReserve.toLong.toPosLongUnsafe),
      Amount(NonNegLong.unsafeFrom(receivedAmount.toLong)),
      Amount(NonNegLong.unsafeFrom(effectivePrice))
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
    applicationConfig: ApplicationConfig,
    acc: DataState[AmmOnChainState, AmmCalculatedState],
    signedSwapUpdate: Signed[SwapUpdate],
    currentSnapshotOrdinal: SnapshotOrdinal,
    lastSyncGlobalEpochProgress: EpochProgress
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
          updatedTokenInformation = getUpdatedTokenInformation(swapUpdate, liquidityPool)
          maybeFailedUpdate = validateUpdate(
            applicationConfig,
            updatedTokenInformation,
            signedSwapUpdate,
            hashedAllowSpend,
            lastSyncGlobalEpochProgress
          )

          response = maybeFailedUpdate match {
            case Some(failedCalculatedState) =>
              val updatedSwapCalculatedState = swapCalculatedState
                .focus(_.failed)
                .modify(_ + failedCalculatedState)

              val updatedCalculatedState = acc.calculated
                .focus(_.operations)
                .modify(_.updated(OperationType.Swap, updatedSwapCalculatedState))

              DataState(
                AmmOnChainState(updates),
                updatedCalculatedState
              )

            case None =>
              val liquidityPoolUpdated = updateLiquidityPool(
                liquidityPool,
                updatedTokenInformation.primaryTokenInformation,
                updatedTokenInformation.pairTokenInformation
              )

              val swapCalculatedStateAddress = SwapCalculatedStateAddress(
                swapUpdate.sourceAddress,
                updatedTokenInformation.primaryTokenInformation,
                updatedTokenInformation.pairTokenInformation,
                swapUpdate.allowSpendReference,
                swapUpdate.minAmount,
                swapUpdate.maxAmount,
                swapUpdate.maxValidGsEpochProgress,
                poolId.some,
                swapUpdate.minPrice,
                swapUpdate.maxPrice,
                currentSnapshotOrdinal
              )

              val updatedPendingCalculatedState = swapCalculatedState.pending - signedSwapUpdate

              val newSwapState = swapCalculatedState
                .focus(_.confirmed.value)
                .modify(current =>
                  current.updatedWith(swapUpdate.sourceAddress) {
                    case Some(confirmedSwaps) => Some(confirmedSwaps + swapCalculatedStateAddress)
                    case None                 => Some(Set(swapCalculatedStateAddress))
                  }
                )
                .focus(_.pending)
                .replace(updatedPendingCalculatedState)

              val newLiquidityPoolState =
                liquidityPoolsCalculatedState
                  .focus(_.confirmed.value)
                  .modify(_.updated(poolId.value, liquidityPoolUpdated))

              val updatedCalculatedState = acc.calculated
                .focus(_.operations)
                .modify(_.updated(OperationType.Swap, newSwapState))
                .focus(_.operations)
                .modify(_.updated(OperationType.LiquidityPool, newLiquidityPoolState))

              val spendAction: SharedArtifact = generateSpendAction(hashedAllowSpend)

              DataState(
                AmmOnChainState(updates),
                updatedCalculatedState,
                SortedSet(spendAction)
              )
          }
        } yield response

    }
  }

  def validateUpdate(
    applicationConfig: ApplicationConfig,
    updatedTokenInformation: UpdatedTokenInformation,
    signedUpdate: Signed[SwapUpdate],
    allowSpend: Hashed[AllowSpend],
    lastSyncGlobalEpochProgress: EpochProgress
  ): Option[FailedCalculatedState] =
    if (updatedTokenInformation.receivedAmount.value.value < signedUpdate.minAmount.value.value) {
      FailedCalculatedState(
        SwapLessThanMinAmount(),
        EpochProgress(
          NonNegLong.unsafeFrom(
            lastSyncGlobalEpochProgress.value.value + applicationConfig.failedOperationsExpirationEpochProgresses.value.value
          )
        ),
        signedUpdate
      ).some
    } else if (signedUpdate.minPrice.exists(p => updatedTokenInformation.effectivePrice.value.value < p.value)) {
      FailedCalculatedState(
        SwapPriceBelowAcceptableMinPrice(),
        EpochProgress(
          NonNegLong.unsafeFrom(
            lastSyncGlobalEpochProgress.value.value + applicationConfig.failedOperationsExpirationEpochProgresses.value.value
          )
        ),
        signedUpdate
      ).some
    } else if (signedUpdate.maxPrice.exists(p => updatedTokenInformation.effectivePrice.value.value > p.value)) {
      FailedCalculatedState(
        SwapPriceExceedsAcceptableMaxPrice(),
        EpochProgress(
          NonNegLong.unsafeFrom(
            lastSyncGlobalEpochProgress.value.value + applicationConfig.failedOperationsExpirationEpochProgresses.value.value
          )
        ),
        signedUpdate
      ).some
    } else if (allowSpend.lastValidEpochProgress.value.value < lastSyncGlobalEpochProgress.value.value) {
      FailedCalculatedState(
        AllowSpendExpired(allowSpend.signed.value),
        EpochProgress(NonNegLong.unsafeFrom(lastSyncGlobalEpochProgress.value.value + 30L)),
        signedUpdate
      ).some
    } else {
      None
    }
}
