package org.amm_metagraph.shared_data.combiners

import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.SortedSet

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact._
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.swap.{CurrencyId, SwapAmount}
import io.constellationnetwork.security.Hasher
import io.constellationnetwork.security.signature.Signed

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.{NonNegLong, PosLong}
import monocle.syntax.all._
import org.amm_metagraph.shared_data.SpendTransactions.generateSpendActionWithoutInput
import org.amm_metagraph.shared_data.globalSnapshots.getLastSyncGlobalIncrementalSnapshot
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.types.DataUpdates.WithdrawalUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.Withdrawal.{WithdrawalCalculatedStateAddress, getWithdrawalCalculatedState}

object WithdrawalCombiner {
  private case class WithdrawalTokenAmounts(
    tokenAAmount: PosLong,
    tokenBAmount: PosLong
  )

  private def calculateWithdrawalAmounts(
    liquidityPool: LiquidityPool,
    sharesToWithdraw: ShareAmount,
    signerAddress: Address
  ): Option[WithdrawalTokenAmounts] = {
    val PRECISION = 8
    val SCALING_FACTOR = BigInt(10).pow(PRECISION)

    liquidityPool.poolShares.addressShares.get(signerAddress).flatMap { addressShares =>
      if (sharesToWithdraw.value <= addressShares.value) {
        val totalShares = BigInt(liquidityPool.poolShares.totalShares.value)
        val sharesBigInt = BigInt(sharesToWithdraw.value.value)

        // Scale up by SCALING_FACTOR to maintain precision during division
        val tokenAOut = (BigInt(liquidityPool.tokenA.amount.value) * sharesBigInt * SCALING_FACTOR / totalShares) / SCALING_FACTOR
        val tokenBOut = (BigInt(liquidityPool.tokenB.amount.value) * sharesBigInt * SCALING_FACTOR / totalShares) / SCALING_FACTOR

        if (tokenAOut > 0 && tokenBOut > 0) {
          Some(
            WithdrawalTokenAmounts(
              PosLong.unsafeFrom(tokenAOut.toLong),
              PosLong.unsafeFrom(tokenBOut.toLong)
            )
          )
        } else None
      } else None
    }
  }

  private def updateLiquidityPool(
    liquidityPool: LiquidityPool,
    signerAddress: Address,
    sharesToWithdraw: ShareAmount,
    withdrawalAmounts: WithdrawalTokenAmounts
  ): LiquidityPool = {
    val updatedTokenAAmount = (liquidityPool.tokenA.amount.value - withdrawalAmounts.tokenAAmount.value).toPosLongUnsafe
    val updatedTokenBAmount = (liquidityPool.tokenB.amount.value - withdrawalAmounts.tokenBAmount.value).toPosLongUnsafe

    val addressSharesAmount = liquidityPool.poolShares.addressShares.getOrElse(signerAddress, ShareAmount(Amount.empty))
    val updatedAddressSharesAmount = ShareAmount(
      Amount(NonNegLong.unsafeFrom(addressSharesAmount.value.value - sharesToWithdraw.value.value))
    )
    val updatedAddressShares =
      if (updatedAddressSharesAmount.value === Amount.empty) {
        liquidityPool.poolShares.addressShares - signerAddress
      } else {
        liquidityPool.poolShares.addressShares.updated(signerAddress, updatedAddressSharesAmount)
      }

    val k = BigInt(updatedTokenAAmount.value) * BigInt(updatedTokenBAmount.value)

    liquidityPool.copy(
      tokenA = liquidityPool.tokenA.copy(amount = updatedTokenAAmount),
      tokenB = liquidityPool.tokenB.copy(amount = updatedTokenBAmount),
      k = k,
      poolShares = PoolShares(
        (liquidityPool.poolShares.totalShares.value - sharesToWithdraw.value.value).toPosLongUnsafe,
        updatedAddressShares
      )
    )
  }

  private def generateSpendTransactions(
    tokenA: Option[CurrencyId],
    tokenAAmount: SwapAmount,
    tokenB: Option[CurrencyId],
    tokenBAmount: SwapAmount,
    destination: Address
  ): SortedSet[SharedArtifact] = {
    val spendTransactionTokenA = generateSpendActionWithoutInput(tokenA, tokenAAmount, destination)
    val spendTransactionTokenB = generateSpendActionWithoutInput(tokenB, tokenBAmount, destination)
    SortedSet[SharedArtifact](
      spendTransactionTokenA,
      spendTransactionTokenB
    )
  }

  def combineWithdrawal[F[_]: Async: Hasher](
    acc: DataState[AmmOnChainState, AmmCalculatedState],
    signedWithdrawalUpdate: Signed[WithdrawalUpdate],
    signerAddress: Address,
    currentSnapshotOrdinal: SnapshotOrdinal
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
    val withdrawalUpdate = signedWithdrawalUpdate.value
    val updates = withdrawalUpdate :: acc.onChain.updates
    val withdrawalCalculatedState = getWithdrawalCalculatedState(acc.calculated)

    for {
      lastSyncHashedGIS <- getLastSyncGlobalIncrementalSnapshot
      gsEpochProgress = lastSyncHashedGIS.epochProgress

      liquidityPoolsCalculatedState = getLiquidityPoolCalculatedState(acc.calculated)
      poolId <- buildLiquidityPoolUniqueIdentifier(withdrawalUpdate.tokenAId, withdrawalUpdate.tokenBId)
      liquidityPool <- getLiquidityPoolByPoolId(liquidityPoolsCalculatedState.confirmed.value, poolId)

      result <- calculateWithdrawalAmounts(liquidityPool, withdrawalUpdate.shareToWithdraw, signerAddress) match {
        case Some(withdrawalAmounts) =>
          val liquidityPoolUpdated = updateLiquidityPool(
            liquidityPool,
            signerAddress,
            withdrawalUpdate.shareToWithdraw,
            withdrawalAmounts
          )

          val withdrawalCalculatedStateAddress =
            WithdrawalCalculatedStateAddress(
              withdrawalUpdate.tokenAId,
              withdrawalUpdate.tokenBId,
              withdrawalUpdate.shareToWithdraw,
              withdrawalUpdate.parent,
              withdrawalUpdate.ordinal
            )

          val updatedPendingCalculatedState = withdrawalCalculatedState.pending - signedWithdrawalUpdate
          val newWithdrawalState =
            withdrawalCalculatedState
              .focus(_.confirmed.value)
              .modify(current =>
                current.updatedWith(signerAddress) {
                  case Some(confirmedWithdrawals) => Some(confirmedWithdrawals + withdrawalCalculatedStateAddress)
                  case None                       => Some(Set(withdrawalCalculatedStateAddress))
                }
              )
              .focus(_.pending)
              .replace(updatedPendingCalculatedState)

          val newLiquidityPoolState =
            liquidityPoolsCalculatedState
              .focus(_.confirmed.value)
              .modify(_.updated(poolId.value, liquidityPoolUpdated))

          val spendTransactions = generateSpendTransactions(
            withdrawalUpdate.tokenAId,
            SwapAmount(withdrawalAmounts.tokenAAmount),
            withdrawalUpdate.tokenBId,
            SwapAmount(withdrawalAmounts.tokenBAmount),
            signerAddress
          )

          val updatedCalculatedState = acc.calculated
            .focus(_.operations)
            .modify(_.updated(OperationType.Withdrawal, newWithdrawalState))
            .focus(_.operations)
            .modify(_.updated(OperationType.LiquidityPool, newLiquidityPoolState))

          DataState[AmmOnChainState, AmmCalculatedState](
            AmmOnChainState(updates),
            updatedCalculatedState,
            spendTransactions
          ).pure[F]

        case None =>
          val updatedPendingWithdrawalsCalculatedState =
            if (withdrawalUpdate.maxValidGsEpochProgress < gsEpochProgress) {
              withdrawalCalculatedState.pending - signedWithdrawalUpdate
            } else if (!withdrawalCalculatedState.pending.contains(signedWithdrawalUpdate)) {
              withdrawalCalculatedState.pending + signedWithdrawalUpdate
            } else {
              withdrawalCalculatedState.pending
            }

          val newWithdrawalState = withdrawalCalculatedState
            .focus(_.pending)
            .replace(updatedPendingWithdrawalsCalculatedState)

          val updatedCalculatedState = acc.calculated
            .focus(_.operations)
            .modify(_.updated(OperationType.Withdrawal, newWithdrawalState))

          DataState(
            AmmOnChainState(updates),
            updatedCalculatedState
          ).pure[F]

      }
    } yield result
  }
}
