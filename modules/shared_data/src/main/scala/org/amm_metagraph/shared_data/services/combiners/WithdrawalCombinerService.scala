package org.amm_metagraph.shared_data.services.combiners

import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact._
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.{AllowSpend, CurrencyId, SwapAmount}
import io.constellationnetwork.security.SecurityProvider
import io.constellationnetwork.security.signature.Signed

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.{NonNegLong, PosLong}
import monocle.syntax.all._
import org.amm_metagraph.shared_data.SpendTransactions.generateSpendActionWithoutAllowSpends
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.types.DataUpdates.WithdrawalUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.Withdrawal.{WithdrawalCalculatedStateAddress, getWithdrawalCalculatedState}

trait WithdrawalCombinerService[F[_]] {
  def combineNew(
    signedUpdate: Signed[WithdrawalUpdate],
    oldState: DataState[AmmOnChainState, AmmCalculatedState],
    globalEpochProgress: EpochProgress,
    lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
    currencyId: CurrencyId
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]]
}

object WithdrawalCombinerService {
  def make[F[_]: Async: SecurityProvider]: WithdrawalCombinerService[F] =
    new WithdrawalCombinerService[F] {
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

      def combineNew(
        signedUpdate: Signed[WithdrawalUpdate],
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        globalEpochProgress: EpochProgress,
        lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
        currencyId: CurrencyId
      )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
        val withdrawalUpdate = signedUpdate.value
        val updates = withdrawalUpdate :: oldState.onChain.updates
        val withdrawalCalculatedState = getWithdrawalCalculatedState(oldState.calculated)

        for {
          poolId <- buildLiquidityPoolUniqueIdentifier(withdrawalUpdate.tokenAId, withdrawalUpdate.tokenBId)
          liquidityPoolsCalculatedState = getLiquidityPoolCalculatedState(oldState.calculated)
          liquidityPool <- getLiquidityPoolByPoolId(liquidityPoolsCalculatedState.confirmed.value, poolId)
          sourceAddress = signedUpdate.source

          result <- calculateWithdrawalAmounts(liquidityPool, withdrawalUpdate.shareToWithdraw, sourceAddress) match {
            case Some(withdrawalAmounts) =>
              val liquidityPoolUpdated = updateLiquidityPool(
                liquidityPool,
                sourceAddress,
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

              val newWithdrawalState =
                withdrawalCalculatedState
                  .focus(_.confirmed.value)
                  .modify(current =>
                    current.updatedWith(sourceAddress) {
                      case Some(confirmedWithdrawals) => Some(confirmedWithdrawals + withdrawalCalculatedStateAddress)
                      case None                       => Some(Set(withdrawalCalculatedStateAddress))
                    }
                  )

              val newLiquidityPoolState =
                liquidityPoolsCalculatedState
                  .focus(_.confirmed.value)
                  .modify(_.updated(poolId.value, liquidityPoolUpdated))

              val spendAction = generateSpendActionWithoutAllowSpends(
                signedUpdate.tokenAId,
                SwapAmount(withdrawalAmounts.tokenAAmount),
                signedUpdate.tokenBId,
                SwapAmount(withdrawalAmounts.tokenBAmount),
                signedUpdate.source,
                currencyId
              )

              val updatedCalculatedState = oldState.calculated
                .focus(_.operations)
                .modify(_.updated(OperationType.Withdrawal, newWithdrawalState))
                .focus(_.operations)
                .modify(_.updated(OperationType.LiquidityPool, newLiquidityPoolState))

              val updatedSharedArtifacts = oldState.sharedArtifacts ++ SortedSet[SharedArtifact](spendAction)

              DataState[AmmOnChainState, AmmCalculatedState](
                AmmOnChainState(updates),
                updatedCalculatedState,
                updatedSharedArtifacts
              ).pure[F]

            case None =>
              oldState.pure

          }
        } yield result
      }
    }
}
