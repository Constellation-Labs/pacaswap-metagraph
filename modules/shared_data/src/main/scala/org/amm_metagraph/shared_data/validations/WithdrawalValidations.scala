package org.amm_metagraph.shared_data.validations

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.swap.CurrencyId
import io.constellationnetwork.security.SecurityProvider
import io.constellationnetwork.security.signature.Signed

import eu.timepit.refined.auto._
import org.amm_metagraph.shared_data.types.DataUpdates.WithdrawalUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool.{LiquidityPool, buildLiquidityPoolUniqueIdentifier, getConfirmedLiquidityPools}
import org.amm_metagraph.shared_data.types.States.{AmmCalculatedState, WithdrawalCalculatedState}
import org.amm_metagraph.shared_data.types.Withdrawal.{WithdrawalReference, getWithdrawalCalculatedState}
import org.amm_metagraph.shared_data.validations.Errors._
import org.amm_metagraph.shared_data.validations.SharedValidations._

object WithdrawalValidations {
  def withdrawalValidationsL1[F[_]: Async](
    withdrawalUpdate: WithdrawalUpdate
  ): F[DataApplicationValidationErrorOr[Unit]] = Async[F].delay {
    validateIfTokenIdsAreTheSame(withdrawalUpdate.tokenAId, withdrawalUpdate.tokenBId)
  }

  def withdrawalValidationsL0[F[_]: Async](
    signedWithdrawalUpdate: Signed[WithdrawalUpdate],
    state: AmmCalculatedState
  )(implicit sp: SecurityProvider[F]): F[DataApplicationValidationErrorOr[Unit]] = for {
    l1Validation <- withdrawalValidationsL1(signedWithdrawalUpdate.value)
    signatures <- signatureValidations(signedWithdrawalUpdate, signedWithdrawalUpdate.source)
    sourceAddress = signedWithdrawalUpdate.source
    withdrawalUpdate = signedWithdrawalUpdate.value
    withdrawalCalculatedState = getWithdrawalCalculatedState(state)

    liquidityPoolsCalculatedState = getConfirmedLiquidityPools(state)

    liquidityPoolExists <- validateIfLiquidityPoolExists(
      withdrawalUpdate,
      liquidityPoolsCalculatedState
    )

    hasEnoughShares <- validateIfHasEnoughShares(
      withdrawalUpdate,
      liquidityPoolsCalculatedState,
      sourceAddress
    )

    withdrawsAllLPShares <- validateIfWithdrawsAllLPShares(
      withdrawalUpdate,
      liquidityPoolsCalculatedState
    )

    withdrawalNotPending = validateIfWithdrawalNotPending(
      signedWithdrawalUpdate,
      withdrawalCalculatedState.getPendingUpdates
    )

    lastRef = lastRefValidation(withdrawalCalculatedState, signedWithdrawalUpdate, sourceAddress)

  } yield
    signatures
      .productR(l1Validation)
      .productR(liquidityPoolExists)
      .productR(hasEnoughShares)
      .productR(withdrawsAllLPShares)
      .productR(withdrawalNotPending)
      .productR(lastRef)

  private def validateIfLiquidityPoolExists[F[_]: Async](
    withdrawalUpdate: WithdrawalUpdate,
    currentLiquidityPools: Map[String, LiquidityPool]
  ): F[DataApplicationValidationErrorOr[Unit]] = for {
    poolId <- buildLiquidityPoolUniqueIdentifier(withdrawalUpdate.tokenAId, withdrawalUpdate.tokenBId)
    result = LiquidityPoolDoesNotExists.unlessA(currentLiquidityPools.contains(poolId.value))
  } yield result

  private def validateIfWithdrawsAllLPShares[F[_]: Async](
    withdrawalUpdate: WithdrawalUpdate,
    currentLiquidityPools: Map[String, LiquidityPool]
  ): F[DataApplicationValidationErrorOr[Unit]] = for {
    poolId <- buildLiquidityPoolUniqueIdentifier(withdrawalUpdate.tokenAId, withdrawalUpdate.tokenBId)
    result = currentLiquidityPools.get(poolId.value) match {
      case Some(pool) if withdrawalUpdate.shareToWithdraw.value.value >= pool.poolShares.totalShares.value =>
        WithdrawalAllLPShares.invalid
      case None =>
        LiquidityPoolDoesNotExists.invalid
      case _ => valid
    }
  } yield result

  private def validateIfHasEnoughShares[F[_]: Async](
    withdrawalUpdate: WithdrawalUpdate,
    currentLiquidityPools: Map[String, LiquidityPool],
    address: Address
  ): F[DataApplicationValidationErrorOr[Unit]] = for {
    poolId <- buildLiquidityPoolUniqueIdentifier(withdrawalUpdate.tokenAId, withdrawalUpdate.tokenBId)
    result = currentLiquidityPools.get(poolId.value) match {
      case Some(pool) =>
        pool.poolShares.addressShares.get(address) match {
          case Some(userShares) if userShares.value.value.value >= withdrawalUpdate.shareToWithdraw.value.value.value =>
            valid
          case _ =>
            WithdrawalInsufficientShares.invalid
        }
      case None =>
        LiquidityPoolDoesNotExists.invalid
    }
  } yield result

  private def lastRefValidation(
    withdrawalCalculatedState: WithdrawalCalculatedState,
    signedWithdrawal: Signed[WithdrawalUpdate],
    address: Address
  ): DataApplicationValidationErrorOr[Unit] = {
    val lastConfirmed: Option[WithdrawalReference] = withdrawalCalculatedState.confirmed.value
      .get(address)
      .flatMap(_.maxByOption(_.parent.ordinal))
      .map(_.parent)

    lastConfirmed match {
      case Some(last) if signedWithdrawal.ordinal =!= last.ordinal.next || signedWithdrawal.parent =!= last =>
        InvalidWithdrawalParent.invalid
      case _ => valid
    }
  }

  private def validateIfWithdrawalNotPending(
    signedWithdrawal: Signed[WithdrawalUpdate],
    pendingUpdates: Set[Signed[WithdrawalUpdate]]
  ): DataApplicationValidationErrorOr[Unit] =
    pendingUpdates.toList.collectFirst {
      case pending if pending === signedWithdrawal => pending
    }.fold(valid)(_ => WithdrawalAlreadyPending.invalid)
}
