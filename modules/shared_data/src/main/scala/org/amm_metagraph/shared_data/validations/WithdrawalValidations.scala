package org.amm_metagraph.shared_data.validations

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hasher, SecurityProvider}

import eu.timepit.refined.auto._
import org.amm_metagraph.shared_data.types.DataUpdates.WithdrawalUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool.{LiquidityPool, buildLiquidityPoolUniqueIdentifier, getLiquidityPools}
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.Withdrawal.{WithdrawalOrdinal, getWithdrawalCalculatedState}
import org.amm_metagraph.shared_data.types.codecs.HasherSelector
import org.amm_metagraph.shared_data.validations.Errors._
import org.amm_metagraph.shared_data.validations.SharedValidations._

object WithdrawalValidations {
  def withdrawalValidationsL1[F[_]: Async](
    withdrawalUpdate: WithdrawalUpdate
  ): F[DataApplicationValidationErrorOr[Unit]] =
    valid.pure

  def withdrawalValidationsL0[F[_]: Async: HasherSelector](
    signedWithdrawalUpdate: Signed[WithdrawalUpdate],
    state: AmmCalculatedState
  )(implicit sp: SecurityProvider[F]): F[DataApplicationValidationErrorOr[Unit]] = for {
    sourceAddress <- signedWithdrawalUpdate.proofs.head.id.toAddress
    maxSignatureValidation = validateHasSingleSignature(signedWithdrawalUpdate)
    withdrawalUpdate = signedWithdrawalUpdate.value
    withdrawalCalculatedState = getWithdrawalCalculatedState(state)

    liquidityPoolsCalculatedState = getLiquidityPools(state)

    liquidityPoolExists <- validateIfLiquidityPoolExists(
      withdrawalUpdate,
      liquidityPoolsCalculatedState
    )

    hasEnoughShares <- validateIfHasEnoughShares(
      withdrawalUpdate,
      liquidityPoolsCalculatedState,
      sourceAddress
    )

    withdrawalNotPending <- HasherSelector[F].withCurrent { implicit hs =>
      validateIfWithdrawalNotPending(
        signedWithdrawalUpdate,
        withdrawalCalculatedState.getPendingUpdates
      )
    }

    lastRef = lastRefValidation(withdrawalCalculatedState, signedWithdrawalUpdate, sourceAddress)

  } yield
    maxSignatureValidation
      .productR(liquidityPoolExists)
      .productR(hasEnoughShares)
      .productR(withdrawalNotPending)
      .productR(lastRef)

  private def validateIfLiquidityPoolExists[F[_]: Async](
    withdrawalUpdate: WithdrawalUpdate,
    currentLiquidityPools: Map[String, LiquidityPool]
  ): F[DataApplicationValidationErrorOr[Unit]] = for {
    poolId <- buildLiquidityPoolUniqueIdentifier(withdrawalUpdate.tokenAId, withdrawalUpdate.tokenBId)
    result = LiquidityPoolDoesNotExists.unlessA(currentLiquidityPools.contains(poolId.value))
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
    val lastConfirmedOrdinal: Option[WithdrawalOrdinal] = withdrawalCalculatedState.confirmed.value
      .get(address)
      .flatMap(_.maxByOption(_.ordinal.value.value))
      .map(_.ordinal)

    lastConfirmedOrdinal match {
      case Some(last) if last.value >= signedWithdrawal.ordinal.value => WithdrawalOrdinalLowerThanLastConfirmed.invalid
      case _                                                          => valid
    }
  }

  private def validateIfWithdrawalNotPending[F[_]: Async: Hasher](
    signedWithdrawal: Signed[WithdrawalUpdate],
    pendingUpdates: Set[Signed[WithdrawalUpdate]]
  ): F[DataApplicationValidationErrorOr[Unit]] =
    for {
      updateHash <- signedWithdrawal.toHashed.map(_.hash)
      matchingWithdrawal <- pendingUpdates.toList.collectFirstSomeM {
        case Signed(w: WithdrawalUpdate, proofs) =>
          Signed(w, proofs).toHashed.map(h => Option.when(h.hash === updateHash)(()))
        case _ => none[Unit].pure[F]
      }
    } yield matchingWithdrawal.fold(valid)(_ => WithdrawalAlreadyPending.invalid)
}
