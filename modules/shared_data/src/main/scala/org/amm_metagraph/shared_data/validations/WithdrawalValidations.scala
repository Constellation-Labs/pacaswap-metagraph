package org.amm_metagraph.shared_data.validations

import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.SortedSet

import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hashed, SecurityProvider}

import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.NonNegLong
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.epochProgress.getFailureExpireEpochProgress
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, WithdrawalUpdate}
import org.amm_metagraph.shared_data.types.LiquidityPool.{LiquidityPool, buildLiquidityPoolUniqueIdentifier, getConfirmedLiquidityPools}
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.Withdrawal.{
  WithdrawalReference,
  getPendingSpendActionWithdrawalUpdates,
  getWithdrawalCalculatedState
}
import org.amm_metagraph.shared_data.types.codecs.{HasherSelector, JsonWithBase64BinaryCodec}
import org.amm_metagraph.shared_data.validations.Errors._
import org.amm_metagraph.shared_data.validations.SharedValidations._

trait WithdrawalValidations[F[_]] {
  def l1Validations(
    withdrawalUpdate: WithdrawalUpdate
  ): F[DataApplicationValidationErrorOr[Unit]]

  def l0Validations(
    signedWithdrawalUpdate: Signed[WithdrawalUpdate],
    state: AmmCalculatedState,
    lastSyncGlobalEpochProgress: EpochProgress
  ): F[Either[FailedCalculatedState, Signed[WithdrawalUpdate]]]

  def newUpdateValidations(
    signedUpdate: Signed[WithdrawalUpdate],
    withdrawalAmounts: WithdrawalTokenInfo,
    lastSyncGlobalEpochProgress: EpochProgress,
    updatedPool: LiquidityPool
  ): F[Either[FailedCalculatedState, Signed[WithdrawalUpdate]]]

  def pendingSpendActionsValidation(
    signedUpdate: Signed[WithdrawalUpdate],
    isSpendTransactionAcceptedInGL0: Boolean,
    lastSyncGlobalEpochProgress: EpochProgress
  ): F[Either[FailedCalculatedState, Signed[WithdrawalUpdate]]]
}

object WithdrawalValidations {
  def make[F[_]: Async: HasherSelector: SecurityProvider](
    applicationConfig: ApplicationConfig,
    dataUpdateCodec: JsonWithBase64BinaryCodec[F, AmmUpdate]
  ): WithdrawalValidations[F] = new WithdrawalValidations[F] {
    def l1Validations(
      withdrawalUpdate: WithdrawalUpdate
    ): F[DataApplicationValidationErrorOr[Unit]] = Async[F].delay {
      validateIfTokenIdsAreTheSame(withdrawalUpdate.tokenAId, withdrawalUpdate.tokenBId)
    }

    def l0Validations(
      signedWithdrawalUpdate: Signed[WithdrawalUpdate],
      state: AmmCalculatedState,
      lastSyncGlobalEpochProgress: EpochProgress
    ): F[Either[FailedCalculatedState, Signed[WithdrawalUpdate]]] =
      for {
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

        hashedUpdate <- HasherSelector[F].withCurrent(implicit hs => signedWithdrawalUpdate.toHashed(dataUpdateCodec.serialize))
        updateHash = hashedUpdate.hash
        duplicatedUpdate = validateDuplicatedUpdate(state, hashedUpdate)

        lastRef = lastRefValidation(withdrawalCalculatedState, signedWithdrawalUpdate, sourceAddress)
        expireEpochProgress = getFailureExpireEpochProgress(applicationConfig, lastSyncGlobalEpochProgress)

        result =
          if (duplicatedUpdate.isInvalid) {
            failWith(DuplicatedUpdate(signedWithdrawalUpdate.value), expireEpochProgress, signedWithdrawalUpdate, updateHash)
          } else if (liquidityPoolExists.isInvalid) {
            failWith(InvalidLiquidityPool(), expireEpochProgress, signedWithdrawalUpdate, updateHash)
          } else if (signatures.isInvalid) {
            failWith(InvalidSignatures(signatures.map(_.show).mkString_(",")), expireEpochProgress, signedWithdrawalUpdate, updateHash)
          } else if (hasEnoughShares.isInvalid) {
            failWith(NotEnoughShares(), expireEpochProgress, signedWithdrawalUpdate, updateHash)
          } else if (withdrawsAllLPShares.isInvalid) {
            failWith(WithdrawalAllLPSharesError(), expireEpochProgress, signedWithdrawalUpdate, updateHash)
          } else if (withdrawalNotPending.isInvalid) {
            failWith(WithdrawalNotPendingError(), expireEpochProgress, signedWithdrawalUpdate, updateHash)
          } else if (lastRef.isInvalid) {
            failWith(InvalidLastReference(), expireEpochProgress, signedWithdrawalUpdate, updateHash)
          } else {
            signedWithdrawalUpdate.asRight
          }
      } yield result

    def newUpdateValidations(
      signedUpdate: Signed[WithdrawalUpdate],
      withdrawalAmounts: WithdrawalTokenInfo,
      lastSyncGlobalEpochProgress: EpochProgress,
      updatedPool: LiquidityPool
    ): F[Either[FailedCalculatedState, Signed[WithdrawalUpdate]]] = {
      val expireEpochProgress = getFailureExpireEpochProgress(applicationConfig, lastSyncGlobalEpochProgress)
      for {
        hashedUpdate <- HasherSelector[F].withCurrent(implicit hs => signedUpdate.toHashed(dataUpdateCodec.serialize))
        updateHash = hashedUpdate.hash
        result =
          if (signedUpdate.maxValidGsEpochProgress < lastSyncGlobalEpochProgress) {
            failWith(OperationExpired(signedUpdate), expireEpochProgress, signedUpdate, updateHash)
          } else if (
            updatedPool.tokenA.amount < applicationConfig.tokenLimits.minTokens ||
            updatedPool.tokenB.amount < applicationConfig.tokenLimits.minTokens
          ) {
            failWith(WithdrawalWouldDrainPoolBalance(), expireEpochProgress, signedUpdate, updateHash)
          } else if (
            signedUpdate.minAmountAOut.exists(_ > withdrawalAmounts.tokenAAmount) ||
            signedUpdate.minAmountBOut.exists(_ > withdrawalAmounts.tokenBAmount)
          ) {
            failWith(WithdrawalLessThanMinAmount(), expireEpochProgress, signedUpdate, updateHash)
          } else if (
            signedUpdate.maxAmountAOut.exists(_ < withdrawalAmounts.tokenAAmount) ||
            signedUpdate.maxAmountBOut.exists(_ < withdrawalAmounts.tokenBAmount)
          ) {
            failWith(WithdrawalHigherThanMaxAmount(), expireEpochProgress, signedUpdate, updateHash)
          } else {
            Right(signedUpdate)
          }
      } yield result
    }

    def pendingSpendActionsValidation(
      signedUpdate: Signed[WithdrawalUpdate],
      isSpendTransactionAcceptedInGL0: Boolean,
      lastSyncGlobalEpochProgress: EpochProgress
    ): F[Either[FailedCalculatedState, Signed[WithdrawalUpdate]]] = {
      val expireEpochProgress = getFailureExpireEpochProgress(applicationConfig, lastSyncGlobalEpochProgress)
      for {
        hashedUpdate <- HasherSelector[F].withCurrent(implicit hs => signedUpdate.toHashed(dataUpdateCodec.serialize))
        updateHash = hashedUpdate.hash
        result =
          if (signedUpdate.maxValidGsEpochProgress < lastSyncGlobalEpochProgress && !isSpendTransactionAcceptedInGL0) {
            failWith(OperationExpired(signedUpdate), expireEpochProgress, signedUpdate, updateHash)
          } else {
            Right(signedUpdate)
          }
      } yield result
    }

    private def validateIfLiquidityPoolExists(
      withdrawalUpdate: WithdrawalUpdate,
      currentLiquidityPools: Map[String, LiquidityPool]
    ): F[DataApplicationValidationErrorOr[Unit]] = for {
      poolId <- buildLiquidityPoolUniqueIdentifier(withdrawalUpdate.tokenAId, withdrawalUpdate.tokenBId)
      result = LiquidityPoolDoesNotExists.unlessA(currentLiquidityPools.contains(poolId.value))
    } yield result

    private def validateIfWithdrawsAllLPShares(
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

    private def validateIfHasEnoughShares(
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
        .map(_.lastReference)

      lastConfirmed match {
        case Some(last) if signedWithdrawal.ordinal =!= last.ordinal.next || signedWithdrawal.parent =!= last =>
          InvalidWithdrawalParent.invalid
        case _ => valid
      }
    }

    private def validateIfWithdrawalNotPending(
      signedWithdrawal: Signed[WithdrawalUpdate],
      pendingUpdates: SortedSet[Signed[WithdrawalUpdate]]
    ): DataApplicationValidationErrorOr[Unit] =
      pendingUpdates.toList.collectFirst {
        case pending if pending === signedWithdrawal => pending
      }.fold(valid)(_ => WithdrawalAlreadyPending.invalid)

    private def validateDuplicatedUpdate(
      calculatedState: AmmCalculatedState,
      hashedUpdate: Hashed[WithdrawalUpdate]
    ): DataApplicationValidationErrorOr[Unit] = {
      val updateHash = hashedUpdate.hash
      val pendingSpendActions = getPendingSpendActionWithdrawalUpdates(calculatedState)
      if (pendingSpendActions.exists(_.updateHash === updateHash)) {
        DuplicatedOperation.invalidNec
      } else {
        valid
      }
    }
  }
}
