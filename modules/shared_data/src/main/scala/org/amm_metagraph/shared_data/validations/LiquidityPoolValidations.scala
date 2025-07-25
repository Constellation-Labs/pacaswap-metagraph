package org.amm_metagraph.shared_data.validations

import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.SortedSet

import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.{AllowSpend, CurrencyId}
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hashed, SecurityProvider}

import org.amm_metagraph.shared_data.AllowSpends._
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.app.ApplicationConfig.Environment
import org.amm_metagraph.shared_data.epochProgress.getFailureExpireEpochProgress
import org.amm_metagraph.shared_data.refined.Percentage
import org.amm_metagraph.shared_data.refined.Percentage._
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, LiquidityPoolUpdate}
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.codecs.{HasherSelector, JsonWithBase64BinaryCodec}
import org.amm_metagraph.shared_data.validations.Errors._
import org.amm_metagraph.shared_data.validations.SharedValidations._

trait LiquidityPoolValidations[F[_]] {
  def l1Validations(
    liquidityPoolUpdate: LiquidityPoolUpdate
  ): F[DataApplicationValidationErrorOr[Unit]]

  def l0Validations(
    signedLiquidityPoolUpdate: Signed[LiquidityPoolUpdate],
    state: AmmCalculatedState,
    lastSyncGlobalEpochProgress: EpochProgress
  ): F[Either[FailedCalculatedState, Signed[LiquidityPoolUpdate]]]

  def newUpdateValidations(
    oldState: AmmCalculatedState,
    poolId: PoolId,
    signedUpdate: Signed[LiquidityPoolUpdate],
    lastSyncGlobalEpochProgress: EpochProgress,
    confirmedLps: Map[String, LiquidityPool],
    pendingLps: List[PoolId]
  ): F[Either[FailedCalculatedState, Signed[LiquidityPoolUpdate]]]

  def pendingAllowSpendsValidations(
    signedUpdate: Signed[LiquidityPoolUpdate],
    lastSyncGlobalEpochProgress: EpochProgress,
    currencyId: CurrencyId,
    allowSpendTokenA: Hashed[AllowSpend],
    allowSpendTokenB: Hashed[AllowSpend]
  ): F[Either[FailedCalculatedState, Signed[LiquidityPoolUpdate]]]

  def pendingSpendActionsValidation(
    signedUpdate: Signed[LiquidityPoolUpdate],
    lastSyncGlobalEpochProgress: EpochProgress
  ): F[Either[FailedCalculatedState, Signed[LiquidityPoolUpdate]]]

}
object LiquidityPoolValidations {
  def make[F[_]: Async: SecurityProvider: HasherSelector](
    applicationConfig: ApplicationConfig,
    dataUpdateCodec: JsonWithBase64BinaryCodec[F, AmmUpdate]
  ): LiquidityPoolValidations[F] = new LiquidityPoolValidations[F] {
    def l1Validations(
      liquidityPoolUpdate: LiquidityPoolUpdate
    ): F[DataApplicationValidationErrorOr[Unit]] = Async[F].delay {
      val tokensArePresent = validateIfTokensArePresent(liquidityPoolUpdate)
      val tokenIdsAreTheSame = validateIfTokenIdsAreTheSame(liquidityPoolUpdate.tokenAId, liquidityPoolUpdate.tokenBId)
      val componentsSumToTotal = validateComponentsSumToTotal(liquidityPoolUpdate)
      val feePercentage = validateFeePercentage(applicationConfig.environment, liquidityPoolUpdate)

      tokensArePresent
        .productR(tokenIdsAreTheSame)
        .productR(feePercentage)
        .productR(componentsSumToTotal)
    }

    def l0Validations(
      signedLiquidityPoolUpdate: Signed[LiquidityPoolUpdate],
      state: AmmCalculatedState,
      lastSyncGlobalEpochProgress: EpochProgress
    ): F[Either[FailedCalculatedState, Signed[LiquidityPoolUpdate]]] = {
      val liquidityPoolUpdate = signedLiquidityPoolUpdate.value
      for {
        signatures <- signatureValidations(signedLiquidityPoolUpdate, signedLiquidityPoolUpdate.source)

        liquidityPoolCalculatedState = getLiquidityPoolCalculatedState(state)
        confirmedLiquidityPools = getConfirmedLiquidityPools(state)
        poolAlreadyExists <- validateIfPoolAlreadyExists(
          liquidityPoolUpdate,
          confirmedLiquidityPools,
          liquidityPoolCalculatedState.getPendingUpdates
        ).handleErrorWith(_ => LiquidityPoolNotEnoughInformation.whenA(true).pure)

        expireEpochProgress = getFailureExpireEpochProgress(applicationConfig, lastSyncGlobalEpochProgress)
        hashedUpdate <- HasherSelector[F].withCurrent(implicit hs => signedLiquidityPoolUpdate.toHashed(dataUpdateCodec.serialize))
        updateHash = hashedUpdate.hash

        duplicatedUpdate = validateDuplicatedUpdate(state, hashedUpdate)

        result =
          if (duplicatedUpdate.isInvalid) {
            failWith(DuplicatedUpdate(liquidityPoolUpdate), expireEpochProgress, signedLiquidityPoolUpdate, updateHash)
          } else if (signatures.isInvalid) {
            failWith(InvalidSignatures(signatures.map(_.show).mkString_(",")), expireEpochProgress, signedLiquidityPoolUpdate, updateHash)
          } else if (poolAlreadyExists.isInvalid) {
            failWith(InvalidLiquidityPool(), expireEpochProgress, signedLiquidityPoolUpdate, updateHash)
          } else {
            signedLiquidityPoolUpdate.asRight
          }

      } yield result
    }

    def newUpdateValidations(
      oldState: AmmCalculatedState,
      poolId: PoolId,
      signedUpdate: Signed[LiquidityPoolUpdate],
      lastSyncGlobalEpochProgress: EpochProgress,
      confirmedLps: Map[String, LiquidityPool],
      pendingLps: List[PoolId]
    ): F[Either[FailedCalculatedState, Signed[LiquidityPoolUpdate]]] = {
      val expireEpochProgress = getFailureExpireEpochProgress(applicationConfig, lastSyncGlobalEpochProgress)

      val allAllowSpendsInUse = getAllAllowSpendsInUseFromState(oldState)

      val tokenAAllowSpendIsDuplicated = validateIfAllowSpendsAreDuplicated(
        signedUpdate.tokenAAllowSpend,
        allAllowSpendsInUse
      )
      val tokenBAllowSpendIsDuplicated = validateIfAllowSpendsAreDuplicated(
        signedUpdate.tokenBAllowSpend,
        allAllowSpendsInUse
      )

      for {
        hashedUpdate <- HasherSelector[F].withCurrent(implicit hs => signedUpdate.toHashed(dataUpdateCodec.serialize))
        updateHash = hashedUpdate.hash
        result =
          if (tokenAAllowSpendIsDuplicated.isInvalid || tokenBAllowSpendIsDuplicated.isInvalid) {
            failWith(DuplicatedAllowSpend(signedUpdate), expireEpochProgress, signedUpdate, updateHash)
          } else if (signedUpdate.maxValidGsEpochProgress < lastSyncGlobalEpochProgress) {
            failWith(OperationExpired(signedUpdate), expireEpochProgress, signedUpdate, updateHash)
          } else if (confirmedLps.contains(poolId.value) || pendingLps.contains(poolId)) {
            failWith(DuplicatedLiquidityPoolRequest(signedUpdate), expireEpochProgress, signedUpdate, updateHash)
          } else {
            signedUpdate.asRight
          }
      } yield result
    }

    def pendingAllowSpendsValidations(
      signedUpdate: Signed[LiquidityPoolUpdate],
      lastSyncGlobalEpochProgress: EpochProgress,
      currencyId: CurrencyId,
      allowSpendTokenA: Hashed[AllowSpend],
      allowSpendTokenB: Hashed[AllowSpend]
    ): F[Either[FailedCalculatedState, Signed[LiquidityPoolUpdate]]] = {
      val expireEpochProgress = getFailureExpireEpochProgress(applicationConfig, lastSyncGlobalEpochProgress)

      val update = signedUpdate.value
      val allowSpendDelay = applicationConfig.allowSpendEpochBufferDelay.value.value

      for {
        hashedUpdate <- HasherSelector[F].withCurrent(implicit hs => signedUpdate.toHashed(dataUpdateCodec.serialize))
        updateHash = hashedUpdate.hash
        result =
          if (signedUpdate.maxValidGsEpochProgress < lastSyncGlobalEpochProgress) {
            failWith(OperationExpired(signedUpdate), expireEpochProgress, signedUpdate, updateHash)
          } else if (allowSpendTokenA.source =!= signedUpdate.source || allowSpendTokenB.source =!= signedUpdate.source) {
            failWith(SourceAddressBetweenUpdateAndAllowSpendDifferent(signedUpdate), expireEpochProgress, signedUpdate, updateHash)
          } else if (allowSpendTokenA.destination =!= currencyId.value || allowSpendTokenB.destination =!= currencyId.value) {
            failWith(AllowSpendsDestinationAddressInvalid(), expireEpochProgress, signedUpdate, updateHash)
          } else if (allowSpendTokenA.currencyId =!= signedUpdate.tokenAId || allowSpendTokenB.currencyId =!= signedUpdate.tokenBId) {
            failWith(InvalidCurrencyIdsBetweenAllowSpendsAndDataUpdate(signedUpdate), expireEpochProgress, signedUpdate, updateHash)
          } else if (update.tokenAAmount.value > allowSpendTokenA.amount.value.value) {
            failWith(AmountGreaterThanAllowSpendLimit(allowSpendTokenA.signed.value), expireEpochProgress, signedUpdate, updateHash)
          } else if (update.tokenBAmount.value > allowSpendTokenB.amount.value.value) {
            failWith(AmountGreaterThanAllowSpendLimit(allowSpendTokenB.signed.value), expireEpochProgress, signedUpdate, updateHash)
          } else if (allowSpendTokenA.lastValidEpochProgress.value.value + allowSpendDelay < lastSyncGlobalEpochProgress.value.value) {
            failWith(AllowSpendExpired(allowSpendTokenA.signed.value), expireEpochProgress, signedUpdate, updateHash)
          } else if (allowSpendTokenB.lastValidEpochProgress.value.value + allowSpendDelay < lastSyncGlobalEpochProgress.value.value) {
            failWith(AllowSpendExpired(allowSpendTokenB.signed.value), expireEpochProgress, signedUpdate, updateHash)
          } else {
            Right(signedUpdate)
          }
      } yield result
    }

    def pendingSpendActionsValidation(
      signedUpdate: Signed[LiquidityPoolUpdate],
      lastSyncGlobalEpochProgress: EpochProgress
    ): F[Either[FailedCalculatedState, Signed[LiquidityPoolUpdate]]] = {
      val expireEpochProgress = getFailureExpireEpochProgress(applicationConfig, lastSyncGlobalEpochProgress)

      for {
        hashedUpdate <- HasherSelector[F].withCurrent(implicit hs => signedUpdate.toHashed(dataUpdateCodec.serialize))
        updateHash = hashedUpdate.hash
        result =
          if (signedUpdate.maxValidGsEpochProgress < lastSyncGlobalEpochProgress) {
            failWith(OperationExpired(signedUpdate), expireEpochProgress, signedUpdate, updateHash)
          } else {
            signedUpdate.asRight
          }
      } yield result
    }

    private def validateIfTokensArePresent(
      liquidityPoolUpdate: LiquidityPoolUpdate
    ): DataApplicationValidationErrorOr[Unit] =
      LiquidityPoolNotEnoughInformation.whenA(liquidityPoolUpdate.tokenAId.isEmpty && liquidityPoolUpdate.tokenBId.isEmpty)

    private def validateIfPoolAlreadyExists(
      liquidityPoolUpdate: LiquidityPoolUpdate,
      currentConfirmedLiquidityPools: Map[String, LiquidityPool],
      currentPendingUpdates: SortedSet[Signed[LiquidityPoolUpdate]]
    ): F[DataApplicationValidationErrorOr[Unit]] =
      for {
        poolId <- buildLiquidityPoolUniqueIdentifier(liquidityPoolUpdate.tokenAId, liquidityPoolUpdate.tokenBId)
        currentPendingLiquidityPoolsIds <- currentPendingUpdates.toList.traverse { pending =>
          buildLiquidityPoolUniqueIdentifier(pending.tokenAId, pending.tokenBId)
        }
      } yield
        LiquidityPoolAlreadyExists.whenA(
          currentConfirmedLiquidityPools.contains(poolId.value) ||
            currentPendingLiquidityPoolsIds.contains(poolId)
        )

    private def validateComponentsSumToTotal(
      liquidityPoolUpdate: LiquidityPoolUpdate
    ): DataApplicationValidationErrorOr[Unit] =
      liquidityPoolUpdate.poolFees match {
        case Some(percentages) =>
          if (percentages.total.toDecimal == percentages.providers.toDecimal + percentages.operators.toDecimal) {
            valid
          } else FeePercentagesMustEqualTotal.invalidNec
        case None => valid

      }

    private def validateFeePercentage(
      environment: Environment,
      liquidityPoolUpdate: LiquidityPoolUpdate
    ): DataApplicationValidationErrorOr[Unit] =
      if (liquidityPoolUpdate.poolFees.isEmpty) {
        valid // Since we will get the standard value when it's none
      } else {
        val feePercentages = liquidityPoolUpdate.poolFees.get
        if (feePercentages.total <= Percentage.zero)
          environment match {
            case ApplicationConfig.Dev | ApplicationConfig.Testnet => valid
            case _                                                 => FeePercentageTotalMustBeGreaterThanZero.invalidNec
          }
        else
          valid

      }

    private def validateDuplicatedUpdate(
      calculatedState: AmmCalculatedState,
      hashedUpdate: Hashed[LiquidityPoolUpdate]
    ): DataApplicationValidationErrorOr[Unit] = {
      val updateHash = hashedUpdate.hash
      val pendingAllowSpends = getPendingAllowSpendsLiquidityPoolUpdates(calculatedState)
      val pendingSpendActions = getPendingSpendActionLiquidityPoolUpdates(calculatedState)

      if (
        pendingAllowSpends.exists(_.updateHash === updateHash) ||
        pendingSpendActions.exists(_.updateHash === updateHash)
      ) {
        DuplicatedOperation.invalidNec
      } else {
        valid
      }
    }
  }
}
