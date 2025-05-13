package org.amm_metagraph.shared_data.validations

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.{AllowSpend, CurrencyId}
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hashed, SecurityProvider}

import eu.timepit.refined.types.numeric.NonNegLong
import org.amm_metagraph.shared_data.AllowSpends._
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.app.ApplicationConfig.Environment
import org.amm_metagraph.shared_data.epochProgress.getFailureExpireEpochProgress
import org.amm_metagraph.shared_data.refined.Percentage
import org.amm_metagraph.shared_data.refined.Percentage._
import org.amm_metagraph.shared_data.types.DataUpdates.LiquidityPoolUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.validations.Errors._
import org.amm_metagraph.shared_data.validations.SharedValidations._

trait LiquidityPoolValidations[F[_]] {
  def l1Validations(
    applicationConfig: ApplicationConfig,
    liquidityPoolUpdate: LiquidityPoolUpdate
  ): F[DataApplicationValidationErrorOr[Unit]]

  def l0Validations(
    applicationConfig: ApplicationConfig,
    signedLiquidityPoolUpdate: Signed[LiquidityPoolUpdate],
    state: AmmCalculatedState
  ): F[DataApplicationValidationErrorOr[Unit]]

  def newUpdateValidations(
    oldState: AmmCalculatedState,
    poolId: PoolId,
    signedUpdate: Signed[LiquidityPoolUpdate],
    lastSyncGlobalEpochProgress: EpochProgress,
    confirmedLps: Map[String, LiquidityPool],
    pendingLps: List[PoolId]
  ): Either[FailedCalculatedState, Signed[LiquidityPoolUpdate]]

  def pendingAllowSpendsValidations(
    signedUpdate: Signed[LiquidityPoolUpdate],
    lastSyncGlobalEpochProgress: EpochProgress,
    currencyId: CurrencyId,
    allowSpendTokenA: Hashed[AllowSpend],
    allowSpendTokenB: Hashed[AllowSpend]
  ): Either[FailedCalculatedState, Signed[LiquidityPoolUpdate]]

  def pendingSpendActionsValidation(
    signedUpdate: Signed[LiquidityPoolUpdate],
    lastSyncGlobalEpochProgress: EpochProgress
  ): Either[FailedCalculatedState, Signed[LiquidityPoolUpdate]]

}
object LiquidityPoolValidations {
  def make[F[_]: Async: SecurityProvider](
    applicationConfig: ApplicationConfig
  ): LiquidityPoolValidations[F] = new LiquidityPoolValidations[F] {
    def l1Validations(
      applicationConfig: ApplicationConfig,
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
      applicationConfig: ApplicationConfig,
      signedLiquidityPoolUpdate: Signed[LiquidityPoolUpdate],
      state: AmmCalculatedState
    ): F[DataApplicationValidationErrorOr[Unit]] = {
      val liquidityPoolUpdate = signedLiquidityPoolUpdate.value
      for {
        l1Validations <- l1Validations(applicationConfig, liquidityPoolUpdate)
        signatures <- signatureValidations(signedLiquidityPoolUpdate, signedLiquidityPoolUpdate.source)

        liquidityPoolCalculatedState = getLiquidityPoolCalculatedState(state)
        confirmedLiquidityPools = getConfirmedLiquidityPools(state)

        tokenIdsAreTheSame = validateIfTokenIdsAreTheSame(liquidityPoolUpdate.tokenAId, liquidityPoolUpdate.tokenBId)
        poolAlreadyExists <- validateIfPoolAlreadyExists(
          liquidityPoolUpdate,
          confirmedLiquidityPools,
          liquidityPoolCalculatedState.getPendingUpdates
        ).handleErrorWith(_ => LiquidityPoolNotEnoughInformation.whenA(true).pure)

        allAllowSpendsInUse = getAllAllowSpendsInUseFromState(state)

        tokenAAllowSpendIsDuplicated = validateIfAllowSpendsAreDuplicated(
          liquidityPoolUpdate.tokenAAllowSpend,
          allAllowSpendsInUse
        )
        tokenBAllowSpendIsDuplicated = validateIfAllowSpendsAreDuplicated(
          liquidityPoolUpdate.tokenBAllowSpend,
          allAllowSpendsInUse
        )
      } yield
        l1Validations
          .productR(signatures)
          .productR(tokenIdsAreTheSame)
          .productR(poolAlreadyExists)
          .productR(tokenAAllowSpendIsDuplicated)
          .productR(tokenBAllowSpendIsDuplicated)
    }

    def newUpdateValidations(
      oldState: AmmCalculatedState,
      poolId: PoolId,
      signedUpdate: Signed[LiquidityPoolUpdate],
      lastSyncGlobalEpochProgress: EpochProgress,
      confirmedLps: Map[String, LiquidityPool],
      pendingLps: List[PoolId]
    ): Either[FailedCalculatedState, Signed[LiquidityPoolUpdate]] = {
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

      if (tokenAAllowSpendIsDuplicated.isInvalid || tokenBAllowSpendIsDuplicated.isInvalid) {
        failWith(DuplicatedAllowSpend(signedUpdate), expireEpochProgress, signedUpdate)
      } else if (signedUpdate.maxValidGsEpochProgress < lastSyncGlobalEpochProgress) {
        failWith(OperationExpired(signedUpdate), expireEpochProgress, signedUpdate)
      } else if (confirmedLps.contains(poolId.value) || pendingLps.contains(poolId)) {
        failWith(DuplicatedLiquidityPoolRequest(signedUpdate), expireEpochProgress, signedUpdate)
      } else {
        signedUpdate.asRight
      }
    }

    def pendingAllowSpendsValidations(
      signedUpdate: Signed[LiquidityPoolUpdate],
      lastSyncGlobalEpochProgress: EpochProgress,
      currencyId: CurrencyId,
      allowSpendTokenA: Hashed[AllowSpend],
      allowSpendTokenB: Hashed[AllowSpend]
    ): Either[FailedCalculatedState, Signed[LiquidityPoolUpdate]] = {
      val expireEpochProgress = getFailureExpireEpochProgress(applicationConfig, lastSyncGlobalEpochProgress)

      val update = signedUpdate.value
      val allowSpendDelay = applicationConfig.allowSpendEpochBufferDelay.value.value

      if (allowSpendTokenA.source =!= signedUpdate.source || allowSpendTokenB.source =!= signedUpdate.source) {
        failWith(SourceAddressBetweenUpdateAndAllowSpendDifferent(signedUpdate), expireEpochProgress, signedUpdate)
      } else if (allowSpendTokenA.destination =!= currencyId.value || allowSpendTokenB.destination =!= currencyId.value) {
        failWith(AllowSpendsDestinationAddressInvalid(), expireEpochProgress, signedUpdate)
      } else if (allowSpendTokenA.currencyId =!= signedUpdate.tokenAId || allowSpendTokenB.currencyId =!= signedUpdate.tokenBId) {
        failWith(InvalidCurrencyIdsBetweenAllowSpendsAndDataUpdate(signedUpdate), expireEpochProgress, signedUpdate)
      } else if (update.tokenAAmount.value > allowSpendTokenA.amount.value.value) {
        failWith(AmountGreaterThanAllowSpendLimit(allowSpendTokenA.signed.value), expireEpochProgress, signedUpdate)
      } else if (update.tokenBAmount.value > allowSpendTokenB.amount.value.value) {
        failWith(AmountGreaterThanAllowSpendLimit(allowSpendTokenB.signed.value), expireEpochProgress, signedUpdate)
      } else if (allowSpendTokenA.lastValidEpochProgress.value.value + allowSpendDelay < lastSyncGlobalEpochProgress.value.value) {
        failWith(AllowSpendExpired(allowSpendTokenA.signed.value), expireEpochProgress, signedUpdate)
      } else if (allowSpendTokenB.lastValidEpochProgress.value.value + allowSpendDelay < lastSyncGlobalEpochProgress.value.value) {
        failWith(AllowSpendExpired(allowSpendTokenB.signed.value), expireEpochProgress, signedUpdate)
      } else {
        Right(signedUpdate)
      }
    }

    def pendingSpendActionsValidation(
      signedUpdate: Signed[LiquidityPoolUpdate],
      lastSyncGlobalEpochProgress: EpochProgress
    ): Either[FailedCalculatedState, Signed[LiquidityPoolUpdate]] = {
      val expireEpochProgress = getFailureExpireEpochProgress(applicationConfig, lastSyncGlobalEpochProgress)

      if (signedUpdate.maxValidGsEpochProgress < lastSyncGlobalEpochProgress) {
        failWith(OperationExpired(signedUpdate), expireEpochProgress, signedUpdate)
      } else {
        signedUpdate.asRight
      }
    }

    private def validateIfTokensArePresent(
      liquidityPoolUpdate: LiquidityPoolUpdate
    ): DataApplicationValidationErrorOr[Unit] =
      LiquidityPoolNotEnoughInformation.whenA(liquidityPoolUpdate.tokenAId.isEmpty && liquidityPoolUpdate.tokenBId.isEmpty)

    private def validateIfPoolAlreadyExists(
      liquidityPoolUpdate: LiquidityPoolUpdate,
      currentConfirmedLiquidityPools: Map[String, LiquidityPool],
      currentPendingUpdates: Set[Signed[LiquidityPoolUpdate]]
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
  }
}
