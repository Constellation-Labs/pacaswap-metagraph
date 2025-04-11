package org.amm_metagraph.shared_data.validations

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.security.SecurityProvider
import io.constellationnetwork.security.signature.Signed

import org.amm_metagraph.shared_data.AllowSpends._
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.app.ApplicationConfig.Environment
import org.amm_metagraph.shared_data.refined.Percentage
import org.amm_metagraph.shared_data.refined.Percentage._
import org.amm_metagraph.shared_data.types.DataUpdates.LiquidityPoolUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.validations.Errors._
import org.amm_metagraph.shared_data.validations.SharedValidations._

object LiquidityPoolValidations {
  def liquidityPoolValidationsL1[F[_]: Async](
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

  def liquidityPoolValidationsL0[F[_]: Async: SecurityProvider](
    applicationConfig: ApplicationConfig,
    signedLiquidityPoolUpdate: Signed[LiquidityPoolUpdate],
    state: AmmCalculatedState
  ): F[DataApplicationValidationErrorOr[Unit]] = {
    val liquidityPoolUpdate = signedLiquidityPoolUpdate.value
    for {
      l1Validations <- liquidityPoolValidationsL1(applicationConfig, liquidityPoolUpdate)
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

  private def validateIfTokensArePresent(
    liquidityPoolUpdate: LiquidityPoolUpdate
  ): DataApplicationValidationErrorOr[Unit] =
    LiquidityPoolNotEnoughInformation.whenA(liquidityPoolUpdate.tokenAId.isEmpty && liquidityPoolUpdate.tokenBId.isEmpty)

  private def validateIfPoolAlreadyExists[F[_]: Async](
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
