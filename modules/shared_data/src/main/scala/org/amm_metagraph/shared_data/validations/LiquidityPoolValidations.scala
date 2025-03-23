package org.amm_metagraph.shared_data.validations

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.security.SecurityProvider
import io.constellationnetwork.security.signature.Signed

import org.amm_metagraph.shared_data.types.DataUpdates.LiquidityPoolUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.validations.Errors.{LiquidityPoolAlreadyExists, LiquidityPoolNotEnoughInformation}
import org.amm_metagraph.shared_data.validations.SharedValidations._

object LiquidityPoolValidations {
  def liquidityPoolValidationsL1[F[_]: Async](
    liquidityPoolUpdate: LiquidityPoolUpdate
  ): F[DataApplicationValidationErrorOr[Unit]] = Async[F].delay {
    LiquidityPoolValidations.validateIfTokensArePresent(liquidityPoolUpdate)
  }

  def liquidityPoolValidationsL0[F[_]: Async: SecurityProvider](
    signedLiquidityPoolUpdate: Signed[LiquidityPoolUpdate],
    state: AmmCalculatedState
  ): F[DataApplicationValidationErrorOr[Unit]] = {
    val liquidityPoolUpdate = signedLiquidityPoolUpdate.value
    for {
      liquidityPoolValidationsL1 <- liquidityPoolValidationsL1(liquidityPoolUpdate)
      signatures <- signatureValidations(signedLiquidityPoolUpdate, signedLiquidityPoolUpdate.source)

      calculatedState = getLiquidityPools(state)
      liquidityPoolCalculatedState = getLiquidityPoolCalculatedState(state)
      poolAlreadyExists <- validateIfPoolAlreadyExists(
        liquidityPoolUpdate,
        calculatedState
      ).handleErrorWith(_ => LiquidityPoolNotEnoughInformation.whenA(true).pure)
      tokenAAllowSpendIsDuplicated = validateIfAllowSpendsAreDuplicated(
        liquidityPoolUpdate.tokenAAllowSpend,
        liquidityPoolCalculatedState.getPendingUpdates
      )
      tokenBAllowSpendIsDuplicated = validateIfAllowSpendsAreDuplicated(
        liquidityPoolUpdate.tokenBAllowSpend,
        liquidityPoolCalculatedState.getPendingUpdates
      )
    } yield
      liquidityPoolValidationsL1
        .productR(signatures)
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
    currentLiquidityPools: Map[String, LiquidityPool]
  ): F[DataApplicationValidationErrorOr[Unit]] =
    for {
      poolId <- buildLiquidityPoolUniqueIdentifier(liquidityPoolUpdate.tokenAId, liquidityPoolUpdate.tokenBId)
    } yield LiquidityPoolAlreadyExists.whenA(currentLiquidityPools.contains(poolId.value))
}
