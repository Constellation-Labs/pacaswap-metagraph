package org.amm_metagraph.shared_data.validations

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr

import org.amm_metagraph.shared_data.Utils.buildLiquidityPoolUniqueIdentifier
import org.amm_metagraph.shared_data.types.DataUpdates.LiquidityPoolUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool.LiquidityPool
import org.amm_metagraph.shared_data.types.States.{AmmCalculatedState, LiquidityPoolCalculatedState, OperationType}
import org.amm_metagraph.shared_data.validations.Errors.{LiquidityPoolAlreadyExists, LiquidityPoolNotEnoughInformation}

object LiquidityPoolValidations {
  def liquidityPoolValidationsL1[F[_]: Async](
    liquidityPoolUpdate: LiquidityPoolUpdate
  ): F[DataApplicationValidationErrorOr[Unit]] = Async[F].delay {
    LiquidityPoolValidations.validateIfTokensArePresent(liquidityPoolUpdate)
  }

  def liquidityPoolValidationsL0[F[_]: Async](
    liquidityPoolUpdate: LiquidityPoolUpdate,
    state: AmmCalculatedState
  ): F[DataApplicationValidationErrorOr[Unit]] =
    for {
      liquidityPoolValidationsL1 <- liquidityPoolValidationsL1(liquidityPoolUpdate)
      calculatedState = state.ammState.get(OperationType.LiquidityPool).fold(Map.empty[String, LiquidityPool]) {
        case liquidityPoolCalculatedState: LiquidityPoolCalculatedState => liquidityPoolCalculatedState.liquidityPools
        case _                                                          => Map.empty[String, LiquidityPool]
      }
      poolAlreadyExists <- validateIfPoolAlreadyExists(
        liquidityPoolUpdate,
        calculatedState
      ).handleErrorWith(_ => LiquidityPoolNotEnoughInformation.whenA(true).pure)
    } yield liquidityPoolValidationsL1.productR(poolAlreadyExists)

  private def validateIfTokensArePresent(
    liquidityPoolUpdate: LiquidityPoolUpdate
  ): DataApplicationValidationErrorOr[Unit] =
    LiquidityPoolNotEnoughInformation.whenA(liquidityPoolUpdate.tokenA.identifier.isEmpty && liquidityPoolUpdate.tokenB.identifier.isEmpty)

  private def validateIfPoolAlreadyExists[F[_]: Async](
    liquidityPoolUpdate: LiquidityPoolUpdate,
    currentLiquidityPools: Map[String, LiquidityPool]
  ): F[DataApplicationValidationErrorOr[Unit]] =
    for {
      poolId <- buildLiquidityPoolUniqueIdentifier(liquidityPoolUpdate.tokenA.identifier, liquidityPoolUpdate.tokenB.identifier)
    } yield LiquidityPoolAlreadyExists.whenA(currentLiquidityPools.contains(poolId))
}
