package org.amm_metagraph.shared_data.validations

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr

import org.amm_metagraph.shared_data.Utils.buildLiquidityPoolUniqueIdentifier
import org.amm_metagraph.shared_data.types.DataUpdates.SwapUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool.LiquidityPool
import org.amm_metagraph.shared_data.types.States.{AmmCalculatedState, LiquidityPoolCalculatedState, OperationType}
import org.amm_metagraph.shared_data.validations.Errors._

object SwapValidations {
  def swapValidationsL1[F[_]: Async](
    stakingUpdate: SwapUpdate
  ): F[DataApplicationValidationErrorOr[Unit]] = Async[F].delay {
    valid
  }

  def swapValidationsL0[F[_]: Async](
    swapUpdate: SwapUpdate,
    state: AmmCalculatedState
  ): F[DataApplicationValidationErrorOr[Unit]] = {
    val liquidityPoolsCalculatedState = state.ammState.get(OperationType.LiquidityPool).fold(Map.empty[String, LiquidityPool]) {
      case liquidityPoolCalculatedState: LiquidityPoolCalculatedState => liquidityPoolCalculatedState.liquidityPools
      case _                                                          => Map.empty[String, LiquidityPool]
    }

    for {
      liquidityPoolExists <- validateIfLiquidityPoolExists(
        swapUpdate,
        liquidityPoolsCalculatedState
      )

      poolHaveEnoughTokens <- validateIfPoolHaveEnoughTokens(
        swapUpdate,
        liquidityPoolsCalculatedState
      )

      swapValidationsL1 <- swapValidationsL1(swapUpdate)
      result = swapValidationsL1.productR(liquidityPoolExists).productR(poolHaveEnoughTokens)
    } yield result
  }

  private def validateIfLiquidityPoolExists[F[_]: Async](
    swapUpdate: SwapUpdate,
    currentLiquidityPools: Map[String, LiquidityPool]
  ): F[DataApplicationValidationErrorOr[Unit]] = for {
    poolId <- buildLiquidityPoolUniqueIdentifier(swapUpdate.swapFromToken, swapUpdate.swapToToken)
    result = SwapLiquidityPoolDoesNotExists.unlessA(currentLiquidityPools.contains(poolId))
  } yield result

  private def validateIfPoolHaveEnoughTokens[F[_]: Async](
    swapUpdate: SwapUpdate,
    currentLiquidityPools: Map[String, LiquidityPool]
  ): F[DataApplicationValidationErrorOr[Unit]] = {
    val hasEnoughTokens: LiquidityPool => Boolean = lp => {
      val maybeToken = swapUpdate.swapToToken match {
        case None => Option.when(lp.tokenA.identifier.isEmpty)(lp.tokenA).orElse(lp.tokenB.some)
        case Some(value) if lp.tokenA.identifier.contains(value) => lp.tokenA.some
        case Some(value)                                         => Option.when(lp.tokenB.identifier.contains(value))(lp.tokenB)
      }
      maybeToken.exists(_.amount.value > swapUpdate.maxAmount.value)
    }

    buildLiquidityPoolUniqueIdentifier(swapUpdate.swapFromToken, swapUpdate.swapToToken)
      .map(currentLiquidityPools.get)
      .map(maybePool => SwapLiquidityPoolNotEnoughTokens.unlessA(maybePool.exists(hasEnoughTokens)))
  }
}
