package org.amm_metagraph.shared_data.validations

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr

import org.amm_metagraph.shared_data.types.DataUpdates.SwapUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool.{LiquidityPool, buildLiquidityPoolUniqueIdentifier, getLiquidityPools}
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.Swap.getSwapCalculatedState
import org.amm_metagraph.shared_data.validations.Errors._
import org.amm_metagraph.shared_data.validations.SharedValidations.validateIfAllowSpendsAreDuplicated

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
    val liquidityPoolsCalculatedState = getLiquidityPools(state)

    for {
      liquidityPoolExists <- validateIfLiquidityPoolExists(
        swapUpdate,
        liquidityPoolsCalculatedState
      )
      poolHaveEnoughTokens <- validateIfPoolHaveEnoughTokens(
        swapUpdate,
        liquidityPoolsCalculatedState
      )
      swapCalculatedState = getSwapCalculatedState(state)
      swapValidationsL1 <- swapValidationsL1(swapUpdate)
      allowSpendIsDuplicated = validateIfAllowSpendsAreDuplicated(
        swapUpdate.allowSpendReference,
        swapCalculatedState.getPendingUpdates
      )
    } yield
      swapValidationsL1
        .productR(liquidityPoolExists)
        .productR(poolHaveEnoughTokens)
        .productR(allowSpendIsDuplicated)
  }

  private def validateIfLiquidityPoolExists[F[_]: Async](
    swapUpdate: SwapUpdate,
    currentLiquidityPools: Map[String, LiquidityPool]
  ): F[DataApplicationValidationErrorOr[Unit]] = for {
    poolId <- buildLiquidityPoolUniqueIdentifier(swapUpdate.swapFromPair, swapUpdate.swapToPair)
    result = SwapLiquidityPoolDoesNotExists.unlessA(currentLiquidityPools.contains(poolId.value))
  } yield result

  private def validateIfPoolHaveEnoughTokens[F[_]: Async](
    swapUpdate: SwapUpdate,
    currentLiquidityPools: Map[String, LiquidityPool]
  ): F[DataApplicationValidationErrorOr[Unit]] = {
    val hasEnoughTokens: LiquidityPool => Boolean = lp => {
      val maybeToken = swapUpdate.swapToPair match {
        case None => Option.when(lp.tokenA.identifier.isEmpty)(lp.tokenA).orElse(lp.tokenB.some)
        case Some(value) if lp.tokenA.identifier.contains(value) => lp.tokenA.some
        case Some(value)                                         => Option.when(lp.tokenB.identifier.contains(value))(lp.tokenB)
      }
      maybeToken.exists(_.amount.value > swapUpdate.maxAmount.value.value)
    }

    buildLiquidityPoolUniqueIdentifier(swapUpdate.swapFromPair, swapUpdate.swapToPair)
      .map(poolId => currentLiquidityPools.get(poolId.value))
      .map(maybePool => SwapLiquidityPoolNotEnoughTokens.unlessA(maybePool.exists(hasEnoughTokens)))
  }
}
