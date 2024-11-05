package org.amm_metagraph.shared_data.validations

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.L0NodeContext
import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.security.Hasher

import org.amm_metagraph.shared_data.Utils.{buildLiquidityPoolUniqueIdentifier, getAllowSpendLastSyncGlobalSnapshotState}
import org.amm_metagraph.shared_data.types.DataUpdates.SwapUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool.{LiquidityPool, getLiquidityPools}
import org.amm_metagraph.shared_data.types.States.AmmCalculatedState
import org.amm_metagraph.shared_data.validations.Errors._

object SwapValidations {
  def swapValidationsL1[F[_]: Async](
    stakingUpdate: SwapUpdate
  ): F[DataApplicationValidationErrorOr[Unit]] = Async[F].delay {
    valid
  }

  def swapValidationsL0[F[_]: Async: Hasher](
    swapUpdate: SwapUpdate,
    state: AmmCalculatedState
  )(implicit context: L0NodeContext[F]): F[DataApplicationValidationErrorOr[Unit]] = {
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
      validAllowSpend <- validateSwapAllowSpend(
        swapUpdate
      )
      swapValidationsL1 <- swapValidationsL1(swapUpdate)
      result = swapValidationsL1.productR(liquidityPoolExists).productR(poolHaveEnoughTokens).productR(validAllowSpend)
    } yield result
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

  private def validateSwapAllowSpend[F[_]: Async: Hasher](
    swapUpdate: SwapUpdate
  )(implicit context: L0NodeContext[F]): F[DataApplicationValidationErrorOr[Unit]] = for {
    swapAllowSpend <- getAllowSpendLastSyncGlobalSnapshotState(
      swapUpdate.allowSpendReference
    )
  } yield
    if (swapAllowSpend.isEmpty) {
      SwapMissingAllowSpend.invalid
    } else if (swapAllowSpend.get.currency != swapUpdate.swapFromPair) {
      SwapAllowSpendDifferentCurrency.invalid
    } else if (swapAllowSpend.get.source != swapUpdate.sourceAddress) {
      SwapAllowSpendDifferentSourceAddress.invalid
    } else {
      valid
    }
}
