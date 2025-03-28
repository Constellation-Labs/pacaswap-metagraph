package org.amm_metagraph.shared_data.validations

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.ext.cats.syntax.next._
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.security.SecurityProvider
import io.constellationnetwork.security.signature.Signed

import eu.timepit.refined.auto._
import eu.timepit.refined.cats.refTypeEq
import org.amm_metagraph.shared_data.types.DataUpdates.SwapUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool.{LiquidityPool, buildLiquidityPoolUniqueIdentifier, getLiquidityPools}
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.Swap.{SwapReference, getSwapCalculatedState}
import org.amm_metagraph.shared_data.validations.Errors._
import org.amm_metagraph.shared_data.validations.SharedValidations._

object SwapValidations {
  def swapValidationsL1[F[_]: Async](
    swapUpdate: SwapUpdate
  ): F[DataApplicationValidationErrorOr[Unit]] = Async[F].delay {
    val tokenIdsAreTheSame = validateIfTokenIdsAreTheSame(swapUpdate.swapFromPair, swapUpdate.swapToPair)
    tokenIdsAreTheSame
  }

  def swapValidationsL0[F[_]: Async: SecurityProvider](
    signedSwapUpdate: Signed[SwapUpdate],
    state: AmmCalculatedState
  ): F[DataApplicationValidationErrorOr[Unit]] = {
    val liquidityPoolsCalculatedState = getLiquidityPools(state)
    val swapUpdate = signedSwapUpdate.value

    for {
      l1Validations <- swapValidationsL1(swapUpdate)
      signatures <- signatureValidations(signedSwapUpdate, signedSwapUpdate.source)
      liquidityPoolExists <- validateIfLiquidityPoolExists(
        swapUpdate,
        liquidityPoolsCalculatedState
      )
      sourceAddress = signedSwapUpdate.source
      poolHaveEnoughTokens <- validateIfPoolHaveEnoughTokens(
        swapUpdate,
        liquidityPoolsCalculatedState
      )
      swapCalculatedState = getSwapCalculatedState(state)
      allowSpendIsDuplicated = validateIfAllowSpendsAreDuplicated(
        swapUpdate.allowSpendReference,
        swapCalculatedState.getPendingUpdates
      )

      lastRef = lastRefValidation(swapCalculatedState, signedSwapUpdate, sourceAddress)
    } yield
      l1Validations
        .productR(signatures)
        .productR(liquidityPoolExists)
        .productR(poolHaveEnoughTokens)
        .productR(allowSpendIsDuplicated)
        .productR(lastRef)
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
  private def lastRefValidation(
    swapCalculatedState: SwapCalculatedState,
    signedSwap: Signed[SwapUpdate],
    address: Address
  ): DataApplicationValidationErrorOr[Unit] = {
    val lastConfirmed: Option[SwapReference] = swapCalculatedState.confirmed.value
      .get(address)
      .flatMap(_.maxByOption(_.parent.ordinal))
      .map(_.parent)

    lastConfirmed match {
      case Some(last) if signedSwap.ordinal.value =!= last.ordinal.next.value || signedSwap.parent =!= last =>
        InvalidSwapParent.invalid
      case _ => valid
    }
  }
}
