package org.amm_metagraph.shared_data.services.pricing

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.{CurrencyId, SwapAmount}
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed

import eu.timepit.refined.types.all.PosLong
import monocle.syntax.all._
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.epochProgress.getFailureExpireEpochProgress
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.LiquidityPool.LiquidityPool
import org.amm_metagraph.shared_data.types.States.FailedCalculatedState
import org.amm_metagraph.shared_data.validations.Errors.ArithmeticError

class RollbackOperations[F[_]: Async](
  config: ApplicationConfig,
  logger: PoolLogger[F]
) {

  def rollbackSwap(
    signedUpdate: Signed[SwapUpdate],
    updateHash: Hash,
    lastSyncGlobalEpochProgress: EpochProgress,
    liquidityPool: LiquidityPool,
    tokenAAmountToReturn: SwapAmount,
    tokenBAmountToReturn: SwapAmount,
    metagraphId: CurrencyId
  ): F[Either[FailedCalculatedState, LiquidityPool]] = {
    val tokenAIsFrom = liquidityPool.tokenA.identifier === signedUpdate.swapFromPair
    val tokenBIsTo = liquidityPool.tokenB.identifier === signedUpdate.swapToPair
    val expireEpochProgress = getFailureExpireEpochProgress(config, lastSyncGlobalEpochProgress)

    def error(msg: String): FailedCalculatedState =
      FailedCalculatedState(ArithmeticError(msg), expireEpochProgress, updateHash, signedUpdate)

    val newTokenAAmountEither: Either[FailedCalculatedState, PosLong] =
      if (tokenAIsFrom)
        (liquidityPool.tokenA.amount.value - tokenAAmountToReturn.value.value).toPosLong
          .leftMap(_ => error("Rolling back token A results in negative balance"))
      else if (tokenBIsTo)
        (liquidityPool.tokenA.amount.value + tokenBAmountToReturn.value.value).toPosLong
          .leftMap(_ => error("Rolling back token A results in invalid addition"))
      else Right(liquidityPool.tokenA.amount)

    val newTokenBAmountEither: Either[FailedCalculatedState, PosLong] =
      if (tokenBIsTo)
        (liquidityPool.tokenB.amount.value + tokenBAmountToReturn.value.value).toPosLong
          .leftMap(_ => error("Rolling back token B results in invalid addition"))
      else if (tokenAIsFrom)
        (liquidityPool.tokenB.amount.value - tokenAAmountToReturn.value.value).toPosLong
          .leftMap(_ => error("Rolling back token B results in negative balance"))
      else Right(liquidityPool.tokenB.amount)

    val result = for {
      newTokenA <- newTokenAAmountEither.map(amount => liquidityPool.tokenA.copy(amount = amount))
      newTokenB <- newTokenBAmountEither.map(amount => liquidityPool.tokenB.copy(amount = amount))

      updatedPool = liquidityPool
        .focus(_.tokenA)
        .replace(newTokenA)
        .focus(_.tokenB)
        .replace(newTokenB)
    } yield updatedPool

    result match {
      case Right(updatedPool) =>
        logger
          .logPoolOperation(
            operation = "SWAP_ROLLBACK",
            beforePool = liquidityPool,
            afterPool = updatedPool,
            epochProgress = Some(lastSyncGlobalEpochProgress),
            updateHash = Some(updateHash),
            additionalInfo = Map(
              "tokenAReturned" -> tokenAAmountToReturn.value.toString,
              "tokenBReturned" -> tokenBAmountToReturn.value.toString,
              "metagraphId" -> metagraphId.toString,
              "swapFromPair" -> signedUpdate.swapFromPair.toString,
              "swapToPair" -> signedUpdate.swapToPair.toString
            )
          )
          .as(Right(updatedPool))
      case Left(error) =>
        Async[F].pure(Left(error))
    }
  }

  def rollbackWithdrawal(
    signedUpdate: Signed[WithdrawalUpdate],
    updateHash: Hash,
    lastSyncGlobalEpochProgress: EpochProgress,
    liquidityPool: LiquidityPool,
    tokenAAmountToReturn: SwapAmount,
    tokenBAmountToReturn: SwapAmount
  ): F[Either[FailedCalculatedState, LiquidityPool]] = {
    val expireEpochProgress = getFailureExpireEpochProgress(config, lastSyncGlobalEpochProgress)

    def error(msg: String): FailedCalculatedState =
      FailedCalculatedState(ArithmeticError(msg), expireEpochProgress, updateHash, signedUpdate)

    val newTokenAAmountEither: Either[FailedCalculatedState, PosLong] =
      (liquidityPool.tokenA.amount.value + tokenAAmountToReturn.value.value).toPosLong
        .leftMap(_ => error("Rolling back token A results in invalid addition"))

    val newTokenBAmountEither: Either[FailedCalculatedState, PosLong] =
      (liquidityPool.tokenB.amount.value + tokenBAmountToReturn.value.value).toPosLong
        .leftMap(_ => error("Rolling back token B results in invalid addition"))

    val result = for {
      newTokenA <- newTokenAAmountEither.map(amount => liquidityPool.tokenA.copy(amount = amount))
      newTokenB <- newTokenBAmountEither.map(amount => liquidityPool.tokenB.copy(amount = amount))
      updatedPool = liquidityPool.copy(tokenA = newTokenA, tokenB = newTokenB)
    } yield updatedPool

    result match {
      case Right(updatedPool) =>
        logger
          .logPoolOperation(
            operation = "WITHDRAWAL_ROLLBACK",
            beforePool = liquidityPool,
            afterPool = updatedPool,
            epochProgress = Some(lastSyncGlobalEpochProgress),
            updateHash = Some(updateHash),
            address = Some(signedUpdate.source),
            additionalInfo = Map(
              "tokenAReturned" -> tokenAAmountToReturn.value.toString,
              "tokenBReturned" -> tokenBAmountToReturn.value.toString,
              "originalSharesWithdrawn" -> signedUpdate.shareToWithdraw.value.toString
            )
          )
          .as(Right(updatedPool))
      case Left(error) =>
        Async[F].pure(Left(error))
    }
  }
}
