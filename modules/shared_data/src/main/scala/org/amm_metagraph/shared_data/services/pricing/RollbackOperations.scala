package org.amm_metagraph.shared_data.services.pricing

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.{CurrencyId, SwapAmount}
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed

import eu.timepit.refined.types.all.PosLong
import monocle.syntax.all._
import org.amm_metagraph.shared_data.ProtocolActivation
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
    metagraphId: CurrencyId,
    currencyOrdinal: SnapshotOrdinal
  ): F[Either[FailedCalculatedState, LiquidityPool]] = {
    val expireEpochProgress = getFailureExpireEpochProgress(config, lastSyncGlobalEpochProgress)

    def error(msg: String): FailedCalculatedState =
      FailedCalculatedState(ArithmeticError(msg), expireEpochProgress, updateHash, signedUpdate)

    val (newTokenAAmountEither, newTokenBAmountEither) =
      if (ProtocolActivation.rollbackDirectionFixActive(currencyOrdinal))
        directedRollbackAmounts(signedUpdate, liquidityPool, tokenAAmountToReturn, tokenBAmountToReturn, error)
      else
        legacyRollbackAmounts(signedUpdate, liquidityPool, tokenAAmountToReturn, tokenBAmountToReturn, error)

    val result = for {
      newTokenA <- newTokenAAmountEither.map(amount => liquidityPool.tokenA.copy(amount = amount))
      newTokenB <- newTokenBAmountEither.map(amount => liquidityPool.tokenB.copy(amount = amount))

      updatedPool = liquidityPool
        .focus(_.tokenA)
        .replace(newTokenA)
        .focus(_.tokenB)
        .replace(newTokenB)
      // Every other pool mutation recomputes k; the rollbacks did not, so k stayed frozen at
      // the post-operation product while A and B reverted. Gated so history replays as recorded.
      withK =
        if (ProtocolActivation.reserveAccountingFixesActive(currencyOrdinal))
          updatedPool.focus(_.k).replace(BigInt(newTokenA.amount.value) * BigInt(newTokenB.amount.value))
        else updatedPool
    } yield withK

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
              "swapToPair" -> signedUpdate.swapToPair.toString,
              "currencyOrdinal" -> currencyOrdinal.show
            )
          )
          .as(Right(updatedPool))
      case Left(error) =>
        Async[F].pure(Left(error))
    }
  }

  /** The forward swap credits the sold side with `amountIn` and debits the bought side with `netReceived`, whichever pool side each is. The
    * rollback is that mirror: match each side of the pool against the update's pair and reverse it. A pool that matches neither side cannot
    * be rolled back and must say so rather than hand back the unchanged pool as a success.
    */
  private def directedRollbackAmounts(
    signedUpdate: Signed[SwapUpdate],
    liquidityPool: LiquidityPool,
    amountIn: SwapAmount,
    netReceived: SwapAmount,
    error: String => FailedCalculatedState
  ): (Either[FailedCalculatedState, PosLong], Either[FailedCalculatedState, PosLong]) = {
    def reverse(side: String, current: PosLong, identifier: Option[CurrencyId]): Either[FailedCalculatedState, PosLong] =
      if (identifier === signedUpdate.swapFromPair)
        (current.value - amountIn.value.value).toPosLong
          .leftMap(_ => error(s"Rolling back token $side results in negative balance"))
      else if (identifier === signedUpdate.swapToPair)
        (current.value + netReceived.value.value).toPosLong
          .leftMap(_ => error(s"Rolling back token $side results in invalid addition"))
      else
        Left(error(s"Token $side of the pool is neither side of the swap being rolled back"))

    (
      reverse("A", liquidityPool.tokenA.amount, liquidityPool.tokenA.identifier),
      reverse("B", liquidityPool.tokenB.amount, liquidityPool.tokenB.identifier)
    )
  }

  /** Pre-activation behaviour, kept exactly so history replays: only a token A -> token B swap is reversed; any other direction leaves both
    * reserves untouched. See ProtocolActivation.rollbackDirectionFix.
    */
  private def legacyRollbackAmounts(
    signedUpdate: Signed[SwapUpdate],
    liquidityPool: LiquidityPool,
    tokenAAmountToReturn: SwapAmount,
    tokenBAmountToReturn: SwapAmount,
    error: String => FailedCalculatedState
  ): (Either[FailedCalculatedState, PosLong], Either[FailedCalculatedState, PosLong]) = {
    val tokenAIsFrom = liquidityPool.tokenA.identifier === signedUpdate.swapFromPair
    val tokenBIsTo = liquidityPool.tokenB.identifier === signedUpdate.swapToPair

    val newTokenA: Either[FailedCalculatedState, PosLong] =
      if (tokenAIsFrom)
        (liquidityPool.tokenA.amount.value - tokenAAmountToReturn.value.value).toPosLong
          .leftMap(_ => error("Rolling back token A results in negative balance"))
      else if (tokenBIsTo)
        (liquidityPool.tokenA.amount.value + tokenBAmountToReturn.value.value).toPosLong
          .leftMap(_ => error("Rolling back token A results in invalid addition"))
      else Right(liquidityPool.tokenA.amount)

    val newTokenB: Either[FailedCalculatedState, PosLong] =
      if (tokenBIsTo)
        (liquidityPool.tokenB.amount.value + tokenBAmountToReturn.value.value).toPosLong
          .leftMap(_ => error("Rolling back token B results in invalid addition"))
      else if (tokenAIsFrom)
        (liquidityPool.tokenB.amount.value - tokenAAmountToReturn.value.value).toPosLong
          .leftMap(_ => error("Rolling back token B results in negative balance"))
      else Right(liquidityPool.tokenB.amount)

    (newTokenA, newTokenB)
  }

  def rollbackWithdrawal(
    signedUpdate: Signed[WithdrawalUpdate],
    updateHash: Hash,
    lastSyncGlobalEpochProgress: EpochProgress,
    liquidityPool: LiquidityPool,
    tokenAAmountToReturn: SwapAmount,
    tokenBAmountToReturn: SwapAmount,
    currencyOrdinal: SnapshotOrdinal
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
      base = liquidityPool.copy(tokenA = newTokenA, tokenB = newTokenB)
      updatedPool =
        if (ProtocolActivation.reserveAccountingFixesActive(currencyOrdinal))
          base.copy(k = BigInt(newTokenA.amount.value) * BigInt(newTokenB.amount.value))
        else base
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
              "originalSharesWithdrawn" -> signedUpdate.shareToWithdraw.value.toString,
              "currencyOrdinal" -> currencyOrdinal.show
            )
          )
          .as(Right(updatedPool))
      case Left(error) =>
        Async[F].pure(Left(error))
    }
  }
}
