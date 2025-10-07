package org.amm_metagraph.shared_data.services.pricing

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.{CurrencyId, SwapAmount}
import io.constellationnetwork.security.Hashed
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.{NonNegLong, PosLong}
import monocle.syntax.all._
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.epochProgress.getFailureExpireEpochProgress
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.validations.Errors._

class LiquidityPoolOperations[F[_]: Async](
  config: ApplicationConfig,
  logger: PoolLogger[F]
) {

  def updatePoolForStaking(
    liquidityPool: LiquidityPool,
    signedUpdate: Signed[StakingUpdate],
    updateHash: Hash,
    signerAddress: Address,
    stakingTokenInformation: StakingTokenInfo,
    lastSyncGlobalEpochProgress: EpochProgress,
    currencyOrdinal: SnapshotOrdinal
  ): F[Either[FailedCalculatedState, LiquidityPool]] = {
    val primaryToken = stakingTokenInformation.primaryTokenInformation
    val pairToken = stakingTokenInformation.pairTokenInformation
    val newlyIssuedShares = stakingTokenInformation.newlyIssuedShares
    val expireEpochProgress = getFailureExpireEpochProgress(config, lastSyncGlobalEpochProgress)

    val tokenA = if (liquidityPool.tokenA.identifier == primaryToken.identifier) primaryToken else pairToken
    val tokenB = if (liquidityPool.tokenB.identifier == pairToken.identifier) pairToken else primaryToken
    val tokenAValue = liquidityPool.tokenA.amount.value + tokenA.amount.value
    val tokenBValue = liquidityPool.tokenB.amount.value + tokenB.amount.value

    val result = for {
      updatedTokenAAmount <- tokenAValue.toPosLong
        .leftMap(_ =>
          FailedCalculatedState(
            ArithmeticError(s"Updated token A amount $tokenAValue is not positive"),
            expireEpochProgress,
            updateHash,
            signedUpdate
          )
        )
      updatedTokenBAmount <- tokenBValue.toPosLong
        .leftMap(_ =>
          FailedCalculatedState(
            ArithmeticError(s"Updated token B amount $tokenBValue is not positive"),
            expireEpochProgress,
            updateHash,
            signedUpdate
          )
        )
      addressSharesAmount = liquidityPool.poolShares.addressShares.getOrElse(signerAddress, ShareAmount(Amount.empty))
      updatedAddressSharesAmount =
        ShareAmount(Amount(NonNegLong.unsafeFrom(addressSharesAmount.value.value.value + newlyIssuedShares)))
      updatedAddressShares = liquidityPool.poolShares.addressShares.updated(signerAddress, updatedAddressSharesAmount)
      poolShares <- (liquidityPool.poolShares.totalShares.value + newlyIssuedShares).toPosLong
        .leftMap(_ =>
          FailedCalculatedState(
            ArithmeticError(s"poolShares $tokenBValue is not positive"),
            expireEpochProgress,
            updateHash,
            signedUpdate
          )
        )

      updatedPool = liquidityPool
        .focus(_.tokenA)
        .modify(_.focus(_.amount).replace(updatedTokenAAmount))
        .focus(_.tokenB)
        .modify(_.focus(_.amount).replace(updatedTokenBAmount))
        .focus(_.k)
        .replace(BigInt(updatedTokenAAmount.value) * BigInt(updatedTokenBAmount.value))
        .focus(_.poolShares)
        .replace(
          PoolShares(
            poolShares,
            updatedAddressShares
          )
        )
    } yield updatedPool

    result match {
      case Right(updatedPool) =>
        logger
          .logPoolOperation(
            operation = "STAKING",
            beforePool = liquidityPool,
            afterPool = updatedPool,
            epochProgress = Some(lastSyncGlobalEpochProgress),
            updateHash = Some(updateHash),
            address = Some(signerAddress),
            additionalInfo = Map(
              "newlyIssuedShares" -> newlyIssuedShares.toString,
              "primaryTokenAmount" -> primaryToken.amount.value.toString,
              "pairTokenAmount" -> pairToken.amount.value.toString,
              "currencyOrdinal" -> currencyOrdinal.show
            )
          )
          .as(Right(updatedPool))
      case Left(error) =>
        Async[F].pure(Left(error))
    }
  }

  def updatePoolForSwap(
    hashedSwapUpdate: Hashed[SwapUpdate],
    liquidityPool: LiquidityPool,
    swapTokenInfo: SwapTokenInfo,
    metagraphId: CurrencyId,
    lastSyncGlobalEpochProgress: EpochProgress,
    currencyOrdinal: SnapshotOrdinal
  ): F[Either[FailedCalculatedState, LiquidityPool]] = {
    val expireEpochProgress = getFailureExpireEpochProgress(config, lastSyncGlobalEpochProgress)
    val swapUpdate = hashedSwapUpdate.signed.value
    val fromTokenInfo = swapUpdate.swapFromPair
    val toTokenInfo = swapUpdate.swapToPair

    val (tokenA, tokenB) = if (liquidityPool.tokenA.identifier === fromTokenInfo) {
      (liquidityPool.tokenA, liquidityPool.tokenB)
    } else {
      (liquidityPool.tokenB, liquidityPool.tokenA)
    }

    val result = for {
      newTokenAAmount <- PosLong
        .from(tokenA.amount.value + swapTokenInfo.amount.value.value)
        .leftMap(_ =>
          FailedCalculatedState(
            ArithmeticError(s"Token A amount overflow: ${tokenA.amount.value} + ${swapTokenInfo.amount.value.value}"),
            expireEpochProgress,
            hashedSwapUpdate.hash,
            hashedSwapUpdate.signed
          )
        )

      newTokenBAmount <- PosLong
        .from(tokenB.amount.value - swapTokenInfo.netReceived.value.value)
        .leftMap(_ =>
          FailedCalculatedState(
            ArithmeticError(s"Token B amount underflow or negative: ${tokenB.amount.value} - ${swapTokenInfo.netReceived.value.value}"),
            expireEpochProgress,
            hashedSwapUpdate.hash,
            hashedSwapUpdate.signed
          )
        )

      updatedTokenA = tokenA.copy(amount = newTokenAAmount)
      updatedTokenB = tokenB.copy(amount = newTokenBAmount)

      k = BigInt(updatedTokenA.amount.value) * BigInt(updatedTokenB.amount.value)

      updatedPool =
        if (liquidityPool.tokenA.identifier === fromTokenInfo) {
          liquidityPool
            .focus(_.tokenA)
            .replace(updatedTokenA)
            .focus(_.tokenB)
            .replace(updatedTokenB)
            .focus(_.k)
            .replace(k)
        } else {
          liquidityPool
            .focus(_.tokenA)
            .replace(updatedTokenB)
            .focus(_.tokenB)
            .replace(updatedTokenA)
            .focus(_.k)
            .replace(k)
        }
    } yield updatedPool

    result match {
      case Right(pool) =>
        logger
          .logPoolOperation(
            operation = "SWAP",
            beforePool = liquidityPool,
            afterPool = pool,
            updateHash = Some(hashedSwapUpdate.hash),
            additionalInfo = Map(
              "fromToken" -> fromTokenInfo.toString,
              "toToken" -> toTokenInfo.toString,
              "amountIn" -> swapTokenInfo.amount.value.toString,
              "grossReceived" -> swapTokenInfo.grossReceived.value.toString,
              "netReceived" -> swapTokenInfo.netReceived.value.toString,
              "metagraphId" -> metagraphId.toString,
              "currencyOrdinal" -> currencyOrdinal.show,
              "kBefore" -> liquidityPool.k.toString,
              "kAfter" -> pool.k.toString
            )
          )
          .as(Right(pool))

      case Left(error) => error.asLeft[LiquidityPool].pure
    }
  }

  def updatePoolForWithdrawal(
    signedUpdate: Signed[WithdrawalUpdate],
    updateHash: Hash,
    liquidityPool: LiquidityPool,
    withdrawalAmounts: WithdrawalTokenInfo,
    lastSyncGlobalEpochProgress: EpochProgress,
    currencyOrdinal: SnapshotOrdinal
  ): F[Either[FailedCalculatedState, LiquidityPool]] = {
    val tokenAValue = liquidityPool.tokenA.amount.value - withdrawalAmounts.tokenAAmount.value
    val tokenBValue = liquidityPool.tokenB.amount.value - withdrawalAmounts.tokenBAmount.value
    val totalSharesValue = liquidityPool.poolShares.totalShares.value - signedUpdate.shareToWithdraw.value.value
    val expireEpochProgress = getFailureExpireEpochProgress(config, lastSyncGlobalEpochProgress)

    val result = for {
      tokenAAmount <- PosLong
        .from(tokenAValue)
        .leftMap(_ =>
          FailedCalculatedState(
            ArithmeticError(s"Updated token A amount $tokenAValue is not positive"),
            expireEpochProgress,
            updateHash,
            signedUpdate
          )
        )
      tokenBAmount <- PosLong
        .from(tokenBValue)
        .leftMap(_ =>
          FailedCalculatedState(
            ArithmeticError(s"Updated token B amount $tokenBValue is not positive"),
            expireEpochProgress,
            updateHash,
            signedUpdate
          )
        )
      totalSharesAmount <- PosLong
        .from(totalSharesValue)
        .leftMap(_ =>
          FailedCalculatedState(
            ArithmeticError(s"Updated total shares $totalSharesValue is not positive"),
            expireEpochProgress,
            updateHash,
            signedUpdate
          )
        )
      addressSharesAmount = liquidityPool.poolShares.addressShares.getOrElse(signedUpdate.source, ShareAmount(Amount.empty))
      sharesDifference = addressSharesAmount.value.value - signedUpdate.shareToWithdraw.value.value
      updatedAddressSharesAmount <- NonNegLong
        .from(sharesDifference)
        .leftMap(_ =>
          FailedCalculatedState(
            ArithmeticError(s"Updated address shares amount $sharesDifference is negative"),
            expireEpochProgress,
            updateHash,
            signedUpdate
          )
        )
        .map(nonNeg => ShareAmount(Amount(nonNeg)))
      updatedAddressShares =
        if (updatedAddressSharesAmount.value === Amount.empty) {
          liquidityPool.poolShares.addressShares - signedUpdate.source
        } else {
          liquidityPool.poolShares.addressShares.updated(signedUpdate.source, updatedAddressSharesAmount)
        }

      k = BigInt(tokenAAmount.value) * BigInt(tokenBAmount.value)

      updatedPool = liquidityPool.copy(
        tokenA = liquidityPool.tokenA.copy(amount = tokenAAmount),
        tokenB = liquidityPool.tokenB.copy(amount = tokenBAmount),
        k = k,
        poolShares = PoolShares(
          totalSharesAmount,
          updatedAddressShares
        )
      )
    } yield (updatedPool, totalSharesAmount) // Return both values as a tuple

    result match {
      case Right((updatedPool, totalShares)) =>
        logger
          .logPoolOperation(
            operation = "WITHDRAWAL",
            beforePool = liquidityPool,
            afterPool = updatedPool,
            epochProgress = Some(lastSyncGlobalEpochProgress),
            updateHash = Some(updateHash),
            address = Some(signedUpdate.source),
            additionalInfo = Map(
              "sharesToWithdraw" -> signedUpdate.shareToWithdraw.value.toString,
              "tokenAWithdrawn" -> withdrawalAmounts.tokenAAmount.value.toString,
              "tokenBWithdrawn" -> withdrawalAmounts.tokenBAmount.value.toString,
              "remainingShares" -> totalShares.value.toString,
              "currencyOrdinal" -> currencyOrdinal.show
            )
          )
          .as(Right(updatedPool))
      case Left(error) =>
        Async[F].pure(Left(error))
    }
  }

  def calculateWithdrawalAmounts(
    signedUpdate: Signed[WithdrawalUpdate],
    updateHash: Hash,
    liquidityPool: LiquidityPool,
    lastSyncGlobalEpochProgress: EpochProgress
  ): Either[FailedCalculatedState, WithdrawalTokenInfo] = {
    val expireEpochProgress = getFailureExpireEpochProgress(config, lastSyncGlobalEpochProgress)
    val PRECISION = 8
    val SCALING_FACTOR = BigInt(10).pow(PRECISION)

    for {
      _ <- liquidityPool.poolShares.addressShares
        .get(signedUpdate.source)
        .filter(signedUpdate.shareToWithdraw.value <= _.value)
        .toRight(
          FailedCalculatedState(
            WithdrawalAmountExceedsAvailableShares(signedUpdate.shareToWithdraw),
            expireEpochProgress,
            updateHash,
            signedUpdate
          )
        )

      _ <- Either.cond(
        signedUpdate.shareToWithdraw.value.value < liquidityPool.poolShares.totalShares.value,
        (),
        FailedCalculatedState(
          CannotWithdrawAllShares(),
          expireEpochProgress,
          updateHash,
          signedUpdate
        )
      )
      totalShares = BigInt(liquidityPool.poolShares.totalShares.value)
      sharesBigInt = BigInt(signedUpdate.shareToWithdraw.value.value)
      tokenAOut = (BigInt(liquidityPool.tokenA.amount.value) * sharesBigInt * SCALING_FACTOR / totalShares) / SCALING_FACTOR
      tokenBOut = (BigInt(liquidityPool.tokenB.amount.value) * sharesBigInt * SCALING_FACTOR / totalShares) / SCALING_FACTOR

      tokenAAmount <- PosLong
        .from(tokenAOut.toLong)
        .leftMap(_ =>
          FailedCalculatedState(
            ArithmeticError(s"Token A amount doesn't match PosLong: $tokenAOut"),
            expireEpochProgress,
            updateHash,
            signedUpdate
          )
        )
      tokenBAmount <- PosLong
        .from(tokenBOut.toLong)
        .leftMap(_ =>
          FailedCalculatedState(
            ArithmeticError(s"Token B amount doesn't match PosLong: $tokenBOut"),
            expireEpochProgress,
            updateHash,
            signedUpdate
          )
        )

      _ <- Either.cond(
        tokenAAmount <= liquidityPool.tokenA.amount.value,
        (),
        FailedCalculatedState(
          TokenExceedsAvailableAmount(liquidityPool.tokenA.identifier, liquidityPool.tokenA.amount.value, tokenAAmount.value),
          expireEpochProgress,
          updateHash,
          signedUpdate
        )
      )
      _ <- Either.cond(
        tokenBAmount <= liquidityPool.tokenB.amount.value,
        (),
        FailedCalculatedState(
          TokenExceedsAvailableAmount(liquidityPool.tokenB.identifier, liquidityPool.tokenB.amount.value, tokenBAmount.value),
          expireEpochProgress,
          updateHash,
          signedUpdate
        )
      )
    } yield
      WithdrawalTokenInfo(
        liquidityPool.tokenA.identifier,
        SwapAmount(tokenAAmount),
        liquidityPool.tokenB.identifier,
        SwapAmount(tokenBAmount)
      )
  }

  def calculateStakingInfo(
    signedUpdate: Signed[StakingUpdate],
    updateHash: Hash,
    liquidityPool: LiquidityPool,
    lastSyncGlobalEpochProgress: EpochProgress
  ): Either[FailedCalculatedState, StakingTokenInfo] = {
    val stakingUpdate = signedUpdate.value
    val expireEpochProgress = getFailureExpireEpochProgress(config, lastSyncGlobalEpochProgress)

    val (primaryToken, pairToken) = if (stakingUpdate.tokenAId === liquidityPool.tokenA.identifier) {
      (liquidityPool.tokenA, liquidityPool.tokenB)
    } else {
      (liquidityPool.tokenB, liquidityPool.tokenA)
    }

    val currentPrimaryTokenAmount = primaryToken.amount.value
    val currentPairTokenAmount = pairToken.amount.value

    val incomingPrimaryAmount = stakingUpdate.tokenAAmount.value
    val incomingPairAmount =
      (BigDecimal(incomingPrimaryAmount) * BigDecimal(currentPairTokenAmount)) / BigDecimal(
        currentPrimaryTokenAmount
      )

    val relativeDepositIncrease = incomingPrimaryAmount.toDouble / currentPrimaryTokenAmount
    val newlyIssuedShares = relativeDepositIncrease * liquidityPool.poolShares.totalShares.value

    Right(
      StakingTokenInfo(
        primaryToken.copy(amount = incomingPrimaryAmount.toPosLongUnsafe),
        pairToken.copy(amount = incomingPairAmount.toLong.toPosLongUnsafe),
        SwapAmount(incomingPairAmount.toLong.toPosLongUnsafe),
        newlyIssuedShares.toLong
      )
    )
  }
}
