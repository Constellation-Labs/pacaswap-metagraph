package org.amm_metagraph.shared_data.services.pricing

import java.nio.file.Paths

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

import eu.timepit.refined.types.all.PosLong
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.calculated_state.CalculatedStateService
import org.amm_metagraph.shared_data.refined.{BigDecimalOps, Percentage}
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.Swap.{ReverseSwapQuote, SwapQuote}
import org.amm_metagraph.shared_data.validations.Errors._
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

trait PricingService[F[_]] {
  def getSwapQuote(
    fromTokenId: Option[CurrencyId],
    toTokenId: Option[CurrencyId],
    amount: Amount,
    slippagePercent: Percentage
  ): F[Either[String, SwapQuote]]

  def getReverseSwapQuote(
    fromTokenId: Option[CurrencyId],
    toTokenId: Option[CurrencyId],
    desiredOutputAmount: Amount,
    slippagePercent: Percentage
  ): F[Either[String, ReverseSwapQuote]]

  def getLiquidityPoolPrices(poolId: PoolId): F[Either[String, (Long, Long)]]

  def getSwapTokenInfo(
    signedUpdate: Signed[SwapUpdate],
    updateHash: Hash,
    poolId: PoolId,
    lastSyncGlobalEpochProgress: EpochProgress
  ): F[Either[FailedCalculatedState, SwapTokenInfo]]

  def getStakingTokenInfo(
    signedUpdate: Signed[StakingUpdate],
    updateHash: Hash,
    poolId: PoolId,
    lastSyncGlobalEpochProgress: EpochProgress
  ): F[Either[FailedCalculatedState, StakingTokenInfo]]

  def getUpdatedLiquidityPoolDueStaking(
    liquidityPool: LiquidityPool,
    signedUpdate: Signed[StakingUpdate],
    updateHash: Hash,
    signerAddress: Address,
    stakingTokenInformation: StakingTokenInfo,
    lastSyncGlobalEpochProgress: EpochProgress,
    currencyOrdinal: SnapshotOrdinal
  ): F[Either[FailedCalculatedState, LiquidityPool]]

  def getUpdatedLiquidityPoolDueNewSwap(
    hashedSwapUpdate: Hashed[SwapUpdate],
    liquidityPool: LiquidityPool,
    swapTokenInfo: SwapTokenInfo,
    metagraphId: CurrencyId,
    lastSyncGlobalEpochProgress: EpochProgress,
    currencyOrdinal: SnapshotOrdinal
  ): F[Either[FailedCalculatedState, LiquidityPool]]

  def getUpdatedLiquidityPoolDueNewWithdrawal(
    signedUpdate: Signed[WithdrawalUpdate],
    updateHash: Hash,
    liquidityPool: LiquidityPool,
    withdrawalAmounts: WithdrawalTokenInfo,
    lastSyncGlobalEpochProgress: EpochProgress,
    currencyOrdinal: SnapshotOrdinal
  ): F[Either[FailedCalculatedState, LiquidityPool]]

  def getWithdrawalTokenInfo(
    signedUpdate: Signed[WithdrawalUpdate],
    updateHash: Hash,
    liquidityPool: LiquidityPool,
    lastSyncGlobalEpochProgress: EpochProgress
  ): Either[FailedCalculatedState, WithdrawalTokenInfo]

  def rollbackSwapLiquidityPoolAmounts(
    signedUpdate: Signed[SwapUpdate],
    updateHash: Hash,
    lastSyncGlobalEpochProgress: EpochProgress,
    liquidityPool: LiquidityPool,
    tokenAAmountToReturn: SwapAmount,
    tokenBAmountToReturn: SwapAmount,
    metagraphId: CurrencyId,
    currencyOrdinal: SnapshotOrdinal
  ): F[Either[FailedCalculatedState, LiquidityPool]]

  def rollbackWithdrawalLiquidityPoolAmounts(
    signedUpdate: Signed[WithdrawalUpdate],
    updateHash: Hash,
    lastSyncGlobalEpochProgress: EpochProgress,
    liquidityPool: LiquidityPool,
    tokenAAmountToReturn: SwapAmount,
    tokenBAmountToReturn: SwapAmount,
    currencyOrdinal: SnapshotOrdinal
  ): F[Either[FailedCalculatedState, LiquidityPool]]
}

object PricingService {
  def make[F[_]: Async](
    applicationConfig: ApplicationConfig,
    calculatedStateService: CalculatedStateService[F]
  ): F[PricingService[F]] = {
    val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

    val currentPath = Paths.get("").toAbsolutePath.toString
    val logFilePath = s"$currentPath/pools-balances-updates.log"

    for {
      _ <- logger.info(s"Initializing with log file at: $logFilePath")
      _ <- logger.info(s"[PricingService] Current working directory: $currentPath")
      liquidityPoolLogger <- PoolLogger.make(logFilePath)
      service <- make(applicationConfig, calculatedStateService, liquidityPoolLogger)
    } yield service
  }

  def make[F[_]: Async](
    applicationConfig: ApplicationConfig,
    calculatedStateService: CalculatedStateService[F],
    logger: PoolLogger[F]
  ): F[PricingService[F]] = {
    val poolOps = new LiquidityPoolOperations[F](applicationConfig, logger)
    val rollbackOps = new RollbackOperations[F](applicationConfig, logger)

    Async[F].delay {
      new PricingService[F] {
        private def getConfirmedLiquidityPools =
          calculatedStateService.get.map(calculatedState => getLiquidityPoolCalculatedState(calculatedState.state))

        override def getSwapQuote(
          fromTokenId: Option[CurrencyId],
          toTokenId: Option[CurrencyId],
          amount: Amount,
          slippagePercent: Percentage
        ): F[Either[String, SwapQuote]] = for {
          liquidityPools <- getConfirmedLiquidityPools
          poolIdEither <- buildLiquidityPoolUniqueIdentifier(fromTokenId, toTokenId).attempt
          result <- poolIdEither match {
            case Right(poolId) =>
              getLiquidityPoolByPoolId(liquidityPools.confirmed.value, poolId).attempt.map {
                case Right(pool) =>
                  SwapCalculations.buildSwapQuote(pool, fromTokenId, toTokenId, amount, slippagePercent)
                case Left(_) => Left("Liquidity pool does not exist")
              }
            case Left(e) => Async[F].pure(Left(s"Failed to build pool identifier: ${e.getMessage}"))
          }
        } yield result

        def getReverseSwapQuote(
          fromTokenId: Option[CurrencyId],
          toTokenId: Option[CurrencyId],
          desiredOutputAmount: Amount,
          slippagePercent: Percentage
        ): F[Either[String, ReverseSwapQuote]] = for {
          liquidityPools <- getConfirmedLiquidityPools
          poolIdEither <- buildLiquidityPoolUniqueIdentifier(fromTokenId, toTokenId).attempt
          result <- poolIdEither match {
            case Right(poolId) =>
              getLiquidityPoolByPoolId(liquidityPools.confirmed.value, poolId).attempt.map {
                case Right(pool) =>
                  SwapCalculations.buildReverseSwapQuote(pool, fromTokenId, toTokenId, desiredOutputAmount, slippagePercent)
                case Left(_) => Left("Liquidity pool does not exist")
              }
            case Left(e) => Async[F].pure(Left(s"Failed to build pool identifier: ${e.getMessage}"))
          }
        } yield result

        override def getLiquidityPoolPrices(poolId: PoolId): F[Either[String, (Long, Long)]] = for {
          liquidityPools <- getConfirmedLiquidityPools
          result <- getLiquidityPoolByPoolId(liquidityPools.confirmed.value, poolId).attempt.map {
            case Right(liquidityPool) =>
              val tokenAAmount = BigDecimal(liquidityPool.tokenA.amount.value)
              val tokenBAmount = BigDecimal(liquidityPool.tokenB.amount.value)

              val priceTokenA = if (tokenAAmount > 0) {
                (tokenBAmount / tokenAAmount).halfUp.toTokenAmountFormat
              } else 0L

              val priceTokenB = if (tokenBAmount > 0) {
                (tokenAAmount / tokenBAmount).halfUp.toTokenAmountFormat
              } else 0L

              Right((priceTokenA, priceTokenB))
            case Left(_) => Left("Liquidity pool does not exist")
          }
        } yield result

        override def getSwapTokenInfo(
          signedUpdate: Signed[SwapUpdate],
          updateHash: Hash,
          poolId: PoolId,
          lastSyncGlobalEpochProgress: EpochProgress
        ): F[Either[FailedCalculatedState, SwapTokenInfo]] = for {
          liquidityPools <- getConfirmedLiquidityPools
          swapUpdate = signedUpdate.value
          expireEpochProgress = org.amm_metagraph.shared_data.epochProgress.getFailureExpireEpochProgress(
            applicationConfig,
            lastSyncGlobalEpochProgress
          )
          result <- getLiquidityPoolByPoolId(liquidityPools.confirmed.value, poolId).attempt.map {
            case Left(_) =>
              Left(
                FailedCalculatedState(
                  InvalidLiquidityPool(),
                  expireEpochProgress,
                  updateHash,
                  signedUpdate
                )
              )
            case Right(liquidityPool) =>
              val calculationResult = SwapCalculations.calculateReservesAndReceived(
                liquidityPool,
                swapUpdate.swapFromPair,
                swapUpdate.amountIn
              )

              calculationResult match {
                case Right(reservesAndReceived) =>
                  val fromTokenInfo =
                    if (swapUpdate.swapFromPair === liquidityPool.tokenA.identifier) liquidityPool.tokenA
                    else liquidityPool.tokenB

                  val toTokenInfo =
                    if (swapUpdate.swapToPair === liquidityPool.tokenA.identifier) liquidityPool.tokenA
                    else liquidityPool.tokenB

                  val swapTokenInfo = SwapTokenInfo(
                    fromTokenInfo.copy(amount = PosLong.unsafeFrom(reservesAndReceived.updatedInputReserve.toLong)),
                    toTokenInfo.copy(amount = PosLong.unsafeFrom(reservesAndReceived.updatedOutputReserve.toLong)),
                    SwapAmount(PosLong.unsafeFrom(reservesAndReceived.swapAmount.toLong)),
                    SwapAmount(PosLong.unsafeFrom(reservesAndReceived.estimatedReceivedBeforeFee.toLong)),
                    SwapAmount(PosLong.unsafeFrom(reservesAndReceived.estimatedReceivedAfterFee.toLong))
                  )
                  Right(swapTokenInfo)
                case Left(errorMsg) =>
                  Left(
                    FailedCalculatedState(
                      InvalidSwapTokenInfo(errorMsg),
                      expireEpochProgress,
                      updateHash,
                      signedUpdate
                    )
                  )
              }
          }.handleErrorWith { _ =>
            Async[F].pure(
              Left(
                FailedCalculatedState(
                  InvalidLiquidityPool(),
                  expireEpochProgress,
                  updateHash,
                  signedUpdate
                )
              )
            )
          }
        } yield result

        def getStakingTokenInfo(
          signedUpdate: Signed[StakingUpdate],
          updateHash: Hash,
          poolId: PoolId,
          lastSyncGlobalEpochProgress: EpochProgress
        ): F[Either[FailedCalculatedState, StakingTokenInfo]] = for {
          liquidityPools <- getConfirmedLiquidityPools
          expireEpochProgress = org.amm_metagraph.shared_data.epochProgress.getFailureExpireEpochProgress(
            applicationConfig,
            lastSyncGlobalEpochProgress
          )
          result <- getLiquidityPoolByPoolId(liquidityPools.confirmed.value, poolId).attempt.map {
            case Left(_) =>
              Left(
                FailedCalculatedState(
                  InvalidLiquidityPool(),
                  expireEpochProgress,
                  updateHash,
                  signedUpdate
                )
              )
            case Right(liquidityPool) =>
              poolOps.calculateStakingInfo(signedUpdate, updateHash, liquidityPool, lastSyncGlobalEpochProgress)
          }
        } yield result

        def getUpdatedLiquidityPoolDueStaking(
          liquidityPool: LiquidityPool,
          signedUpdate: Signed[StakingUpdate],
          updateHash: Hash,
          signerAddress: Address,
          stakingTokenInformation: StakingTokenInfo,
          lastSyncGlobalEpochProgress: EpochProgress,
          currencyOrdinal: SnapshotOrdinal
        ): F[Either[FailedCalculatedState, LiquidityPool]] =
          poolOps.updatePoolForStaking(
            liquidityPool,
            signedUpdate,
            updateHash,
            signerAddress,
            stakingTokenInformation,
            lastSyncGlobalEpochProgress,
            currencyOrdinal
          )

        def getUpdatedLiquidityPoolDueNewSwap(
          hashedSwapUpdate: Hashed[SwapUpdate],
          liquidityPool: LiquidityPool,
          swapTokenInfo: SwapTokenInfo,
          metagraphId: CurrencyId,
          lastSyncGlobalEpochProgress: EpochProgress,
          currencyOrdinal: SnapshotOrdinal
        ): F[Either[FailedCalculatedState, LiquidityPool]] =
          poolOps.updatePoolForSwap(
            hashedSwapUpdate,
            liquidityPool,
            swapTokenInfo,
            metagraphId,
            lastSyncGlobalEpochProgress,
            currencyOrdinal
          )

        def getUpdatedLiquidityPoolDueNewWithdrawal(
          signedUpdate: Signed[WithdrawalUpdate],
          updateHash: Hash,
          liquidityPool: LiquidityPool,
          withdrawalAmounts: WithdrawalTokenInfo,
          lastSyncGlobalEpochProgress: EpochProgress,
          currencyOrdinal: SnapshotOrdinal
        ): F[Either[FailedCalculatedState, LiquidityPool]] =
          poolOps.updatePoolForWithdrawal(
            signedUpdate,
            updateHash,
            liquidityPool,
            withdrawalAmounts,
            lastSyncGlobalEpochProgress,
            currencyOrdinal
          )

        def getWithdrawalTokenInfo(
          signedUpdate: Signed[WithdrawalUpdate],
          updateHash: Hash,
          liquidityPool: LiquidityPool,
          lastSyncGlobalEpochProgress: EpochProgress
        ): Either[FailedCalculatedState, WithdrawalTokenInfo] =
          poolOps.calculateWithdrawalAmounts(signedUpdate, updateHash, liquidityPool, lastSyncGlobalEpochProgress)

        def rollbackSwapLiquidityPoolAmounts(
          signedUpdate: Signed[SwapUpdate],
          updateHash: Hash,
          lastSyncGlobalEpochProgress: EpochProgress,
          liquidityPool: LiquidityPool,
          tokenAAmountToReturn: SwapAmount,
          tokenBAmountToReturn: SwapAmount,
          metagraphId: CurrencyId,
          currencyOrdinal: SnapshotOrdinal
        ): F[Either[FailedCalculatedState, LiquidityPool]] =
          rollbackOps.rollbackSwap(
            signedUpdate,
            updateHash,
            lastSyncGlobalEpochProgress,
            liquidityPool,
            tokenAAmountToReturn,
            tokenBAmountToReturn,
            metagraphId,
            currencyOrdinal
          )

        def rollbackWithdrawalLiquidityPoolAmounts(
          signedUpdate: Signed[WithdrawalUpdate],
          updateHash: Hash,
          lastSyncGlobalEpochProgress: EpochProgress,
          liquidityPool: LiquidityPool,
          tokenAAmountToReturn: SwapAmount,
          tokenBAmountToReturn: SwapAmount,
          currencyOrdinal: SnapshotOrdinal
        ): F[Either[FailedCalculatedState, LiquidityPool]] =
          rollbackOps.rollbackWithdrawal(
            signedUpdate,
            updateHash,
            lastSyncGlobalEpochProgress,
            liquidityPool,
            tokenAAmountToReturn,
            tokenBAmountToReturn,
            currencyOrdinal
          )
      }
    }
  }
}
