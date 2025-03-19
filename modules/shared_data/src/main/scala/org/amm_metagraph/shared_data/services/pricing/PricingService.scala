package org.amm_metagraph.shared_data.services.pricing

import cats.effect.Async
import cats.syntax.all._

import scala.math.BigDecimal.RoundingMode

import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.swap.CurrencyId

import eu.timepit.refined.types.all.NonNegLong
import org.amm_metagraph.shared_data.calculated_state.CalculatedStateService
import org.amm_metagraph.shared_data.refined.Percentage._
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.types.DataUpdates.{StakingUpdate, SwapUpdate}
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.Staking.StakingTokenInformation
import org.amm_metagraph.shared_data.types.Swap.{SwapQuote, SwapTokenInfo}

trait PricingService[F[_]] {
  def getSwapQuote(
    fromTokenId: Option[CurrencyId],
    toTokenId: Option[CurrencyId],
    amount: Amount,
    slippagePercent: Percentage
  ): F[Either[String, SwapQuote]]

  def getLiquidityPoolPrices(poolId: PoolId): F[Either[String, (Long, Long)]]

  def getSwapTokenInfo(
    swapUpdate: SwapUpdate,
    poolId: PoolId
  ): F[Either[String, SwapTokenInfo]]

  def getStakingTokenInfo(
    stakingUpdate: StakingUpdate,
    poolId: PoolId
  ): F[Either[String, StakingTokenInformation]]

}

object PricingService {
  def make[F[_]: Async](
    calculatedStateService: CalculatedStateService[F]
  ): F[PricingService[F]] = Async[F].delay {
    new PricingService[F] {
      private def getConfirmedLiquidityPools =
        calculatedStateService.get.map(calculatedState => getLiquidityPoolCalculatedState(calculatedState.state))

      private def calculateReservesAndReceived(
        pool: LiquidityPool,
        fromTokenId: Option[CurrencyId],
        amount: Amount
      ): Either[String, (BigInt, BigInt, BigInt)] =
        for {
          (inputToken, outputToken) <- fromTokenId match {
            case id if id === pool.tokenA.identifier => Right((pool.tokenA, pool.tokenB))
            case id if id === pool.tokenB.identifier => Right((pool.tokenB, pool.tokenA))
            case _                                   => Left("Invalid token pair for swap")
          }

          inputReserve = BigInt(inputToken.amount.value)
          outputReserve = BigInt(outputToken.amount.value)

          _ <- Either.cond(
            inputReserve > 0 && outputReserve > 0,
            (),
            "Insufficient liquidity in the pool"
          )

          newInputReserve = inputReserve + BigInt(amount.value.value)
          newOutputReserve = pool.k / newInputReserve
          estimatedReceived = outputReserve - newOutputReserve
        } yield (newInputReserve, newOutputReserve, estimatedReceived)

      private def calculateSwapQuote(
        pool: LiquidityPool,
        fromTokenId: Option[CurrencyId],
        toTokenId: Option[CurrencyId],
        amount: Amount,
        slippagePercent: Percentage
      ): Either[String, SwapQuote] =
        for {
          (_, newOutputReserve, estimatedReceived) <- calculateReservesAndReceived(pool, fromTokenId, amount)
          outputToken = if (fromTokenId == pool.tokenA.identifier) pool.tokenB else pool.tokenA

          _ <- Either.cond(
            outputToken.identifier === toTokenId,
            (),
            "Invalid output token"
          )

          estimatedReceivedDecimal = estimatedReceived.toBigDecimal
          minimumReceived = (estimatedReceivedDecimal * slippagePercent.toFactor).floorToLong

          amountValue = BigDecimal(amount.value.value)
          rate = if (amountValue > 0) estimatedReceivedDecimal / amountValue else BigDecimal(0)

          outputReserve = BigInt(outputToken.amount.value)
          outputReserveDecimal = outputReserve.toBigDecimal
          newOutputReserveDecimal = newOutputReserve.toBigDecimal

          priceImpactPercent =
            if (outputReserve > 0) {
              ((outputReserveDecimal - newOutputReserveDecimal) / outputReserveDecimal).toPercentage
            } else BigDecimal(0)
        } yield
          SwapQuote(
            fromTokenId = fromTokenId,
            toTokenId = toTokenId,
            amount = amount,
            slippagePercent = slippagePercent,
            rate = rate,
            priceImpactPercent = priceImpactPercent.setScale(2, RoundingMode.HALF_UP),
            estimatedReceived = estimatedReceived,
            minimumReceived = minimumReceived
          )

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
                calculateSwapQuote(pool, fromTokenId, toTokenId, amount, slippagePercent)
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
              (tokenBAmount / tokenAAmount).halfUpToLong.toTokenAmountFormat
            } else 0L

            val priceTokenB = if (tokenBAmount > 0) {
              (tokenAAmount / tokenBAmount).halfUpToLong.toTokenAmountFormat
            } else 0L

            Right((priceTokenA, priceTokenB))
          case Left(_) => Left("Liquidity pool does not exist")
        }
      } yield result

      override def getSwapTokenInfo(
        swapUpdate: SwapUpdate,
        poolId: PoolId
      ): F[Either[String, SwapTokenInfo]] = for {
        liquidityPools <- getConfirmedLiquidityPools
        result <- getLiquidityPoolByPoolId(liquidityPools.confirmed.value, poolId).attempt.map {
          case Left(_) => Left("Liquidity pool does not exist")
          case Right(liquidityPool) =>
            val calculationResult = calculateReservesAndReceived(liquidityPool, swapUpdate.swapFromPair, swapUpdate.maxAmount)

            calculationResult match {
              case Right((newFromTokenReserve, newToTokenReserve, receivedAmount)) =>
                val fromTokenInfo =
                  if (swapUpdate.swapFromPair === liquidityPool.tokenA.identifier) liquidityPool.tokenA
                  else liquidityPool.tokenB

                val toTokenInfo =
                  if (swapUpdate.swapToPair === liquidityPool.tokenA.identifier) liquidityPool.tokenA
                  else liquidityPool.tokenB

                val maxAmountValue = BigDecimal(swapUpdate.maxAmount.value.value)
                val receivedAmountDecimal = receivedAmount.toBigDecimal

                val effectivePrice = if (maxAmountValue > 0) {
                  ((receivedAmountDecimal / maxAmountValue) * BigDecimal(1e8)).halfUpToLong
                } else 0L

                val swapTokenInfo = SwapTokenInfo(
                  fromTokenInfo.copy(amount = newFromTokenReserve.toLong.toPosLongUnsafe),
                  toTokenInfo.copy(amount = newToTokenReserve.toLong.toPosLongUnsafe),
                  Amount(NonNegLong.unsafeFrom(receivedAmount.toLong)),
                  Amount(NonNegLong.unsafeFrom(effectivePrice))
                )
                Right(swapTokenInfo)
              case Left(errorMsg) =>
                Left(errorMsg)
            }
        }.handleErrorWith { _ =>
          Async[F].pure(Left("Could not get pool"))
        }
      } yield result

      def getStakingTokenInfo(
        stakingUpdate: StakingUpdate,
        poolId: PoolId
      ): F[Either[String, StakingTokenInformation]] = for {
        liquidityPools <- getConfirmedLiquidityPools
        result <- getLiquidityPoolByPoolId(liquidityPools.confirmed.value, poolId).attempt.map {
          case Left(_) => Left("Liquidity pool does not exist")
          case Right(liquidityPool) =>
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
              ) // Maintain invariant

            val relativeDepositIncrease = incomingPrimaryAmount.toDouble / (currentPrimaryTokenAmount + incomingPrimaryAmount)
            val newlyIssuedShares = relativeDepositIncrease * liquidityPool.poolShares.totalShares.value

            Right(
              StakingTokenInformation(
                primaryToken.copy(amount = incomingPrimaryAmount.toPosLongUnsafe),
                pairToken.copy(amount = incomingPairAmount.toLong.toPosLongUnsafe),
                newlyIssuedShares.toLong
              )
            )
        }
      } yield result
    }
  }
}
