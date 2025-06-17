package org.amm_metagraph.shared_data.services.pricing

import cats.Semigroup
import cats.effect.Async
import cats.syntax.all._

import scala.math.BigDecimal.RoundingMode

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
import org.amm_metagraph.shared_data.FeeDistributor
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.calculated_state.CalculatedStateService
import org.amm_metagraph.shared_data.epochProgress.getFailureExpireEpochProgress
import org.amm_metagraph.shared_data.refined.Percentage._
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.LiquidityPool.PoolShares.toPendingFeeShares
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.Staking.StakingTokenInformation
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.Swap.{ReverseSwapQuote, SwapQuote}
import org.amm_metagraph.shared_data.validations.Errors._

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
  ): F[Either[FailedCalculatedState, StakingTokenInformation]]

  def getUpdatedLiquidityPoolDueStaking(
    liquidityPool: LiquidityPool,
    signedUpdate: Signed[StakingUpdate],
    updateHash: Hash,
    signerAddress: Address,
    stakingTokenInformation: StakingTokenInformation,
    lastSyncGlobalEpochProgress: EpochProgress
  ): Either[FailedCalculatedState, LiquidityPool]

  def getUpdatedLiquidityPoolDueNewSwap(
    hashedSwapUpdate: Hashed[SwapUpdate],
    liquidityPool: LiquidityPool,
    fromTokenInfo: TokenInformation,
    toTokenInfo: TokenInformation,
    grossAmount: SwapAmount,
    metagraphId: CurrencyId
  ): Either[FailedCalculatedState, LiquidityPool]

  def getUpdatedLiquidityPoolDueConfirmedSwap(
    updateHash: Hash,
    liquidityPool: LiquidityPool
  ): Either[FailedCalculatedState, LiquidityPool]

  def getUpdatedLiquidityPoolDueNewWithdrawal(
    signedUpdate: Signed[WithdrawalUpdate],
    updateHash: Hash,
    liquidityPool: LiquidityPool,
    withdrawalAmounts: WithdrawalTokenAmounts,
    lastSyncGlobalEpochProgress: EpochProgress
  ): Either[FailedCalculatedState, LiquidityPool]

  def calculateWithdrawalAmounts(
    signedUpdate: Signed[WithdrawalUpdate],
    updateHash: Hash,
    liquidityPool: LiquidityPool,
    lastSyncGlobalEpochProgress: EpochProgress
  ): Either[FailedCalculatedState, WithdrawalTokenAmounts]

  def rollbackSwapLiquidityPoolAmounts(
    signedUpdate: Signed[SwapUpdate],
    updateHash: Hash,
    lastSyncGlobalEpochProgress: EpochProgress,
    liquidityPool: LiquidityPool,
    tokenAAmountToReturn: SwapAmount,
    tokenBAmountToReturn: SwapAmount,
    metagraphId: CurrencyId
  ): Either[FailedCalculatedState, LiquidityPool]

  def rollbackWithdrawalLiquidityPoolAmounts(
    signedUpdate: Signed[WithdrawalUpdate],
    updateHash: Hash,
    lastSyncGlobalEpochProgress: EpochProgress,
    liquidityPool: LiquidityPool,
    tokenAAmountToReturn: SwapAmount,
    tokenBAmountToReturn: SwapAmount
  ): Either[FailedCalculatedState, LiquidityPool]
}

object PricingService {
  def make[F[_]: Async](
    applicationConfig: ApplicationConfig,
    calculatedStateService: CalculatedStateService[F]
  ): PricingService[F] = {
    case class ReservesAndReceived(
      swapAmount: BigInt,
      newInputReserveBeforeFee: BigInt,
      newOutputReserveBeforeFee: BigInt,
      estimatedReceivedBeforeFee: BigInt,
      newInputReserveAfterFee: BigInt,
      newOutputReserveAfterFee: BigInt,
      estimatedReceivedAfterFee: BigInt
    )

    case class InputRequiredAndReserves(
      requiredInputAmount: BigInt,
      newInputReserveBeforeFee: BigInt,
      newOutputReserveBeforeFee: BigInt,
      outputBeforeFee: BigInt,
      newInputReserveAfterFee: BigInt,
      newOutputReserveAfterFee: BigInt,
      actualOutputAfterFee: BigInt
    )

    new PricingService[F] {
      private def getConfirmedLiquidityPools =
        calculatedStateService.get.map(calculatedState => getLiquidityPoolCalculatedState(calculatedState.state))

      private def calculateReservesAndReceived(
        pool: LiquidityPool,
        fromTokenId: Option[CurrencyId],
        amount: Amount
      ): Either[String, ReservesAndReceived] =
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

          newInputReserveBeforeFee = inputReserve + BigInt(amount.value.value)
          newOutputReserveBeforeFee = pool.k / newInputReserveBeforeFee
          estimatedReceivedBeforeFee = outputReserve - newOutputReserveBeforeFee

          fees = FeeDistributor.calculateFeeAmounts(estimatedReceivedBeforeFee, pool.poolFees)
          estimatedReceivedAfterFee = estimatedReceivedBeforeFee - fees.total
          newOutputReserveAfterFee = newOutputReserveBeforeFee + fees.total
          newInputReserveAfterFee = newInputReserveBeforeFee
        } yield
          ReservesAndReceived(
            BigInt(amount.value),
            newInputReserveBeforeFee,
            newOutputReserveBeforeFee,
            estimatedReceivedBeforeFee,
            newInputReserveAfterFee,
            newOutputReserveAfterFee,
            estimatedReceivedAfterFee
          )

      private def calculateReverseReservesAndReceived(
        pool: LiquidityPool,
        fromTokenId: Option[CurrencyId],
        desiredOutputAmount: Amount
      ): Either[String, InputRequiredAndReserves] =
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

          desiredOutputBigInt = BigInt(desiredOutputAmount.value.value)

          _ <- Either.cond(
            desiredOutputBigInt < outputReserve,
            (),
            "Desired output amount exceeds available liquidity"
          )

          totalFeeDecimal = pool.poolFees.total
          outputBeforeFeeDecimal = desiredOutputBigInt.toBigDecimal / (BigDecimal(1) - totalFeeDecimal)
          outputBeforeFee = outputBeforeFeeDecimal.toBigInt
          totalFeeAmount = outputBeforeFee - desiredOutputBigInt

          newOutputReserveBeforeFee = outputReserve - outputBeforeFee

          _ <- Either.cond(
            newOutputReserveBeforeFee > 0,
            (),
            "Required output would drain the pool"
          )

          newInputReserveBeforeFee = pool.k / newOutputReserveBeforeFee
          requiredInputAmount = newInputReserveBeforeFee - inputReserve

          _ <- Either.cond(
            requiredInputAmount > 0,
            (),
            "Invalid calculation: negative input required"
          )

          newInputReserveAfterFee = newInputReserveBeforeFee
          newOutputReserveAfterFee = newOutputReserveBeforeFee + totalFeeAmount

        } yield
          InputRequiredAndReserves(
            requiredInputAmount,
            newInputReserveBeforeFee,
            newOutputReserveBeforeFee,
            outputBeforeFee,
            newInputReserveAfterFee,
            newOutputReserveAfterFee,
            desiredOutputBigInt
          )

      private def calculateSwapQuote(
        pool: LiquidityPool,
        fromTokenId: Option[CurrencyId],
        toTokenId: Option[CurrencyId],
        amount: Amount,
        slippagePercent: Percentage
      ): Either[String, SwapQuote] =
        for {
          reservesAndReceived <- calculateReservesAndReceived(pool, fromTokenId, amount)
          outputToken = if (fromTokenId === pool.tokenA.identifier) pool.tokenB else pool.tokenA

          _ <- Either.cond(
            outputToken.identifier === toTokenId,
            (),
            "Invalid output token"
          )

          estimatedReceivedDecimal = reservesAndReceived.estimatedReceivedAfterFee.toBigDecimal
          minimumReceived = (estimatedReceivedDecimal * slippagePercent.toFactor).floor.toLong

          amountValue = BigDecimal(amount.value.value)
          rate = if (amountValue > 0) estimatedReceivedDecimal / amountValue else BigDecimal(0)

          outputReserve = BigInt(outputToken.amount.value)
          outputReserveDecimal = outputReserve.toBigDecimal
          newOutputReserveDecimal = reservesAndReceived.newOutputReserveAfterFee.toBigDecimal

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
            estimatedReceived = reservesAndReceived.estimatedReceivedAfterFee,
            minimumReceived = minimumReceived
          )

      private def calculateReverseSwapQuote(
        pool: LiquidityPool,
        fromTokenId: Option[CurrencyId],
        toTokenId: Option[CurrencyId],
        desiredOutputAmount: Amount,
        slippagePercent: Percentage
      ): Either[String, ReverseSwapQuote] =
        for {
          inputAndReserves <- calculateReverseReservesAndReceived(pool, fromTokenId, desiredOutputAmount)
          inputToken = if (fromTokenId === pool.tokenA.identifier) pool.tokenA else pool.tokenB

          _ <- Either.cond(
            inputToken.identifier === fromTokenId,
            (),
            "Invalid input token"
          )

          requiredInputDecimal = inputAndReserves.requiredInputAmount.toBigDecimal

          maxInputRequired = (requiredInputDecimal / slippagePercent.toFactor)
            .setScale(0, RoundingMode.CEILING)
            .toLong

          desiredOutputDecimal = BigDecimal(desiredOutputAmount.value.value)
          rate = if (requiredInputDecimal > 0) desiredOutputDecimal / requiredInputDecimal else BigDecimal(0)

          inputReserve = BigInt(inputToken.amount.value)
          inputReserveDecimal = inputReserve.toBigDecimal
          newInputReserveDecimal = inputAndReserves.newInputReserveAfterFee.toBigDecimal

          priceImpactPercent =
            if (inputReserve > 0) {
              ((newInputReserveDecimal - inputReserveDecimal) / inputReserveDecimal).toPercentage
            } else BigDecimal(0)
        } yield
          ReverseSwapQuote(
            fromTokenId = fromTokenId,
            toTokenId = toTokenId,
            desiredOutputAmount = desiredOutputAmount,
            slippagePercent = slippagePercent,
            rate = rate,
            priceImpactPercent = priceImpactPercent.setScale(2, RoundingMode.HALF_UP),
            requiredInputAmount = inputAndReserves.requiredInputAmount,
            maxInputRequired = maxInputRequired
          )

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
                calculateReverseSwapQuote(pool, fromTokenId, toTokenId, desiredOutputAmount, slippagePercent)
              case Left(_) => Left("Liquidity pool does not exist")
            }
          case Left(e) => Async[F].pure(Left(s"Failed to build pool identifier: ${e.getMessage}"))
        }
      } yield result

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
        expireEpochProgress = getFailureExpireEpochProgress(applicationConfig, lastSyncGlobalEpochProgress)
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
            val calculationResult = calculateReservesAndReceived(liquidityPool, swapUpdate.swapFromPair, swapUpdate.amountIn)

            calculationResult match {
              case Right(reservesAndReceived: ReservesAndReceived) =>
                val fromTokenInfo =
                  if (swapUpdate.swapFromPair === liquidityPool.tokenA.identifier) liquidityPool.tokenA
                  else liquidityPool.tokenB

                val toTokenInfo =
                  if (swapUpdate.swapToPair === liquidityPool.tokenA.identifier) liquidityPool.tokenA
                  else liquidityPool.tokenB

                val swapTokenInfo = SwapTokenInfo(
                  fromTokenInfo.copy(amount = reservesAndReceived.newInputReserveAfterFee.toLong.toPosLongUnsafe),
                  toTokenInfo.copy(amount = reservesAndReceived.newOutputReserveAfterFee.toLong.toPosLongUnsafe),
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
      ): F[Either[FailedCalculatedState, StakingTokenInformation]] = for {
        liquidityPools <- getConfirmedLiquidityPools
        stakingUpdate = signedUpdate.value
        expireEpochProgress = getFailureExpireEpochProgress(applicationConfig, lastSyncGlobalEpochProgress)
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

            val relativeDepositIncrease = incomingPrimaryAmount.toDouble / currentPrimaryTokenAmount
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

      def getUpdatedLiquidityPoolDueStaking(
        liquidityPool: LiquidityPool,
        signedUpdate: Signed[StakingUpdate],
        updateHash: Hash,
        signerAddress: Address,
        stakingTokenInformation: StakingTokenInformation,
        lastSyncGlobalEpochProgress: EpochProgress
      ): Either[FailedCalculatedState, LiquidityPool] = {
        val primaryToken = stakingTokenInformation.primaryTokenInformation
        val pairToken = stakingTokenInformation.pairTokenInformation
        val newlyIssuedShares = stakingTokenInformation.newlyIssuedShares
        val expireEpochProgress = getFailureExpireEpochProgress(applicationConfig, lastSyncGlobalEpochProgress)

        val tokenA = if (liquidityPool.tokenA.identifier == primaryToken.identifier) primaryToken else pairToken
        val tokenB = if (liquidityPool.tokenB.identifier == pairToken.identifier) pairToken else primaryToken
        val tokenAValue = liquidityPool.tokenA.amount.value + tokenA.amount.value
        val tokenBValue = liquidityPool.tokenB.amount.value + tokenB.amount.value

        for {
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
        } yield
          liquidityPool
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
                updatedAddressShares,
                liquidityPool.poolShares.pendingFeeShares,
                liquidityPool.poolShares.feeShares
              )
            )
      }

      def getUpdatedLiquidityPoolDueNewSwap(
        hashedSwapUpdate: Hashed[SwapUpdate],
        liquidityPool: LiquidityPool,
        fromTokenInfo: TokenInformation,
        toTokenInfo: TokenInformation,
        grossAmount: SwapAmount,
        metagraphId: CurrencyId
      ): Either[FailedCalculatedState, LiquidityPool] = {
        def distributeFees: PoolShares = {
          val fees = FeeDistributor.calculateFeeAmounts(
            BigInt(grossAmount.value.value),
            liquidityPool.poolFees
          )

          val updateFeeShares = FeeDistributor.distributeProviderFees(
            fees.providers,
            fees.operators,
            liquidityPool.poolShares,
            metagraphId
          )

          liquidityPool.poolShares.copy(pendingFeeShares = toPendingFeeShares(updateFeeShares, hashedSwapUpdate.hash))
        }

        val tokenA = if (liquidityPool.tokenA.identifier === fromTokenInfo.identifier) fromTokenInfo else toTokenInfo
        val tokenB = if (liquidityPool.tokenB.identifier === toTokenInfo.identifier) toTokenInfo else fromTokenInfo

        Right(
          liquidityPool
            .focus(_.tokenA)
            .replace(tokenA)
            .focus(_.tokenB)
            .replace(tokenB)
            .focus(_.poolShares)
            .replace(distributeFees)
        )
      }

      def getUpdatedLiquidityPoolDueConfirmedSwap(
        updateHash: Hash,
        liquidityPool: LiquidityPool
      ): Either[FailedCalculatedState, LiquidityPool] = {
        implicit val semigroupNonNegLong: Semigroup[NonNegLong] =
          Semigroup.instance((a, b) => NonNegLong.unsafeFrom(a.value + b.value))

        def applyPendingFees: PoolShares = {
          val pendingFeesToConfirm =
            liquidityPool.poolShares.pendingFeeShares.getOrElse(updateHash, Map.empty[Address, NonNegLong])

          liquidityPool.poolShares
            .focus(_.feeShares)
            .modify { existing =>
              existing |+| pendingFeesToConfirm
            }
            .focus(_.pendingFeeShares)
            .modify { existing =>
              existing.removed(updateHash)
            }
        }

        Right(
          liquidityPool
            .focus(_.poolShares)
            .replace(applyPendingFees)
        )
      }

      def getUpdatedLiquidityPoolDueNewWithdrawal(
        signedUpdate: Signed[WithdrawalUpdate],
        updateHash: Hash,
        liquidityPool: LiquidityPool,
        withdrawalAmounts: WithdrawalTokenAmounts,
        lastSyncGlobalEpochProgress: EpochProgress
      ): Either[FailedCalculatedState, LiquidityPool] = {
        val tokenAValue = liquidityPool.tokenA.amount.value - withdrawalAmounts.tokenAAmount.value
        val tokenBValue = liquidityPool.tokenB.amount.value - withdrawalAmounts.tokenBAmount.value
        val totalSharesValue = liquidityPool.poolShares.totalShares.value - signedUpdate.shareToWithdraw.value.value
        val expireEpochProgress = getFailureExpireEpochProgress(applicationConfig, lastSyncGlobalEpochProgress)

        for {
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
        } yield
          liquidityPool.copy(
            tokenA = liquidityPool.tokenA.copy(amount = tokenAAmount),
            tokenB = liquidityPool.tokenB.copy(amount = tokenBAmount),
            k = k,
            poolShares = PoolShares(
              totalSharesAmount,
              updatedAddressShares,
              liquidityPool.poolShares.pendingFeeShares,
              liquidityPool.poolShares.feeShares
            )
          )
      }

      def calculateWithdrawalAmounts(
        signedUpdate: Signed[WithdrawalUpdate],
        updateHash: Hash,
        liquidityPool: LiquidityPool,
        lastSyncGlobalEpochProgress: EpochProgress
      ): Either[FailedCalculatedState, WithdrawalTokenAmounts] = {
        val expireEpochProgress = getFailureExpireEpochProgress(applicationConfig, lastSyncGlobalEpochProgress)

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
          WithdrawalTokenAmounts(
            liquidityPool.tokenA.identifier,
            SwapAmount(tokenAAmount),
            liquidityPool.tokenB.identifier,
            SwapAmount(tokenBAmount)
          )
      }

      def rollbackSwapLiquidityPoolAmounts(
        signedUpdate: Signed[SwapUpdate],
        updateHash: Hash,
        lastSyncGlobalEpochProgress: EpochProgress,
        liquidityPool: LiquidityPool,
        tokenAAmountToReturn: SwapAmount,
        tokenBAmountToReturn: SwapAmount,
        metagraphId: CurrencyId
      ): Either[FailedCalculatedState, LiquidityPool] = {
        val tokenAIsFrom = liquidityPool.tokenA.identifier === signedUpdate.swapFromPair
        val tokenBIsTo = liquidityPool.tokenB.identifier === signedUpdate.swapToPair
        val expireEpochProgress = getFailureExpireEpochProgress(applicationConfig, lastSyncGlobalEpochProgress)

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

        for {
          newTokenA <- newTokenAAmountEither.map(amount => liquidityPool.tokenA.copy(amount = amount))
          newTokenB <- newTokenBAmountEither.map(amount => liquidityPool.tokenB.copy(amount = amount))
        } yield
          liquidityPool
            .focus(_.tokenA)
            .replace(newTokenA)
            .focus(_.tokenB)
            .replace(newTokenB)
            .focus(_.poolShares.pendingFeeShares)
            .modify { existing =>
              existing.removed(updateHash)
            }
      }

      def rollbackWithdrawalLiquidityPoolAmounts(
        signedUpdate: Signed[WithdrawalUpdate],
        updateHash: Hash,
        lastSyncGlobalEpochProgress: EpochProgress,
        liquidityPool: LiquidityPool,
        tokenAAmountToReturn: SwapAmount,
        tokenBAmountToReturn: SwapAmount
      ): Either[FailedCalculatedState, LiquidityPool] = {
        val expireEpochProgress = getFailureExpireEpochProgress(applicationConfig, lastSyncGlobalEpochProgress)

        def error(msg: String): FailedCalculatedState =
          FailedCalculatedState(ArithmeticError(msg), expireEpochProgress, updateHash, signedUpdate)

        val newTokenAAmountEither: Either[FailedCalculatedState, PosLong] =
          (liquidityPool.tokenA.amount.value + tokenAAmountToReturn.value.value).toPosLong
            .leftMap(_ => error("Rolling back token A results in invalid addition"))

        val newTokenBAmountEither: Either[FailedCalculatedState, PosLong] =
          (liquidityPool.tokenB.amount.value + tokenBAmountToReturn.value.value).toPosLong
            .leftMap(_ => error("Rolling back token B results in invalid addition"))

        for {
          newTokenA <- newTokenAAmountEither.map(amount => liquidityPool.tokenA.copy(amount = amount))
          newTokenB <- newTokenBAmountEither.map(amount => liquidityPool.tokenB.copy(amount = amount))
        } yield liquidityPool.copy(tokenA = newTokenA, tokenB = newTokenB)
      }
    }
  }
}
