package org.amm_metagraph.shared_data.services.pricing

import cats.syntax.all._

import scala.math.BigDecimal.RoundingMode

import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.swap.CurrencyId

import eu.timepit.refined.auto._
import org.amm_metagraph.shared_data.FeeDistributor
import org.amm_metagraph.shared_data.refined.Percentage._
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.Swap.{ReverseSwapQuote, SwapQuote}

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

object SwapCalculations {

  def calculateReservesAndReceived(
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
      k = BigInt(pool.tokenA.amount) * BigInt(pool.tokenB.amount)
      newOutputReserveBeforeFee = k / newInputReserveBeforeFee
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

  def calculateReverseReservesAndReceived(
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

  def buildSwapQuote(
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

  def buildReverseSwapQuote(
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
}
