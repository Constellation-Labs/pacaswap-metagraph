package org.amm_metagraph.shared_data

import cats.Order
import cats.syntax.all._

import scala.math.BigDecimal.RoundingMode

import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.swap.CurrencyId

import derevo.cats.{eqv, show}
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import eu.timepit.refined.types.all.NonNegLong
import org.amm_metagraph.shared_data.refined.Percentage._
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.types.LiquidityPool.{LiquidityPool, PoolShares}

object FeeDistributor {

  def getFeeShare(feeShares: Map[Address, NonNegLong], address: Address): NonNegLong =
    feeShares.getOrElse(address, 0L.toNonNegLongUnsafe)

  def standard: FeePercentages = FeePercentages(
    Percentage.unsafeFrom(0.3),
    Percentage.unsafeFrom(0.25),
    Percentage.unsafeFrom(0.05)
  )

  def empty: FeePercentages = FeePercentages(
    Percentage.unsafeFrom(0.0),
    Percentage.unsafeFrom(0.0),
    Percentage.unsafeFrom(0.0)
  )

  @derive(eqv, encoder, decoder, show)
  case class FeePercentages(total: Percentage, providers: Percentage, operators: Percentage)

  object FeePercentages {
    implicit val order: Order[FeePercentages] = Order.by(_.total)
  }

  @derive(eqv, encoder, decoder)
  case class FeeAmounts(total: Long, providers: Long, operators: Long)

  def calculateFeeAmounts(
    amountBeforeFee: BigInt,
    feeDistribution: FeePercentages
  ): FeeAmounts = {
    val totalFeeAmount = (BigDecimal(amountBeforeFee) * feeDistribution.total.toDecimal)
      .setScale(0, RoundingMode.HALF_UP)
      .toLong

    val totalPercentage = feeDistribution.providers.value + feeDistribution.operators.value

    if (totalPercentage == BigDecimal(0)) FeeAmounts(0L, 0L, 0L)
    else {
      val providerPortion = feeDistribution.providers.value / totalPercentage
      val providerFeeAmount = (BigDecimal(totalFeeAmount) * providerPortion)
        .setScale(0, RoundingMode.HALF_UP)
        .toLong

      val operatorFeeAmount = totalFeeAmount - providerFeeAmount

      FeeAmounts(totalFeeAmount, providerFeeAmount, operatorFeeAmount)
    }
  }

  def distributeProviderFees(
    providerFeeAmount: Long,
    operatorFeeAmount: Long,
    poolShares: PoolShares,
    metagraphId: CurrencyId
  ): Map[Address, NonNegLong] = {
    val totalShares = poolShares.totalShares.value

    if (totalShares == 0) Map.empty[Address, NonNegLong]
    else {
      val providerDistribution = poolShares.addressShares.foldLeft(Map.empty[Address, Long]) {
        case (distributionMap, (address, shareAmount)) =>
          val sharePercentage = BigDecimal(shareAmount.value.value.value) / BigDecimal(totalShares)
          val providerFeeShareAmount = (BigDecimal(providerFeeAmount) * sharePercentage)
            .setScale(0, RoundingMode.HALF_UP)
            .toLong

          distributionMap + (address -> providerFeeShareAmount)
      }

      val roundingDifference = providerFeeAmount - providerDistribution.values.sum
      val operatorDistribution = Map(metagraphId.value -> (operatorFeeAmount + roundingDifference))

      val allDistributions = operatorDistribution.foldLeft(providerDistribution) {
        case (acc, (address, operatorAmount)) =>
          val existingAmount = acc.getOrElse(address, 0L)
          acc + (address -> (existingAmount + operatorAmount))
      }
      
      allDistributions.map { case (address, amount) =>
        address -> amount.toNonNegLongUnsafe
      }
    }
  }
}
