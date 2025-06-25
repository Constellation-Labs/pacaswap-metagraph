package org.amm_metagraph.shared_data

import cats.{Eq, Order, Show}

import scala.math.BigDecimal.RoundingMode

import io.constellationnetwork.schema.balance.Amount

import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Interval
import eu.timepit.refined.types.all.{NonNegDouble, NonNegLong}
import eu.timepit.refined.types.numeric.{NonNegInt, PosLong}
import eu.timepit.refined.{W, refineV}
import io.circe.{Decoder, Encoder}

object refined {

  implicit class PosLongOps(value: Long) {
    def toPosLongUnsafe: PosLong =
      PosLong.unsafeFrom(value)

    def toPosLong: Either[String, PosLong] =
      PosLong.from(value)
  }

  implicit class LongOps(value: Long) {
    def fromTokenAmountFormat: Double =
      value / 10e7

    def toTokenAmountFormat: Long =
      (value * 10e7).toLong
  }

  implicit class DoubleOps(value: Double) {
    def toTokenAmountFormat: Long =
      (value * 10e7).toLong
  }

  implicit class NonNegDoubleOps(value: Double) {
    def toNonNegDoubleUnsafe: NonNegDouble =
      NonNegDouble.unsafeFrom(value)
  }

  implicit class NonNegLongOps(value: Long) {
    def toNonNegLongUnsafe: NonNegLong =
      NonNegLong.unsafeFrom(value)
  }

  implicit class NonNegIntOps(value: Int) {
    def toNonNegIntUnsafe: NonNegInt =
      NonNegInt.unsafeFrom(value)
  }

  implicit class BigDecimalOps(val bd: BigDecimal) extends AnyVal {
    def toPercentage: BigDecimal = bd * BigDecimal(100)
    def toTokenAmountFormat: Long =
      (bd * 10e7).toLong

    def floor: BigDecimal = bd.setScale(8, RoundingMode.FLOOR)
    def halfUp: BigDecimal = bd.setScale(8, RoundingMode.HALF_UP)

    def toAmountUnsafe: Amount = Amount(NonNegLong.unsafeFrom(floor.toLong))
  }

  implicit class BigIntOps(val bi: BigInt) extends AnyVal {
    def toBigDecimal: BigDecimal = BigDecimal(bi)
  }

  type PercentageRange = Interval.Closed[W.`0`.T, W.`100`.T]
  type Percentage = BigDecimal Refined PercentageRange

  object Percentage {
    def apply(value: BigDecimal): Either[String, Percentage] =
      refineV[PercentageRange](value)

    def unsafeFrom(value: BigDecimal): Percentage =
      refineV[PercentageRange](value) match {
        case Right(refined) => refined
        case Left(error)    => throw new IllegalArgumentException(s"Invalid percentage value: $value. Error: $error")
      }

    val zero: Percentage = unsafeFrom(0.0)
    val one: Percentage = unsafeFrom(1.0)

    implicit val percentageEncoder: Encoder[Percentage] =
      Encoder.encodeBigDecimal.contramap(_.value)

    implicit val percentageDecoder: Decoder[Percentage] =
      Decoder.decodeBigDecimal.emap { value =>
        eu.timepit.refined.refineV[PercentageRange](value)
      }

    implicit val percentageEq: Eq[Percentage] = Eq.by(_.value)
    implicit val percentageOrder: Order[Percentage] = Order.by(_.value)
    implicit val percentageShow: Show[Percentage] = p => f"${p.value}%.3f"

    implicit class PercentageOps(percentage: Percentage) {
      lazy val toDecimal: BigDecimal = percentage.value.setScale(8) / BigDecimal(100)
      def of(amount: BigDecimal): BigDecimal = amount * toDecimal
      def toFactor: BigDecimal = BigDecimal(1) - toDecimal
    }
  }
}
