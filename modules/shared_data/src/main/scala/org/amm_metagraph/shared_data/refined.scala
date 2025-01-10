package org.amm_metagraph.shared_data

import eu.timepit.refined.types.all.NonNegDouble
import eu.timepit.refined.types.numeric.PosLong

object refined {

  implicit class PosLongOps(value: Long) {
    def toPosLongUnsafe: PosLong =
      PosLong.unsafeFrom(value)
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
}
