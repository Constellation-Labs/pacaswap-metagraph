package org.amm_metagraph.shared_data

import scala.concurrent.duration._

object epochProgress {
  private val oneEpochProgress = 43.seconds

  val oneEpochProgressInSeconds: Int = oneEpochProgress.toSeconds.toInt
  val epochProgressOneDay: Long = (1.day / oneEpochProgress).toLong
  val epochProgress1Month: Long = epochProgressOneDay * 30L
  val epochProgress6Months: Long = epochProgress1Month * 6L
  val epochProgress1Year: Long = epochProgress6Months * 2
  val epochProgress2Years: Long = epochProgress1Year * 2
}
