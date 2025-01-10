package org.amm_metagraph.shared_data

import scala.concurrent.duration._

object epochProgress {
  private val oneEpochProgress = 43.seconds

  val oneEpochProgressInSeconds: Int = oneEpochProgress.toSeconds.toInt
  val epochProgressOneDay: Int = (1.day / oneEpochProgress).toInt
  val epochProgress6Months: Long = epochProgressOneDay * 180L
  val epochProgress1Year: Long = epochProgress6Months * 2
  val epochProgress2Years: Long = epochProgress1Year * 2
}
