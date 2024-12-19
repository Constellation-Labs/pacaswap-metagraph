package org.amm_metagraph.shared_data

import org.amm_metagraph.shared_data.epochProgress.epochProgressOneDay

object credits {
  def getUpdatedCredits(
    lastUpdateEpoch: Long,
    lastUpdateCredits: Double,
    currentEpoch: Long,
    maxCredits: Double
  ): Either[String, Double] = {
    val regenerationRate = maxCredits / epochProgressOneDay

    val elapsedEpochs = currentEpoch - lastUpdateEpoch
    if (elapsedEpochs < 0) return Left("Invalid epoch: time went backwards")

    val regeneratedCredits = elapsedEpochs * regenerationRate
    val updatedCredits = Math.min(lastUpdateCredits + regeneratedCredits, maxCredits)

    if (updatedCredits < 1) {
      Left("Not enough credits to make an update")
    } else {
      val finalCredits = updatedCredits - 1
      Right(finalCredits)
    }
  }
}
