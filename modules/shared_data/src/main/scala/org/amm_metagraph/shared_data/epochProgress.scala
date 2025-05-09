package org.amm_metagraph.shared_data

import io.constellationnetwork.schema.epoch.EpochProgress

import eu.timepit.refined.types.all.NonNegLong
import org.amm_metagraph.shared_data.app.ApplicationConfig

object epochProgress {
  def getFailureExpireEpochProgress(
    applicationConfig: ApplicationConfig,
    lastSyncGlobalEpochProgress: EpochProgress
  ): EpochProgress = EpochProgress(
    NonNegLong
      .from(
        lastSyncGlobalEpochProgress.value.value + applicationConfig.expirationEpochProgresses.failedOperations.value.value
      )
      .getOrElse(NonNegLong.MinValue)
  )

  def getConfirmedExpireEpochProgress(
    applicationConfig: ApplicationConfig,
    lastSyncGlobalEpochProgress: EpochProgress
  ): EpochProgress = EpochProgress(
    NonNegLong
      .from(
        lastSyncGlobalEpochProgress.value.value + applicationConfig.expirationEpochProgresses.confirmedOperations.value.value
      )
      .getOrElse(NonNegLong.MinValue)
  )
}
