package org.amm_metagraph.shared_data

import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.epoch.EpochProgress

/** Can a SpendAction emitted now still be settled by the global layer?
  *
  * `lastSyncGlobalEpochProgress` is the epoch of a global snapshot that has already been produced. The SpendAction this metagraph emits
  * is carried in its next currency snapshot and applied by the global layer at least one global snapshot later, by which time the epoch
  * may have advanced. Measured on mainnet the gap between a currency snapshot's synced global ordinal and the global snapshot that applied
  * its SpendAction was five ordinals, about one epoch, so one epoch of headroom is not enough either. Two is the smallest margin that
  * covered every observed settlement.
  */
object AllowSpendSettlement {

  /** Epochs an AllowSpend must still have at emission time, from `ProtocolActivation.spendActionHeadroom`. */
  val headroomEpochs: Long = 2L

  /** True when the AllowSpend must be treated as expired for the purpose of emitting a SpendAction.
    *
    * `legacyExpired` is the pre-activation decision, passed in unchanged so each caller keeps its own historical rule (swaps compared
    * strictly, pools and staking allowed a buffer) and history replays as recorded.
    */
  def expiredForSettlement(
    lastValidEpochProgress: EpochProgress,
    lastSyncGlobalEpochProgress: EpochProgress,
    legacyExpired: Boolean,
    currentSnapshotOrdinal: SnapshotOrdinal
  ): Boolean =
    if (ProtocolActivation.spendActionHeadroomActive(currentSnapshotOrdinal))
      lastValidEpochProgress.value.value < lastSyncGlobalEpochProgress.value.value + headroomEpochs
    else legacyExpired
}
