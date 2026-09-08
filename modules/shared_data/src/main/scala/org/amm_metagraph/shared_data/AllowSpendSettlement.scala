package org.amm_metagraph.shared_data

import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.epoch.EpochProgress

/** Can a SpendAction emitted now still be settled by the global layer?
  *
  * `lastSyncGlobalEpochProgress` is the epoch of a global snapshot that has already been produced. The SpendAction this metagraph emits is
  * carried in its next currency snapshot and applied by the global layer at least one global snapshot later, by which time the epoch may
  * have advanced. Measured on mainnet the gap between a currency snapshot's synced global ordinal and the global snapshot that applied its
  * SpendAction was five ordinals, about one epoch, so one epoch of headroom is not enough either. Two is the smallest margin that covered
  * every observed settlement.
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

  /** Should a SpendAction be emitted at all, given the update's own deadline?
    *
    * The global layer never reads `maxValidGsEpochProgress`, so it cannot protect the metagraph from its own expiry. If the update's
    * deadline is closer than `headroomEpochs`, the metagraph would be committing reserves it may have to disown before the global layer has
    * even seen the action. Refuse at the allow-spend stage instead, before any reserve moves.
    */
  def emissionExpired(
    maxValidGsEpochProgress: EpochProgress,
    lastSyncGlobalEpochProgress: EpochProgress,
    currentSnapshotOrdinal: SnapshotOrdinal
  ): Boolean =
    ProtocolActivation.settlementDeadlineExpiryActive(currentSnapshotOrdinal) &&
      maxValidGsEpochProgress.value.value < lastSyncGlobalEpochProgress.value.value + headroomEpochs

  /** May a pending SpendAction be treated as expired, i.e. as one the global layer will never settle?
    *
    * From `settlementDeadlineExpiry` the only deadline consulted is the one the global layer enforces, carried on the pending action as
    * `settlementDeadline` (the AllowSpend's `lastValidEpochProgress`; for withdrawals, which spend no AllowSpend, the update's own
    * deadline). The global layer drops an AllowSpend once `lastValidEpochProgress < epochProgress`, so the action is unsettleable exactly
    * when `settlementDeadline < epoch`, where `epoch` must be the epoch of a global snapshot the evidence has been read through. A `None`
    * deadline (states written before the field existed) never expires: fail-closed, the same treatment `generatedAfterGlobalOrdinal` gets.
    *
    * `legacyExpired` is the caller's pre-activation decision, passed through unchanged so history replays.
    */
  def pendingExpired(
    settlementDeadline: Option[EpochProgress],
    epoch: EpochProgress,
    legacyExpired: Boolean,
    currentSnapshotOrdinal: SnapshotOrdinal
  ): Boolean =
    if (ProtocolActivation.settlementDeadlineExpiryActive(currentSnapshotOrdinal))
      settlementDeadline.exists(_.value.value < epoch.value.value)
    else legacyExpired

  /** The epoch against which pending SpendActions may be expired: that of the last global snapshot the evidence covers. Before activation
    * the newest synced epoch is used, as it always was.
    */
  def expiryEpoch(
    lastSyncGlobalEpochProgress: EpochProgress,
    settlementEpochProgress: Option[EpochProgress],
    currentSnapshotOrdinal: SnapshotOrdinal
  ): Option[EpochProgress] =
    if (ProtocolActivation.settlementDeadlineExpiryActive(currentSnapshotOrdinal)) settlementEpochProgress
    else Some(lastSyncGlobalEpochProgress)
}
