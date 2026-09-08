package org.amm_metagraph.shared_data

import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.epoch.EpochProgress

/** Can a SpendAction emitted now still be settled by the global layer?
  *
  * `lastSyncGlobalEpochProgress` is the epoch of a global snapshot that has already been produced. The SpendAction this metagraph emits is
  * carried in its next currency snapshot and applied by the global layer at least one global snapshot later, by which time the epoch may
  * have advanced. Measured on mainnet the gap between a currency snapshot's synced global ordinal and the global snapshot that applied its
  * SpendAction was five ordinals; at the observed 2.5 to 4 global snapshots per epoch that is between one and two epochs, so two epochs of
  * headroom is the observed gap with little to spare. That is acceptable only because a headroom miss now degrades safely: the AllowSpend
  * genuinely expires, the metagraph rolls the operation back, and book and wallet agree. Widen it if settlement latency grows.
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

  /** A settlement deadline meaning "the global layer never expires this action". Withdrawals spend no AllowSpend, so their SpendAction is
    * applied whenever the global layer ingests the carrying currency snapshot; there is no epoch after which non-acceptance proves
    * anything.
    */
  val noDeadline: EpochProgress = EpochProgress.MaxValue

  /** May a pending SpendAction be treated as expired, i.e. as one the global layer will never settle?
    *
    * From `settlementDeadlineExpiry` the only deadline consulted is the one the global layer enforces, carried on the pending action as
    * `settlementDeadline` (the AllowSpend's `lastValidEpochProgress`, the earlier of two for pools and staking, `noDeadline` for
    * withdrawals). The global layer validates a SpendAction against the PREVIOUS global snapshot's active AllowSpends and only purges an
    * AllowSpend afterwards, retaining it while `lastValidEpochProgress >= epochProgress` (Tessellation v3.5.29
    * GlobalSnapshotAcceptanceManager:373-377 and :469, AllowSpendAcceptance:83,:116). So an AllowSpend with deadline L can still be spent
    * by a snapshot at epoch L + 1, and the action is unsettleable by epoch arithmetic only once `L + 1 < epoch`, where `epoch` is that of a
    * global snapshot the evidence has been read through.
    *
    * `None` is a pending action written before this field was populated (population is gated on the same ordinal, so on-chain bytes do not
    * change during the rollout window). Those keep the caller's legacy rule so nothing emitted before the gate is stranded.
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
      // Written as `deadline < epoch - 1` so `noDeadline` (Long.MaxValue) cannot overflow into "expired".
      settlementDeadline.fold(legacyExpired)(deadline => deadline.value.value < epoch.value.value - 1L)
    else legacyExpired

  /** Epoch arithmetic alone is not sound for a token leg: the global layer re-filters a token's AllowSpend bucket only in rounds where that
    * token's own metagraph submitted a currency snapshot (AllowSpendAcceptance.acceptAllowSpends folds over
    * `activeAllowSpendsFromCurrencySnapshots`), so while that metagraph is stalled its expired AllowSpends stay spendable. The sound test
    * is positive: every AllowSpend the action spends has left the active set of the last synced global snapshot, and the evidence covers
    * every global snapshot through that one, so a spend of it would already have been seen. An action that spends no AllowSpend
    * (withdrawal) can never be shown retired and is never expired.
    */
  def retiredForSettlement(
    hasAllowSpendRefs: Boolean,
    anyAllowSpendStillActive: Boolean,
    evidenceGapless: Boolean,
    currentSnapshotOrdinal: SnapshotOrdinal
  ): Boolean =
    if (ProtocolActivation.settlementDeadlineExpiryActive(currentSnapshotOrdinal))
      hasAllowSpendRefs && !anyAllowSpendStillActive && evidenceGapless
    else true

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
