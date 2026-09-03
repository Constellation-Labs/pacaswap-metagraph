package org.amm_metagraph.shared_data

import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.epoch.EpochProgress

import eu.timepit.refined.types.all.NonNegLong
import weaver.SimpleIOSuite

/** A SpendAction is only worth emitting if the global layer can still settle it.
  *
  * The incident swap at 747126 referenced an AllowSpend with `lastValidEpochProgress` 2867342 while the synced global epoch was 2867342
  * or 2867343 depending on the combine's frame; the SpendAction reached the global layer at epoch 2867344 and was refused. The pre-activation
  * rule let it through. From activation the AllowSpend must have two epochs left.
  */
object AllowSpendSettlementSpec extends SimpleIOSuite {

  private def ep(v: Long): EpochProgress = EpochProgress(NonNegLong.unsafeFrom(v))
  private def ord(v: Long): SnapshotOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(v))
  private val ACTIVE = ProtocolActivation.spendActionHeadroom
  private val BEFORE = ord(ACTIVE.value.value - 1L)

  private val synced = ep(2867342L)

  pureTest("the gate is later than the corrections it must not reorder, and before enforcement") {
    expect(ACTIVE.value.value == 754000L, "pinned activation ordinal") and
      expect(ACTIVE.value.value > ProtocolActivation.usdcRollbackCorrection.value.value, "after the USDC.dag correction") and
      expect(ACTIVE.value.value < ProtocolActivation.collateralInvariantEnforced.value.value, "before invariant enforcement") and
      expect(!ProtocolActivation.spendActionHeadroomActive(BEFORE) && ProtocolActivation.spendActionHeadroomActive(ACTIVE), "boundary")
  }

  pureTest("before activation the caller's own legacy decision stands, whatever the headroom") {
    val atEdge = AllowSpendSettlement.expiredForSettlement(synced, synced, legacyExpired = false, BEFORE)
    val legacySaysExpired = AllowSpendSettlement.expiredForSettlement(ep(2867350L), synced, legacyExpired = true, BEFORE)
    expect(!atEdge, "the incident case was accepted under the legacy rule and must replay that way") and
      expect(legacySaysExpired, "a legacy expiry decision is passed through unchanged")
  }

  pureTest("from activation an AllowSpend at or one epoch past the synced epoch is expired for settlement") {
    expect(AllowSpendSettlement.expiredForSettlement(synced, synced, legacyExpired = false, ACTIVE), "equal: the incident case") and
      expect(AllowSpendSettlement.expiredForSettlement(ep(2867343L), synced, legacyExpired = false, ACTIVE), "one epoch left") and
      expect(AllowSpendSettlement.expiredForSettlement(ep(2867341L), synced, legacyExpired = false, ACTIVE), "already past")
  }

  pureTest("from activation an AllowSpend with the full headroom is accepted, and the legacy decision is ignored") {
    val exact = ep(synced.value.value + AllowSpendSettlement.headroomEpochs)
    expect(!AllowSpendSettlement.expiredForSettlement(exact, synced, legacyExpired = false, ACTIVE), "exactly the headroom") and
      expect(!AllowSpendSettlement.expiredForSettlement(ep(2867353L), synced, legacyExpired = false, ACTIVE), "the swap that settled") and
      expect(!AllowSpendSettlement.expiredForSettlement(exact, synced, legacyExpired = true, ACTIVE), "legacy buffer no longer consulted")
  }

  pureTest("the headroom covers the settlement gap measured on mainnet") {
    // Currency snapshot 747126 synced global 6872641 (epoch 2867343); its SpendAction was applied at 6872646 (epoch 2867344).
    val gapObserved = 2867344L - 2867343L
    expect(AllowSpendSettlement.headroomEpochs > gapObserved, "one epoch was observed, so one epoch of headroom is the edge, not a margin")
  }
}
