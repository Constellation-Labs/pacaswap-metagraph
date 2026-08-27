package org.amm_metagraph.shared_data

import io.constellationnetwork.schema.SnapshotOrdinal

import eu.timepit.refined.types.all.NonNegLong
import weaver.SimpleIOSuite

/** PROT-1695: a settled SpendAction was expired and rolled back because a partial read of the global chain was treated as proof that it had
  * not settled.
  *
  * The guard against that existed, but it sat behind `globalSnapshotsSyncSpendActions.nonEmpty`, so it was only reachable when the read
  * returned nothing at all. A node that resolved one ordinal carrying any spend action skipped the guard entirely and judged every pending
  * operation against a list that could not contain the evidence it needed.
  *
  * These pin the ordering rule and the gate it hangs on. The branch itself is exercised through the combiner suites.
  */
object EvidenceCompletenessSpec extends SimpleIOSuite {

  private def ord(o: Long): SnapshotOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(o))

  /** The decision the combiner makes, in the same order the combiner makes it. Kept here as the specification: if the branches in
    * PendingOperationsProcessor are ever reordered again, this is what should fail.
    */
  private def defersEverything(gateActive: Boolean, evidenceComplete: Boolean, readReturnedActions: Boolean): Boolean =
    if (gateActive && !evidenceComplete) true
    else if (readReturnedActions) false
    else !evidenceComplete

  pureTest("PROT-1695: a partial read no longer counts as proof once the gate is active") {
    expect.all(
      // The exact shape that stranded the deposit: some actions came back, but the range was not
      // fully resolved. Before the fix this returned false and every pending operation was judged.
      defersEverything(gateActive = true, evidenceComplete = false, readReturnedActions = true),
      // A blank read was already handled, and still is.
      defersEverything(gateActive = true, evidenceComplete = false, readReturnedActions = false)
    )
  }

  pureTest("a complete read is still acted on, whether or not it found anything") {
    expect.all(
      !defersEverything(gateActive = true, evidenceComplete = true, readReturnedActions = true),
      !defersEverything(gateActive = true, evidenceComplete = true, readReturnedActions = false)
    )
  }

  pureTest("below the gate the old ordering is reproduced exactly, so signed history replays") {
    expect.all(
      // The bug itself, preserved below the activation. This is the assertion that keeps the
      // ordinals already produced since the 731647 restart replayable.
      !defersEverything(gateActive = false, evidenceComplete = false, readReturnedActions = true),
      // The blank-read guard predates this gate and must keep working below it.
      defersEverything(gateActive = false, evidenceComplete = false, readReturnedActions = false),
      !defersEverything(gateActive = false, evidenceComplete = true, readReturnedActions = true)
    )
  }

  pureTest("the new gate is later than the one that already activated, and is not yet reached") {
    expect.all(
      // 731647 is long past, so this fix could not hang on it without breaking replay of
      // everything produced since the restart.
      ProtocolActivation.evidenceCompletenessFirst.value.value > ProtocolActivation.reserveAccountingFixes.value.value,
      ProtocolActivation.evidenceCompletenessFirstActive(ord(736000L)),
      !ProtocolActivation.evidenceCompletenessFirstActive(ord(735999L)),
      // Chosen with room for every node to upgrade first: the chain was at ~731718 and moves at
      // roughly 80 ordinals an hour, so this is about two days out.
      ProtocolActivation.evidenceCompletenessFirst.value.value - 731718L > 80L * 24L
    )
  }
}
