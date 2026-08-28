package org.amm_metagraph.shared_data

import scala.collection.immutable.SortedSet

import io.constellationnetwork.schema.SnapshotOrdinal

import eu.timepit.refined.types.all.NonNegLong
import org.amm_metagraph.shared_data.services.combiners.PendingOperationsProcessor
import weaver.SimpleIOSuite

/** PROT-1695: a settled SpendAction was expired and rolled back because a partial read of the global chain was treated as proof that it had
  * not settled.
  *
  * The guard against that existed, but it sat behind `globalSnapshotsSyncSpendActions.nonEmpty`, so it was only reachable when the read
  * returned nothing at all. A node that resolved one ordinal carrying any spend action skipped the guard entirely and judged every pending
  * operation against a list that could not contain the evidence it needed.
  *
  * These pin the production selector's ordering rule and the gate it hangs on.
  */
object EvidenceCompletenessSpec extends SimpleIOSuite {

  private def ord(o: Long): SnapshotOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(o))

  private val pending = SortedSet(1, 2)

  private def selected(
    ordinal: Long,
    evidenceComplete: Boolean,
    readReturnedActions: Boolean,
    expired: Int => Boolean = _ => true
  ): SortedSet[Int] =
    PendingOperationsProcessor.selectPendingSpendActions(
      ord(ordinal),
      evidenceComplete,
      readReturnedActions,
      pending
    )(expired)

  pureTest("PROT-1695: a partial read no longer counts as proof once the gate is active") {
    expect.all(
      // The exact shape that stranded the deposit: some actions came back, but the range was not
      // fully resolved. Before the fix every pending operation was judged.
      selected(740000L, evidenceComplete = false, readReturnedActions = true).isEmpty,
      // A blank read was already handled, and still is.
      selected(740000L, evidenceComplete = false, readReturnedActions = false).isEmpty
    )
  }

  pureTest("a complete read is still acted on, whether or not it found anything") {
    expect.all(
      selected(740000L, evidenceComplete = true, readReturnedActions = true) == pending,
      selected(740000L, evidenceComplete = true, readReturnedActions = false) == pending,
      selected(740000L, evidenceComplete = true, readReturnedActions = false, _ == 2) == SortedSet(2)
    )
  }

  pureTest("below the gate the old ordering is reproduced exactly, so signed history replays") {
    expect.all(
      // The bug itself, preserved below the activation. This is the assertion that keeps the
      // ordinals already produced since the 731647 restart replayable.
      selected(739999L, evidenceComplete = false, readReturnedActions = true) == pending,
      // The blank-read guard predates this gate and must keep working below it.
      selected(739999L, evidenceComplete = false, readReturnedActions = false).isEmpty,
      selected(739999L, evidenceComplete = true, readReturnedActions = true) == pending
    )
  }

  pureTest("the new gate is later than the one that already activated, and is not yet reached") {
    expect.all(
      // 731647 is long past, so this fix could not hang on it without breaking replay of
      // everything produced since the restart.
      ProtocolActivation.evidenceCompletenessFirst.value.value > ProtocolActivation.reserveAccountingFixes.value.value,
      ProtocolActivation.evidenceCompletenessFirstActive(ord(740000L)),
      !ProtocolActivation.evidenceCompletenessFirstActive(ord(739999L)),
      // Supersedes the unreleased 736000 gate by at least another two days at the observed rate.
      ProtocolActivation.evidenceCompletenessFirst.value.value - 736000L >= 80L * 48L
    )
  }
}
