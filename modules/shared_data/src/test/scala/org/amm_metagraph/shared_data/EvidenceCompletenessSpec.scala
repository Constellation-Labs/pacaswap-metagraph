package org.amm_metagraph.shared_data

import scala.collection.immutable.SortedSet

import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.artifact.SpendAction

import eu.timepit.refined.types.all.NonNegLong
import org.amm_metagraph.shared_data.globalSnapshots.summarizeSpendActionsRead
import org.amm_metagraph.shared_data.services.combiners.{PendingOperationsProcessor, StateManager}
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

  pureTest("an unresolved middle ordinal stops the evidence cursor before the gap") {
    val resolved = (List.empty[SpendAction], true)
    val missing = (List.empty[SpendAction], false)
    val read = summarizeSpendActionsRead(
      ord(10L),
      List(ord(10L) -> resolved, ord(11L) -> resolved, ord(12L) -> missing, ord(13L) -> resolved)
    )

    expect.all(
      !read.complete,
      read.lastContiguousGlobalSnapshotOrdinal == ord(11L),
      StateManager.selectNextGlobalSnapshotCursor(
        ord(740000L),
        read.complete,
        ord(13L),
        read.lastContiguousGlobalSnapshotOrdinal
      ) == ord(11L)
    )
  }

  pureTest("cursor hold is gated so historical snapshots retain the old advancement") {
    expect.all(
      StateManager.selectNextGlobalSnapshotCursor(
        ord(739999L),
        evidenceComplete = false,
        ord(13L),
        ord(11L)
      ) == ord(13L),
      StateManager.selectNextGlobalSnapshotCursor(
        ord(740000L),
        evidenceComplete = true,
        ord(13L),
        ord(11L)
      ) == ord(13L)
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

  pureTest("governance month-boundary ordering changes with the same coordinated activation") {
    expect.all(
      ProtocolActivation.governanceMonthBoundaryFix == ProtocolActivation.evidenceCompletenessFirst,
      !ProtocolActivation.governanceMonthBoundaryFixActive(ord(739999L)),
      ProtocolActivation.governanceMonthBoundaryFixActive(ord(740000L))
    )
  }

  pureTest("the cursor hold is bounded, so it cannot starve the monitoring service into a restart loop") {
    val head = ord(6856623L)
    val stuck = ord(6855921L)
    val recent = ord(6856600L)

    expect.all(
      // Within the bound the hold still works: this is what keeps unresolved evidence reachable.
      StateManager.selectNextGlobalSnapshotCursor(ord(740000L), evidenceComplete = false, head, recent) == recent,
      // Past it the cursor is released. On 2026-08-31 it sat at 6855921 while the head ran to
      // 6856623, the monitoring service saw no progress in that exact value, and force-restarted
      // all three nodes thirteen times. Each restart re-emptied the cache that caused the hold.
      head.value.value - stuck.value.value > StateManager.maxCursorHoldOrdinals,
      StateManager.selectNextGlobalSnapshotCursor(ord(740000L), evidenceComplete = false, head, stuck) == head
    )
  }

  pureTest("a scan that started after an operation was generated can never expire it") {
    val generatedAt = ord(6855900L)

    expect.all(
      // The 741789 case: the cursor had moved above the point the operation was generated under, so
      // the scan could not contain its acceptance. Finding nothing there proves nothing.
      !PendingOperationsProcessor.evidenceCoversOperation(ord(6856000L), Some(generatedAt)),
      // A scan reaching back to or below that point is real evidence.
      PendingOperationsProcessor.evidenceCoversOperation(generatedAt, Some(generatedAt)),
      PendingOperationsProcessor.evidenceCoversOperation(ord(6855800L), Some(generatedAt)),
      // Unknown provenance, from a state written before the field existed, is not provable either.
      !PendingOperationsProcessor.evidenceCoversOperation(ord(1L), None)
    )
  }

  pureTest("coverage gates expiry only once the gate is active, so signed history is untouched") {
    val covered = Set(1)
    def sel(o: Long) =
      PendingOperationsProcessor.selectPendingSpendActions(
        ord(o),
        evidenceComplete = true,
        readReturnedActions = false,
        pending,
        (i: Int) => covered.contains(i)
      )(_ => true)

    expect.all(
      // Active: only the operation whose lifetime the scan covered may be expired.
      sel(740000L) == SortedSet(1),
      // Below the gate the old behaviour is reproduced exactly, uncovered operation included.
      sel(739999L) == pending
    )
  }
}
