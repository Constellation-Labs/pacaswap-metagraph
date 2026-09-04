package org.amm_metagraph.shared_data

import scala.collection.immutable.SortedSet

import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.artifact.SpendAction

import eu.timepit.refined.types.all.NonNegLong
import org.amm_metagraph.shared_data.globalSnapshots.summarizeSpendActionsRead
import org.amm_metagraph.shared_data.services.combiners.{ContextHelper, PendingOperationsProcessor, StateManager}
import weaver.SimpleIOSuite

/** Regression coverage for settlement evidence used to confirm or expire pending SpendActions. */
object EvidenceCompletenessSpec extends SimpleIOSuite {

  private def ord(o: Long): SnapshotOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(o))

  // Relative to the gate, never hardcoded: these ordinals are re-pinned against the live head at
  // every release, and a literal here silently turns "below the gate" into "above it".
  private val gate = ProtocolActivation.spendActionEvidenceSafety.value.value
  private val belowGate = gate - 1L

  private val pending = SortedSet(1, 2)

  private def selected(
    ordinal: Long,
    evidenceComplete: Boolean,
    readReturnedActions: Boolean,
    accepted: Set[Int] = Set.empty,
    covered: Int => Boolean = _ => true,
    expired: Int => Boolean = _ => true
  ): SortedSet[Int] =
    PendingOperationsProcessor.selectPendingSpendActions(
      ord(ordinal),
      evidenceComplete,
      readReturnedActions,
      pending,
      accepted.contains,
      covered
    )(expired)

  pureTest("new gate: incomplete evidence can confirm only the exact matching SpendAction") {
    expect.all(
      selected(gate, evidenceComplete = false, readReturnedActions = true, accepted = Set(1)) == SortedSet(1),
      selected(gate, evidenceComplete = false, readReturnedActions = true).isEmpty,
      selected(gate, evidenceComplete = false, readReturnedActions = false).isEmpty
    )
  }

  pureTest("new gate: only a complete read may expire an unmatched operation") {
    expect.all(
      selected(gate, evidenceComplete = true, readReturnedActions = false) == pending,
      selected(
        gate,
        evidenceComplete = true,
        readReturnedActions = true,
        accepted = Set(1),
        expired = _ == 2
      ) == pending,
      selected(gate, evidenceComplete = true, readReturnedActions = true, expired = _ == 2) == SortedSet(2)
    )
  }

  pureTest("new gate: a complete scan still cannot expire an operation whose lifetime it did not cover") {
    expect.all(
      selected(gate, evidenceComplete = true, readReturnedActions = false, covered = _ => false).isEmpty,
      selected(
        gate,
        evidenceComplete = true,
        readReturnedActions = true,
        accepted = Set(1),
        covered = _ => false
      ) == SortedSet(1)
    )
  }

  pureTest("signed history below the new gate keeps the existing selector behavior") {
    expect.all(
      selected(740000L, evidenceComplete = false, readReturnedActions = true, accepted = Set(1)).isEmpty,
      selected(740000L, evidenceComplete = true, readReturnedActions = true) == pending,
      selected(739999L, evidenceComplete = false, readReturnedActions = true) == pending,
      selected(739999L, evidenceComplete = false, readReturnedActions = false).isEmpty
    )
  }

  pureTest("an unresolved middle ordinal stops the contiguous evidence range before the gap") {
    val resolved = (List.empty[SpendAction], true)
    val missing = (List.empty[SpendAction], false)
    val read = summarizeSpendActionsRead(
      ord(10L),
      List(ord(10L) -> resolved, ord(11L) -> resolved, ord(12L) -> missing, ord(13L) -> resolved)
    )

    expect.all(!read.complete, read.lastContiguousGlobalSnapshotOrdinal == ord(11L))
  }

  pureTest("new gate: an incomplete cursor cannot skip pending settlement evidence") {
    val head = ord(6856623L)
    val contiguous = ord(6855921L)

    expect.all(
      StateManager.selectNextGlobalSnapshotCursor(
        ord(gate),
        evidenceComplete = false,
        head,
        contiguous,
        hasPendingSpendActions = true
      ) == contiguous,
      StateManager.selectNextGlobalSnapshotCursor(
        ord(gate),
        evidenceComplete = false,
        head,
        contiguous,
        hasPendingSpendActions = false
      ) == head,
      StateManager.selectNextGlobalSnapshotCursor(
        ord(gate),
        evidenceComplete = true,
        head,
        contiguous,
        hasPendingSpendActions = true
      ) == head
    )
  }

  pureTest("cursor behavior below the new gate is replay-compatible") {
    val head = ord(13L)
    val contiguous = ord(11L)

    expect.all(
      StateManager.selectNextGlobalSnapshotCursor(
        ord(740000L),
        evidenceComplete = false,
        head,
        contiguous,
        hasPendingSpendActions = false
      ) == contiguous,
      StateManager.selectNextGlobalSnapshotCursor(
        ord(739999L),
        evidenceComplete = false,
        head,
        contiguous,
        hasPendingSpendActions = true
      ) == head
    )
  }

  pureTest("the stronger evidence rule has its own future activation") {
    expect.all(
      ProtocolActivation.spendActionEvidenceSafety.value.value > ProtocolActivation.evidenceCompletenessFirst.value.value,
      !ProtocolActivation.spendActionEvidenceSafetyActive(ord(belowGate)),
      ProtocolActivation.spendActionEvidenceSafetyActive(ord(gate))
    )
  }

  pureTest("new gate: the state ordinal wins over a live-context tip during historical replay") {
    expect.all(
      ContextHelper.selectCurrentSnapshotOrdinal(ord(900000L), Some(ord(belowGate))) == ord(gate),
      ContextHelper.selectCurrentSnapshotOrdinal(ord(900000L), Some(ord(739999L))) == ord(740000L),
      ContextHelper.selectCurrentSnapshotOrdinal(ord(900000L), None) == ord(900001L)
    )
  }
}
