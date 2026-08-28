package org.amm_metagraph.shared_data

import io.constellationnetwork.schema.SnapshotOrdinal

import eu.timepit.refined.types.all.NonNegLong

/** Ordinal gates for behaviour changes that alter the calculated state.
  *
  * The calculated-state proof committed in every currency snapshot is derived from the confirmed operations only (see
  * CalculatedStateService.hash). A node that reconstructs from genesis replays every ordinal and accepts a state only where the recomputed
  * hash equals the proof recorded at that ordinal (DataApplicationTraverse). So any fix that changes what lands in `confirmed` would make
  * historical ordinals unreplayable — the chain would no longer be able to rebuild itself.
  *
  * Fixes that change confirmed state are therefore gated here: below the activation ordinal the original — including buggy — behaviour is
  * preserved exactly, so all existing history replays byte for byte; from the activation ordinal onwards the corrected behaviour applies.
  *
  * Never move an activation ordinal once it is released. Add a new one.
  */
object ProtocolActivation {

  /** The metagraph stopped at currency ordinal 731646. 731647 is the first ordinal it produces on restart, so everything already on chain
    * replays under the old behaviour and every new ordinal gets the fixes.
    */
  val reserveAccountingFixes: SnapshotOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(731647L))

  def reserveAccountingFixesActive(ordinal: SnapshotOrdinal): Boolean =
    ordinal.value.value >= reserveAccountingFixes.value.value

  /** Partial spend-action evidence must stop counting as complete evidence.
    *
    * `reserveAccountingFixes` cannot carry this: it activated at 731647 and the chain is already well past it, so changing behaviour under
    * it would make the ordinals produced since the restart unreplayable. This is a second, later gate, and it is deliberately far enough
    * ahead that every node can be upgraded before it takes effect. The original 736000 activation was not released before the chain
    * approached it, so it was superseded before activation. At ~80 ordinals an hour, 740000 is roughly two days out from when it was
    * chosen.
    *
    * Activate only once every metagraph L0 node runs a build containing it. A node that has not upgraded will decide differently at this
    * ordinal and fork.
    */
  val evidenceCompletenessFirst: SnapshotOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(740000L))

  def evidenceCompletenessFirstActive(ordinal: SnapshotOrdinal): Boolean =
    ordinal.value.value >= evidenceCompletenessFirst.value.value

  /** Roll an expired governance month before applying updates from the first epoch of the next month.
    *
    * This ships in the same coordinated, not-yet-released upgrade as `evidenceCompletenessFirst`, so both corrections intentionally share
    * an activation ordinal. Below it, month expiration remains after incoming-update processing to preserve historical calculated-state
    * proofs. From it onwards, a vote accepted at the month boundary belongs to the new month instead of being excluded from the closing
    * result and then cleared.
    */
  val governanceMonthBoundaryFix: SnapshotOrdinal = evidenceCompletenessFirst

  def governanceMonthBoundaryFixActive(ordinal: SnapshotOrdinal): Boolean =
    ordinal.value.value >= governanceMonthBoundaryFix.value.value
}
