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

  /* Three corrections activate together at 740000: the seven-lock incident remediation below, plus
   * evidenceCompletenessFirst and governanceMonthBoundaryFix further down. They ship in one binary
   * and one coordinated upgrade, so one ordinal is correct - splitting them would mean two
   * coordinated deploys for no benefit. It also means a node missing this build diverges on three
   * counts at once, which makes the upgrade non-optional rather than merely advisable.
   */

  /** The calculated state was at currency ordinal 736006 when this remediation was prepared. At the nominal ~43s snapshot cadence the 3994
    * ordinals to 740000 are roughly 47 hours, so this leaves under two days for every ML0 node to deploy the new binary while remaining
    * well ahead of the month-13 governance freeze at metagraph epoch 604800. The activation removes only the seven incident TokenLocks
    * listed in IncidentTokenLockRemediation; all other state owned by the same addresses remains intact.
    */
  val incidentTokenLockRemediation: SnapshotOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(740000L))

  def incidentTokenLockRemediationActive(ordinal: SnapshotOrdinal): Boolean =
    ordinal.value.value >= incidentTokenLockRemediation.value.value

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

  /** The collateral invariant stops observing and starts refusing.
    *
    * On 2026-08-31 at ordinal 741789 the combine rolled back two swaps that had already settled on the global ledger, and built the
    * snapshot anyway. The invariant SAW it - `COLLATERAL_INVARIANT BREACH ordinal=741800 ledger=DAG` is in the node log - but it sampled
    * one ordinal in fifty and only warned, so the divergence became history. From this ordinal it runs on every snapshot and raises, which
    * means a combine that would leave the book disagreeing with the wallet produces no snapshot at all.
    *
    * The cost is explicit: a false positive halts the chain rather than corrupting it. That trade is only defensible because the check nets
    * in-flight value from the metagraph's own pending spend actions, and because it was run in report-only mode on every ordinal first to
    * confirm it stays silent under normal trading.
    */
  val collateralInvariantEnforced: SnapshotOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(760000L))

  def collateralInvariantEnforcedActive(ordinal: SnapshotOrdinal): Boolean =
    ordinal.value.value >= collateralInvariantEnforced.value.value

  /** Restores the book after the 741789 rollback. See IncidentSwapRollbackCorrection.
    *
    * Prepared against live ordinal 743536 on 2026-09-01. At the nominal ~43s cadence, 747000 leaves about 41 hours for CI and the
    * coordinated full-cluster rollout. This exact ordinal MUST be checked against the live head immediately before deployment; if it can no
    * longer be reached by every node on the new binary, move it forward before publishing the release.
    */
  val swapRollbackCorrection: SnapshotOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(747000L))

  def swapRollbackCorrectionActive(ordinal: SnapshotOrdinal): Boolean =
    ordinal.value.value >= swapRollbackCorrection.value.value
}
