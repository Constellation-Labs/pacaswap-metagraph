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

  /** Makes settlement resolution operation-specific and prevents the global evidence cursor from skipping an unresolved range while a
    * SpendAction is pending.
    *
    * `evidenceCompletenessFirst` is already part of signed history, so the stronger rule needs a new activation. From this ordinal, an
    * incomplete scan may confirm an operation only when it contains that operation's exact SpendAction. Absence is not evidence, and
    * therefore cannot expire or roll back an operation. The cursor may skip a cold-cache gap only when there are no pending SpendActions
    * whose settlement could be hidden by that gap.
    *
    * PacaSwap deploys a single version to the full ML0 cluster; mixed-version operation is not a supported rollout mode. Set this ordinal
    * far enough ahead for that coordinated rollout.
    */
  val spendActionEvidenceSafety: SnapshotOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(743656L))

  def spendActionEvidenceSafetyActive(ordinal: SnapshotOrdinal): Boolean =
    ordinal.value.value >= spendActionEvidenceSafety.value.value

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

  /** Reserved for switching the collateral check from reporting to refusing.
    *
    * Not read anywhere yet. Refusing a snapshot converts a book discrepancy into a halt, so it is only worth enabling once every-ordinal
    * reporting has shown how often it would trigger under normal trading.
    */
  val collateralInvariantEnforced: SnapshotOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(760000L))

  def collateralInvariantEnforcedActive(ordinal: SnapshotOrdinal): Boolean =
    ordinal.value.value >= collateralInvariantEnforced.value.value

  /** Applies the one-shot SWAP/DAG reserve correction. See IncidentSwapRollbackCorrection.
    *
    * Deliberately AFTER `spendActionEvidenceSafety`. Correcting the book while the defect that corrupted it is still gated off would fix
    * the number and leave the mechanism live: a restart in the window between the two would roll back another settled operation on top of a
    * book we had just declared correct, and the fixed deltas here would then close the old gap and not the new one. Ordering it after means
    * that when the delta lands, the thing that made it necessary can no longer happen. There is a test asserting this.
    *
    * The metagraph was stopped at 743646 on 2026-09-01 before this release, so there is no race against a moving head: 743656 is ten
    * ordinals after the last signed snapshot and is reached within seconds of the cluster coming back. The correction sits one ordinal
    * later so the defect is gated off first.
    *
    * Prepared against stopped head 743646 on 2026-09-01. This exact ordinal MUST be re-checked against the live head immediately before
    * deployment; if it can no longer be reached by every node on the new binary, move it forward before publishing the release.
    */
  val swapRollbackCorrection: SnapshotOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(743657L))

  def swapRollbackCorrectionActive(ordinal: SnapshotOrdinal): Boolean =
    ordinal.value.value >= swapRollbackCorrection.value.value

  /** A failed swap that sold the pool's token B was never rolled back.
    *
    * `rollbackSwap` chose which reserve to restore by testing `tokenA.identifier === swapFromPair` and `tokenB.identifier === swapToPair`
    * only. A swap from token B into token A satisfies neither, so both reserves were returned unchanged, wrapped in a success. This is how
    * the USDC.dag pool came to carry a phantom 50 USDC.dag and -7,036.36031393 DAG at currency ordinal 747127 (2026-09-03). The no-op is
    * now part of signed history and must replay as recorded, so the corrected rollback is gated here. Chosen at head ~747360; at ~80
    * ordinals an hour, 752000 is roughly two days out. Re-check against the live head immediately before deployment.
    *
    * Activate only once every metagraph L0 node runs a build containing it.
    */
  val rollbackDirectionFix: SnapshotOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(752000L))

  def rollbackDirectionFixActive(ordinal: SnapshotOrdinal): Boolean =
    ordinal.value.value >= rollbackDirectionFix.value.value

  /** Applies the one-shot USDC.dag/DAG reserve correction. See IncidentUsdcRollbackCorrection.
    *
    * One ordinal after `rollbackDirectionFix`, for the same reason `swapRollbackCorrection` follows `spendActionEvidenceSafety`: the defect
    * that corrupted the book must be gated off before the number is fixed, or a second failed token B -> token A swap in the window would
    * leave a fresh gap that these fixed deltas would not close. Before `collateralInvariantEnforced`, so the book is right before the
    * invariant may refuse snapshots on it. There is a test asserting both orderings.
    */
  val usdcRollbackCorrection: SnapshotOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(rollbackDirectionFix.value.value + 1L))

  def usdcRollbackCorrectionActive(ordinal: SnapshotOrdinal): Boolean =
    ordinal.value.value >= usdcRollbackCorrection.value.value
}
