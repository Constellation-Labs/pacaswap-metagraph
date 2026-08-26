package org.amm_metagraph.shared_data

import io.constellationnetwork.schema.SnapshotOrdinal

import eu.timepit.refined.types.all.NonNegLong

/** Ordinal gates for behaviour changes that alter the calculated state.
  *
  * The calculated-state proof committed in every currency snapshot is derived from the confirmed
  * operations only (see CalculatedStateService.hash). A node that reconstructs from genesis replays
  * every ordinal and accepts a state only where the recomputed hash equals the proof recorded at
  * that ordinal (DataApplicationTraverse). So any fix that changes what lands in `confirmed` would
  * make historical ordinals unreplayable — the chain would no longer be able to rebuild itself.
  *
  * Fixes that change confirmed state are therefore gated here: below the activation ordinal the
  * original — including buggy — behaviour is preserved exactly, so all existing history replays
  * byte for byte; from the activation ordinal onwards the corrected behaviour applies.
  *
  * Never move an activation ordinal once it is released. Add a new one.
  */
object ProtocolActivation {

  /** The metagraph stopped at currency ordinal 731646. 731647 is the first ordinal it produces on
    * restart, so everything already on chain replays under the old behaviour and every new ordinal
    * gets the fixes.
    */
  val reserveAccountingFixes: SnapshotOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(731647L))

  def reserveAccountingFixesActive(ordinal: SnapshotOrdinal): Boolean =
    ordinal.value.value >= reserveAccountingFixes.value.value
}
