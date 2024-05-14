package org.amm_metagraph.shared_data.calculated_state

import org.amm_metagraph.shared_data.types.States.AmmCalculatedState
import org.tessellation.schema.SnapshotOrdinal

case class CalculatedState(ordinal: SnapshotOrdinal, state: AmmCalculatedState)

object CalculatedState {
  def empty: CalculatedState =
    CalculatedState(SnapshotOrdinal.MinValue, AmmCalculatedState(Map.empty))
}
