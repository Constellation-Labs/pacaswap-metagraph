package org.amm_metagraph.shared_data.calculated_state

import scala.collection.immutable.SortedSet

import io.constellationnetwork.schema.SnapshotOrdinal

import org.amm_metagraph.shared_data.types.States.AmmCalculatedState

case class CalculatedState(ordinal: SnapshotOrdinal, state: AmmCalculatedState)

object CalculatedState {
  def empty: CalculatedState =
    CalculatedState(SnapshotOrdinal.MinValue, AmmCalculatedState(Map.empty, SortedSet.empty))
}
