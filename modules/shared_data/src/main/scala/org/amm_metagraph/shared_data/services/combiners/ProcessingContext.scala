package org.amm_metagraph.shared_data.services.combiners

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.currency.schema.currency.{CurrencyIncrementalSnapshot, CurrencySnapshotInfo}
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact.SpendAction
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.{AllowSpend, CurrencyId}
import io.constellationnetwork.security.Hashed
import io.constellationnetwork.security.signature.Signed

case class ProcessingContext(
  lastSyncGlobalEpochProgress: EpochProgress,
  lastSyncGlobalOrdinal: SnapshotOrdinal,
  currentSnapshotEpochProgress: EpochProgress,
  currentSnapshotOrdinal: SnapshotOrdinal,
  globalSnapshotSyncAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
  globalSnapshotsSyncSpendActions: List[SpendAction],
  currencyId: CurrencyId,
  lastCurrencySnapshot: Hashed[CurrencyIncrementalSnapshot],
  lastCurrencySnapshotInfo: CurrencySnapshotInfo
)
