package org.amm_metagraph.shared_data.services.combiners

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.currency.schema.currency.{CurrencyIncrementalSnapshot, CurrencySnapshotInfo}
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact.SpendAction
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.{AllowSpend, CurrencyId}
import io.constellationnetwork.schema.{GlobalSnapshotInfo, SnapshotOrdinal}
import io.constellationnetwork.security.Hashed
import io.constellationnetwork.security.signature.Signed

case class ProcessingContext(
  lastSyncGlobalEpochProgress: EpochProgress,
  lastSyncGlobalOrdinal: SnapshotOrdinal,
  currentSnapshotEpochProgress: EpochProgress,
  currentSnapshotOrdinal: SnapshotOrdinal,
  globalSnapshotSyncAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
  globalSnapshotsSyncSpendActions: List[SpendAction],
  // False when the spend-action read could not resolve every ordinal in the range. An empty
  // list then proves nothing, so no pending operation may be expired on the strength of it.
  spendActionsEvidenceComplete: Boolean,
  currencyId: CurrencyId,
  lastCurrencySnapshot: Hashed[CurrencyIncrementalSnapshot],
  lastCurrencySnapshotInfo: CurrencySnapshotInfo,
  // The global ledger state as of the last synchronized global snapshot. Carries the DAG
  // balances map and every currency's last snapshot, which is the only way the metagraph can
  // see the wallet that actually backs its pools. None if it could not be read; the invariant
  // check then reports "unknown" rather than a false pass.
  lastSyncGlobalSnapshotInfo: Option[GlobalSnapshotInfo]
)
