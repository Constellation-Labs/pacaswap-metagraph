package org.amm_metagraph.shared_data

import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact.SpendAction
import io.constellationnetwork.schema.swap.{AllowSpend, CurrencyId}
import io.constellationnetwork.schema.{GlobalIncrementalSnapshot, GlobalSnapshotInfo, SnapshotOrdinal}
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hashed, Hasher}

import eu.timepit.refined.types.all.NonNegLong
import org.amm_metagraph.shared_data.storages.GlobalSnapshotsStorage

object globalSnapshots {

  def getAllowSpendGlobalSnapshotsState[F[_]: Async: Hasher](
    allowSpendHash: Hash,
    tokenId: Option[CurrencyId],
    lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]]
  ): F[Option[Hashed[AllowSpend]]] = for {
    confirmedAllowSpend <- findAllowSpendInGlobalState(allowSpendHash, tokenId.map(_.value), lastGlobalSnapshotsAllowSpends)
  } yield confirmedAllowSpend

  def getAllowSpendsGlobalSnapshotsState[F[_]: Async: Hasher](
    allowSpendHashA: Hash,
    tokenAId: Option[CurrencyId],
    allowSpendHashB: Hash,
    tokenBId: Option[CurrencyId],
    lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]]
  ): F[(Option[Hashed[AllowSpend]], Option[Hashed[AllowSpend]])] = for {
    confirmedAllowSpendA <- findAllowSpendInGlobalState(allowSpendHashA, tokenAId.map(_.value), lastGlobalSnapshotsAllowSpends)
    confirmedAllowSpendB <- findAllowSpendInGlobalState(allowSpendHashB, tokenBId.map(_.value), lastGlobalSnapshotsAllowSpends)
  } yield (confirmedAllowSpendA, confirmedAllowSpendB)

  def getAllowSpendsFromGlobalSnapshotState(
    globalSnapshotState: GlobalSnapshotInfo
  ) =
    globalSnapshotState.activeAllowSpends
      .getOrElse(SortedMap.empty[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]])

  /** Spend actions read from the global chain, plus whether the read was COMPLETE.
    *
    * `complete = false` means at least one ordinal in the requested range could not be resolved, so an empty or partial `actions` list is
    * not evidence that nothing was accepted. Callers must never conclude "not accepted" from an incomplete read: the spend action may well
    * have settled on the global ledger, and rolling the pool back on that assumption desynchronises the book from the ledger permanently.
    */
  case class SpendActionsRead(actions: List[SpendAction], complete: Boolean)

  def getSpendActionsFromGlobalSnapshots[F[_]: Async](
    lastSyncGlobalOrdinal: SnapshotOrdinal,
    currentSyncGlobalOrdinal: SnapshotOrdinal,
    globalSnapshotsStorage: GlobalSnapshotsStorage[F],
    fallbackSnapshot: Option[GlobalIncrementalSnapshot] = None
  ): F[SpendActionsRead] = {
    val ordinals = (lastSyncGlobalOrdinal.value.value to currentSyncGlobalOrdinal.value.value)
      .map(o => SnapshotOrdinal(NonNegLong.unsafeFrom(o)))
      .toList

    ordinals.traverse { ordinal =>
      globalSnapshotsStorage.get(ordinal).flatMap {
        case Some(snapshot) =>
          (snapshot.spendActions.fold(List.empty[SpendAction])(_.values.toList.flatten), true).pure[F]
        case None =>
          // After a restart/rollback the in-memory cache is empty. Fall back to the last synchronized
          // snapshot (read from disk by LastSyncGlobalSnapshotStorage) when the ordinal matches, so we
          // don't silently miss spend actions and fork from the majority.
          fallbackSnapshot match {
            case Some(s) if s.ordinal === ordinal =>
              (s.spendActions.fold(List.empty[SpendAction])(_.values.toList.flatten), true).pure[F]
            case _ =>
              (List.empty[SpendAction], false).pure[F]
          }
      }
    }.map(rs => SpendActionsRead(rs.flatMap(_._1), rs.forall(_._2)))
  }

  private def findAllowSpendInGlobalState[F[_]: Async: Hasher](
    allowSpendHash: Hash,
    tokenId: Option[Address],
    lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]]
  ): F[Option[Hashed[AllowSpend]]] =
    lastGlobalSnapshotsAllowSpends.get(tokenId).flatTraverse { activeAllowSpends =>
      activeAllowSpends.values.toList.flatten.collectFirstSomeM { activeAllowSpend =>
        activeAllowSpend.toHashed.map { hashed =>
          Option.when(hashed.hash === allowSpendHash)(hashed)
        }
      }
    }
}
