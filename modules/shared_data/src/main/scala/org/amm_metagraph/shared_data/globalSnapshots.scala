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
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object globalSnapshots {

  def logger[F[_]: Async]: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("globalSnapshots")

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
    fallbackSnapshot: Option[GlobalIncrementalSnapshot] = None,
    failOnMissing: Boolean = false
  ): F[SpendActionsRead] = {
    val ordinals = (lastSyncGlobalOrdinal.value.value to currentSyncGlobalOrdinal.value.value)
      .map(o => SnapshotOrdinal(NonNegLong.unsafeFrom(o)))
      .toList

    // D1-01: the [lastSync..currentSync] range is consensus-agreed (derived from the consensus-validated
    // globalSyncView), but the per-ordinal DATA is read from a node-local, non-persistent, forward-only cache.
    // A node whose cache is missing any ordinal in the range would silently compute FEWER accepted spend actions,
    // hence a different operations.confirmed and a different calculated-state proof hash than the majority -> fork.
    // When failOnMissing is active we instead raise, so the node does NOT finalize a divergent state: combine falls
    // back to oldState and retries once the missing global snapshots have been (re)pulled/loaded. Nodes WITH complete
    // data are unaffected, so the majority's hash is identical either way (the flip is for coordinated rollout).
    ordinals.traverse { ordinal =>
      globalSnapshotsStorage
        .get(ordinal)
        .flatMap {
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
        .map(r => (ordinal, r))
    }.flatMap { results =>
      // Only ordinals STRICTLY ABOVE the lower bound are "critical" for the raise: the lower bound
      // (lastSyncGlobalOrdinal) was already processed in the previous combine and its acceptances live in
      // oldState, so re-scanning it is idempotent and its absence right after a restart cannot change this
      // ordinal's result. `complete` below is deliberately stricter and counts every unresolved ordinal,
      // because it answers a different question: not "may I finalize?" but "may I conclude a SpendAction
      // was NOT accepted?", and for that no unresolved ordinal is tolerable.
      val missingCritical = results.collect {
        case (o, (_, false)) if o.value.value > lastSyncGlobalOrdinal.value.value => o
      }
      val read = SpendActionsRead(results.flatMap(_._2._1), results.forall(_._2._2))

      if (failOnMissing && missingCritical.nonEmpty)
        logger[F].warn(
          s"Global sync data incomplete: missing global snapshots $missingCritical in consensus-agreed range " +
            s"($lastSyncGlobalOrdinal..$currentSyncGlobalOrdinal]; not ready to finalize this ordinal."
        ) *> new IllegalStateException(
          s"Incomplete global-sync data: missing $missingCritical in ($lastSyncGlobalOrdinal..$currentSyncGlobalOrdinal]"
        ).raiseError[F, SpendActionsRead]
      else
        read.pure[F]
    }
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
