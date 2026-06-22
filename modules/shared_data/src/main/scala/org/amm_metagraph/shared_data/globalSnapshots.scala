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

  def getSpendActionsFromGlobalSnapshots[F[_]: Async](
    lastSyncGlobalOrdinal: SnapshotOrdinal,
    currentSyncGlobalOrdinal: SnapshotOrdinal,
    globalSnapshotsStorage: GlobalSnapshotsStorage[F],
    failOnMissing: Boolean = false
  ): F[List[SpendAction]] = {
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
      globalSnapshotsStorage.get(ordinal).map {
        case Some(snapshot) =>
          Right(snapshot.spendActions.fold(List.empty[SpendAction])(_.values.toList.flatten))
        case None =>
          if (failOnMissing) Left(ordinal) else Right(List.empty[SpendAction])
      }
    }.flatMap { results =>
      // Only ordinals STRICTLY ABOVE the lower bound are "critical": the lower bound (lastSyncGlobalOrdinal) was
      // already processed in the previous combine and its acceptances live in oldState, so re-scanning it is
      // idempotent and its absence (e.g. right after a restart) cannot change this ordinal's result. Raising only on
      // genuinely-new missing ordinals catches every fork case while avoiding needless halts.
      val missingCritical = results.collect { case Left(o) if o.value.value > lastSyncGlobalOrdinal.value.value => o }
      if (missingCritical.nonEmpty)
        logger[F].warn(
          s"Global sync data incomplete: missing global snapshots $missingCritical in consensus-agreed range " +
            s"($lastSyncGlobalOrdinal..$currentSyncGlobalOrdinal]; not ready to finalize this ordinal."
        ) *> new IllegalStateException(
          s"Incomplete global-sync data: missing $missingCritical in ($lastSyncGlobalOrdinal..$currentSyncGlobalOrdinal]"
        ).raiseError[F, List[SpendAction]]
      else
        results.collect { case Right(actions) => actions }.flatten.pure[F]
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
