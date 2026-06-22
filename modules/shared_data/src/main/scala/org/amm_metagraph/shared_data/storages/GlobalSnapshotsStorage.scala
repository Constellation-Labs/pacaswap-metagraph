package org.amm_metagraph.shared_data.storages

import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.SortedMap

import io.constellationnetwork.schema.{GlobalIncrementalSnapshot, GlobalSnapshotInfo, SnapshotOrdinal}
import io.constellationnetwork.security.Hashed

import fs2.concurrent.SignallingRef

trait GlobalSnapshotsStorage[F[_]] {
  def set(snapshot: GlobalIncrementalSnapshot): F[Unit]
  def get: F[Option[GlobalIncrementalSnapshot]]
  def get(ordinal: SnapshotOrdinal): F[Option[GlobalIncrementalSnapshot]]
}

object GlobalSnapshotsStorage {
  // Retain only the newest N global snapshots. The consensus-agreed sync range (lastSync..current) is always at the
  // TOP of the SortedMap, so keeping the highest N keys can never evict an ordinal the D1-01 integrity check needs,
  // while bounding memory (the cache was previously unbounded). N is large enough to cover any realistic sync lag.
  private val RetentionWindow = 10000

  def make[F[_]: Async]: F[GlobalSnapshotsStorage[F]] =
    SignallingRef
      .of[F, SortedMap[SnapshotOrdinal, GlobalIncrementalSnapshot]](SortedMap.empty)
      .map(make(_))

  def make[F[_]: Async](
    snapshotsR: SignallingRef[F, SortedMap[SnapshotOrdinal, GlobalIncrementalSnapshot]]
  ): GlobalSnapshotsStorage[F] =
    new GlobalSnapshotsStorage[F] {
      def set(snapshot: GlobalIncrementalSnapshot): F[Unit] =
        snapshotsR.update { snapshots =>
          val updated = snapshots.updated(snapshot.ordinal, snapshot)
          if (updated.size > RetentionWindow) updated.takeRight(RetentionWindow) else updated
        }

      def get: F[Option[GlobalIncrementalSnapshot]] = snapshotsR.get.map {
        _.lastOption.map { case (_, snapshot) => snapshot }
      }

      def get(ordinal: SnapshotOrdinal): F[Option[GlobalIncrementalSnapshot]] =
        snapshotsR.get.map(_.get(ordinal))
    }
}
