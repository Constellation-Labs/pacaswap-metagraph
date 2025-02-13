package org.amm_metagraph.shared_data

import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.currency.dataApplication.L0NodeContext
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.swap.AllowSpend
import io.constellationnetwork.schema.{GlobalIncrementalSnapshot, GlobalSnapshotInfo}
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hashed, Hasher}

import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object globalSnapshots {
  def logger[F[_]: Async]: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("globalSnapshots")
  def getLastSyncGlobalIncrementalSnapshot[F[_]: Async](implicit context: L0NodeContext[F]): F[Hashed[GlobalIncrementalSnapshot]] =
    context.getLastSynchronizedGlobalSnapshotCombined.flatMap {
      case Some(value) =>
        val (globalIncrementalSnapshot, _) = value
        globalIncrementalSnapshot.pure
      case None =>
        val message = "Could not get last sync global incremental snapshot"
        logger.error(message) >> new Exception(message).raiseError[F, Hashed[GlobalIncrementalSnapshot]]
    }

  def getLastSyncGlobalSnapshotState[F[_]: Async](implicit context: L0NodeContext[F]): F[GlobalSnapshotInfo] =
    context.getLastSynchronizedGlobalSnapshotCombined.flatMap {
      case Some(value) =>
        val (_, snapshotInfo) = value
        snapshotInfo.pure
      case None =>
        val message = "Could not get last sync global snapshot state"
        logger.error(message) >> new Exception(message).raiseError[F, GlobalSnapshotInfo]
    }

  def getAllowSpendLastSyncGlobalSnapshotState[F[_]: Async: Hasher](
    allowSpendHash: Hash
  )(implicit context: L0NodeContext[F]): F[Option[Hashed[AllowSpend]]] = for {
    globalSnapshotInfo <- getLastSyncGlobalSnapshotState
    currencyId <- context.getCurrencyId
    activeAllowSpends = globalSnapshotInfo.activeAllowSpends
      .getOrElse(SortedMap.empty[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]])

    response <- activeAllowSpends.get(currencyId.value.some) match {
      case Some(active) =>
        active.values.flatten.toList.traverse(_.toHashed).map { hashedAllowSpends =>
          hashedAllowSpends.find(_.hash === allowSpendHash)
        }
      case None =>
        none.pure
    }
  } yield response

}
