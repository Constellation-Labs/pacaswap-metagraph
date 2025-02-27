package org.amm_metagraph.shared_data

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.L0NodeContext
import io.constellationnetwork.schema.swap.{AllowSpend, CurrencyId}
import io.constellationnetwork.schema.{GlobalIncrementalSnapshot, GlobalSnapshotInfo}
import io.constellationnetwork.security.hash.Hash
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
    allowSpendHash: Hash,
    tokenId: Option[CurrencyId]
  )(implicit context: L0NodeContext[F]): F[Option[Hashed[AllowSpend]]] = for {
    globalSnapshotInfo <- getLastSyncGlobalSnapshotState
    confirmedAllowSpend <- findAllowSpendInGlobalState(allowSpendHash, tokenId, globalSnapshotInfo)
  } yield confirmedAllowSpend

  def getAllowSpendsLastSyncGlobalSnapshotState[F[_]: Async: Hasher](
    allowSpendHashA: Hash,
    tokenAId: Option[CurrencyId],
    allowSpendHashB: Hash,
    tokenBId: Option[CurrencyId]
  )(implicit context: L0NodeContext[F]): F[(Option[Hashed[AllowSpend]], Option[Hashed[AllowSpend]])] = for {
    globalSnapshotInfo <- getLastSyncGlobalSnapshotState
    confirmedAllowSpendA <- findAllowSpendInGlobalState(allowSpendHashA, tokenAId, globalSnapshotInfo)
    confirmedAllowSpendB <- findAllowSpendInGlobalState(allowSpendHashB, tokenBId, globalSnapshotInfo)
  } yield (confirmedAllowSpendA, confirmedAllowSpendB)

  private def findAllowSpendInGlobalState[F[_]: Async: Hasher](
    allowSpendHash: Hash,
    tokenId: Option[CurrencyId],
    globalSnapshotInfo: GlobalSnapshotInfo
  ): F[Option[Hashed[AllowSpend]]] =
    globalSnapshotInfo.activeAllowSpends.flatTraverse { activeAllowSpends =>
      activeAllowSpends.get(tokenId.map(_.value)).flatTraverse { tokenAllowSpends =>
        tokenAllowSpends.values.toList.flatten.collectFirstSomeM { activeAllowSpend =>
          activeAllowSpend.toHashed.map { hashed =>
            Option.when(hashed.hash === allowSpendHash)(hashed)
          }
        }
      }
    }
}
