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
    globalSnapshotsStorage: GlobalSnapshotsStorage[F]
  ): F[List[SpendAction]] = {
    val ordinals = (lastSyncGlobalOrdinal.value.value to currentSyncGlobalOrdinal.value.value)
      .map(o => SnapshotOrdinal(NonNegLong.unsafeFrom(o)))
      .toList

    ordinals.traverse { ordinal =>
      globalSnapshotsStorage.get(ordinal).map {
        case Some(snapshot) =>
          snapshot.spendActions.fold(List.empty[SpendAction])(_.values.toList.flatten)
        case None =>
          List.empty[SpendAction]
      }
    }.map(_.flatten)
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
