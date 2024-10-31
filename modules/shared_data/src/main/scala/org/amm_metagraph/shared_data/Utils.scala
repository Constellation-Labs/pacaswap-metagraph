package org.amm_metagraph.shared_data

import cats.MonadThrow
import cats.effect.Async
import cats.syntax.all._

import scala.collection.SortedSet

import io.constellationnetwork.currency.dataApplication.L0NodeContext
import io.constellationnetwork.schema.GlobalSnapshotInfo
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.swap.{AllowSpend, CurrencyId}
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.signature.SignatureProof
import io.constellationnetwork.security.{Hashed, Hasher, SecurityProvider}

import eu.timepit.refined.types.numeric.PosLong
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object Utils {
  def logger[F[_]: Async]: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("Utils")

  def toAddress[F[_]: Async: SecurityProvider](proof: SignatureProof): F[Address] =
    proof.id.toAddress

  def buildLiquidityPoolUniqueIdentifier[F[_]: MonadThrow](
    maybeTokenAId: Option[CurrencyId],
    maybeTokenBId: Option[CurrencyId]
  ): F[String] =
    SortedSet(maybeTokenAId, maybeTokenBId).flatten
      .mkString("-")
      .pure[F]
      .ensure(new IllegalArgumentException("You should provide at least one currency token identifier"))(_.nonEmpty)

  def getLastSyncGlobalSnapshotState[F[_]: Async](implicit context: L0NodeContext[F]): F[GlobalSnapshotInfo] =
    context.getLastSynchronizedGlobalSnapshotCombined.flatMap {
      case Some(value) =>
        val (_, snapshotInfo) = value
        snapshotInfo.pure
      case None =>
        val message = "Could not get last sync global snapshot state"
        logger.error(message) >> new Exception(message).raiseError[F, GlobalSnapshotInfo]
    }

  def getAllowSpendsLastSyncGlobalSnapshotState[F[_]: Async: Hasher](
    tokenAAllowSpend: Hash,
    tokenBAllowSpend: Hash
  )(implicit context: L0NodeContext[F]): F[(Option[Hashed[AllowSpend]], Option[Hashed[AllowSpend]])] = for {
    globalSnapshotInfo <- getLastSyncGlobalSnapshotState
    currencyId <- context.getMetagraphId
    response <- globalSnapshotInfo
      .activeAllowSpends(currencyId)
      .values
      .flatten
      .toList
      .traverse(_.toHashed)
      .map { hashedAllowSpends =>
        (hashedAllowSpends.find(_.hash === tokenAAllowSpend), hashedAllowSpends.find(_.hash === tokenBAllowSpend))
      }
  } yield response

  implicit class PosLongOps(value: Long) {
    def toPosLongUnsafe: PosLong =
      PosLong.unsafeFrom(value)
  }

  implicit class LongOps(value: Long) {
    def fromTokenAmountFormat: Double =
      value / 10e7

    def toTokenAmountFormat: Long =
      (value * 10e7).toLong
  }

  implicit class DoubleOps(value: Double) {
    def toTokenAmountFormat: Long =
      (value * 10e7).toLong
  }
}
