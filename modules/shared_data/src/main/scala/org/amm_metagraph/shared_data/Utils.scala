package org.amm_metagraph.shared_data

import cats.MonadThrow
import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.SortedSet

import io.constellationnetwork.currency.dataApplication.L0NodeContext
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.swap.{AllowSpend, CurrencyId}
import io.constellationnetwork.schema.{GlobalIncrementalSnapshot, GlobalSnapshotInfo}
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.signature.SignatureProof
import io.constellationnetwork.security.{Hashed, Hasher, SecurityProvider}

import eu.timepit.refined.types.numeric.PosLong
import org.amm_metagraph.shared_data.types.LiquidityPool.{LiquidityPool, PoolId}
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object Utils {
  def logger[F[_]: Async]: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("Utils")

  def toAddress[F[_]: Async: SecurityProvider](proof: SignatureProof): F[Address] =
    proof.id.toAddress

  def buildLiquidityPoolUniqueIdentifier[F[_]: MonadThrow](
    maybeTokenAId: Option[CurrencyId],
    maybeTokenBId: Option[CurrencyId]
  ): F[PoolId] =
    SortedSet(maybeTokenAId, maybeTokenBId).flatten
      .mkString("-")
      .pure[F]
      .ensure(new IllegalArgumentException("You should provide at least one currency token identifier"))(_.nonEmpty)
      .map(PoolId(_))

  // TODO: Remove this in the future, it's just used to mock while context.getLastSynchronizedGlobalSnapshotCombined isn't ready
  private def getLastSynchronizedGlobalSnapshotCombined[F[_]: Async]: F[Option[(Hashed[GlobalIncrementalSnapshot], GlobalSnapshotInfo)]] =
    none.pure[F]

  def getLastSyncGlobalIncrementalSnapshot[F[_]: Async](implicit context: L0NodeContext[F]): F[Hashed[GlobalIncrementalSnapshot]] =
    getLastSynchronizedGlobalSnapshotCombined.flatMap {
      case Some(value) =>
        val (globalIncrementalSnapshot, _) = value
        globalIncrementalSnapshot.pure
      case None =>
        val message = "Could not get last sync global incremental snapshot"
        logger.error(message) >> new Exception(message).raiseError[F, Hashed[GlobalIncrementalSnapshot]]
    }

  def getLastSyncGlobalSnapshotState[F[_]: Async](implicit context: L0NodeContext[F]): F[GlobalSnapshotInfo] =
    getLastSynchronizedGlobalSnapshotCombined.flatMap {
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
    response <- globalSnapshotInfo
      .activeAllowSpends(currencyId.value)
      .values
      .flatten
      .toList
      .traverse(_.toHashed)
      .map { hashedAllowSpends =>
        hashedAllowSpends.find(_.hash === allowSpendHash)
      }
  } yield response

  def getLiquidityPoolByPoolId[F[_]: Async](
    liquidityPoolsCalculatedState: Map[String, LiquidityPool],
    poolId: PoolId
  ): F[LiquidityPool] =
    liquidityPoolsCalculatedState
      .get(poolId.value)
      .fold(
        Async[F].raiseError[LiquidityPool](new IllegalStateException("Liquidity Pool does not exist"))
      )(Async[F].pure)

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
