package org.amm_metagraph.shared_data.services.combiners

import cats.data.OptionT
import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.currency.schema.currency.{CurrencyIncrementalSnapshot, CurrencySnapshotInfo}
import io.constellationnetwork.ext.cats.syntax.next.catsSyntaxNext
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.{GlobalSnapshotInfo, SnapshotOrdinal}
import io.constellationnetwork.security.Hashed

import org.amm_metagraph.shared_data.globalSnapshots._
import org.amm_metagraph.shared_data.storages.GlobalSnapshotsStorage
import org.amm_metagraph.shared_data.types.States.{AmmCalculatedState, AmmOnChainState}
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

trait ContextHelper[F[_]] {
  def buildProcessingContext(
    lastCurrencySnapshot: Hashed[CurrencyIncrementalSnapshot],
    lastCurrencySnapshotInfo: CurrencySnapshotInfo,
    state: DataState[AmmOnChainState, AmmCalculatedState]
  )(implicit context: L0NodeContext[F]): F[ProcessingContext]
}

object ContextHelper {
  def make[F[_]: Async](
    globalSnapshotsStorage: GlobalSnapshotsStorage[F]
  ): ContextHelper[F] = new ContextHelper[F] {

    val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

    override def buildProcessingContext(
      lastCurrencySnapshot: Hashed[CurrencyIncrementalSnapshot],
      lastCurrencySnapshotInfo: CurrencySnapshotInfo,
      state: DataState[AmmOnChainState, AmmCalculatedState]
    )(implicit context: L0NodeContext[F]): F[ProcessingContext] =
      for {
        (lastSyncGlobalEpochProgress, lastSyncGlobalOrdinal, lastSyncState) <- OptionT(
          context.getLastSynchronizedGlobalSnapshotCombined
        ).map {
          case (snapshot, info) => (snapshot.epochProgress, snapshot.ordinal, info)
        }.getOrElseF {
          val message = "Could not get last synchronized global snapshot data"
          logger.error(message) >> Async[F].raiseError(new Exception(message))
        }

        currentSnapshotOrdinal = lastCurrencySnapshot.ordinal.next
        currentSnapshotEpochProgress = lastCurrencySnapshot.epochProgress.next

        _ <- logger.info(s"lastSyncGlobalEpochProgress=$lastSyncGlobalEpochProgress")
        _ <- logger.info(s"lastSyncGlobalOrdinal=$lastSyncGlobalOrdinal")

        globalSnapshotSyncAllowSpends = getAllowSpendsFromGlobalSnapshotState(lastSyncState)
        globalSnapshotsSyncSpendActions <- getSpendActionsFromGlobalSnapshots(
          state.calculated.lastSyncGlobalSnapshotOrdinal,
          lastSyncGlobalOrdinal,
          globalSnapshotsStorage
        )

        currencyId <- context.getCurrencyId

      } yield
        ProcessingContext(
          lastSyncGlobalEpochProgress = lastSyncGlobalEpochProgress,
          lastSyncGlobalOrdinal = lastSyncGlobalOrdinal,
          currentSnapshotEpochProgress = currentSnapshotEpochProgress,
          currentSnapshotOrdinal = currentSnapshotOrdinal,
          globalSnapshotSyncAllowSpends = globalSnapshotSyncAllowSpends,
          globalSnapshotsSyncSpendActions = globalSnapshotsSyncSpendActions,
          currencyId = currencyId,
          lastCurrencySnapshot = lastCurrencySnapshot,
          lastCurrencySnapshotInfo = lastCurrencySnapshotInfo
        )
  }
}
