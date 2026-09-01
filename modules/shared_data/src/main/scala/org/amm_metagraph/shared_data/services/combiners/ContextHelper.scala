package org.amm_metagraph.shared_data.services.combiners

import cats.data.OptionT
import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.currency.schema.currency.{CurrencyIncrementalSnapshot, CurrencySnapshotInfo}
import io.constellationnetwork.ext.cats.syntax.next.catsSyntaxNext
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.security.Hashed

import org.amm_metagraph.shared_data.ProtocolActivation
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
  private[shared_data] def selectCurrentSnapshotOrdinal(
    contextPredecessorOrdinal: SnapshotOrdinal,
    lastProcessedCurrencyOrdinal: Option[SnapshotOrdinal]
  ): SnapshotOrdinal = {
    val contextFrame = contextPredecessorOrdinal.next

    // v3.5.29 exposes the live currency head while DataApplicationTraverse replays historical
    // states, so the context can be far ahead of the state being rebuilt. Conversely, live
    // acceptance calls combine once per data block: after block one the chained state is one frame
    // ahead while the context still identifies the same snapshot. The earlier frame is correct in
    // both cases.
    lastProcessedCurrencyOrdinal
      .map(_.next)
      .fold(contextFrame)(stateFrame => if (stateFrame < contextFrame) stateFrame else contextFrame)
  }

  def make[F[_]: Async](
    globalSnapshotsStorage: GlobalSnapshotsStorage[F],
    globalSyncDataIntegrityActivation: EpochProgress = EpochProgress.MaxValue
  ): ContextHelper[F] = new ContextHelper[F] {

    val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

    override def buildProcessingContext(
      lastCurrencySnapshot: Hashed[CurrencyIncrementalSnapshot],
      lastCurrencySnapshotInfo: CurrencySnapshotInfo,
      state: DataState[AmmOnChainState, AmmCalculatedState]
    )(implicit context: L0NodeContext[F]): F[ProcessingContext] =
      for {
        lastSyncGlobalSnapshotOpt <- context.getLastSynchronizedGlobalSnapshot
        (lastSyncGlobalEpochProgress, lastSyncGlobalOrdinal, fallbackSnapshot) <- OptionT(
          lastSyncGlobalSnapshotOpt.pure[F]
        ).map(snapshot => (snapshot.epochProgress, snapshot.ordinal, snapshot.some)).getOrElseF {
          val message = "Could not get last synchronized global snapshot data"
          logger.error(message) >> Async[F].raiseError(new Exception(message))
        }
        globalSnapshotSyncAllowSpends <- OptionT(
          context.getLastSynchronizedAllowSpends
        ).getOrElseF {
          val message = "Could not get last synchronized allow spends"
          logger.error(message) >> Async[F].raiseError(new Exception(message))
        }

        currentSnapshotOrdinal = selectCurrentSnapshotOrdinal(
          lastCurrencySnapshot.ordinal,
          state.calculated.lastProcessedCurrencyOrdinal
        )
        currentSnapshotEpochProgress = lastCurrencySnapshot.epochProgress.next

        _ <- logger.info(s"lastSyncGlobalEpochProgress=$lastSyncGlobalEpochProgress")
        _ <- logger.info(s"lastSyncGlobalOrdinal=$lastSyncGlobalOrdinal")

        spendActionsRead <- getSpendActionsFromGlobalSnapshots(
          state.calculated.lastSyncGlobalSnapshotOrdinal,
          lastSyncGlobalOrdinal,
          globalSnapshotsStorage,
          // Gated: below the activation ordinal the cold-cache read stays empty, so all
          // existing history replays exactly as it was recorded.
          fallbackSnapshot =
            if (ProtocolActivation.reserveAccountingFixesActive(currentSnapshotOrdinal)) fallbackSnapshot
            else None,
          // Independent of the above and currently disabled in application.conf: this halts the combine
          // instead of finalizing a divergent state. It stays wired so the coordinated re-activation only
          // needs a config change once GlobalSnapshotsStorage backfills on startup.
          failOnMissing = currentSnapshotEpochProgress >= globalSyncDataIntegrityActivation
        )

        _ <-
          if (
            !spendActionsRead.complete &&
            ProtocolActivation.evidenceCompletenessFirstActive(currentSnapshotOrdinal)
          )
            logger.warn(
              s"Spend-action evidence INCOMPLETE for range " +
                s"${state.calculated.lastSyncGlobalSnapshotOrdinal.show}..${lastSyncGlobalOrdinal.show}. " +
                "No pending operation will be expired and the global evidence cursor will stop at " +
                s"${spendActionsRead.lastContiguousGlobalSnapshotOrdinal.show}."
            )
          else Async[F].unit

        lastSyncGlobalSnapshotInfo <- context.getLastSynchronizedGlobalSnapshotCombined.map(_.map(_._2))

        currencyId <- context.getCurrencyId

      } yield
        ProcessingContext(
          lastSyncGlobalEpochProgress = lastSyncGlobalEpochProgress,
          lastSyncGlobalOrdinal = lastSyncGlobalOrdinal,
          currentSnapshotEpochProgress = currentSnapshotEpochProgress,
          currentSnapshotOrdinal = currentSnapshotOrdinal,
          globalSnapshotSyncAllowSpends = globalSnapshotSyncAllowSpends,
          globalSnapshotsSyncSpendActions = spendActionsRead.actions,
          spendActionsEvidenceComplete =
            // Pre-activation the flag is forced true so the old (unsafe) expiry behaviour is
            // reproduced exactly and history replays byte for byte.
            !ProtocolActivation.reserveAccountingFixesActive(currentSnapshotOrdinal) || spendActionsRead.complete,
          lastContiguousGlobalSnapshotOrdinal = spendActionsRead.lastContiguousGlobalSnapshotOrdinal,
          currencyId = currencyId,
          lastCurrencySnapshot = lastCurrencySnapshot,
          lastCurrencySnapshotInfo = lastCurrencySnapshotInfo,
          lastSyncGlobalSnapshotInfo = lastSyncGlobalSnapshotInfo
        )
  }
}
