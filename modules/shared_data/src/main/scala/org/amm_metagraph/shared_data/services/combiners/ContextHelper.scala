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
  ): SnapshotOrdinal =
    // The SDK context is the live node context even while v3.5.29 rebuilds historical state.
    // Once the state-carried ordinal exists it must win on BOTH sides of every gate; otherwise
    // replaying a pre-activation snapshot after activation evaluates it as present-day history.
    lastProcessedCurrencyOrdinal.map(_.next).getOrElse(contextPredecessorOrdinal.next)

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

        evidenceLowerBound = PendingOperationsProcessor.pendingSpendActionEvidenceLowerBound(
          currentSnapshotOrdinal,
          state.calculated.lastSyncGlobalSnapshotOrdinal,
          state.calculated
        )

        _ <- logger.info(s"lastSyncGlobalEpochProgress=$lastSyncGlobalEpochProgress")
        _ <- logger.info(s"lastSyncGlobalOrdinal=$lastSyncGlobalOrdinal")

        spendActionsRead <- getSpendActionsFromGlobalSnapshots(
          evidenceLowerBound,
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
                s"${evidenceLowerBound.show}..${lastSyncGlobalOrdinal.show}. " +
                "No unmatched pending operation will be expired. When pending SpendActions remain, " +
                "the global evidence cursor will stop at " +
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
          spendActionsEvidenceLowerBound = evidenceLowerBound,
          lastContiguousGlobalSnapshotOrdinal = spendActionsRead.lastContiguousGlobalSnapshotOrdinal,
          currencyId = currencyId,
          lastCurrencySnapshot = lastCurrencySnapshot,
          lastCurrencySnapshotInfo = lastCurrencySnapshotInfo,
          lastSyncGlobalSnapshotInfo = lastSyncGlobalSnapshotInfo
        )
  }
}
