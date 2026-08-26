package org.amm_metagraph.shared_data.services.combiners

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.ext.cats.syntax.next.catsSyntaxNext
import io.constellationnetwork.security.signature.Signed

import org.amm_metagraph.shared_data.types.DataUpdates.AmmUpdate
import org.amm_metagraph.shared_data.types.States.{AmmCalculatedState, AmmOnChainState}
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

trait L0CombinerService[F[_]] {
  def combine(
    oldState: DataState[AmmOnChainState, AmmCalculatedState],
    updates: List[Signed[AmmUpdate]]
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]]
}

object L0CombinerService {
  def make[F[_]: Async](
    stateManager: StateManager[F],
    updateProcessor: NewUpdatesProcessor[F],
    pendingOperationsProcessor: PendingOperationsProcessor[F],
    oneTimeFixesHandler: OneTimeFixesHandler[F],
    contextHelper: ContextHelper[F],
    collateralInvariant: CollateralInvariant[F]
  ): L0CombinerService[F] = new L0CombinerService[F] {

    val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

    override def combine(
      oldState: DataState[AmmOnChainState, AmmCalculatedState],
      incomingUpdates: List[Signed[AmmUpdate]]
    )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
      val combined = for {
        _ <- logger.info("Starting combine function")
        currencySnapshotOpt <- context.getLastCurrencySnapshotCombined

        result <- currencySnapshotOpt match {
          case Some((lastCurrencySnapshot, lastCurrencySnapshotInfo)) =>
            val currentSnapshotOrdinal = lastCurrencySnapshot.ordinal.next

            for {
              _ <- logger.info(s"currentSnapshotOrdinal=$currentSnapshotOrdinal")

              oneTimeFixesResult <- oneTimeFixesHandler.handleOneTimeFixesOrdinals(
                oldState,
                currentSnapshotOrdinal
              )
              result <- oneTimeFixesResult match {
                case Some(specialState) => specialState.pure[F]
                case None =>
                  for {
                    processingContext <- contextHelper.buildProcessingContext(
                      lastCurrencySnapshot,
                      lastCurrencySnapshotInfo,
                      oldState
                    )
                    preparedState <- stateManager.prepareStateForNewOrdinal(
                      oldState,
                      processingContext
                    )
                    processedState <- updateProcessor.processIncomingUpdates(
                      preparedState,
                      incomingUpdates,
                      processingContext
                    )
                    finalState <- pendingOperationsProcessor.processPendingOperations(
                      processedState,
                      processingContext
                    )
                    cleanedState <- stateManager.cleanupAndFinalize(
                      finalState,
                      processingContext
                    )
                    // The book must always equal the wallet. Checked every snapshot, so a
                    // divergence is visible in one snapshot rather than after months.
                    // Observability only: it must never be able to fail the combine it observes.
                    _ <- collateralInvariant.check(cleanedState, processingContext)
                  } yield cleanedState
              }
            } yield result

          case None =>
            logger.warn("lastCurrencySnapshot unavailable, returning current state unchanged") *>
              oldState.pure[F]
        }
      } yield result
      combined.handleErrorWith { e =>
        val updateHashes = incomingUpdates.map(_.value.getClass.getSimpleName)
        logger
          .error(e)(
            s"COMBINE_FAILED: dropping ${incomingUpdates.size} update(s) $updateHashes and returning previous state. " +
              s"If this error is non-deterministic across nodes it WILL fork consensus — investigate immediately."
          )
          .as(oldState)
      }
    }
  }
}
