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
    contextHelper: ContextHelper[F]
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
              _ <- logger.info(s"CURRENT SNAPSHOT ORDINAL: $currentSnapshotOrdinal")

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
                  } yield cleanedState
              }
            } yield result

          case None =>
            logger.warn("lastCurrencySnapshot unavailable, returning current state unchanged") *>
              oldState.pure[F]
        }
      } yield result
      combined.handleErrorWith { e =>
        logger.error(s"Error when combining: ${e.getMessage}").as(oldState)
      }
    }
  }
}
