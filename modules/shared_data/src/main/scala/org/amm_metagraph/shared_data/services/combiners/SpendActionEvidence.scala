package org.amm_metagraph.shared_data.services.combiners

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.ext.cats.syntax.next.catsSyntaxNext
import io.constellationnetwork.schema.SnapshotOrdinal

import org.amm_metagraph.shared_data.ProtocolActivation
import org.amm_metagraph.shared_data.types.States.{AmmCalculatedState, AmmOnChainState}

/** Derives settlement provenance from a signed currency snapshot, never from the node's live GL0
  * head.
  *
  * Tessellation 3.5.29 supplies the live node context while rebuilding historical calculated
  * state, so its `getLastCurrencySnapshot` accessor cannot safely be used here. The calculated
  * state carries the exact predecessor currency ordinal; looking that snapshot up by ordinal gives
  * the same signed global view during live consensus and replay. Once active, missing predecessor
  * data fails closed instead of allowing validators to persist different provenance.
  */
object SpendActionEvidence {

  def generatedAfterGlobalOrdinal[F[_]: Async](
    state: DataState[AmmOnChainState, AmmCalculatedState]
  )(implicit context: L0NodeContext[F]): F[Option[SnapshotOrdinal]] =
    state.calculated.lastProcessedCurrencyOrdinal match {
      case Some(expectedPredecessorOrdinal)
          if ProtocolActivation.spendActionEvidenceSafetyActive(expectedPredecessorOrdinal.next) =>
        context.getCurrencySnapshot(expectedPredecessorOrdinal).flatMap {
          case Some(snapshot) if snapshot.ordinal === expectedPredecessorOrdinal =>
            snapshot.signed.value.globalSyncView
              .map(_.ordinal.some.pure[F])
              .getOrElse(
                new IllegalStateException(
                  s"Currency snapshot $expectedPredecessorOrdinal has no signed globalSyncView; " +
                    "refusing to emit a SpendAction without deterministic settlement provenance"
                ).raiseError[F, Option[SnapshotOrdinal]]
              )
          case Some(snapshot) =>
            new IllegalStateException(
              s"Requested predecessor $expectedPredecessorOrdinal but received ${snapshot.ordinal}; " +
                "refusing to emit a SpendAction with mismatched settlement provenance"
            ).raiseError[F, Option[SnapshotOrdinal]]
          case None =>
            new IllegalStateException(
              s"Currency snapshot $expectedPredecessorOrdinal is unavailable; " +
                "refusing to emit a SpendAction without deterministic settlement provenance"
            ).raiseError[F, Option[SnapshotOrdinal]]
        }
      case _ => none[SnapshotOrdinal].pure[F]
    }
}
