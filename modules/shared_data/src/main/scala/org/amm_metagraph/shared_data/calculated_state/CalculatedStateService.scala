package org.amm_metagraph.shared_data.calculated_state

import java.nio.charset.StandardCharsets

import cats.effect.Ref
import cats.effect.kernel.Async
import cats.syntax.all._

import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.security.hash.Hash

import io.circe.syntax.EncoderOps
import org.amm_metagraph.shared_data.types.States.AmmCalculatedState

trait CalculatedStateService[F[_]] {
  def get: F[CalculatedState]

  def update(
    snapshotOrdinal: SnapshotOrdinal,
    state: AmmCalculatedState
  ): F[Boolean]

  def hash(
    state: AmmCalculatedState
  ): F[Hash]
}

object CalculatedStateService {
  def make[F[_]: Async]: F[CalculatedStateService[F]] =
    Ref.of[F, CalculatedState](CalculatedState.empty).map { stateRef =>
      new CalculatedStateService[F] {
        override def get: F[CalculatedState] = stateRef.get

        override def update(
          snapshotOrdinal: SnapshotOrdinal,
          state: AmmCalculatedState
        ): F[Boolean] =
          stateRef.modify { currentState =>
            val currentCalculatedState = currentState.state
            val updatedOperations = state.confirmedOperations.foldLeft(currentCalculatedState.confirmedOperations) {
              case (acc, (address, value)) =>
                acc.updated(address, value)
            }

            val updatedPendingUpdates = currentCalculatedState.pendingUpdates ++ state.pendingUpdates
            val updatedSpendTransactions = currentCalculatedState.spendTransactions ++ state.spendTransactions

            (
              CalculatedState(
                snapshotOrdinal,
                AmmCalculatedState(
                  updatedOperations,
                  updatedPendingUpdates,
                  updatedSpendTransactions,
                  state.votingWeights,
                  state.allocations
                )
              ),
              true
            )
          }

        override def hash(
          state: AmmCalculatedState
        ): F[Hash] = Async[F].delay {
          val jsonState = state.asJson.deepDropNullValues.noSpaces
          Hash.fromBytes(jsonState.getBytes(StandardCharsets.UTF_8))
        }
      }
    }
}
