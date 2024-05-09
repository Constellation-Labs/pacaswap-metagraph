package org.amm_metagraph.shared_data.calculated_state

import cats.effect.Ref
import cats.effect.kernel.Async
import cats.syntax.functor._
import io.circe.syntax.EncoderOps
import org.amm_metagraph.shared_data.types.States.AmmCalculatedState
import org.tessellation.schema.SnapshotOrdinal
import org.tessellation.security.hash.Hash

import java.nio.charset.StandardCharsets

trait CalculatedStateService[F[_]] {
  def get: F[CalculatedState]

  def update(
    snapshotOrdinal: SnapshotOrdinal,
    state          : AmmCalculatedState
  ): F[Boolean]

  def hash(
    state: AmmCalculatedState
  ): F[Hash]
}

object CalculatedStateService {
  def make[F[_] : Async]: F[CalculatedStateService[F]] = {
    Ref.of[F, CalculatedState](CalculatedState.empty).map { stateRef =>
      new CalculatedStateService[F] {
        override def get: F[CalculatedState] = stateRef.get

        override def update(
          snapshotOrdinal: SnapshotOrdinal,
          state          : AmmCalculatedState
        ): F[Boolean] =
          stateRef.update { currentState =>
            val currentCalculatedState = currentState.state
            val updatedDevices = state.addresses.foldLeft(currentCalculatedState.addresses) {
              case (acc, (address, value)) =>
                acc.updated(address, value)
            }

            CalculatedState(snapshotOrdinal, AmmCalculatedState(updatedDevices))
          }.as(true)

        override def hash(
          state: AmmCalculatedState
        ): F[Hash] = Async[F].delay {
          val jsonState = state.asJson.deepDropNullValues.noSpaces
          Hash.fromBytes(jsonState.getBytes(StandardCharsets.UTF_8))
        }
      }
    }
  }
}
