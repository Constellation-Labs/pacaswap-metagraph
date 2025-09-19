package org.amm_metagraph.shared_data.calculated_state

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import cats.effect.Ref
import cats.effect.kernel.Async
import cats.syntax.all._

import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.security.hash.Hash

import io.circe.syntax.EncoderOps
import io.circe.{Json, JsonObject}
import monocle.syntax.all._
import org.amm_metagraph.shared_data.types.States._

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
            val newState = currentState.focus(_.state).replace(state)
            (newState, true)
          }

        private def sortJsonKeys(json: Json): Json =
          json.arrayOrObject(
            json,
            arr => Json.fromValues(arr.map(sortJsonKeys)),
            obj =>
              Json.fromJsonObject(
                JsonObject.fromIterable(
                  obj.toIterable.toList.sortBy(_._1).map { case (k, v) => k -> sortJsonKeys(v) }
                )
              )
          )

        private def stateToCanonicalString(state: ConfirmedCalculatedState): String = {
          val json = state.asJson.deepDropNullValues
          val sortedJson = sortJsonKeys(json)
          s"${state.getClass.getSimpleName}|${sortedJson.noSpaces}"
        }

        private def createCanonicalRepresentation(operations: List[ConfirmedCalculatedState]): String = {
          val canonicalElements = operations.map(stateToCanonicalString).sorted
          canonicalElements.mkString("||")
        }

        override def hash(
          state: AmmCalculatedState
        ): F[Hash] = Async[F].delay {
          val operations = state.operations.map(_._2.confirmed).toList
          val canonicalData = createCanonicalRepresentation(operations)
          val digest = MessageDigest.getInstance("SHA-256")
          val hashBytes = digest.digest(canonicalData.getBytes(StandardCharsets.UTF_8))

          Hash.fromBytes(hashBytes)
        }
      }
    }
}
