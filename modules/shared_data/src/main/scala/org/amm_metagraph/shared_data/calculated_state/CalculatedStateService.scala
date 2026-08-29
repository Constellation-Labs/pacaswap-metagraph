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
import org.amm_metagraph.shared_data.ProtocolActivation
import org.amm_metagraph.shared_data.types.States._

trait CalculatedStateService[F[_]] {
  def get: F[CalculatedState]

  def update(
    snapshotOrdinal: SnapshotOrdinal,
    state: AmmCalculatedState
  ): F[Boolean]

  /** Deterministic consensus proof of the calculated state.
    *
    * IMPORTANT: this MUST cover only state that every validator can reproduce byte-identically from the consensus inputs (previous state +
    * ordered updates) — i.e. the SETTLED `operations.confirmed`. It deliberately EXCLUDES `pending`, `lastSyncGlobalSnapshotOrdinal` and
    * anything derived from each node's GL0 (hypergraph) sync view: at a given metagraph ordinal those legitimately differ across nodes
    * (different sync POV) and only converge later, so hashing them would CAUSE forks rather than prevent them.
    */
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
            val updatedOperations = state.operations.foldLeft(currentCalculatedState.operations) {
              case (acc, (address, value)) =>
                acc.updated(address, value)
            }

            (
              CalculatedState(
                snapshotOrdinal,
                state.copy(operations = updatedOperations)
              ),
              true
            )
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
          // Only the settled, consensus-reproducible operations. See the trait doc for why pending / GL0-sync-derived
          // state must stay OUT of the proof.
          val operations = state.operations.map(_._2.confirmed).toList
          val canonicalData = createCanonicalRepresentation(operations)

          // NOT widened to cover pending/failed/rewards, deliberately.
          //
          // Doing so would catch node divergence in one snapshot instead of letting it drift,
          // which is worth having. But everything that feeds the proof must be provably
          // deterministic, and these are not: `rewardsBuffer.data` is a List whose order comes
          // from how it was built, `RewardInfo` is a plain Map, and `pending` holds an existential
          // element type with no Encoder, so it cannot even be canonicalised without new codecs.
          // sortJsonKeys fixes object-key order but not array order.
          //
          // If the ordering of any one of those differs between nodes, the recomputed proof
          // differs, consensus fails, and the metagraph cannot restart. That is a worse failure
          // than the one being fixed. Widen only after each nested collection has been made
          // provably ordered (SortedMap/SortedSet throughout) and covered by a test that hashes
          // the same state built two different ways and asserts the proofs match.

          val digest = MessageDigest.getInstance("SHA-256")
          val hashBytes = digest.digest(canonicalData.getBytes(StandardCharsets.UTF_8))

          Hash.fromBytes(hashBytes)
        }
      }
    }
}
