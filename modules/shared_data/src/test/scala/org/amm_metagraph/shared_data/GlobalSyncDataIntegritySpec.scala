package org.amm_metagraph.shared_data

import cats.effect.IO

import io.constellationnetwork.schema.{GlobalIncrementalSnapshot, SnapshotOrdinal}

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.NonNegLong
import org.amm_metagraph.shared_data.globalSnapshots.getSpendActionsFromGlobalSnapshots
import org.amm_metagraph.shared_data.storages.GlobalSnapshotsStorage
import weaver.SimpleIOSuite

/** D1-01: when the node-local global-snapshot cache is missing an ordinal STRICTLY ABOVE the previously-processed
  * lower bound (inside the consensus-agreed sync range), the node must fail-closed (raise) rather than silently treat
  * it as empty — otherwise it computes a divergent operations.confirmed and forks. A missing lower-bound ordinal is
  * tolerated (already processed; re-scan is idempotent). Gated, so legacy behavior is preserved when inactive.
  */
object GlobalSyncDataIntegritySpec extends SimpleIOSuite {

  private def ord(o: Long): SnapshotOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(o))

  // Storage that has NONE of the requested ordinals (simulates a freshly-restarted / lagging node).
  private val emptyStorage: GlobalSnapshotsStorage[IO] = new GlobalSnapshotsStorage[IO] {
    def set(snapshot: GlobalIncrementalSnapshot): IO[Unit] = IO.unit
    def get: IO[Option[GlobalIncrementalSnapshot]] = IO.pure(None)
    def get(ordinal: SnapshotOrdinal): IO[Option[GlobalIncrementalSnapshot]] = IO.pure(None)
  }

  test("active + a new ordinal (> lower bound) is missing -> fail closed (raises)") {
    getSpendActionsFromGlobalSnapshots[IO](ord(5), ord(7), emptyStorage, failOnMissing = true).attempt.map { r =>
      expect(r.isLeft)
    }
  }

  test("active + only the lower-bound ordinal is missing -> tolerated (no raise), returns empty") {
    // range [5..5]: the single ordinal equals the lower bound, already processed -> not critical
    getSpendActionsFromGlobalSnapshots[IO](ord(5), ord(5), emptyStorage, failOnMissing = true).map { actions =>
      expect(actions.isEmpty)
    }
  }

  test("inactive (legacy) + missing ordinals -> silently returns empty, never raises") {
    getSpendActionsFromGlobalSnapshots[IO](ord(5), ord(7), emptyStorage, failOnMissing = false).map { actions =>
      expect(actions.isEmpty)
    }
  }
}
