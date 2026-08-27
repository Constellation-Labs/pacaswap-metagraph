package org.amm_metagraph.shared_data

import cats.effect.IO
import cats.syntax.all._

import io.constellationnetwork.schema.SnapshotOrdinal

import eu.timepit.refined.types.all.NonNegLong
import io.circe.parser.decode
import io.circe.syntax._
import org.amm_metagraph.shared_data.calculated_state.CalculatedStateService
import org.amm_metagraph.shared_data.types.States._
import weaver.SimpleIOSuite

/** Backward compatibility of the calculated state, and determinism of the proof.
  *
  * A node must be able to rebuild itself from genesis. It replays every ordinal and accepts a state only where the recomputed proof equals
  * the proof recorded in that snapshot. So two things must hold for any schema change: old serialised states must still decode, and the
  * proof must not move for any state that existed before the change.
  */
object CalculatedStateCompatibilitySpec extends SimpleIOSuite {

  private def ord(v: Long) = SnapshotOrdinal(NonNegLong.unsafeFrom(v))

  test("a state serialised before lastProcessedCurrencyOrdinal existed still decodes, as None") {
    // Exactly the shape written by every node up to now: the field is simply absent.
    val legacyJson =
      """{"operations":{},"votingPowers":{},"allocations":{"usersAllocations":{},"monthlyReference":{"firstEpochOfMonth":0,"lastEpochOfMonth":0,"monthReference":0},"frozenUsedUserVotes":{"votes":{},"monthlyReference":{"firstEpochOfMonth":0,"lastEpochOfMonth":0,"monthReference":0},"votingPowerForAddresses":{}}},"lastSyncGlobalSnapshotOrdinal":123,"rewards":{"availableRewards":{"info":{}},"rewardsBuffer":{"data":[]},"withdraws":{"confirmed":{},"pending":{}},"lastProcessedEpoch":0,"distributedRewards":{}}}"""
    IO.pure(decode[AmmCalculatedState](legacyJson)).map { r =>
      expect.all(
        r.isRight,
        r.exists(_.lastProcessedCurrencyOrdinal.isEmpty),
        r.exists(_.lastSyncGlobalSnapshotOrdinal.value.value == 123L)
      )
    }
  }

  test("the field round-trips once populated") {
    val s = AmmCalculatedState(lastProcessedCurrencyOrdinal = Some(ord(731647L)))
    IO.pure(decode[AmmCalculatedState](s.asJson.noSpaces)).map { r =>
      expect(r.exists(_.lastProcessedCurrencyOrdinal.contains(ord(731647L))))
    }
  }

  test("adding the field does NOT move the proof - the whole point of the gate") {
    // If this ever fails, every historical ordinal becomes unreplayable and the metagraph
    // cannot restart. The proof is derived from operations.confirmed only.
    val before = AmmCalculatedState(lastProcessedCurrencyOrdinal = None)
    val afterRestart = AmmCalculatedState(lastProcessedCurrencyOrdinal = Some(ord(731647L)))
    val wayLater = AmmCalculatedState(lastProcessedCurrencyOrdinal = Some(ord(9999999L)))
    for {
      svc <- CalculatedStateService.make[IO]
      h1 <- svc.hash(before)
      h2 <- svc.hash(afterRestart)
      h3 <- svc.hash(wayLater)
    } yield expect.all(h1 == h2, h2 == h3)
  }

  test("the proof is stable across repeated hashing of the same state") {
    val s = AmmCalculatedState(lastProcessedCurrencyOrdinal = Some(ord(731647L)))
    for {
      svc <- CalculatedStateService.make[IO]
      hs <- List.fill(20)(svc.hash(s)).traverse(identity)
    } yield expect(hs.distinct.size == 1)
  }

  test("the proof is independent of which service instance computes it") {
    val s = AmmCalculatedState(lastSyncGlobalSnapshotOrdinal = ord(6815497L))
    for {
      a <- CalculatedStateService.make[IO]
      b <- CalculatedStateService.make[IO]
      ha <- a.hash(s)
      hb <- b.hash(s)
    } yield expect(ha == hb)
  }

  test("fields outside operations.confirmed do not enter the proof") {
    // Documents the current contract precisely, so that widening it later is a deliberate,
    // visible change rather than an accident.
    val base = AmmCalculatedState()
    val differentGlobalOrdinal = AmmCalculatedState(lastSyncGlobalSnapshotOrdinal = ord(999L))
    for {
      svc <- CalculatedStateService.make[IO]
      h1 <- svc.hash(base)
      h2 <- svc.hash(differentGlobalOrdinal)
    } yield expect(h1 == h2)
  }
}
