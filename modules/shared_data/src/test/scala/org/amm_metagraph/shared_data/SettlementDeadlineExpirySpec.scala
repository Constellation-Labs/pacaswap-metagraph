package org.amm_metagraph.shared_data

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all._

import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact.{SpendAction, SpendTransaction}
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.{CurrencyId, SwapAmount}
import io.constellationnetwork.security.hash.Hash

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.{NonNegLong, PosLong}
import io.circe.parser.decode
import io.circe.syntax._
import org.amm_metagraph.shared_data.Shared.getFakeSignedUpdate
import org.amm_metagraph.shared_data.types.DataUpdates.SwapUpdate
import org.amm_metagraph.shared_data.types.States.PendingSpendAction
import org.amm_metagraph.shared_data.types.Swap.SwapReference
import weaver.SimpleIOSuite

/** Once a SpendAction is out, the only deadline that matters is the one the global layer enforces.
  *
  * The incident at 756059..756063 (2026-09-08): swap e7912a191a carried `maxValidGsEpochProgress` 2871490 and was emitted at synced global
  * epoch 2871490 against AllowSpend cbe819da, whose `lastValidEpochProgress` had headroom. At 756063 the metagraph read epoch 2871491,
  * judged the swap expired on its own field, and reversed the book. The global layer, which reads only the AllowSpend, settled the same
  * SpendAction at 6887529. These tests pin the pre-activation decisions so history replays, and the post-activation rules that make the
  * metagraph's expiry agree with the global layer's.
  */
object SettlementDeadlineExpirySpec extends SimpleIOSuite {

  private def ep(v: Long): EpochProgress = EpochProgress(NonNegLong.unsafeFrom(v))
  private def ord(v: Long): SnapshotOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(v))
  private val ACTIVE = ProtocolActivation.settlementDeadlineExpiry
  private val BEFORE = ord(ACTIVE.value.value - 1L)

  // The incident figures.
  private val emissionEpoch = ep(2871490L)
  private val swapDeadline = ep(2871490L)
  private val allowSpendDeadline = ep(2871492L)
  private val expiryReadEpoch = ep(2871491L)

  pureTest("the gate is fresh, follows every earlier activation, and the correction follows it before enforcement") {
    expect(ACTIVE.value.value == 760500L, "pinned activation ordinal")
      .and(expect(ACTIVE.value.value > ProtocolActivation.spendActionHeadroom.value.value, "after the AllowSpend headroom gate"))
      .and(expect(ACTIVE.value.value > ProtocolActivation.usdcRollbackCorrection.value.value, "after the previous correction"))
      .and(
        expect(
          ProtocolActivation.usdcSettlementCorrection.value.value == ACTIVE.value.value + 1L,
          "the correction lands one ordinal after the defect is gated off"
        )
      )
      .and(
        expect(
          ProtocolActivation.usdcSettlementCorrection.value.value < ProtocolActivation.collateralInvariantEnforced.value.value,
          "the book is right before the invariant may refuse snapshots"
        )
      )
      .and(
        expect(
          !ProtocolActivation.settlementDeadlineExpiryActive(BEFORE) && ProtocolActivation.settlementDeadlineExpiryActive(ACTIVE),
          "boundary"
        )
      )
  }

  // ------------------------------------------------------------------ pendingExpired

  pureTest("before activation the caller's legacy decision stands: the incident swap was expired") {
    val legacy = swapDeadline < expiryReadEpoch
    expect(legacy, "the legacy rule reads maxValid 2871490 < 2871491 as expired").and(
      expect(
        AllowSpendSettlement.pendingExpired(Some(allowSpendDeadline), expiryReadEpoch, legacyExpired = legacy, BEFORE),
        "and below the gate that decision must be reproduced so 756063 replays as signed"
      )
    )
  }

  pureTest("from activation the incident swap is NOT expired at 2871491, because its AllowSpend lives to 2871492") {
    expect(
      !AllowSpendSettlement.pendingExpired(Some(allowSpendDeadline), expiryReadEpoch, legacyExpired = true, ACTIVE),
      "the legacy decision is ignored; the AllowSpend deadline has not passed"
    ).and(
      expect(
        !AllowSpendSettlement.pendingExpired(Some(allowSpendDeadline), allowSpendDeadline, legacyExpired = true, ACTIVE),
        "at epoch == deadline the global layer still settles, so not expired"
      )
    )
  }

  pureTest("from activation the action expires exactly when the global layer would drop its AllowSpend") {
    expect(
      AllowSpendSettlement.pendingExpired(Some(allowSpendDeadline), ep(2871493L), legacyExpired = false, ACTIVE),
      "lastValid < epoch is the global layer's expiry rule"
    )
  }

  pureTest("from activation a pending action without a recorded settlement deadline is never expired") {
    expect(!AllowSpendSettlement.pendingExpired(None, ep(9999999L), legacyExpired = true, ACTIVE), "fail-closed on None")
  }

  // ------------------------------------------------------------------ emissionExpired

  pureTest("before activation the update's own deadline is not consulted at emission") {
    expect(!AllowSpendSettlement.emissionExpired(swapDeadline, emissionEpoch, BEFORE))
  }

  pureTest("from activation a SpendAction is not emitted unless the update's deadline has settlement headroom") {
    val headroom = AllowSpendSettlement.headroomEpochs
    expect(AllowSpendSettlement.emissionExpired(swapDeadline, emissionEpoch, ACTIVE), "the incident case: zero headroom")
      .and(expect(AllowSpendSettlement.emissionExpired(ep(emissionEpoch.value.value + headroom - 1), emissionEpoch, ACTIVE), "one short"))
      .and(
        expect(
          !AllowSpendSettlement.emissionExpired(ep(emissionEpoch.value.value + headroom), emissionEpoch, ACTIVE),
          "exactly the headroom"
        )
      )
  }

  // ------------------------------------------------------------------ expiryEpoch

  pureTest("before activation the expiry clock is the newest synced epoch") {
    expect(AllowSpendSettlement.expiryEpoch(ep(10L), Some(ep(9L)), BEFORE) == Some(ep(10L)))
      .and(expect(AllowSpendSettlement.expiryEpoch(ep(10L), None, BEFORE) == Some(ep(10L))))
  }

  pureTest("from activation the expiry clock is the epoch the evidence reached, and absent when unknown") {
    expect(AllowSpendSettlement.expiryEpoch(ep(10L), Some(ep(9L)), ACTIVE) == Some(ep(9L)), "evidence one snapshot behind the sync")
      .and(expect(AllowSpendSettlement.expiryEpoch(ep(10L), None, ACTIVE).isEmpty, "no evidence epoch, no expiry"))
  }

  // ------------------------------------------------------------------ persistence

  test("a pending action written before the field existed decodes with no settlement deadline") {
    val update = getFakeSignedUpdate(
      SwapUpdate(
        CurrencyId(Address("DAG7X5idd4aLfp4XC6WQdG1eDfR3LGPVEwtUUB2W")),
        Address("DAG1EDUPtcqksPcpkqgL1YDM9N8CgYhKSvCpu82r"),
        Some(CurrencyId(Address("DAG0S16WDgdAvh8VvroR6MWLdjmHYdzAF5S181xh"))),
        None,
        Hash.empty,
        SwapAmount(PosLong.unsafeFrom(5306141748L)),
        SwapAmount(PosLong.unsafeFrom(1L)),
        None,
        swapDeadline,
        SwapReference.empty
      )
    )
    val action = SpendAction(
      NonEmptyList.one(
        SpendTransaction(
          Some(Hash.empty),
          Some(CurrencyId(Address("DAG0S16WDgdAvh8VvroR6MWLdjmHYdzAF5S181xh"))),
          SwapAmount(PosLong.unsafeFrom(5306141748L)),
          Address("DAG1EDUPtcqksPcpkqgL1YDM9N8CgYhKSvCpu82r"),
          Address("DAG7X5idd4aLfp4XC6WQdG1eDfR3LGPVEwtUUB2W")
        )
      )
    )
    val pending = PendingSpendAction(update, Hash.empty, action, settlementDeadline = Some(allowSpendDeadline))
    val legacyJson = pending.asJson.mapObject(_.remove("settlementDeadline")).noSpaces

    IO.pure(decode[PendingSpendAction[SwapUpdate]](legacyJson)).map { decoded =>
      expect(decoded.isRight, s"legacy JSON must decode, got $decoded")
        .and(expect(decoded.exists(_.settlementDeadline.isEmpty), "a missing field is None, which pendingExpired treats as never expired"))
        .and(
          expect(
            decode[PendingSpendAction[SwapUpdate]](pending.asJson.noSpaces).exists(_.settlementDeadline.contains(allowSpendDeadline)),
            "a present field round-trips"
          )
        )
    }
  }
}
