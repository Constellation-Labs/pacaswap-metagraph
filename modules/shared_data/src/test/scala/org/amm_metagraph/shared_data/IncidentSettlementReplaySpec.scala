package org.amm_metagraph.shared_data

import cats.effect.{IO, Resource}
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.ext.cats.effect.ResourceIO
import io.constellationnetwork.json.JsonSerializer
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact.SpendAction
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap._
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hasher, KeyPairGenerator, SecurityProvider}

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.{NonNegLong, PosLong}
import org.amm_metagraph.shared_data.DummyL0Context.buildL0NodeContext
import org.amm_metagraph.shared_data.Shared._
import org.amm_metagraph.shared_data.calculated_state.CalculatedStateService
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.services.combiners.operations.SwapCombinerService
import org.amm_metagraph.shared_data.services.pricing.PricingService
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, SwapUpdate}
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.Swap._
import org.amm_metagraph.shared_data.types.codecs.{HasherSelector, JsonWithBase64BinaryCodec}
import org.amm_metagraph.shared_data.validations.Errors.OperationExpired
import org.amm_metagraph.shared_data.validations.SwapValidations
import weaver.MutableIOSuite

/** Replays the 756059..756063 sequence through the real swap combiner, below and above the gate.
  *
  * Incident: swap deadline 2871490, AllowSpend deadline 2871492, SpendAction emitted at synced epoch 2871490, no acceptance in evidence
  * at epoch 2871491. Below the gate the combiner expires and rolls back at 2871491. Above it the same swap is refused at emission for
  * lack of headroom, and a swap that IS emitted survives 2871491 and 2871493 (the last epoch the global layer can still settle it), is
  * expired only at 2871494, and is confirmed whenever its SpendAction shows up in evidence.
  */
object IncidentSettlementReplaySpec extends MutableIOSuite {
  type Res = (Hasher[IO], HasherSelector[IO], SecurityProvider[IO])

  override def sharedResource: Resource[IO, Res] = for {
    sp <- SecurityProvider.forAsync[IO]
    implicit0(j: JsonSerializer[IO]) <- JsonSerializer.forSync[IO].asResource
    h = Hasher.forJson[IO]
    hs = HasherSelector.forSync(h, h)
  } yield (h, hs, sp)

  private def ep(v: Long): EpochProgress = EpochProgress(NonNegLong.unsafeFrom(v))
  private val ACTIVE = ProtocolActivation.settlementDeadlineExpiry
  private val BEFORE = SnapshotOrdinal(NonNegLong.unsafeFrom(ACTIVE.value.value - 1L))

  private val emissionEpoch = ep(2871490L)
  private val allowSpendDeadline = ep(2871492L)

  private val usdc = CurrencyId(Address("DAG0S16WDgdAvh8VvroR6MWLdjmHYdzAF5S181xh"))
  // The test pool helper keys pools by "<a>-<b>", which does not match the id the combiner derives for a native-DAG side, so the
  // pool's other side is an identified token. Nothing under test depends on it being DAG.
  private val other = CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb"))
  private val owner = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")
  // getFakeSignedUpdate only verifies for the fixture's canonical signer, so the trader must be that address.
  private val trader = sourceAddress
  private val metagraph = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAP")

  private case class Rig(
    combiner: SwapCombinerService[IO],
    state: DataState[AmmOnChainState, AmmCalculatedState],
    update: Signed[SwapUpdate],
    allowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
    context: L0NodeContext[IO]
  )

  /** A token/USDC.dag pool (token A = other, token B = USDC.dag) and one USDC.dag -> other swap whose AllowSpend lives to `allowSpendDeadline`. */
  private def rig(swapDeadline: EpochProgress)(implicit res: Res): IO[Rig] = {
    implicit val (h, hs, sp) = res
    val dagSide = TokenInformation(other.some, PosLong.unsafeFrom(toFixedPoint(1000000.0)))
    val usdcSide = TokenInformation(usdc.some, PosLong.unsafeFrom(toFixedPoint(7000.0)))
    val (_, lpState) = buildLiquidityPoolCalculatedState(dagSide, usdcSide, owner)
    val state = DataState(AmmOnChainState.empty, AmmCalculatedState(SortedMap(OperationType.LiquidityPool -> lpState)))
    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      allowSpend = AllowSpend(
        trader, metagraph, usdc.some, SwapAmount(PosLong.MaxValue), AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, io.constellationnetwork.security.hash.Hash.empty), allowSpendDeadline, List.empty
      )
      signedAllowSpend <- Signed.forAsyncHasher[IO, AllowSpend](allowSpend, keyPair).flatMap(_.toHashed[IO])
      update = getFakeSignedUpdate(
        SwapUpdate(
          CurrencyId(metagraph), trader, usdc.some, other.some, signedAllowSpend.hash,
          SwapAmount(PosLong.unsafeFrom(5306141748L)), SwapAmount(PosLong.unsafeFrom(1L)), none, swapDeadline, SwapReference.empty
        )
      )
      allowSpends = SortedMap(usdc.value.some -> SortedMap(trader -> SortedSet(signedAllowSpend.signed)))
      context = buildL0NodeContext[IO](
        keyPair, allowSpends, emissionEpoch, SnapshotOrdinal.MinValue, SortedMap.empty, emissionEpoch, SnapshotOrdinal.MinValue, metagraph
      )
      calc <- CalculatedStateService.make[IO]
      _ <- calc.update(SnapshotOrdinal.MinValue, state.calculated)
      pricing <- PricingService.make[IO](config, calc)
      codec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      combiner = SwapCombinerService.make[IO](config, pricing, SwapValidations.make[IO](config, codec), codec)
    } yield Rig(combiner, state, update, allowSpends, context)
  }

  private def emit(r: Rig, at: SnapshotOrdinal): IO[DataState[AmmOnChainState, AmmCalculatedState]] = {
    implicit val ctx: L0NodeContext[IO] = r.context
    r.combiner.combineNew(r.update, r.state, emissionEpoch, r.allowSpends, at, CurrencyId(metagraph))
  }

  private def pendingOf(s: DataState[AmmOnChainState, AmmCalculatedState]) =
    getPendingSpendActionSwapUpdates(s.calculated).toList

  private def swapState(s: DataState[AmmOnChainState, AmmCalculatedState]) =
    s.calculated.operations(OperationType.Swap).asInstanceOf[SwapCalculatedState]

  private def resolve(r: Rig, emitted: DataState[AmmOnChainState, AmmCalculatedState], epoch: EpochProgress, evidence: List[SpendAction], at: SnapshotOrdinal) = {
    implicit val ctx: L0NodeContext[IO] = r.context
    val p = pendingOf(emitted).headOption.getOrElse(
      throw new AssertionError(s"nothing pending after emission; failed = ${swapState(emitted).failed.toList.map(_.reason)}")
    )
    val pending = PendingSpendAction(r.update, p.updateHash, p.generatedSpendAction, p.pricingTokenInfo, p.generatedAfterGlobalOrdinal, p.settlementDeadline)
    r.combiner.combinePendingSpendAction(pending, emitted.copy(sharedArtifacts = SortedSet.empty), epoch, evidence, at, CurrencyId(metagraph))
  }

  test("below the gate the incident swap is emitted at zero headroom and expired one epoch later, rolling the pool back") { implicit res =>
    for {
      r <- rig(swapDeadline = emissionEpoch)
      emitted <- emit(r, BEFORE)
      pending = pendingOf(emitted)
      poolAfterEmit = getLiquidityPoolCalculatedState(emitted.calculated).confirmed.value.values.head
      failed <- resolve(r, emitted, ep(2871491L), List.empty, BEFORE)
      poolAfterFail = getLiquidityPoolCalculatedState(failed.calculated).confirmed.value.values.head
      poolBefore = getLiquidityPoolCalculatedState(r.state.calculated).confirmed.value.values.head
    } yield
      expect(pending.size == 1, "the SpendAction was emitted") and
        expect(pending.head.settlementDeadline.isEmpty, "no settlement deadline is written below the gate (on-chain bytes unchanged)") and
        expect(poolAfterEmit.tokenB.amount.value > poolBefore.tokenB.amount.value, "USDC.dag credited on emission") and
        expect(swapState(failed).failed.exists(_.reason == OperationExpired(r.update)), "expired at 2871491, the incident") and
        expect(poolAfterFail.tokenA.amount == poolBefore.tokenA.amount && poolAfterFail.tokenB.amount == poolBefore.tokenB.amount, "and rolled back")
  }

  test("from the gate the incident swap is refused at emission for lack of headroom and never touches the reserves") { implicit res =>
    for {
      r <- rig(swapDeadline = emissionEpoch)
      out <- emit(r, ACTIVE)
      poolBefore = getLiquidityPoolCalculatedState(r.state.calculated).confirmed.value.values.head
      poolAfter = getLiquidityPoolCalculatedState(out.calculated).confirmed.value.values.head
    } yield
      expect(pendingOf(out).isEmpty, "no SpendAction emitted") and
        expect(swapState(out).failed.exists(_.reason == OperationExpired(r.update)), "refused as expired at the allow-spend stage") and
        expect(poolAfter.tokenA.amount == poolBefore.tokenA.amount && poolAfter.tokenB.amount == poolBefore.tokenB.amount, "reserves untouched")
  }

  test("from the gate an emitted swap carries the AllowSpend deadline and survives until the global layer could no longer settle it") { implicit res =>
    for {
      r <- rig(swapDeadline = ep(2871499L))
      emitted <- emit(r, ACTIVE)
      pending = pendingOf(emitted)
      still1 <- resolve(r, emitted, ep(2871491L), List.empty, ACTIVE)
      still3 <- resolve(r, emitted, ep(2871493L), List.empty, ACTIVE)
      failed <- resolve(r, emitted, ep(2871494L), List.empty, ACTIVE)
      poolBefore = getLiquidityPoolCalculatedState(r.state.calculated).confirmed.value.values.head
      poolAfterFail = getLiquidityPoolCalculatedState(failed.calculated).confirmed.value.values.head
    } yield
      expect(pending.size == 1 && pending.head.settlementDeadline.contains(allowSpendDeadline), "settlementDeadline = the AllowSpend's lastValidEpochProgress") and
        expect(pendingOf(still1).size == 1 && swapState(still1).failed.isEmpty, "still pending at 2871491, where the incident was reversed") and
        expect(pendingOf(still3).size == 1 && swapState(still3).failed.isEmpty, "still pending at L + 1 = 2871493, which the global layer can still settle") and
        expect(pendingOf(failed).isEmpty && swapState(failed).failed.exists(_.reason == OperationExpired(r.update)), "expired at L + 2") and
        expect(poolAfterFail.tokenA.amount == poolBefore.tokenA.amount && poolAfterFail.tokenB.amount == poolBefore.tokenB.amount, "and rolled back")
  }

  test("from the gate an emitted swap is confirmed when its SpendAction appears in evidence, whatever the epoch") { implicit res =>
    for {
      r <- rig(swapDeadline = ep(2871499L))
      emitted <- emit(r, ACTIVE)
      evidence = emitted.sharedArtifacts.collect { case a: SpendAction => a }.toList
      confirmed <- resolve(r, emitted, ep(2871493L), evidence, ACTIVE)
    } yield
      expect(evidence.size == 1, "one SpendAction was emitted") and
        expect(pendingOf(confirmed).isEmpty && swapState(confirmed).failed.isEmpty, "no longer pending, not failed") and
        expect(swapState(confirmed).confirmed.value.get(trader).exists(_.values.nonEmpty), "confirmed")
  }
}
