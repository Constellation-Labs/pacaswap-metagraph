package org.amm_metagraph.shared_data.services.combiners

import cats.effect.{IO, Resource}
import cats.syntax.all._

import scala.collection.immutable.SortedMap

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.currency.schema.currency.{CurrencyIncrementalSnapshot, CurrencySnapshotInfo}
import io.constellationnetwork.ext.cats.effect.ResourceIO
import io.constellationnetwork.json.JsonSerializer
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.security._
import io.constellationnetwork.security.signature.Signed

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.NonNegLong
import fs2.concurrent.SignallingRef
import org.amm_metagraph.shared_data.DummyL0Context.buildL0NodeContext
import org.amm_metagraph.shared_data.ProtocolActivation
import org.amm_metagraph.shared_data.types.DataUpdates.AmmUpdate
import org.amm_metagraph.shared_data.types.States._
import weaver.MutableIOSuite

/** A one-time state rewrite is paired with balance artifacts emitted on a different path (`Main.customArtifacts`), and that path already
  * fails closed — it throws rather than emitting nothing.
  *
  * If the combiner swallowed a failure in the state rewrite, the deductions would ship WITHOUT the reserve restoration and the frozen-state
  * purge: a partial snapshot with the balances taken and the corrupted reserves left in place, and no automatic retry. The two must land
  * together, or the snapshot must not be built at all.
  *
  * These tests go through `L0CombinerService.combine`, the production path. Exercising `OneTimeFixesHandler` directly cannot catch this,
  * because the swallow lives in the caller.
  */
object OneTimeFixFailClosedSpec extends MutableIOSuite {

  type Res = (Hasher[IO], SecurityProvider[IO])

  override def sharedResource: Resource[IO, Res] = for {
    sp <- SecurityProvider.forAsync[IO]
    implicit0(j: JsonSerializer[IO]) <- JsonSerializer.forSync[IO].asResource
    h = Hasher.forJson[IO]
  } yield (h, sp)

  private val AMM: Address = Address("DAG7X5idd4aLfp4XC6WQdG1eDfR3LGPVEwtUUB2W")
  private val boom = new RuntimeException("updated-pools-13.json could not be loaded")
  private val emptyState = DataState(AmmOnChainState.empty, AmmCalculatedState())
  private def ord(v: Long) = SnapshotOrdinal(NonNegLong.unsafeFrom(v))

  private def handler(fixOrdinals: Set[Long], raiseAlways: Boolean = false): OneTimeFixesHandler[IO] =
    new OneTimeFixesHandler[IO] {
      def handleOneTimeFixesOrdinals(
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        currentSnapshotOrdinal: SnapshotOrdinal
      ): IO[Option[DataState[AmmOnChainState, AmmCalculatedState]]] =
        if (raiseAlways || fixOrdinals.contains(currentSnapshotOrdinal.value.value)) IO.raiseError(boom)
        else IO.pure(none)

      def isOneTimeFixOrdinal(ordinal: SnapshotOrdinal): Boolean =
        fixOrdinals.contains(ordinal.value.value)
    }

  private def combinerWith(h: OneTimeFixesHandler[IO]): L0CombinerService[IO] = {
    val unreachable = new RuntimeException("must not be reached when the one-time fix raises")
    L0CombinerService.make[IO](
      new StateManager[IO] {
        def prepareStateForNewOrdinal(s: DataState[AmmOnChainState, AmmCalculatedState], c: ProcessingContext) =
          IO.raiseError(unreachable)
        def cleanupAndFinalize(s: DataState[AmmOnChainState, AmmCalculatedState], c: ProcessingContext)(
          implicit l0Context: L0NodeContext[IO]
        ) = IO.raiseError(unreachable)
      },
      new NewUpdatesProcessor[IO] {
        def processIncomingUpdates(
          state: DataState[AmmOnChainState, AmmCalculatedState],
          incomingUpdates: List[Signed[AmmUpdate]],
          context: ProcessingContext
        )(implicit l0Context: L0NodeContext[IO]) = IO.raiseError(unreachable)
      },
      new PendingOperationsProcessor[IO] {
        def processPendingOperations(
          state: DataState[AmmOnChainState, AmmCalculatedState],
          context: ProcessingContext
        )(implicit l0Context: L0NodeContext[IO]) = IO.raiseError(unreachable)
      },
      h,
      new ContextHelper[IO] {
        def buildProcessingContext(
          lastCurrencySnapshot: Hashed[CurrencyIncrementalSnapshot],
          lastCurrencySnapshotInfo: CurrencySnapshotInfo,
          state: DataState[AmmOnChainState, AmmCalculatedState]
        )(implicit context: L0NodeContext[IO]) = IO.raiseError(unreachable)
      },
      new CollateralInvariant[IO] {
        def check(state: DataState[AmmOnChainState, AmmCalculatedState], context: ProcessingContext) = IO.unit
      }
    )
  }

  test("a failing one-time fix must NOT return oldState - the snapshot must fail") { res =>
    implicit val (h, sp) = res
    for {
      kp <- KeyPairGenerator.makeKeyPair[IO]
      // The dummy context puts the last currency snapshot at MinValue, so the next ordinal is 1.
      // Declaring 1 as a fix ordinal exercises the incident branch through the production path.
      ctx = buildL0NodeContext[IO](
        kp,
        SortedMap.empty,
        EpochProgress.MinValue,
        SnapshotOrdinal.MinValue,
        SortedMap.empty,
        EpochProgress.MinValue,
        SnapshotOrdinal.MinValue,
        AMM
      )
      r <- combinerWith(handler(Set(1L))).combine(emptyState, List.empty)(ctx).attempt
    } yield
      expect.all(
        r.isLeft,
        r.swap.exists(_.getMessage.contains("updated-pools-13.json")),
        !r.exists(_ == emptyState)
      )
  }

  test("an ordinary combine still swallows and returns oldState") { res =>
    implicit val (h, sp) = res
    for {
      kp <- KeyPairGenerator.makeKeyPair[IO]
      ctx = buildL0NodeContext[IO](
        kp,
        SortedMap.empty,
        EpochProgress.MinValue,
        SnapshotOrdinal.MinValue,
        SortedMap.empty,
        EpochProgress.MinValue,
        SnapshotOrdinal.MinValue,
        AMM
      )
      // Same failure, but no ordinal is declared a fix ordinal: the ordinary product behaviour is
      // preserved, so the change is scoped to the incident branch only.
      r <- combinerWith(handler(Set.empty, raiseAlways = true)).combine(emptyState, List.empty)(ctx).attempt
    } yield expect.all(r.isRight, r.exists(_ == emptyState))
  }

  test("a failure at the swap rollback correction ordinal must NOT return oldState") { res =>
    implicit val (h, sp) = res
    for {
      kp <- KeyPairGenerator.makeKeyPair[IO]
      ctx = buildL0NodeContext[IO](
        kp,
        SortedMap.empty,
        EpochProgress.MinValue,
        SnapshotOrdinal.MinValue,
        SortedMap.empty,
        EpochProgress.MinValue,
        ord(ProtocolActivation.swapRollbackCorrection.value.value - 1L),
        AMM
      )
      // No OneTimeFixesHandler ordinal is active. The stub StateManager raises after that handler,
      // proving the correction ordinal itself selects the fail-closed branch in the production
      // L0CombinerService wrapper.
      r <- combinerWith(handler(Set.empty)).combine(emptyState, List.empty)(ctx).attempt
    } yield
      expect.all(
        r.isLeft,
        r.swap.exists(_.getMessage.contains("must not be reached")),
        !r.exists(_ == emptyState)
      )
  }

  test("rollback correction failure remains fail-closed after activation until completion is recorded") { _ =>
    val at = ProtocolActivation.swapRollbackCorrection
    val before = Some(ord(at.value.value - 1L))
    val completed = Some(at)

    expect
      .all(
        L0CombinerService.mustFailClosed(Some(at), before, atOneTimeFix = false),
        L0CombinerService.mustFailClosed(Some(ord(at.value.value + 1L)), before, atOneTimeFix = false),
        !L0CombinerService.mustFailClosed(Some(ord(at.value.value + 1L)), completed, atOneTimeFix = false),
        !L0CombinerService.mustFailClosed(Some(ord(at.value.value - 1L)), before, atOneTimeFix = false)
      )
      .pure[IO]
  }

  test("the real handler declares 731647, and only fix ordinals, as one-time") { _ =>
    for {
      ref <- SignallingRef.of[IO, SnapshotOrdinal](SnapshotOrdinal.MinValue)
      real = OneTimeFixesHandler.make[IO](ref)
    } yield
      expect.all(
        real.isOneTimeFixOrdinal(ord(731647L)),
        real.isOneTimeFixOrdinal(ord(111700L)),
        real.isOneTimeFixOrdinal(ord(161148L)),
        real.isOneTimeFixOrdinal(ord(731648L)), // normalization, updated-pools-14
        // The incident token-lock remediation is NOT a one-time fix: it is applied every ordinal by
        // the persistent filter in StateManager.prepareStateForNewOrdinal, so it must not short-circuit
        // the normal snapshot transition.
        !real.isOneTimeFixOrdinal(ProtocolActivation.incidentTokenLockRemediation),
        !real.isOneTimeFixOrdinal(ord(731646L)), // the last ordinal before the stop
        !real.isOneTimeFixOrdinal(ord(731649L)), // and ordinary ordinals after
        !real.isOneTimeFixOrdinal(ord(ProtocolActivation.incidentTokenLockRemediation.value.value - 1L))
      )
  }
}
