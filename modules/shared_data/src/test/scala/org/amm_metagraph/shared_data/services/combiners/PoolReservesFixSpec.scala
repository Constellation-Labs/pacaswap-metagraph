package org.amm_metagraph.shared_data.services.combiners

import cats.effect.IO
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.currency.dataApplication.DataState
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact.{AllowSpendExpiration, SharedArtifact}
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.swap.CurrencyId
import io.constellationnetwork.security.hash.Hash

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.{NonNegLong, PosLong}
import fs2.concurrent.SignallingRef
import org.amm_metagraph.shared_data.FeeDistributor
import org.amm_metagraph.shared_data.loaders.PoolReservesLoader
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._
import weaver.SimpleIOSuite

object PoolReservesFixSpec extends SimpleIOSuite {

  private val paca = Address("DAG7X5idd4aLfp4XC6WQdG1eDfR3LGPVEwtUUB2W")
  private val restoreOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(731647L))

  // Counterfactual reserves: where the pool would be had the mint never happened but the ten
  // legitimate purchases had. Slightly below the pre-attack PACA because buyers took some out,
  // slightly above the pre-attack DAG because they paid some in. Restoring the pre-attack figures
  // instead would leave the pool at 274 PACA/DAG, still 65x off, and hand the difference to the
  // first arbitrageur through the door.
  private val targetPaca = 5039524334506729L
  private val targetDag = 1230855384768349L

  /** The PACA/DAG pool as the swaps left it: reserves wrecked, share ledger untouched because no add or withdraw ever settled during the
    * incident.
    */
  private val corruptedPool = LiquidityPool(
    Hash.empty,
    PoolId(paca.value.value),
    TokenInformation(CurrencyId(paca).some, PosLong.unsafeFrom(360348314082000000L)),
    TokenInformation(none, PosLong.unsafeFrom(33440634000000L)),
    Address("DAG62QdFnvW8xX3uGmo6F3yB2CT5i25hZoVmN6za"),
    BigInt(360348314082000000L) * BigInt(33440634000000L),
    PoolShares(
      PosLong.unsafeFrom(952679463L),
      Map(
        Address("DAG62QdFnvW8xX3uGmo6F3yB2CT5i25hZoVmN6za") -> ShareAmount(Amount(PosLong.unsafeFrom(100000000L))),
        Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5") -> ShareAmount(Amount(PosLong.unsafeFrom(494L)))
      )
    ),
    FeeDistributor.empty
  )

  private val stateBeforeFix = DataState(
    AmmOnChainState.empty,
    AmmCalculatedState(
      SortedMap(
        OperationType.LiquidityPool -> LiquidityPoolCalculatedState.empty.copy(confirmed =
          ConfirmedLiquidityPoolCalculatedState.empty.copy(value = SortedMap(corruptedPool.poolId.value -> corruptedPool))
        )
      )
    )
  )

  test("updated-pools-13.json restores the PACA reserve and leaves the share ledger alone") {
    for {
      ordinalR <- SignallingRef.of[IO, SnapshotOrdinal](SnapshotOrdinal.MinValue)
      handler = OneTimeFixesHandler.make[IO](ordinalR)
      result <- handler.handleOneTimeFixesOrdinals(stateBeforeFix, restoreOrdinal)
    } yield {
      val fixed = result.flatMap(
        _.calculated
          .operations(OperationType.LiquidityPool)
          .asInstanceOf[LiquidityPoolCalculatedState]
          .confirmed
          .value
          .get(corruptedPool.poolId.value)
      )

      expect.all(
        result.isDefined,
        fixed.exists(_.tokenA.amount.value == targetPaca),
        // The DAG side is restored in full, backed by the 11,974,147.51 DAG treasury injection.
        // Writing a reserve the metagraph address cannot cover leaves the pool insolvent.
        fixed.exists(_.tokenB.amount.value == targetDag),
        // SwapCalculations prices off k directly instead of deriving it, so a k left over from
        // the corrupted reserves would keep quoting the attack price.
        fixed.exists(_.k == BigInt(targetPaca) * BigInt(targetDag)),
        fixed.exists(_.poolShares == corruptedPool.poolShares),
        fixed.exists(_.owner == corruptedPool.owner)
      )
    }
  }

  test("updated-pools-13.json touches one pool and carries a consistent k") {
    IO.fromTry(PoolReservesLoader.loadReserves("updated-pools-13.json")).map { pools =>
      expect.all(
        pools.size == 1,
        pools.values.forall(p => p.k == BigInt(p.tokenA.amount.value) * BigInt(p.tokenB.amount.value)),
        pools.values.forall(p => p.tokenA.identifier.contains(CurrencyId(paca))),
        pools.values.forall(_.tokenB.identifier.isEmpty)
      )
    }
  }

  test("the fix does not fire on the surrounding ordinals") {
    for {
      ordinalR <- SignallingRef.of[IO, SnapshotOrdinal](SnapshotOrdinal.MinValue)
      handler = OneTimeFixesHandler.make[IO](ordinalR)
      before <- handler.handleOneTimeFixesOrdinals(stateBeforeFix, SnapshotOrdinal(NonNegLong.unsafeFrom(731646L)))
      after <- handler.handleOneTimeFixesOrdinals(stateBeforeFix, SnapshotOrdinal(NonNegLong.unsafeFrom(731648L)))
    } yield expect.all(before.isEmpty, after.isEmpty)
  }

  test("a state without the pool fails loudly rather than skipping the fix") {
    val empty = DataState(
      AmmOnChainState.empty,
      AmmCalculatedState(SortedMap(OperationType.LiquidityPool -> LiquidityPoolCalculatedState.empty))
    )

    for {
      ordinalR <- SignallingRef.of[IO, SnapshotOrdinal](SnapshotOrdinal.MinValue)
      handler = OneTimeFixesHandler.make[IO](ordinalR)
      outcome <- handler.handleOneTimeFixesOrdinals(empty, restoreOrdinal).attempt
    } yield expect(outcome.isLeft)
  }

  test("pending state quoted against the corrupted reserves is dropped") {
    val stale: SortedSet[SharedArtifact] = SortedSet(AllowSpendExpiration(Hash.empty))

    for {
      ordinalR <- SignallingRef.of[IO, SnapshotOrdinal](SnapshotOrdinal.MinValue)
      handler = OneTimeFixesHandler.make[IO](ordinalR)
      result <- handler.handleOneTimeFixesOrdinals(
        stateBeforeFix.copy(sharedArtifacts = stale),
        restoreOrdinal
      )
    } yield
      expect.all(
        result.exists(_.onChain == AmmOnChainState.empty),
        result.exists(_.sharedArtifacts.isEmpty)
      )
  }
}
