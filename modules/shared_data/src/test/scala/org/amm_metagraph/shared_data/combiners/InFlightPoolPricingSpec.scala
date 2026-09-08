package org.amm_metagraph.shared_data.combiners

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
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hasher, KeyPairGenerator, SecurityProvider}

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.{NonNegLong, PosLong}
import org.amm_metagraph.shared_data.DummyL0Context.buildL0NodeContext
import org.amm_metagraph.shared_data.ProtocolActivation
import org.amm_metagraph.shared_data.Shared._
import org.amm_metagraph.shared_data.calculated_state.CalculatedStateService
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.services.combiners.operations.SwapCombinerService
import org.amm_metagraph.shared_data.services.pricing.PricingService
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, SwapUpdate}
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.Swap.{SwapReference, getSwapCalculatedState}
import org.amm_metagraph.shared_data.types.codecs.{HasherSelector, JsonWithBase64BinaryCodec}
import org.amm_metagraph.shared_data.validations.SwapValidations
import weaver.MutableIOSuite

/** Pins the behaviour of the in-flight pool pricing fix.
  *
  * Before `ProtocolActivation.inFlightPoolPricing`, every swap in a batch was quoted against the pool as of the last committed snapshot
  * while the debit was applied to the in-flight state, so constant-product slippage did not apply within a snapshot and a pool could be
  * drained in one batch. The rest of the suite runs at SnapshotOrdinal.MinValue and therefore exercises the pre-activation path, which must
  * reproduce the original behaviour so history replays.
  */
object InFlightPoolPricingSpec extends MutableIOSuite {
  type Res = (Hasher[IO], HasherSelector[IO], SecurityProvider[IO])

  override def sharedResource: Resource[IO, Res] = for {
    sp <- SecurityProvider.forAsync[IO]
    implicit0(j: JsonSerializer[IO]) <- JsonSerializer.forSync[IO].asResource
    h = Hasher.forJson[IO]
    hs = HasherSelector.forSync(h, h)
  } yield (h, hs, sp)

  private val ACTIVE = SnapshotOrdinal(NonNegLong.unsafeFrom(758500L))
  private val BEFORE = SnapshotOrdinal(NonNegLong.unsafeFrom(758499L))

  pureTest("gate: activates at 758500, never before") {
    expect.all(
      ProtocolActivation.inFlightPoolPricing.value.value == 758500L,
      !ProtocolActivation.inFlightPoolPricingActive(SnapshotOrdinal.MinValue),
      !ProtocolActivation.inFlightPoolPricingActive(BEFORE),
      ProtocolActivation.inFlightPoolPricingActive(ACTIVE),
      ProtocolActivation.inFlightPoolPricingActive(SnapshotOrdinal(NonNegLong.unsafeFrom(999999L)))
    )
  }

  private val primaryToken = TokenInformation(
    CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some,
    PosLong.unsafeFrom(toFixedPoint(1000.0))
  )

  private val pairToken = TokenInformation(
    CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some,
    PosLong.unsafeFrom(toFixedPoint(1000.0))
  )

  private val sourceAddress = Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc")
  private val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")
  private val destinationAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAP")

  private def makeAllowSpend(referenceHash: Hash) = AllowSpend(
    ownerAddress,
    destinationAddress,
    primaryToken.identifier,
    SwapAmount(PosLong.MaxValue),
    AllowSpendFee(PosLong.MinValue),
    AllowSpendReference(AllowSpendOrdinal.first, referenceHash),
    EpochProgress.MaxValue,
    List.empty
  )

  private def makeSwapUpdate(allowSpendReference: Hash, parent: SwapReference = SwapReference.empty) =
    getFakeSignedUpdate(
      SwapUpdate(
        CurrencyId(destinationAddress),
        ownerAddress,
        primaryToken.identifier,
        pairToken.identifier,
        allowSpendReference,
        SwapAmount(PosLong.unsafeFrom(toFixedPoint(100.0))),
        SwapAmount(PosLong.unsafeFrom(toFixedPoint(40.0))),
        none,
        EpochProgress.MaxValue,
        parent
      )
    )

  /** Runs two swaps of 100 A each through `combineNew`, threading the state exactly as the batch processor does and never touching the
    * committed CalculatedStateService Ref. Returns each swap's netReceived and the final pool.
    */
  private def runTwoSwapBatch(ordinal: SnapshotOrdinal)(implicit res: Res) = {
    implicit val (h, hs, sp) = res

    val (poolId, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val state = DataState(
      AmmOnChainState.empty,
      AmmCalculatedState(SortedMap(OperationType.LiquidityPool -> liquidityPoolCalculatedState))
    )

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      signedAllowSpend1 <- Signed.forAsyncHasher[IO, AllowSpend](makeAllowSpend(Hash.empty), keyPair).flatMap(_.toHashed[IO])
      signedAllowSpend2 <- Signed.forAsyncHasher[IO, AllowSpend](makeAllowSpend(Hash("second")), keyPair).flatMap(_.toHashed[IO])

      swapUpdate1 = makeSwapUpdate(signedAllowSpend1.hash)
      swapUpdate2 = makeSwapUpdate(signedAllowSpend2.hash)

      allowSpends = SortedMap(
        primaryToken.identifier.get.value.some ->
          SortedMap(sourceAddress -> SortedSet(signedAllowSpend1.signed, signedAllowSpend2.signed))
      )

      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(
        keyPair,
        allowSpends,
        EpochProgress.MinValue,
        SnapshotOrdinal.MinValue,
        SortedMap.empty,
        EpochProgress.MinValue,
        SnapshotOrdinal.MinValue,
        destinationAddress
      )

      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService <- PricingService.make[IO](config, calculatedStateService)
      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      swapValidations = SwapValidations.make[IO](config, jsonBase64BinaryCodec)
      swapCombinerService = SwapCombinerService.make[IO](config, pricingService, swapValidations, jsonBase64BinaryCodec)

      response1 <- swapCombinerService.combineNew(
        swapUpdate1,
        state,
        EpochProgress.MinValue,
        allowSpends,
        ordinal,
        CurrencyId(destinationAddress)
      )
      response2 <- swapCombinerService.combineNew(
        swapUpdate2,
        response1,
        EpochProgress.MinValue,
        allowSpends,
        ordinal,
        CurrencyId(destinationAddress)
      )

      pendingActions = getSwapCalculatedState(response2.calculated).pending.collect {
        case pending: PendingSpendAction[SwapUpdate] => pending
      }
      netReceived1 = pendingActions
        .find(_.update === swapUpdate1)
        .flatMap(_.pricingTokenInfo.collect { case info: SwapTokenInfo => info })
        .map(_.netReceived.value.value)
      netReceived2 = pendingActions
        .find(_.update === swapUpdate2)
        .flatMap(_.pricingTokenInfo.collect { case info: SwapTokenInfo => info })
        .map(_.netReceived.value.value)

      updatedPool = getLiquidityPoolCalculatedState(response2.calculated).confirmed.value(poolId)
    } yield (netReceived1, netReceived2, updatedPool)
  }

  /** The invariant rollbackSwap depends on: the SwapTokenInfo persisted on the PendingSpendAction at emission is the same quote the reserve
    * debit was computed from. rollbackSwap reverses the pool using the persisted info, never a recomputation, so if these two ever diverged
    * an emit-then-rollback cycle would ratchet the reserves instead of being net-zero. k recomputes to a consistent product either way, so
    * nothing would look wrong — pin it directly.
    */
  test("post-activation: the PricingTokenInfo persisted at emission is the one used for the reserve debit") { implicit res =>
    implicit val (h, hs, sp) = res

    val (poolId, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val state = DataState(
      AmmOnChainState.empty,
      AmmCalculatedState(SortedMap(OperationType.LiquidityPool -> liquidityPoolCalculatedState))
    )
    val emissionPools = liquidityPoolCalculatedState.confirmed.value

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      signedAllowSpend <- Signed.forAsyncHasher[IO, AllowSpend](makeAllowSpend(Hash.empty), keyPair).flatMap(_.toHashed[IO])
      swapUpdate = makeSwapUpdate(signedAllowSpend.hash)

      allowSpends = SortedMap(
        primaryToken.identifier.get.value.some ->
          SortedMap(sourceAddress -> SortedSet(signedAllowSpend.signed))
      )

      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(
        keyPair,
        allowSpends,
        EpochProgress.MinValue,
        SnapshotOrdinal.MinValue,
        SortedMap.empty,
        EpochProgress.MinValue,
        SnapshotOrdinal.MinValue,
        destinationAddress
      )

      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService <- PricingService.make[IO](config, calculatedStateService)
      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      swapValidations = SwapValidations.make[IO](config, jsonBase64BinaryCodec)
      swapCombinerService = SwapCombinerService.make[IO](config, pricingService, swapValidations, jsonBase64BinaryCodec)

      response <- swapCombinerService.combineNew(
        swapUpdate,
        state,
        EpochProgress.MinValue,
        allowSpends,
        ACTIVE,
        CurrencyId(destinationAddress)
      )

      pendingAction = getSwapCalculatedState(response.calculated).pending.collect {
        case pending: PendingSpendAction[SwapUpdate] if pending.update === swapUpdate => pending
      }.head
      persistedInfo = pendingAction.pricingTokenInfo.collect { case info: SwapTokenInfo => info }.get

      freshQuote <- pricingService.getSwapTokenInfo(
        swapUpdate,
        pendingAction.updateHash,
        PoolId(poolId),
        EpochProgress.MinValue,
        emissionPools.some
      )

      updatedPool = getLiquidityPoolCalculatedState(response.calculated).confirmed.value(poolId)
    } yield
      expect.all(
        freshQuote.contains(persistedInfo),
        updatedPool.tokenA.amount.value === primaryToken.amount.value + persistedInfo.amount.value.value,
        updatedPool.tokenB.amount.value === pairToken.amount.value - persistedInfo.netReceived.value.value
      )
  }

  test("pre-activation: both swaps in a batch are quoted against the stale committed pool (legacy behaviour pinned)") { implicit res =>
    runTwoSwapBatch(SnapshotOrdinal.MinValue).map {
      case (netReceived1, netReceived2, updatedPool) =>
        expect.all(
          netReceived1.contains(9090909091L),
          netReceived2 === netReceived1,
          updatedPool.tokenA.amount.value === toFixedPoint(1200.0),
          updatedPool.tokenB.amount.value === toFixedPoint(1000.0) - 2 * 9090909091L
        )
    }
  }

  test("post-activation: the second swap is priced against the in-flight pool, so slippage applies within the batch") { implicit res =>
    runTwoSwapBatch(ACTIVE).map {
      case (netReceived1, netReceived2, updatedPool) =>
        expect.all(
          netReceived1.contains(9090909091L),
          netReceived2.exists(_ < 9090909091L),
          updatedPool.tokenA.amount.value === toFixedPoint(1200.0),
          updatedPool.tokenB.amount.value === toFixedPoint(1000.0) - netReceived1.get - netReceived2.get
        )
    }
  }

  test("post-activation: lastReference advances on every confirmed swap") { implicit res =>
    implicit val (h, hs, sp) = res

    val (_, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val state = DataState(
      AmmOnChainState.empty,
      AmmCalculatedState(SortedMap(OperationType.LiquidityPool -> liquidityPoolCalculatedState))
    )

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      signedAllowSpend1 <- Signed.forAsyncHasher[IO, AllowSpend](makeAllowSpend(Hash.empty), keyPair).flatMap(_.toHashed[IO])
      signedAllowSpend2 <- Signed.forAsyncHasher[IO, AllowSpend](makeAllowSpend(Hash("second")), keyPair).flatMap(_.toHashed[IO])

      swapUpdate1 = makeSwapUpdate(signedAllowSpend1.hash)
      swapReference1 <- SwapReference.of[IO](swapUpdate1)
      swapUpdate2 = makeSwapUpdate(signedAllowSpend2.hash, swapReference1)
      swapReference2 <- SwapReference.of[IO](swapUpdate2)

      allowSpends = SortedMap(
        primaryToken.identifier.get.value.some ->
          SortedMap(sourceAddress -> SortedSet(signedAllowSpend1.signed, signedAllowSpend2.signed))
      )

      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(
        keyPair,
        allowSpends,
        EpochProgress.MinValue,
        SnapshotOrdinal.MinValue,
        SortedMap.empty,
        EpochProgress.MinValue,
        SnapshotOrdinal.MinValue,
        destinationAddress
      )

      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService <- PricingService.make[IO](config, calculatedStateService)
      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      swapValidations = SwapValidations.make[IO](config, jsonBase64BinaryCodec)
      swapCombinerService = SwapCombinerService.make[IO](config, pricingService, swapValidations, jsonBase64BinaryCodec)

      response1 <- swapCombinerService.combineNew(
        swapUpdate1,
        state,
        EpochProgress.MinValue,
        allowSpends,
        ACTIVE,
        CurrencyId(destinationAddress)
      )
      pendingAction1 = getSwapCalculatedState(response1.calculated).pending.collect {
        case pending: PendingSpendAction[SwapUpdate] if pending.update === swapUpdate1 => pending
      }.head
      confirmed1 <- swapCombinerService.combinePendingSpendAction(
        pendingAction1,
        response1,
        EpochProgress.MinValue,
        response1.sharedArtifacts.map(_.asInstanceOf[SpendAction]).toList,
        ACTIVE,
        CurrencyId(destinationAddress)
      )

      response2 <- swapCombinerService.combineNew(
        swapUpdate2,
        confirmed1,
        EpochProgress.MinValue,
        allowSpends,
        ACTIVE,
        CurrencyId(destinationAddress)
      )
      pendingAction2 = getSwapCalculatedState(response2.calculated).pending.collect {
        case pending: PendingSpendAction[SwapUpdate] if pending.update === swapUpdate2 => pending
      }.head
      confirmed2 <- swapCombinerService.combinePendingSpendAction(
        pendingAction2,
        response2,
        EpochProgress.MinValue,
        response2.sharedArtifacts.map(_.asInstanceOf[SpendAction]).toList,
        ACTIVE,
        CurrencyId(destinationAddress)
      )

      confirmedInfo = getSwapCalculatedState(confirmed2.calculated).confirmed.value(ownerAddress)
    } yield
      expect.all(
        confirmedInfo.values.size === 2,
        confirmedInfo.lastReference === swapReference2,
        confirmedInfo.lastReference =!= swapReference1
      )
  }

  test("pre-activation: lastReference stays frozen on the first confirmed swap (legacy behaviour pinned)") { implicit res =>
    implicit val (h, hs, sp) = res

    val (_, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val state = DataState(
      AmmOnChainState.empty,
      AmmCalculatedState(SortedMap(OperationType.LiquidityPool -> liquidityPoolCalculatedState))
    )

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      signedAllowSpend1 <- Signed.forAsyncHasher[IO, AllowSpend](makeAllowSpend(Hash.empty), keyPair).flatMap(_.toHashed[IO])
      signedAllowSpend2 <- Signed.forAsyncHasher[IO, AllowSpend](makeAllowSpend(Hash("second")), keyPair).flatMap(_.toHashed[IO])

      swapUpdate1 = makeSwapUpdate(signedAllowSpend1.hash)
      swapReference1 <- SwapReference.of[IO](swapUpdate1)
      swapUpdate2 = makeSwapUpdate(signedAllowSpend2.hash, swapReference1)

      allowSpends = SortedMap(
        primaryToken.identifier.get.value.some ->
          SortedMap(sourceAddress -> SortedSet(signedAllowSpend1.signed, signedAllowSpend2.signed))
      )

      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(
        keyPair,
        allowSpends,
        EpochProgress.MinValue,
        SnapshotOrdinal.MinValue,
        SortedMap.empty,
        EpochProgress.MinValue,
        SnapshotOrdinal.MinValue,
        destinationAddress
      )

      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService <- PricingService.make[IO](config, calculatedStateService)
      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      swapValidations = SwapValidations.make[IO](config, jsonBase64BinaryCodec)
      swapCombinerService = SwapCombinerService.make[IO](config, pricingService, swapValidations, jsonBase64BinaryCodec)

      response1 <- swapCombinerService.combineNew(
        swapUpdate1,
        state,
        EpochProgress.MinValue,
        allowSpends,
        SnapshotOrdinal.MinValue,
        CurrencyId(destinationAddress)
      )
      pendingAction1 = getSwapCalculatedState(response1.calculated).pending.collect {
        case pending: PendingSpendAction[SwapUpdate] if pending.update === swapUpdate1 => pending
      }.head
      confirmed1 <- swapCombinerService.combinePendingSpendAction(
        pendingAction1,
        response1,
        EpochProgress.MinValue,
        response1.sharedArtifacts.map(_.asInstanceOf[SpendAction]).toList,
        SnapshotOrdinal.MinValue,
        CurrencyId(destinationAddress)
      )

      response2 <- swapCombinerService.combineNew(
        swapUpdate2,
        confirmed1,
        EpochProgress.MinValue,
        allowSpends,
        SnapshotOrdinal.MinValue,
        CurrencyId(destinationAddress)
      )
      pendingAction2 = getSwapCalculatedState(response2.calculated).pending.collect {
        case pending: PendingSpendAction[SwapUpdate] if pending.update === swapUpdate2 => pending
      }.head
      confirmed2 <- swapCombinerService.combinePendingSpendAction(
        pendingAction2,
        response2,
        EpochProgress.MinValue,
        response2.sharedArtifacts.map(_.asInstanceOf[SpendAction]).toList,
        SnapshotOrdinal.MinValue,
        CurrencyId(destinationAddress)
      )

      confirmedInfo = getSwapCalculatedState(confirmed2.calculated).confirmed.value(ownerAddress)
    } yield
      expect.all(
        confirmedInfo.values.size === 2,
        confirmedInfo.lastReference === swapReference1
      )
  }
}
