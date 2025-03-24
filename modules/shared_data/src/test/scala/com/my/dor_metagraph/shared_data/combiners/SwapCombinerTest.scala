package com.my.dor_metagraph.shared_data.combiners

import cats.data.NonEmptySet
import cats.effect.{IO, Resource}
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.ext.cats.effect.ResourceIO
import io.constellationnetwork.json.JsonSerializer
import io.constellationnetwork.schema.ID.Id
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact.SpendAction
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap._
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.hex.Hex
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.signature.signature.{Signature, SignatureProof}
import io.constellationnetwork.security.{Hasher, KeyPairGenerator, SecurityProvider}

import com.my.dor_metagraph.shared_data.DummyL0Context.buildL0NodeContext
import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.{NonNegLong, PosLong}
import eu.timepit.refined.types.numeric.PosDouble
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.app.ApplicationConfig._
import org.amm_metagraph.shared_data.calculated_state.CalculatedStateService
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.services.combiners.SwapCombinerService
import org.amm_metagraph.shared_data.services.pricing.PricingService
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, SwapUpdate}
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.Swap.SwapReference
import org.amm_metagraph.shared_data.types.codecs.{HasherSelector, JsonWithBase64BinaryCodec}
import weaver.MutableIOSuite

object SwapCombinerTest extends MutableIOSuite {
  type Res = (Hasher[IO], HasherSelector[IO], SecurityProvider[IO])
  val sourceAddress: Address = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")

  private val config = ApplicationConfig(
    EpochProgress(NonNegLong.unsafeFrom(30L)),
    "NodeValidators",
    Dev,
    Governance(
      VotingWeightMultipliers(
        PosDouble.MinValue,
        PosDouble.MinValue,
        PosDouble.MinValue
      )
    ),
    Rewards(
      Amount.empty,
      Amount.empty,
      NonNegLong.MinValue,
      NonNegLong.MinValue,
      NonNegLong.MinValue,
      EpochProgress.MinValue,
      Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")
    )
  )

  private def toFixedPoint(decimal: Double): Long = (decimal * 1e8).toLong

  override def sharedResource: Resource[IO, Res] = for {
    sp <- SecurityProvider.forAsync[IO]
    implicit0(j: JsonSerializer[IO]) <- JsonSerializer.forSync[IO].asResource
    h = Hasher.forJson[IO]
    hs = HasherSelector.forSync(h, h)
  } yield (h, hs, sp)

  def buildLiquidityPoolCalculatedState(
    tokenA: TokenInformation,
    tokenB: TokenInformation,
    owner: Address,
    additionalProvider: Option[(Address, ShareAmount)] = None
  ): (String, LiquidityPoolCalculatedState) = {
    val primaryAddressAsString = tokenA.identifier.fold("")(address => address.value.value)
    val pairAddressAsString = tokenB.identifier.fold("")(address => address.value.value)
    val poolId = PoolId(s"$primaryAddressAsString-$pairAddressAsString")

    val baseShares = Map(owner -> ShareAmount(Amount(PosLong.unsafeFrom(toFixedPoint(1.0)))))
    val shares = additionalProvider.fold(baseShares)(provider => baseShares + (provider._1 -> provider._2))

    val totalShares = shares.values.map(_.value.value.value).sum.toPosLongUnsafe

    val liquidityPool = LiquidityPool(
      poolId,
      tokenA,
      tokenB,
      owner,
      BigInt(tokenA.amount.value) * BigInt(tokenB.amount.value),
      PoolShares(totalShares, shares)
    )
    (
      poolId.value,
      LiquidityPoolCalculatedState.empty.copy(confirmed =
        ConfirmedLiquidityPoolCalculatedState.empty.copy(value = Map(poolId.value -> liquidityPool))
      )
    )
  }

  def getFakeSignedUpdate(
    update: SwapUpdate
  ): Signed[SwapUpdate] =
    Signed(
      update,
      NonEmptySet.one(
        SignatureProof(
          Id(
            Hex(
              "db2faf200159ca3c47924bf5f3bda4f45d681a39f9490053ecf98d788122f7a7973693570bd242e10ab670748e86139847eb682a53c7c5c711b832517ce34860"
            )
          ),
          Signature(
            Hex(
              "3045022100fb26702e976a6569caa3507140756fee96b5ba748719abe1b812b17f7279a3dc0220613db28d5c5a30d7353383358b653aa29772151ccf352a2e67a26a74e49eac57"
            )
          )
        )
      )
    )

  test("Test swap successfully when liquidity pool exists - providing min and max price") { implicit res =>
    implicit val (h, hs, sp) = res

    val primaryToken = TokenInformation(
      CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some,
      PosLong.unsafeFrom(toFixedPoint(1000.0))
    )

    val pairToken = TokenInformation(
      CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some,
      PosLong.unsafeFrom(toFixedPoint(500.0))
    )

    val sourceAddress = Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc")
    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")

    val (poolId, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)
    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      allowSpend = AllowSpend(
        sourceAddress,
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb"),
        Some(CurrencyId(ownerAddress)),
        SwapAmount(PosLong.MaxValue),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        EpochProgress.MaxValue,
        List.empty
      )

      signedAllowSpend <- Signed
        .forAsyncHasher[IO, AllowSpend](allowSpend, keyPair)
        .flatMap(_.toHashed[IO])

      swapUpdate = getFakeSignedUpdate(
        SwapUpdate(
          sourceAddress,
          primaryToken.identifier,
          pairToken.identifier,
          signedAllowSpend.hash,
          SwapAmount(PosLong.unsafeFrom(toFixedPoint(45.0))),
          SwapAmount(PosLong.unsafeFrom(toFixedPoint(100.0))),
          EpochProgress.MaxValue,
          PosLong.unsafeFrom(toFixedPoint(0.4)).some,
          PosLong.unsafeFrom(toFixedPoint(0.6)).some,
          SwapReference.empty
        )
      )

      allowSpends = SortedMap(
        primaryToken.identifier.get.value.some ->
          SortedMap(
            sourceAddress -> SortedSet(signedAllowSpend.signed)
          )
      )

      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(
        keyPair,
        allowSpends,
        EpochProgress.MinValue,
        SnapshotOrdinal.MinValue,
        SortedMap.empty,
        EpochProgress.MinValue,
        SnapshotOrdinal.MinValue,
        ownerAddress
      )

      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService = PricingService.make[IO](calculatedStateService)
      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      swapCombinerService = SwapCombinerService.make[IO](config, pricingService, jsonBase64BinaryCodec)

      swapPendingSpendActionResponse <- swapCombinerService.combineNew(
        swapUpdate,
        state,
        EpochProgress.MinValue,
        allowSpends,
        CurrencyId(ownerAddress)
      )

      spendActions = swapPendingSpendActionResponse.sharedArtifacts.map(_.asInstanceOf[SpendAction]).toList

      swapConfirmedResponse <- swapCombinerService.combinePendingSpendAction(
        PendingSpendAction(swapUpdate, spendActions.head),
        swapPendingSpendActionResponse,
        EpochProgress.MinValue,
        spendActions,
        SnapshotOrdinal.MinValue
      )

      swapCalculatedState = swapConfirmedResponse.calculated.operations(OperationType.Swap).asInstanceOf[SwapCalculatedState]
      addressSwapResponse = swapCalculatedState.confirmed.value(sourceAddress).head

      oldLiquidityPoolCalculatedState = state.calculated
        .operations(OperationType.LiquidityPool)
        .asInstanceOf[LiquidityPoolCalculatedState]
      oldLiquidityPool = oldLiquidityPoolCalculatedState.confirmed.value(poolId)

      updatedLiquidityPoolCalculatedState = swapConfirmedResponse.calculated
        .operations(OperationType.LiquidityPool)
        .asInstanceOf[LiquidityPoolCalculatedState]
      updatedLiquidityPool = updatedLiquidityPoolCalculatedState.confirmed.value(poolId)

    } yield
      expect.eql(toFixedPoint(1100.0), addressSwapResponse.fromToken.amount.value) &&
        expect.eql(primaryToken.identifier.get, addressSwapResponse.fromToken.identifier.get) &&
        expect.eql(toFixedPoint(454.54545454), addressSwapResponse.toToken.amount.value) &&
        expect.eql(toFixedPoint(1000.0), oldLiquidityPool.tokenA.amount.value) &&
        expect.eql(toFixedPoint(500.0), oldLiquidityPool.tokenB.amount.value) &&
        expect.eql(toFixedPoint(1100.0), updatedLiquidityPool.tokenA.amount.value) &&
        expect.eql(toFixedPoint(454.54545454), updatedLiquidityPool.tokenB.amount.value)
  }

  test("Test swap successfully when liquidity pool exists - not providing min and max price") { implicit res =>
    implicit val (h, hs, sp) = res

    val primaryToken = TokenInformation(
      CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some,
      PosLong.unsafeFrom(toFixedPoint(1000.0))
    )

    val pairToken = TokenInformation(
      CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some,
      PosLong.unsafeFrom(toFixedPoint(500.0))
    )

    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")
    val sourceAddress = Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc")

    val (poolId, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)
    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      allowSpend = AllowSpend(
        sourceAddress,
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb"),
        Some(CurrencyId(ownerAddress)),
        SwapAmount(PosLong.MaxValue),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        EpochProgress.MaxValue,
        List.empty
      )

      signedAllowSpend <- Signed
        .forAsyncHasher[IO, AllowSpend](allowSpend, keyPair)
        .flatMap(_.toHashed[IO])

      swapUpdate = getFakeSignedUpdate(
        SwapUpdate(
          sourceAddress,
          primaryToken.identifier,
          pairToken.identifier,
          signedAllowSpend.hash,
          SwapAmount(PosLong.unsafeFrom(toFixedPoint(45.0))),
          SwapAmount(PosLong.unsafeFrom(toFixedPoint(100.0))),
          EpochProgress.MaxValue,
          none,
          none,
          SwapReference.empty
        )
      )

      allowSpends = SortedMap(
        primaryToken.identifier.get.value.some ->
          SortedMap(
            sourceAddress -> SortedSet(signedAllowSpend.signed)
          )
      )

      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(
        keyPair,
        allowSpends,
        EpochProgress.MinValue,
        SnapshotOrdinal.MinValue,
        SortedMap.empty,
        EpochProgress.MinValue,
        SnapshotOrdinal.MinValue,
        ownerAddress
      )

      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService = PricingService.make[IO](calculatedStateService)
      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      swapCombinerService = SwapCombinerService.make[IO](config, pricingService, jsonBase64BinaryCodec)

      swapPendingSpendActionResponse <- swapCombinerService.combineNew(
        swapUpdate,
        state,
        EpochProgress.MinValue,
        allowSpends,
        CurrencyId(ownerAddress)
      )

      spendActions = swapPendingSpendActionResponse.sharedArtifacts.map(_.asInstanceOf[SpendAction]).toList

      swapConfirmedResponse <- swapCombinerService.combinePendingSpendAction(
        PendingSpendAction(swapUpdate, spendActions.head),
        swapPendingSpendActionResponse,
        EpochProgress.MinValue,
        spendActions,
        SnapshotOrdinal.MinValue
      )

      swapCalculatedState = swapConfirmedResponse.calculated.operations(OperationType.Swap).asInstanceOf[SwapCalculatedState]
      addressSwapResponse = swapCalculatedState.confirmed.value(sourceAddress).head

      oldLiquidityPoolCalculatedState = state.calculated
        .operations(OperationType.LiquidityPool)
        .asInstanceOf[LiquidityPoolCalculatedState]
      oldLiquidityPool = oldLiquidityPoolCalculatedState.confirmed.value(poolId)

      updatedLiquidityPoolCalculatedState = swapConfirmedResponse.calculated
        .operations(OperationType.LiquidityPool)
        .asInstanceOf[LiquidityPoolCalculatedState]
      updatedLiquidityPool = updatedLiquidityPoolCalculatedState.confirmed.value(poolId)

    } yield
      expect.eql(toFixedPoint(1100.0), addressSwapResponse.fromToken.amount.value) &&
        expect.eql(primaryToken.identifier.get, addressSwapResponse.fromToken.identifier.get) &&
        expect.eql(toFixedPoint(454.54545454), addressSwapResponse.toToken.amount.value) &&
        expect.eql(toFixedPoint(1000.0), oldLiquidityPool.tokenA.amount.value) &&
        expect.eql(toFixedPoint(500.0), oldLiquidityPool.tokenB.amount.value) &&
        expect.eql(toFixedPoint(1100.0), updatedLiquidityPool.tokenA.amount.value) &&
        expect.eql(toFixedPoint(454.54545454), updatedLiquidityPool.tokenB.amount.value)
  }

  test("Test swap failure when liquidity pool exists - minPrice too high") { implicit res =>
    implicit val (h, hs, sp) = res

    val primaryToken = TokenInformation(
      CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some,
      PosLong.unsafeFrom(toFixedPoint(1000.0))
    )

    val pairToken = TokenInformation(
      CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some,
      PosLong.unsafeFrom(toFixedPoint(500.0))
    )

    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")
    val sourceAddress = Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc")

    val (_, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)
    val futureEpoch = EpochProgress(NonNegLong.unsafeFrom(10L))
    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      allowSpend = AllowSpend(
        sourceAddress,
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb"),
        Some(CurrencyId(ownerAddress)),
        SwapAmount(PosLong.MaxValue),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        EpochProgress.MaxValue,
        List.empty
      )

      signedAllowSpend <- Signed
        .forAsyncHasher[IO, AllowSpend](allowSpend, keyPair)
        .flatMap(_.toHashed[IO])

      swapUpdate = getFakeSignedUpdate(
        SwapUpdate(
          sourceAddress,
          primaryToken.identifier,
          pairToken.identifier,
          signedAllowSpend.hash,
          SwapAmount(PosLong.unsafeFrom(toFixedPoint(45.0))),
          SwapAmount(PosLong.unsafeFrom(toFixedPoint(100.0))),
          EpochProgress.MaxValue,
          PosLong.unsafeFrom(toFixedPoint(0.8)).some,
          PosLong.unsafeFrom(toFixedPoint(0.9)).some,
          SwapReference.empty
        )
      )

      allowSpends = SortedMap(
        primaryToken.identifier.get.value.some ->
          SortedMap(
            sourceAddress -> SortedSet(signedAllowSpend.signed)
          )
      )

      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(
        keyPair,
        allowSpends,
        EpochProgress.MinValue,
        SnapshotOrdinal.MinValue,
        SortedMap.empty,
        EpochProgress.MinValue,
        SnapshotOrdinal.MinValue,
        ownerAddress
      )

      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService = PricingService.make[IO](calculatedStateService)
      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      swapCombinerService = SwapCombinerService.make[IO](config, pricingService, jsonBase64BinaryCodec)

      swapResponse <- swapCombinerService.combineNew(
        swapUpdate,
        state,
        futureEpoch,
        allowSpends,
        CurrencyId(ownerAddress)
      )

      swapCalculatedState = swapResponse.calculated.operations(OperationType.Swap).asInstanceOf[SwapCalculatedState]

    } yield
      expect.all(
        swapCalculatedState.failed.toList.length === 1,
        swapCalculatedState.failed.toList.head.expiringEpochProgress === EpochProgress(
          NonNegLong.unsafeFrom(futureEpoch.value.value + config.failedOperationsExpirationEpochProgresses.value.value)
        ),
        swapCalculatedState.failed.toList.head.reason == SwapPriceBelowAcceptableMinPrice()
      )
  }

  test("Test swap failure when liquidity pool exists - maxPrice too low") { implicit res =>
    implicit val (h, hs, sp) = res

    val primaryToken = TokenInformation(
      CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some,
      PosLong.unsafeFrom(toFixedPoint(1000.0))
    )

    val pairToken = TokenInformation(
      CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some,
      PosLong.unsafeFrom(toFixedPoint(500.0))
    )

    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")
    val sourceAddress = Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc")

    val (_, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)
    val futureEpoch = EpochProgress(NonNegLong.unsafeFrom(10L))
    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      allowSpend = AllowSpend(
        sourceAddress,
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb"),
        Some(CurrencyId(ownerAddress)),
        SwapAmount(PosLong.MaxValue),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        EpochProgress.MaxValue,
        List.empty
      )

      signedAllowSpend <- Signed
        .forAsyncHasher[IO, AllowSpend](allowSpend, keyPair)
        .flatMap(_.toHashed[IO])

      swapUpdate = getFakeSignedUpdate(
        SwapUpdate(
          sourceAddress,
          primaryToken.identifier,
          pairToken.identifier,
          signedAllowSpend.hash,
          SwapAmount(PosLong.unsafeFrom(toFixedPoint(45.0))),
          SwapAmount(PosLong.unsafeFrom(toFixedPoint(100.0))),
          EpochProgress.MaxValue,
          PosLong.unsafeFrom(toFixedPoint(0.1)).some,
          PosLong.unsafeFrom(toFixedPoint(0.2)).some,
          SwapReference.empty
        )
      )

      allowSpends = SortedMap(
        primaryToken.identifier.get.value.some ->
          SortedMap(
            sourceAddress -> SortedSet(signedAllowSpend.signed)
          )
      )

      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(
        keyPair,
        allowSpends,
        EpochProgress.MinValue,
        SnapshotOrdinal.MinValue,
        SortedMap.empty,
        EpochProgress.MinValue,
        SnapshotOrdinal.MinValue,
        ownerAddress
      )

      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService = PricingService.make[IO](calculatedStateService)
      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      swapCombinerService = SwapCombinerService.make[IO](config, pricingService, jsonBase64BinaryCodec)

      swapResponse <- swapCombinerService.combineNew(
        swapUpdate,
        state,
        futureEpoch,
        allowSpends,
        CurrencyId(ownerAddress)
      )

      swapCalculatedState = swapResponse.calculated.operations(OperationType.Swap).asInstanceOf[SwapCalculatedState]

    } yield
      expect.all(
        swapCalculatedState.failed.toList.length === 1,
        swapCalculatedState.failed.toList.head.expiringEpochProgress === EpochProgress(
          NonNegLong.unsafeFrom(futureEpoch.value.value + config.failedOperationsExpirationEpochProgresses.value.value)
        ),
        swapCalculatedState.failed.toList.head.reason == SwapPriceExceedsAcceptableMaxPrice()
      )
  }

  test("Test swap failure when liquidity pool exists - minAmount too high") { implicit res =>
    implicit val (h, hs, sp) = res

    val primaryToken = TokenInformation(
      CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some,
      PosLong.unsafeFrom(toFixedPoint(1000.0))
    )

    val pairToken = TokenInformation(
      CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some,
      PosLong.unsafeFrom(toFixedPoint(500.0))
    )

    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")
    val sourceAddress = Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc")

    val (_, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)
    val futureEpoch = EpochProgress(NonNegLong.unsafeFrom(10L))
    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      allowSpend = AllowSpend(
        sourceAddress,
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb"),
        Some(CurrencyId(ownerAddress)),
        SwapAmount(PosLong.MaxValue),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        EpochProgress.MaxValue,
        List.empty
      )

      signedAllowSpend <- Signed
        .forAsyncHasher[IO, AllowSpend](allowSpend, keyPair)
        .flatMap(_.toHashed[IO])

      swapUpdate = getFakeSignedUpdate(
        SwapUpdate(
          sourceAddress,
          primaryToken.identifier,
          pairToken.identifier,
          signedAllowSpend.hash,
          SwapAmount(PosLong.unsafeFrom(toFixedPoint(50.0))),
          SwapAmount(PosLong.unsafeFrom(toFixedPoint(100.0))),
          EpochProgress.MaxValue,
          PosLong.unsafeFrom(toFixedPoint(0.4)).some,
          PosLong.unsafeFrom(toFixedPoint(0.6)).some,
          SwapReference.empty
        )
      )

      allowSpends = SortedMap(
        primaryToken.identifier.get.value.some ->
          SortedMap(
            sourceAddress -> SortedSet(signedAllowSpend.signed)
          )
      )

      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(
        keyPair,
        allowSpends,
        EpochProgress.MinValue,
        SnapshotOrdinal.MinValue,
        SortedMap.empty,
        EpochProgress.MinValue,
        SnapshotOrdinal.MinValue,
        ownerAddress
      )

      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService = PricingService.make[IO](calculatedStateService)
      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      swapCombinerService = SwapCombinerService.make[IO](config, pricingService, jsonBase64BinaryCodec)

      swapResponse <- swapCombinerService.combineNew(
        swapUpdate,
        state,
        futureEpoch,
        allowSpends,
        CurrencyId(ownerAddress)
      )

      swapCalculatedState = swapResponse.calculated.operations(OperationType.Swap).asInstanceOf[SwapCalculatedState]

    } yield
      expect.all(
        swapCalculatedState.failed.toList.length === 1,
        swapCalculatedState.failed.toList.head.expiringEpochProgress === EpochProgress(
          NonNegLong.unsafeFrom(futureEpoch.value.value + config.failedOperationsExpirationEpochProgresses.value.value)
        ),
        swapCalculatedState.failed.toList.head.reason == SwapLessThanMinAmount()
      )
  }

  test("Test swap - pending swap without allow spend") { implicit res =>
    implicit val (h, hs, sp) = res

    val primaryToken =
      TokenInformation(CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some, 1000000L.toTokenAmountFormat.toPosLongUnsafe)
    val pairToken =
      TokenInformation(CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some, 2000000L.toTokenAmountFormat.toPosLongUnsafe)
    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")

    val (_, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)
    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      allowSpend = AllowSpend(
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc"),
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb"),
        Some(CurrencyId(ownerAddress)),
        SwapAmount(PosLong.MinValue),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        EpochProgress.MaxValue,
        List.empty
      )

      signedAllowSpend <- Signed
        .forAsyncHasher[IO, AllowSpend](allowSpend, keyPair)
        .flatMap(_.toHashed[IO])

      swapUpdate = getFakeSignedUpdate(
        SwapUpdate(
          sourceAddress,
          primaryToken.identifier,
          pairToken.identifier,
          signedAllowSpend.hash,
          SwapAmount(100000L),
          SwapAmount(100000L),
          EpochProgress.MaxValue,
          none,
          none,
          SwapReference.empty
        )
      )

      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(
        keyPair,
        SortedMap.empty,
        EpochProgress.MinValue,
        SnapshotOrdinal.MinValue,
        SortedMap.empty,
        EpochProgress.MinValue,
        SnapshotOrdinal.MinValue,
        ownerAddress
      )

      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService = PricingService.make[IO](calculatedStateService)
      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      swapCombinerService = SwapCombinerService.make[IO](config, pricingService, jsonBase64BinaryCodec)

      swapResponse <- swapCombinerService.combineNew(
        swapUpdate,
        state,
        EpochProgress.MinValue,
        SortedMap.empty,
        CurrencyId(ownerAddress)
      )

      swapCalculatedState = swapResponse.calculated.operations(OperationType.Swap).asInstanceOf[SwapCalculatedState]
      confirmedSwapResponse = swapCalculatedState.confirmed.value.get(ownerAddress)
      pendingSwapResponse = swapCalculatedState.pending
      pendingSwapResponseValue = pendingSwapResponse.head.update.value

    } yield
      expect.eql(none, confirmedSwapResponse) &&
        expect.eql(1, pendingSwapResponse.size) &&
        expect.eql(swapUpdate.value.allowSpendReference, pendingSwapResponseValue.allowSpendReference)
  }

  test("Test swap - ignore swap that exceed limit epoch progress") { implicit res =>
    implicit val (h, hs, sp) = res

    val primaryToken =
      TokenInformation(CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some, 1000000L.toTokenAmountFormat.toPosLongUnsafe)
    val pairToken =
      TokenInformation(CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some, 2000000L.toTokenAmountFormat.toPosLongUnsafe)
    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")

    val (_, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)
    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      allowSpend = AllowSpend(
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc"),
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb"),
        Some(CurrencyId(ownerAddress)),
        SwapAmount(PosLong.MinValue),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        EpochProgress.MaxValue,
        List.empty
      )

      signedAllowSpend <- Signed
        .forAsyncHasher[IO, AllowSpend](allowSpend, keyPair)
        .flatMap(_.toHashed[IO])

      swapUpdate = getFakeSignedUpdate(
        SwapUpdate(
          sourceAddress,
          primaryToken.identifier,
          pairToken.identifier,
          signedAllowSpend.hash,
          SwapAmount(100000L),
          SwapAmount(100000L),
          EpochProgress.MinValue,
          none,
          none,
          SwapReference.empty
        )
      )

      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(
        keyPair,
        SortedMap.empty,
        EpochProgress.MaxValue,
        SnapshotOrdinal.MinValue,
        SortedMap.empty,
        EpochProgress.MinValue,
        SnapshotOrdinal.MinValue,
        ownerAddress
      )

      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService = PricingService.make[IO](calculatedStateService)
      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      swapCombinerService = SwapCombinerService.make[IO](config, pricingService, jsonBase64BinaryCodec)

      swapPendingSpendActionResponse <- swapCombinerService.combineNew(
        swapUpdate,
        state,
        EpochProgress.MaxValue,
        SortedMap.empty,
        CurrencyId(ownerAddress)
      )

      swapCalculatedState = swapPendingSpendActionResponse.calculated.operations(OperationType.Swap).asInstanceOf[SwapCalculatedState]
      confirmedSwapResponse = swapCalculatedState.confirmed.value.get(ownerAddress)
      pendingSwapResponse = swapCalculatedState.pending

    } yield
      expect.eql(none, confirmedSwapResponse) &&
        expect.eql(0, pendingSwapResponse.size)
  }
}
