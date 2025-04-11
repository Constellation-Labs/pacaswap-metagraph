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
import org.amm_metagraph.shared_data.FeeDistributor
import org.amm_metagraph.shared_data.FeeDistributor.FeePercentages
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
    ),
    PosLong.unsafeFrom((100 * 1e8).toLong)
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
    additionalProvider: Option[(Address, ShareAmount)] = None,
    fees: FeePercentages = FeeDistributor.empty
  ): (String, LiquidityPoolCalculatedState) = {
    val primaryAddressAsString = tokenA.identifier.fold("")(address => address.value.value)
    val pairAddressAsString = tokenB.identifier.fold("")(address => address.value.value)
    val poolId = PoolId(s"$primaryAddressAsString-$pairAddressAsString")

    val baseShares = Map(owner -> ShareAmount(Amount(PosLong.unsafeFrom(toFixedPoint(1.0)))))
    val shares = additionalProvider.fold(baseShares)(provider => baseShares + (provider._1 -> provider._2))
    val feeShares = additionalProvider.foldLeft(Map(owner -> 0L.toNonNegLongUnsafe)) {
      case (acc, (addr, _)) => acc + (addr -> 0L.toNonNegLongUnsafe)
    }

    val totalShares = shares.values.map(_.value.value.value).sum.toPosLongUnsafe

    val liquidityPool = LiquidityPool(
      poolId,
      tokenA,
      tokenB,
      owner,
      BigInt(tokenA.amount.value) * BigInt(tokenB.amount.value),
      PoolShares(totalShares, shares, feeShares),
      fees
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

  test("Test swap successfully when liquidity pool exists") { implicit res =>
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
    val destinationAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAP")

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
        destinationAddress,
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
          SwapAmount(PosLong.unsafeFrom(toFixedPoint(100.0))),
          SwapAmount(PosLong.unsafeFrom(toFixedPoint(40.0))),
          EpochProgress.MaxValue,
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
        destinationAddress
      )

      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService = PricingService.make[IO](config, calculatedStateService)
      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      swapCombinerService = SwapCombinerService.make[IO](config, pricingService, jsonBase64BinaryCodec)

      swapPendingSpendActionResponse <- swapCombinerService.combineNew(
        swapUpdate,
        state,
        EpochProgress.MinValue,
        allowSpends,
        CurrencyId(destinationAddress)
      )

      spendActions = swapPendingSpendActionResponse.sharedArtifacts.map(_.asInstanceOf[SpendAction]).toList
      metagraphId <- context.getCurrencyId

      swapConfirmedResponse <- swapCombinerService.combinePendingSpendAction(
        PendingSpendAction(swapUpdate, spendActions.head),
        swapPendingSpendActionResponse,
        EpochProgress.MinValue,
        spendActions,
        SnapshotOrdinal.MinValue,
        metagraphId
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

  test("Test swap failure when liquidity pool exists - received amount less than amountOutMinimum") { implicit res =>
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
    val destinationAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAP")

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
        destinationAddress,
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
          SwapAmount(PosLong.unsafeFrom(toFixedPoint(100.0))),
          SwapAmount(PosLong.unsafeFrom(toFixedPoint(50.0))),
          EpochProgress.MaxValue,
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
        destinationAddress
      )

      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService = PricingService.make[IO](config, calculatedStateService)
      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      swapCombinerService = SwapCombinerService.make[IO](config, pricingService, jsonBase64BinaryCodec)

      swapResponse <- swapCombinerService.combineNew(
        swapUpdate,
        state,
        futureEpoch,
        allowSpends,
        CurrencyId(destinationAddress)
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
      pricingService = PricingService.make[IO](config, calculatedStateService)
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
      pricingService = PricingService.make[IO](config, calculatedStateService)
      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      swapCombinerService = SwapCombinerService.make[IO](config, pricingService, jsonBase64BinaryCodec)

      swapPendingSpendActionResponse <- swapCombinerService.combineNew(
        swapUpdate,
        state,
        EpochProgress(NonNegLong.unsafeFrom(10L)),
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

  test("Test swap - 0.3% fee correctly applies to the swap") { implicit res =>
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
    val destinationAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAP")

    val (poolId, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(
      primaryToken,
      pairToken,
      ownerAddress,
      None,
      FeeDistributor.standard
    )

    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      allowSpend = AllowSpend(
        sourceAddress,
        destinationAddress,
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
          SwapAmount(PosLong.unsafeFrom(toFixedPoint(100.0))),
          SwapAmount(PosLong.unsafeFrom(toFixedPoint(40.0))),
          EpochProgress.MaxValue,
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
        destinationAddress
      )

      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService = PricingService.make[IO](config, calculatedStateService)
      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      swapCombinerService = SwapCombinerService.make[IO](config, pricingService, jsonBase64BinaryCodec)

      swapPendingSpendActionResponse <- swapCombinerService.combineNew(
        swapUpdate,
        state,
        EpochProgress.MinValue,
        allowSpends,
        CurrencyId(destinationAddress)
      )

      spendActions = swapPendingSpendActionResponse.sharedArtifacts.map(_.asInstanceOf[SpendAction]).toList
      metagraphId <- context.getCurrencyId

      swapConfirmedResponse <- swapCombinerService.combinePendingSpendAction(
        PendingSpendAction(swapUpdate, spendActions.head),
        swapPendingSpendActionResponse,
        EpochProgress.MinValue,
        spendActions,
        SnapshotOrdinal.MinValue,
        metagraphId
      )

      swapCalculatedState = swapConfirmedResponse.calculated.operations(OperationType.Swap).asInstanceOf[SwapCalculatedState]
      addressSwapResponse = swapCalculatedState.confirmed.value(sourceAddress).head

      oldLiquidityPoolCalculatedState = state.calculated
        .operations(OperationType.LiquidityPool)
        .asInstanceOf[LiquidityPoolCalculatedState]
      oldLiquidityPool = oldLiquidityPoolCalculatedState.confirmed.value(poolId)

      updatedLiquidityPoolCalculatedState: LiquidityPoolCalculatedState = swapConfirmedResponse.calculated
        .operations(OperationType.LiquidityPool)
        .asInstanceOf[LiquidityPoolCalculatedState]
      updatedLiquidityPool = updatedLiquidityPoolCalculatedState.confirmed.value(poolId)

      fromTokenInfo =
        if (swapUpdate.value.swapFromPair == oldLiquidityPool.tokenA.identifier) oldLiquidityPool.tokenA else oldLiquidityPool.tokenB
      toTokenInfo =
        if (swapUpdate.value.swapToPair == oldLiquidityPool.tokenA.identifier) oldLiquidityPool.tokenA else oldLiquidityPool.tokenB

      swapAmount = swapUpdate.amountIn.value.value
      newFromTokenReserve = BigInt(fromTokenInfo.amount.value) + BigInt(swapAmount)
      toTokenReserveBeforeFee = (oldLiquidityPool.k / newFromTokenReserve).toLong

      userReceivesAmountBeforeFee = toTokenInfo.amount.value - toTokenReserveBeforeFee

      feeBreakdown = FeeDistributor.calculateFeeAmounts(
        userReceivesAmountBeforeFee,
        oldLiquidityPool.poolFees
      )

      totalFeeAmount = feeBreakdown.total
      providerFeeAmount = feeBreakdown.providers
      operatorFeeAmount = feeBreakdown.operators
      toTokenReserveAfterFee = toTokenReserveBeforeFee + totalFeeAmount

      initialProviderFeeShare = FeeDistributor.getFeeShare(oldLiquidityPool.poolShares.feeShares, ownerAddress).value
      initialOperatorFeeShare = FeeDistributor.getFeeShare(oldLiquidityPool.poolShares.feeShares, metagraphId.value).value

      expectedUpdatedFeeShares = FeeDistributor.distributeProviderFees(
        providerFeeAmount,
        operatorFeeAmount,
        oldLiquidityPool.poolShares,
        metagraphId
      )
      expectedProviderFinalFeeShare = expectedUpdatedFeeShares(ownerAddress).value
      expectedOperatorFinalFeeShare = expectedUpdatedFeeShares(metagraphId.value).value

      providerFeeShare = FeeDistributor.getFeeShare(updatedLiquidityPool.poolShares.feeShares, ownerAddress).value
      operatorFeeShare = FeeDistributor.getFeeShare(updatedLiquidityPool.poolShares.feeShares, metagraphId.value).value
      totalDistributedFees = providerFeeShare + operatorFeeShare - initialProviderFeeShare - initialOperatorFeeShare
    } yield
      expect.all(
        addressSwapResponse.fromToken.amount.value === swapUpdate.value.amountIn.value.value + oldLiquidityPool.tokenA.amount.value,
        addressSwapResponse.toToken.amount.value === toTokenReserveAfterFee,
        updatedLiquidityPool.poolShares.feeShares(ownerAddress).value === expectedProviderFinalFeeShare,
        updatedLiquidityPool.poolShares.feeShares(metagraphId.value).value === expectedOperatorFinalFeeShare,
        (totalDistributedFees - totalFeeAmount).abs <= 1
      )
  }

  test("Swap Test A→B: Primary token to pair token - Testing all percentage buckets") { implicit res =>
    implicit val (h, hs, sp) = res

    val primaryToken = TokenInformation(
      CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some,
      PosLong.unsafeFrom(toFixedPoint(1000000.0)) // 1,000,000 units of Token A
    )

    val pairToken = TokenInformation(
      CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some,
      PosLong.unsafeFrom(toFixedPoint(500000.0)) // 500,000 units of Token B
    )

    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")
    val destinationAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAP")
    val sourceAddress = Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc")

    val percentageBuckets = List(1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95)

    percentageBuckets.traverse { percentage =>
      val swapPercentage = percentage / 100.0
      val swapAmount = 1000000.0 * swapPercentage
      val expectedOutput = 500000.0 * swapPercentage / (1.0 + swapPercentage)

      val fixedSwapAmount = toFixedPoint(swapAmount)
      val fixedMinOutputAmount = toFixedPoint(expectedOutput)

      val (poolId, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
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
          destinationAddress,
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
            SwapAmount(PosLong.unsafeFrom(fixedSwapAmount)),
            SwapAmount(PosLong.unsafeFrom(fixedMinOutputAmount)),
            EpochProgress.MaxValue,
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
          destinationAddress
        )

        calculatedStateService <- CalculatedStateService.make[IO]
        _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
        pricingService = PricingService.make[IO](config, calculatedStateService)
        jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
        swapCombinerService = SwapCombinerService.make[IO](config, pricingService, jsonBase64BinaryCodec)

        swapPendingSpendActionResponse <- swapCombinerService.combineNew(
          swapUpdate,
          state,
          futureEpoch,
          allowSpends,
          CurrencyId(destinationAddress)
        )

        spendActions = swapPendingSpendActionResponse.sharedArtifacts.map(_.asInstanceOf[SpendAction]).toList
        metagraphId <- context.getCurrencyId

        swapConfirmedResponse <- swapCombinerService.combinePendingSpendAction(
          PendingSpendAction(swapUpdate, spendActions.head),
          swapPendingSpendActionResponse,
          EpochProgress.MinValue,
          spendActions,
          SnapshotOrdinal.MinValue,
          metagraphId
        )

        swapCalculatedState = swapConfirmedResponse.calculated.operations(OperationType.Swap).asInstanceOf[SwapCalculatedState]
        updatedLiquidityPoolCalculatedState = swapConfirmedResponse.calculated
          .operations(OperationType.LiquidityPool)
          .asInstanceOf[LiquidityPoolCalculatedState]
        updatedLiquidityPool = updatedLiquidityPoolCalculatedState.confirmed.value(poolId)
      } yield
        if (swapCalculatedState.confirmed.value.nonEmpty) {
          val confirmedSwap = swapCalculatedState.confirmed.value.head._2.head
          expect.all(
            swapCalculatedState.confirmed.value.size === 1,
            swapCalculatedState.failed.isEmpty,
            confirmedSwap.netReceived.value.value >= fixedMinOutputAmount,
            updatedLiquidityPool.tokenA.amount.value > primaryToken.amount.value,
            updatedLiquidityPool.tokenB.amount.value < pairToken.amount.value
          )
        } else {
          expect(false, s"Swap failed unexpectedly for $percentage% of pool")
        }
    }.map(_.reduce(_ and _))
  }

  test("Swap Test B→A: Pair token to primary token - Testing all percentage buckets") { implicit res =>
    implicit val (h, hs, sp) = res

    val primaryToken = TokenInformation(
      CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some,
      PosLong.unsafeFrom(toFixedPoint(1000000.0)) // 1,000,000 units of Token A
    )

    val pairToken = TokenInformation(
      CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some,
      PosLong.unsafeFrom(toFixedPoint(500000.0)) // 500,000 units of Token B
    )

    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")
    val destinationAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAP")
    val sourceAddress = Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc")

    val percentageBuckets = List(1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95)

    percentageBuckets.traverse { percentage =>
      val swapPercentage = percentage / 100.0
      val swapAmount = 500000.0 * swapPercentage
      val expectedOutput = 1000000.0 * swapPercentage / (1.0 + swapPercentage)

      val fixedSwapAmount = toFixedPoint(swapAmount)
      val fixedMinOutputAmount = toFixedPoint(expectedOutput)

      val (poolId, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
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
          destinationAddress,
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
            pairToken.identifier,
            primaryToken.identifier,
            signedAllowSpend.hash,
            SwapAmount(PosLong.unsafeFrom(fixedSwapAmount)),
            SwapAmount(PosLong.unsafeFrom(fixedMinOutputAmount)),
            EpochProgress.MaxValue,
            SwapReference.empty
          )
        )

        allowSpends = SortedMap(
          pairToken.identifier.get.value.some ->
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
          destinationAddress
        )

        calculatedStateService <- CalculatedStateService.make[IO]
        _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
        pricingService = PricingService.make[IO](config, calculatedStateService)
        jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
        swapCombinerService = SwapCombinerService.make[IO](config, pricingService, jsonBase64BinaryCodec)

        swapPendingSpendActionResponse <- swapCombinerService.combineNew(
          swapUpdate,
          state,
          futureEpoch,
          allowSpends,
          CurrencyId(destinationAddress)
        )

        spendActions = swapPendingSpendActionResponse.sharedArtifacts.map(_.asInstanceOf[SpendAction]).toList
        metagraphId <- context.getCurrencyId

        swapConfirmedResponse <- swapCombinerService.combinePendingSpendAction(
          PendingSpendAction(swapUpdate, spendActions.head),
          swapPendingSpendActionResponse,
          EpochProgress.MinValue,
          spendActions,
          SnapshotOrdinal.MinValue,
          metagraphId
        )

        swapCalculatedState = swapConfirmedResponse.calculated.operations(OperationType.Swap).asInstanceOf[SwapCalculatedState]
        updatedLiquidityPoolCalculatedState = swapConfirmedResponse.calculated
          .operations(OperationType.LiquidityPool)
          .asInstanceOf[LiquidityPoolCalculatedState]
        updatedLiquidityPool = updatedLiquidityPoolCalculatedState.confirmed.value(poolId)
      } yield
        if (swapCalculatedState.confirmed.value.nonEmpty) {
          val confirmedSwap = swapCalculatedState.confirmed.value.head._2.head
          expect.all(
            swapCalculatedState.confirmed.value.size === 1,
            swapCalculatedState.failed.isEmpty,
            confirmedSwap.netReceived.value.value >= fixedMinOutputAmount,
            updatedLiquidityPool.tokenB.amount.value > pairToken.amount.value,
            updatedLiquidityPool.tokenA.amount.value < primaryToken.amount.value
          )
        } else {
          expect(false, s"Swap failed unexpectedly for $percentage% of pool")
        }
    }.map(_.reduce(_ and _))
  }

  test("Swap Test A→B: Testing various slippage tolerance scenarios") { implicit res =>
    implicit val (h, hs, sp) = res

    val primaryToken = TokenInformation(
      CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some,
      PosLong.unsafeFrom(toFixedPoint(1000000.0)) // 1,000,000 units of Token A
    )

    val pairToken = TokenInformation(
      CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some,
      PosLong.unsafeFrom(toFixedPoint(500000.0)) // 500,000 units of Token B
    )

    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")
    val sourceAddress = Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc")
    val destinationAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAP")

    val testCases = List(
      (30, 0.0, true), // 0% slippage tolerance - should succeed (exact match)
      (30, 0.01, true), // 1% slippage tolerance - should succeed
      (30, 0.05, true), // 5% slippage tolerance - should succeed
      (30, -0.01, false), // -1% slippage (expecting more than theoretical) - should fail
      (30, -0.05, false), // -5% slippage (expecting more than theoretical) - should fail
      (90, 0.1, true), // 10% slippage on a large swap - should succeed
      (90, 0.0, true), // 0% slippage on a large swap - should succeed
      (95, 0.2, true) // 20% slippage on a very large swap - should succeed
    )

    testCases.traverse {
      case (percentage, slippageTolerance, expectedSuccess) =>
        val swapPercentage = percentage / 100.0
        val swapAmount = 1000000.0 * swapPercentage
        val expectedOutput = 500000.0 * swapPercentage / (1.0 + swapPercentage)
        val minOutputWithSlippage = expectedOutput * (1.0 - slippageTolerance)

        val fixedSwapAmount = toFixedPoint(swapAmount)
        val fixedMinOutputAmount = toFixedPoint(minOutputWithSlippage)

        val (poolId, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
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
            destinationAddress,
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
              SwapAmount(PosLong.unsafeFrom(fixedSwapAmount)),
              SwapAmount(PosLong.unsafeFrom(fixedMinOutputAmount)),
              EpochProgress.MaxValue,
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
            destinationAddress
          )

          calculatedStateService <- CalculatedStateService.make[IO]
          _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
          pricingService = PricingService.make[IO](config, calculatedStateService)
          jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
          swapCombinerService = SwapCombinerService.make[IO](config, pricingService, jsonBase64BinaryCodec)

          swapPendingSpendActionResponse <- swapCombinerService.combineNew(
            swapUpdate,
            state,
            futureEpoch,
            allowSpends,
            CurrencyId(destinationAddress)
          )

          spendActions = swapPendingSpendActionResponse.sharedArtifacts.map(_.asInstanceOf[SpendAction]).toList
          metagraphId <- context.getCurrencyId

          swapConfirmedResponse <-
            if (spendActions.nonEmpty) {
              swapCombinerService.combinePendingSpendAction(
                PendingSpendAction(swapUpdate, spendActions.head),
                swapPendingSpendActionResponse,
                EpochProgress.MinValue,
                spendActions,
                SnapshotOrdinal.MinValue,
                metagraphId
              )
            } else {
              IO.pure(swapPendingSpendActionResponse)
            }

          swapCalculatedState = swapConfirmedResponse.calculated.operations(OperationType.Swap).asInstanceOf[SwapCalculatedState]

          updatedLiquidityPoolCalculatedState = swapConfirmedResponse.calculated.operations
            .getOrElse(
              OperationType.LiquidityPool,
              liquidityPoolCalculatedState
            )
            .asInstanceOf[LiquidityPoolCalculatedState]

        } yield {
          val successResult = swapCalculatedState.confirmed.value.nonEmpty
          val failReason = swapCalculatedState.failed.headOption.map(_.reason)

          if (expectedSuccess) {
            expect
              .all(
                successResult === true,
                swapCalculatedState.failed.isEmpty
              )
              .and {
                if (successResult) {
                  val confirmedSwap = swapCalculatedState.confirmed.value.head._2.head
                  val updatedLiquidityPool = updatedLiquidityPoolCalculatedState.confirmed.value.get(poolId)

                  expect.all(
                    confirmedSwap.netReceived.value.value >= fixedMinOutputAmount,
                    updatedLiquidityPool.exists(_.tokenA.amount.value > primaryToken.amount.value),
                    updatedLiquidityPool.exists(_.tokenB.amount.value < pairToken.amount.value)
                  )
                } else {
                  expect(false, s"Swap failed unexpectedly: ${failReason.getOrElse("unknown reason")}")
                }
              }
          } else {
            expect.all(
              successResult === false,
              swapCalculatedState.failed.nonEmpty
            )
          }
        }
    }.map(_.reduce(_ and _))
  }

  test("Swap Test B→A: Testing various slippage tolerance scenarios") { implicit res =>
    implicit val (h, hs, sp) = res

    val tokenA = TokenInformation(
      CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some,
      PosLong.unsafeFrom(toFixedPoint(1_000_000.0)) // 1,000,000 Token A
    )

    val tokenB = TokenInformation(
      CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some,
      PosLong.unsafeFrom(toFixedPoint(500_000.0)) // 500,000 Token B
    )

    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")
    val sourceAddress = Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc")
    val destinationAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAP")

    val testCases = List(
      (30, 0.0, true), // 0% slippage tolerance - should succeed (exact match)
      (30, 0.01, true), // 1% slippage tolerance - should succeed
      (30, 0.05, true), // 5% slippage tolerance - should succeed
      (30, -0.01, false), // -1% slippage (expecting more than theoretical) - should fail
      (30, -0.05, false), // -5% slippage (expecting more than theoretical) - should fail
      (90, 0.1, true), // 10% slippage on a large swap - should succeed
      (90, 0.0, true), // 0% slippage on a large swap - should succeed
      (95, 0.2, true) // 20% slippage on a very large swap - should succeed
    )

    testCases.traverse {
      case (percentage, slippageTolerance, expectedSuccess) =>
        val swapPercentage = percentage / 100.0
        val swapAmount = 500_000.0 * swapPercentage
        val expectedOutput = 1_000_000.0 * swapPercentage / (1.0 + swapPercentage)
        val minOutputWithSlippage = expectedOutput * (1.0 - slippageTolerance)

        val fixedSwapAmount = toFixedPoint(swapAmount)
        val fixedMinOutputAmount = toFixedPoint(minOutputWithSlippage)

        val (poolId, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(tokenA, tokenB, ownerAddress)
        val ammOnChainState = AmmOnChainState(List.empty)
        val ammCalculatedState = AmmCalculatedState(Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState))
        val state = DataState(ammOnChainState, ammCalculatedState)
        val futureEpoch = EpochProgress(NonNegLong.unsafeFrom(10L))

        for {
          keyPair <- KeyPairGenerator.makeKeyPair[IO]
          allowSpend = AllowSpend(
            sourceAddress,
            destinationAddress,
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
              tokenB.identifier,
              tokenA.identifier,
              signedAllowSpend.hash,
              SwapAmount(PosLong.unsafeFrom(fixedSwapAmount)),
              SwapAmount(PosLong.unsafeFrom(fixedMinOutputAmount)),
              EpochProgress.MaxValue,
              SwapReference.empty
            )
          )

          allowSpends = SortedMap(
            tokenB.identifier.get.value.some -> SortedMap(
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
            destinationAddress
          )

          calculatedStateService <- CalculatedStateService.make[IO]
          _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
          pricingService = PricingService.make[IO](config, calculatedStateService)
          jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
          swapCombinerService = SwapCombinerService.make[IO](config, pricingService, jsonBase64BinaryCodec)

          swapPendingSpendActionResponse <- swapCombinerService.combineNew(
            swapUpdate,
            state,
            futureEpoch,
            allowSpends,
            CurrencyId(destinationAddress)
          )

          spendActions = swapPendingSpendActionResponse.sharedArtifacts.map(_.asInstanceOf[SpendAction]).toList
          metagraphId <- context.getCurrencyId

          swapConfirmedResponse <-
            if (spendActions.nonEmpty) {
              swapCombinerService.combinePendingSpendAction(
                PendingSpendAction(swapUpdate, spendActions.head),
                swapPendingSpendActionResponse,
                EpochProgress.MinValue,
                spendActions,
                SnapshotOrdinal.MinValue,
                metagraphId
              )
            } else {
              IO.pure(swapPendingSpendActionResponse)
            }

          swapCalculatedState = swapConfirmedResponse.calculated.operations(OperationType.Swap).asInstanceOf[SwapCalculatedState]
          updatedLiquidityPoolCalculatedState = swapConfirmedResponse.calculated
            .operations(OperationType.LiquidityPool)
            .asInstanceOf[LiquidityPoolCalculatedState]

        } yield {
          val successResult = swapCalculatedState.confirmed.value.nonEmpty
          val failReason = swapCalculatedState.failed.headOption.map(_.reason)

          if (expectedSuccess) {
            expect
              .all(
                successResult === true,
                swapCalculatedState.failed.isEmpty
              )
              .and {
                if (successResult) {
                  val confirmedSwap = swapCalculatedState.confirmed.value.head._2.head
                  val updatedLiquidityPool = updatedLiquidityPoolCalculatedState.confirmed.value.get(poolId)
                  expect.all(
                    confirmedSwap.netReceived.value.value >= fixedMinOutputAmount,
                    updatedLiquidityPool.exists(_.tokenA.amount.value < tokenA.amount.value),
                    updatedLiquidityPool.exists(_.tokenB.amount.value > tokenB.amount.value)
                  )
                } else {
                  expect(false, s"Swap failed unexpectedly: ${failReason.getOrElse("unknown reason")}")
                }
              }
          } else {
            expect.all(
              successResult === false,
              swapCalculatedState.failed.nonEmpty
            )
          }
        }
    }.map(_.reduce(_ and _))
  }

  test("Swap Test A→B: Swap 100x pool balance to drain one side - expect rejection or failure") { implicit res =>
    implicit val (h, hs, sp) = res

    val primaryToken = TokenInformation(
      CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some,
      PosLong.unsafeFrom(toFixedPoint(100.0)) // Pool has 1,000,000 units of Token A
    )

    val pairToken = TokenInformation(
      CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some,
      PosLong.unsafeFrom(toFixedPoint(50.0)) // Pool has 500,000 units of Token B
    )

    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")
    val sourceAddress = Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc")
    val destinationAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAP")

    val swapAmount = 100.0 * 100 // 100x the available Token A
    val expectedOutput = 50.0 * 100 / (1.0 + 100.0) // Expected but huge value
    val fixedSwapAmount = toFixedPoint(swapAmount)
    val fixedMinOutputAmount = toFixedPoint(expectedOutput)

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
        destinationAddress,
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
          SwapAmount(PosLong.unsafeFrom(fixedSwapAmount)),
          SwapAmount(PosLong.unsafeFrom(fixedMinOutputAmount)), // Expected min output
          EpochProgress.MaxValue,
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
        destinationAddress
      )

      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService = PricingService.make[IO](config, calculatedStateService)
      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      swapCombinerService = SwapCombinerService.make[IO](config, pricingService, jsonBase64BinaryCodec)

      swapPendingSpendActionResponse <- swapCombinerService.combineNew(
        swapUpdate,
        state,
        futureEpoch,
        allowSpends,
        CurrencyId(destinationAddress)
      )

      spendActions = swapPendingSpendActionResponse.sharedArtifacts.map(_.asInstanceOf[SpendAction]).toList
      metagraphId <- context.getCurrencyId

      swapConfirmedResponse <-
        if (spendActions.nonEmpty) {
          swapCombinerService.combinePendingSpendAction(
            PendingSpendAction(swapUpdate, spendActions.head),
            swapPendingSpendActionResponse,
            EpochProgress.MinValue,
            spendActions,
            SnapshotOrdinal.MinValue,
            metagraphId
          )
        } else {
          IO.pure(swapPendingSpendActionResponse)
        }

      swapCalculatedState = swapConfirmedResponse.calculated.operations(OperationType.Swap).asInstanceOf[SwapCalculatedState]
    } yield
      expect.all(
        swapCalculatedState.confirmed.value.nonEmpty === false,
        swapCalculatedState.failed.nonEmpty,
        swapCalculatedState.failed.toList.head.reason == SwapWouldDrainPoolBalance()
      )
  }

  test("Test swap successfully - multiple swaps stopping before drain pool") { implicit res =>
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
    val destinationAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAP")

    val (poolId, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)

    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )

    val initialState = DataState(ammOnChainState, ammCalculatedState)
    val numSwaps = 10

    (1 to numSwaps)
      .foldLeft(IO.pure((initialState, expect(true)))) {
        case (accIO, i) =>
          accIO.flatMap {
            case (accState, accExpectations) =>
              val swapAmount = 100.0 * i
              val fixedSwapAmount = toFixedPoint(swapAmount)

              for {
                keyPair <- KeyPairGenerator.makeKeyPair[IO]
                allowSpend = AllowSpend(
                  sourceAddress,
                  destinationAddress,
                  Some(CurrencyId(ownerAddress)),
                  SwapAmount(PosLong.from(toFixedPoint(1000.0) - i).getOrElse(PosLong.MaxValue)),
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
                    SwapAmount(PosLong.unsafeFrom(fixedSwapAmount)),
                    SwapAmount(PosLong.MinValue),
                    EpochProgress.MaxValue,
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
                  destinationAddress
                )

                calculatedStateService <- CalculatedStateService.make[IO]
                _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, accState.calculated)
                pricingService = PricingService.make[IO](config, calculatedStateService)
                jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
                swapCombinerService = SwapCombinerService.make[IO](config, pricingService, jsonBase64BinaryCodec)

                swapPendingSpendActionResponse <- swapCombinerService.combineNew(
                  swapUpdate,
                  accState,
                  EpochProgress.MinValue,
                  allowSpends,
                  CurrencyId(destinationAddress)
                )

                (updatedState, currentExpectations) <-
                  if (i > 8) {
                    IO.pure(
                      (
                        swapPendingSpendActionResponse,
                        expect(swapPendingSpendActionResponse.sharedArtifacts.isEmpty, s"Expected swap $i to fail due to pool depletion")
                      )
                    )
                  } else {
                    for {
                      metagraphId <- context.getCurrencyId
                      spendActions = swapPendingSpendActionResponse.sharedArtifacts.map(_.asInstanceOf[SpendAction]).toList

                      swapConfirmedResponse <- swapCombinerService.combinePendingSpendAction(
                        PendingSpendAction(swapUpdate, spendActions.head),
                        swapPendingSpendActionResponse,
                        EpochProgress.MinValue,
                        spendActions,
                        SnapshotOrdinal.MinValue,
                        metagraphId
                      )

                      oldLiquidityPoolCalculatedState = accState.calculated
                        .operations(OperationType.LiquidityPool)
                        .asInstanceOf[LiquidityPoolCalculatedState]
                      updatedLiquidityPoolCalculatedState = swapConfirmedResponse.calculated
                        .operations(OperationType.LiquidityPool)
                        .asInstanceOf[LiquidityPoolCalculatedState]

                      oldLiquidityPool = oldLiquidityPoolCalculatedState.confirmed.value(poolId)
                      updatedLiquidityPool = updatedLiquidityPoolCalculatedState.confirmed.value(poolId)

                      newTokenAAmount = oldLiquidityPool.tokenA.amount.value + fixedSwapAmount
                      currentExpectations = expect.all(
                        newTokenAAmount === updatedLiquidityPool.tokenA.amount.value,
                        updatedLiquidityPool.k / newTokenAAmount === updatedLiquidityPool.tokenB.amount.value
                      )
                    } yield (swapConfirmedResponse, currentExpectations)
                  }
              } yield (updatedState, accExpectations.and(currentExpectations))
          }
      }
      .map(_._2)
  }
}
