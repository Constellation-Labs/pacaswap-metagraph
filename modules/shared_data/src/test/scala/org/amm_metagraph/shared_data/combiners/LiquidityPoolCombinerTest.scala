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
import io.constellationnetwork.security.key.ops.PublicKeyOps
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hasher, KeyPairGenerator, SecurityProvider}

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.{NonNegLong, PosLong}
import org.amm_metagraph.shared_data.DummyL0Context.buildL0NodeContext
import org.amm_metagraph.shared_data.Shared._
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.services.combiners.LiquidityPoolCombinerService
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.LiquidityPool.{
  TokenInformation,
  buildLiquidityPoolUniqueIdentifier,
  getPendingSpendActionLiquidityPoolUpdates
}
import org.amm_metagraph.shared_data.types.States.OperationType.LiquidityPool
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.codecs.{HasherSelector, JsonWithBase64BinaryCodec}
import org.amm_metagraph.shared_data.validations.Errors._
import org.amm_metagraph.shared_data.validations.LiquidityPoolValidations
import weaver.MutableIOSuite

object LiquidityPoolCombinerTest extends MutableIOSuite {

  type Res = (Hasher[IO], HasherSelector[IO], SecurityProvider[IO])

  override def sharedResource: Resource[IO, Res] = for {
    sp <- SecurityProvider.forAsync[IO]
    implicit0(j: JsonSerializer[IO]) <- JsonSerializer.forSync[IO].asResource
    h = Hasher.forJson[IO]
    hs = HasherSelector.forSync(h, h)
  } yield (h, hs, sp)

  test("Successfully create a liquidity pool - confirmed") { implicit res =>
    implicit val (h, hs, sp) = res

    val tokenAId = CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some
    val tokenAAmount = PosLong.unsafeFrom(100L.toTokenAmountFormat)
    val tokenBId = CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some
    val tokenBAmount = PosLong.unsafeFrom(50L.toTokenAmountFormat)

    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")
    val destinationAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAP")
    val ammOnChainState = AmmOnChainState(SortedSet.empty, None)
    val ammCalculatedState = AmmCalculatedState()
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      allowSpendTokenA = AllowSpend(
        ownerAddress,
        destinationAddress,
        tokenAId,
        SwapAmount(PosLong.MaxValue),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        EpochProgress.MaxValue,
        List.empty
      )
      allowSpendTokenB = AllowSpend(
        ownerAddress,
        destinationAddress,
        tokenBId,
        SwapAmount(PosLong.MaxValue),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        EpochProgress.MaxValue,
        List.empty
      )

      signedAllowSpendA <- Signed
        .forAsyncHasher[IO, AllowSpend](allowSpendTokenA, keyPair)
        .flatMap(_.toHashed[IO])
      signedAllowSpendB <- Signed
        .forAsyncHasher[IO, AllowSpend](allowSpendTokenB, keyPair)
        .flatMap(_.toHashed[IO])

      liquidityPoolUpdate = getFakeSignedUpdate(
        LiquidityPoolUpdate(
          CurrencyId(destinationAddress),
          sourceAddress,
          signedAllowSpendA.hash,
          signedAllowSpendB.hash,
          tokenAId,
          tokenBId,
          tokenAAmount,
          tokenBAmount,
          EpochProgress.MaxValue,
          None
        )
      )

      allowSpends = SortedMap(
        tokenAId.get.value.some ->
          SortedMap(
            ownerAddress -> SortedSet(signedAllowSpendA.signed)
          ),
        tokenBId.get.value.some ->
          SortedMap(
            ownerAddress -> SortedSet(signedAllowSpendB.signed)
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

      liquidityPoolValidations = LiquidityPoolValidations.make[IO](config)
      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      liquidityPoolCombinerService = LiquidityPoolCombinerService.make[IO](liquidityPoolValidations, jsonBase64BinaryCodec)

      liquidityPoolPendingSpendActionResponse <- liquidityPoolCombinerService.combineNew(
        liquidityPoolUpdate,
        state,
        EpochProgress.MinValue,
        allowSpends,
        CurrencyId(destinationAddress)
      )

      spendActions = liquidityPoolPendingSpendActionResponse.sharedArtifacts.map(_.asInstanceOf[SpendAction]).toList
      poolId <- buildLiquidityPoolUniqueIdentifier(tokenAId, tokenBId)
      pending = getPendingSpendActionLiquidityPoolUpdates(liquidityPoolPendingSpendActionResponse.calculated)

      liquidityPoolConfirmedResponse <- liquidityPoolCombinerService.combinePendingSpendAction(
        PendingSpendAction(liquidityPoolUpdate, pending.head.updateHash, spendActions.head),
        liquidityPoolPendingSpendActionResponse,
        EpochProgress.MinValue,
        spendActions,
        SnapshotOrdinal.MinValue,
        CurrencyId(destinationAddress)
      )
      updatedLiquidityPoolCalculatedState = liquidityPoolConfirmedResponse.calculated
        .operations(OperationType.LiquidityPool)
        .asInstanceOf[LiquidityPoolCalculatedState]
      updatedLiquidityPool = updatedLiquidityPoolCalculatedState.confirmed.value(poolId.value)

    } yield
      expect.eql(100L.toTokenAmountFormat, updatedLiquidityPool.tokenA.amount.value) &&
        expect.eql(tokenAId.get, updatedLiquidityPool.tokenA.identifier.get) &&
        expect.eql(50L.toTokenAmountFormat, updatedLiquidityPool.tokenB.amount.value) &&
        expect.eql(tokenBId.get, updatedLiquidityPool.tokenB.identifier.get) &&
        expect.eql(BigInt(100L.toTokenAmountFormat) * BigInt(50.toTokenAmountFormat), updatedLiquidityPool.k) &&
        expect.eql(1.toTokenAmountFormat, updatedLiquidityPool.poolShares.totalShares.value) &&
        expect.eql(1, updatedLiquidityPool.poolShares.addressShares.size) &&
        expect.eql(1.toTokenAmountFormat, updatedLiquidityPool.poolShares.addressShares(ownerAddress).value.value.value) &&
        expect.eql(1, liquidityPoolPendingSpendActionResponse.calculated.operations(OperationType.LiquidityPool).pending.size)
  }

  test("Successfully create a liquidity pool - L0Token - DAG") { implicit res =>
    implicit val (h, hs, sp) = res

    val tokenAId = CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some
    val tokenAAmount = PosLong.unsafeFrom(100L.toTokenAmountFormat)
    val tokenBId = none
    val tokenBAmount = PosLong.unsafeFrom(50L.toTokenAmountFormat)

    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")
    val destinationAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAP")

    val ammOnChainState = AmmOnChainState(SortedSet.empty, None)
    val ammCalculatedState = AmmCalculatedState()
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      allowSpendTokenA = AllowSpend(
        ownerAddress,
        destinationAddress,
        tokenAId,
        SwapAmount(PosLong.MaxValue),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        EpochProgress.MaxValue,
        List.empty
      )
      allowSpendTokenB = AllowSpend(
        ownerAddress,
        destinationAddress,
        tokenBId,
        SwapAmount(PosLong.MaxValue),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        EpochProgress.MaxValue,
        List.empty
      )

      signedAllowSpendA <- Signed
        .forAsyncHasher[IO, AllowSpend](allowSpendTokenA, keyPair)
        .flatMap(_.toHashed[IO])
      signedAllowSpendB <- Signed
        .forAsyncHasher[IO, AllowSpend](allowSpendTokenB, keyPair)
        .flatMap(_.toHashed[IO])

      liquidityPoolUpdate = getFakeSignedUpdate(
        LiquidityPoolUpdate(
          CurrencyId(ownerAddress),
          sourceAddress,
          signedAllowSpendA.hash,
          signedAllowSpendB.hash,
          tokenAId,
          tokenBId,
          tokenAAmount,
          tokenBAmount,
          EpochProgress.MaxValue,
          None
        )
      )

      allowSpends = SortedMap(
        tokenAId.get.value.some ->
          SortedMap(
            ownerAddress -> SortedSet(signedAllowSpendA.signed)
          ),
        tokenBId ->
          SortedMap(
            ownerAddress -> SortedSet(signedAllowSpendB.signed)
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

      liquidityPoolValidations = LiquidityPoolValidations.make[IO](config)
      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      liquidityPoolCombinerService = LiquidityPoolCombinerService.make[IO](liquidityPoolValidations, jsonBase64BinaryCodec)

      liquidityPoolPendingSpendActionResponse <- liquidityPoolCombinerService.combineNew(
        liquidityPoolUpdate,
        state,
        EpochProgress.MinValue,
        allowSpends,
        CurrencyId(destinationAddress)
      )
      spendActions = liquidityPoolPendingSpendActionResponse.sharedArtifacts.map(_.asInstanceOf[SpendAction]).toList
      pending = getPendingSpendActionLiquidityPoolUpdates(liquidityPoolPendingSpendActionResponse.calculated)
      poolId <- buildLiquidityPoolUniqueIdentifier(tokenAId, tokenBId)

      liquidityPoolConfirmedResponse <- liquidityPoolCombinerService.combinePendingSpendAction(
        PendingSpendAction(liquidityPoolUpdate, pending.head.updateHash, spendActions.head),
        liquidityPoolPendingSpendActionResponse,
        EpochProgress.MinValue,
        spendActions,
        SnapshotOrdinal.MinValue,
        CurrencyId(destinationAddress)
      )

      updatedLiquidityPoolCalculatedState = liquidityPoolConfirmedResponse.calculated
        .operations(OperationType.LiquidityPool)
        .asInstanceOf[LiquidityPoolCalculatedState]
      updatedLiquidityPool = updatedLiquidityPoolCalculatedState.confirmed.value(poolId.value)
    } yield
      expect.eql(100L.toTokenAmountFormat, updatedLiquidityPool.tokenA.amount.value) &&
        expect.eql(tokenAId.get, updatedLiquidityPool.tokenA.identifier.get) &&
        expect.eql(50L.toTokenAmountFormat, updatedLiquidityPool.tokenB.amount.value) &&
        expect.eql(tokenBId.isEmpty, updatedLiquidityPool.tokenB.identifier.isEmpty) &&
        expect.eql(BigInt(100L.toTokenAmountFormat) * BigInt(50.toTokenAmountFormat), updatedLiquidityPool.k) &&
        expect.eql(1.toTokenAmountFormat, updatedLiquidityPool.poolShares.totalShares.value) &&
        expect.eql(1, updatedLiquidityPool.poolShares.addressShares.size) &&
        expect.eql(1.toTokenAmountFormat, updatedLiquidityPool.poolShares.addressShares(ownerAddress).value.value.value) &&
        expect.eql(1, liquidityPoolPendingSpendActionResponse.calculated.operations(OperationType.LiquidityPool).pending.size)
  }

  test("Return failed due staking more than allowSpend") { implicit res =>
    implicit val (h, hs, sp) = res

    val primaryToken = TokenInformation(
      CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some,
      PosLong.unsafeFrom(toFixedPoint(100.0))
    )

    val pairToken = TokenInformation(
      CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some,
      PosLong.unsafeFrom(toFixedPoint(50.0))
    )

    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")
    val destinationAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAP")

    val ammOnChainState = AmmOnChainState(SortedSet.empty, None)
    val ammCalculatedState = AmmCalculatedState()

    val state = DataState(ammOnChainState, ammCalculatedState)
    val futureEpoch = EpochProgress(NonNegLong.unsafeFrom(10L))

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      allowSpendTokenA = AllowSpend(
        ownerAddress,
        destinationAddress,
        primaryToken.identifier,
        SwapAmount(PosLong(1)),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        EpochProgress.MinValue,
        List.empty
      )
      allowSpendTokenB = AllowSpend(
        ownerAddress,
        destinationAddress,
        pairToken.identifier,
        SwapAmount(PosLong(1)),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        EpochProgress.MinValue,
        List.empty
      )

      signedAllowSpendA <- Signed
        .forAsyncHasher[IO, AllowSpend](allowSpendTokenA, keyPair)
        .flatMap(_.toHashed[IO])
      signedAllowSpendB <- Signed
        .forAsyncHasher[IO, AllowSpend](allowSpendTokenB, keyPair)
        .flatMap(_.toHashed[IO])

      liquidityPoolUpdate = getFakeSignedUpdate(
        LiquidityPoolUpdate(
          CurrencyId(ownerAddress),
          sourceAddress,
          signedAllowSpendA.hash,
          signedAllowSpendB.hash,
          primaryToken.identifier,
          pairToken.identifier,
          primaryToken.amount,
          pairToken.amount,
          EpochProgress.MaxValue,
          None
        )
      )

      allowSpends = SortedMap(
        primaryToken.identifier.get.value.some ->
          SortedMap(
            ownerAddress -> SortedSet(signedAllowSpendA.signed)
          ),
        pairToken.identifier.get.value.some ->
          SortedMap(
            ownerAddress -> SortedSet(signedAllowSpendB.signed)
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

      liquidityPoolValidations = LiquidityPoolValidations.make[IO](config)
      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      liquidityPoolCombinerService = LiquidityPoolCombinerService.make[IO](liquidityPoolValidations, jsonBase64BinaryCodec)

      liquidityPoolResponse <- liquidityPoolCombinerService.combineNew(
        liquidityPoolUpdate,
        state,
        futureEpoch,
        allowSpends,
        CurrencyId(destinationAddress)
      )
      liquidityPoolCalculatedState = liquidityPoolResponse.calculated.operations(LiquidityPool).asInstanceOf[LiquidityPoolCalculatedState]
    } yield
      expect.all(
        liquidityPoolCalculatedState.failed.toList.length === 1,
        liquidityPoolCalculatedState.failed.toList.head.expiringEpochProgress === EpochProgress(
          NonNegLong.unsafeFrom(futureEpoch.value.value + config.expirationEpochProgresses.failedOperations.value.value)
        ),
        liquidityPoolCalculatedState.failed.toList.head.reason == AmountGreaterThanAllowSpendLimit(allowSpendTokenA)
      )
  }

  test("Return expired epoch progress when update exceeds allowSpend limit") { implicit res =>
    implicit val (h, hs, sp) = res

    val primaryToken = TokenInformation(
      CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some,
      PosLong.unsafeFrom(toFixedPoint(100.0))
    )

    val pairToken = TokenInformation(
      CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some,
      PosLong.unsafeFrom(toFixedPoint(50.0))
    )

    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")
    val destinationAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAP")

    val ammOnChainState = AmmOnChainState(Set.empty, None)
    val ammCalculatedState = AmmCalculatedState()

    val state = DataState(ammOnChainState, ammCalculatedState)
    val futureEpoch = EpochProgress(NonNegLong.unsafeFrom(10L))

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      allowSpendTokenA = AllowSpend(
        ownerAddress,
        destinationAddress,
        primaryToken.identifier,
        SwapAmount(PosLong.MaxValue),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        EpochProgress.MinValue,
        List.empty
      )
      allowSpendTokenB = AllowSpend(
        ownerAddress,
        destinationAddress,
        pairToken.identifier,
        SwapAmount(PosLong.MaxValue),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        EpochProgress.MinValue,
        List.empty
      )

      signedAllowSpendA <- Signed
        .forAsyncHasher[IO, AllowSpend](allowSpendTokenA, keyPair)
        .flatMap(_.toHashed[IO])
      signedAllowSpendB <- Signed
        .forAsyncHasher[IO, AllowSpend](allowSpendTokenB, keyPair)
        .flatMap(_.toHashed[IO])

      liquidityPoolUpdate = getFakeSignedUpdate(
        LiquidityPoolUpdate(
          CurrencyId(destinationAddress),
          sourceAddress,
          signedAllowSpendA.hash,
          signedAllowSpendB.hash,
          primaryToken.identifier,
          pairToken.identifier,
          primaryToken.amount,
          pairToken.amount,
          EpochProgress.MaxValue,
          None
        )
      )

      allowSpends = SortedMap(
        primaryToken.identifier.get.value.some ->
          SortedMap(
            ownerAddress -> SortedSet(signedAllowSpendA.signed)
          ),
        pairToken.identifier.get.value.some ->
          SortedMap(
            ownerAddress -> SortedSet(signedAllowSpendB.signed)
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
      liquidityPoolValidations = LiquidityPoolValidations.make[IO](config)
      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      liquidityPoolCombinerService = LiquidityPoolCombinerService.make[IO](liquidityPoolValidations, jsonBase64BinaryCodec)

      liquidityPoolResponse <- liquidityPoolCombinerService.combineNew(
        liquidityPoolUpdate,
        state,
        futureEpoch,
        allowSpends,
        CurrencyId(destinationAddress)
      )
      liquidityPoolCalculatedState = liquidityPoolResponse.calculated.operations(LiquidityPool).asInstanceOf[LiquidityPoolCalculatedState]
    } yield
      expect.all(
        liquidityPoolCalculatedState.failed.toList.length === 1,
        liquidityPoolCalculatedState.failed.toList.head.expiringEpochProgress === EpochProgress(
          NonNegLong.unsafeFrom(futureEpoch.value.value + config.expirationEpochProgresses.failedOperations.value.value)
        ),
        liquidityPoolCalculatedState.failed.toList.head.reason == AllowSpendExpired(allowSpendTokenA)
      )
  }

  test("Return failed - pool already exists") { implicit res =>
    implicit val (h, hs, sp) = res

    val primaryToken = TokenInformation(
      CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some,
      PosLong.unsafeFrom(toFixedPoint(100.0))
    )

    val pairToken = TokenInformation(
      CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some,
      PosLong.unsafeFrom(toFixedPoint(50.0))
    )

    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")
    val destinationAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAP")

    val (_, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val ammOnChainState = AmmOnChainState(SortedSet.empty, None)
    val ammCalculatedState = AmmCalculatedState(
      SortedMap(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )

    val state = DataState(ammOnChainState, ammCalculatedState)
    val futureEpoch = EpochProgress(NonNegLong.unsafeFrom(10L))

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      allowSpendTokenA = AllowSpend(
        ownerAddress,
        destinationAddress,
        primaryToken.identifier,
        SwapAmount(PosLong.MaxValue),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        EpochProgress.MaxValue,
        List.empty
      )
      allowSpendTokenB = AllowSpend(
        ownerAddress,
        destinationAddress,
        pairToken.identifier,
        SwapAmount(PosLong.MaxValue),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        EpochProgress.MaxValue,
        List.empty
      )

      signedAllowSpendA <- Signed
        .forAsyncHasher[IO, AllowSpend](allowSpendTokenA, keyPair)
        .flatMap(_.toHashed[IO])
      signedAllowSpendB <- Signed
        .forAsyncHasher[IO, AllowSpend](allowSpendTokenB, keyPair)
        .flatMap(_.toHashed[IO])

      liquidityPoolUpdate = getFakeSignedUpdate(
        LiquidityPoolUpdate(
          CurrencyId(destinationAddress),
          sourceAddress,
          signedAllowSpendA.hash,
          signedAllowSpendB.hash,
          primaryToken.identifier,
          pairToken.identifier,
          primaryToken.amount,
          pairToken.amount,
          EpochProgress.MaxValue,
          None
        )
      )

      allowSpends = SortedMap(
        primaryToken.identifier.get.value.some ->
          SortedMap(
            ownerAddress -> SortedSet(signedAllowSpendA.signed)
          ),
        pairToken.identifier.get.value.some ->
          SortedMap(
            ownerAddress -> SortedSet(signedAllowSpendB.signed)
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

      liquidityPoolValidations = LiquidityPoolValidations.make[IO](config)
      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      liquidityPoolCombinerService = LiquidityPoolCombinerService.make[IO](liquidityPoolValidations, jsonBase64BinaryCodec)

      liquidityPoolResponse <- liquidityPoolCombinerService.combineNew(
        liquidityPoolUpdate,
        state,
        futureEpoch,
        allowSpends,
        CurrencyId(destinationAddress)
      )
      liquidityPoolCalculatedState = liquidityPoolResponse.calculated.operations(LiquidityPool).asInstanceOf[LiquidityPoolCalculatedState]
    } yield
      expect.all(
        liquidityPoolCalculatedState.failed.toList.length === 1,
        liquidityPoolCalculatedState.failed.toList.head.expiringEpochProgress === EpochProgress(
          NonNegLong.unsafeFrom(futureEpoch.value.value + config.expirationEpochProgresses.failedOperations.value.value)
        ),
        liquidityPoolCalculatedState.failed.toList.head.reason == DuplicatedLiquidityPoolRequest(liquidityPoolUpdate)
      )
  }

  test("Return fail - different currencyId between allowSpend and update") { implicit res =>
    implicit val (h, hs, sp) = res

    val tokenAId = CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some
    val tokenAAmount = PosLong.unsafeFrom(100L.toTokenAmountFormat)
    val tokenBId = none
    val tokenBAmount = PosLong.unsafeFrom(50L.toTokenAmountFormat)

    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")
    val destinationAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAP")

    val ammOnChainState = AmmOnChainState(SortedSet.empty, None)
    val ammCalculatedState = AmmCalculatedState()
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      allowSpendTokenA = AllowSpend(
        ownerAddress,
        destinationAddress,
        none,
        SwapAmount(PosLong.MaxValue),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        EpochProgress.MaxValue,
        List.empty
      )
      allowSpendTokenB = AllowSpend(
        ownerAddress,
        destinationAddress,
        tokenBId,
        SwapAmount(PosLong.MaxValue),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        EpochProgress.MaxValue,
        List.empty
      )

      signedAllowSpendA <- Signed
        .forAsyncHasher[IO, AllowSpend](allowSpendTokenA, keyPair)
        .flatMap(_.toHashed[IO])
      signedAllowSpendB <- Signed
        .forAsyncHasher[IO, AllowSpend](allowSpendTokenB, keyPair)
        .flatMap(_.toHashed[IO])

      liquidityPoolUpdate = getFakeSignedUpdate(
        LiquidityPoolUpdate(
          CurrencyId(ownerAddress),
          sourceAddress,
          signedAllowSpendA.hash,
          signedAllowSpendB.hash,
          tokenAId,
          tokenBId,
          tokenAAmount,
          tokenBAmount,
          EpochProgress.MaxValue,
          None
        )
      )

      allowSpends = SortedMap(
        tokenAId.get.value.some ->
          SortedMap(
            ownerAddress -> SortedSet(signedAllowSpendA.signed)
          ),
        tokenBId ->
          SortedMap(
            ownerAddress -> SortedSet(signedAllowSpendB.signed)
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

      futureEpoch = EpochProgress(NonNegLong.unsafeFrom(10L))

      liquidityPoolValidations = LiquidityPoolValidations.make[IO](config)
      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      liquidityPoolCombinerService = LiquidityPoolCombinerService.make[IO](liquidityPoolValidations, jsonBase64BinaryCodec)

      liquidityPoolPendingSpendActionResponse <- liquidityPoolCombinerService.combineNew(
        liquidityPoolUpdate,
        state,
        futureEpoch,
        allowSpends,
        CurrencyId(destinationAddress)
      )
      liquidityPoolCalculatedState = liquidityPoolPendingSpendActionResponse.calculated
        .operations(LiquidityPool)
        .asInstanceOf[LiquidityPoolCalculatedState]
    } yield
      expect.all(
        liquidityPoolCalculatedState.failed.toList.length === 1,
        liquidityPoolCalculatedState.failed.toList.head.expiringEpochProgress === EpochProgress(
          NonNegLong.unsafeFrom(futureEpoch.value.value + config.expirationEpochProgresses.failedOperations.value.value)
        ),
        liquidityPoolCalculatedState.failed.toList.head.reason == InvalidCurrencyIdsBetweenAllowSpendsAndDataUpdate(liquidityPoolUpdate)
      )
  }

  test("Failed because of allowSpendEpochBufferDelay") { implicit res =>
    implicit val (h, hs, sp) = res
    val tokensLockEpoch = EpochProgress(NonNegLong.unsafeFrom(10L))
    val bufferDelay = EpochProgress(NonNegLong.unsafeFrom(1L))
    val currentEpoch = EpochProgress(NonNegLong.unsafeFrom(tokensLockEpoch.value.value + bufferDelay.value.value + 1))

    val tokenAId = CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some
    val tokenAAmount = PosLong.unsafeFrom(100L.toTokenAmountFormat)
    val tokenBId = CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some
    val tokenBAmount = PosLong.unsafeFrom(50L.toTokenAmountFormat)

    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")
    val destinationAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAP")
    val ammOnChainState = AmmOnChainState(SortedSet.empty, None)
    val ammCalculatedState = AmmCalculatedState()
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      allowSpendTokenA = AllowSpend(
        ownerAddress,
        destinationAddress,
        tokenAId,
        SwapAmount(PosLong.MaxValue),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        tokensLockEpoch,
        List.empty
      )
      allowSpendTokenB = AllowSpend(
        ownerAddress,
        destinationAddress,
        tokenBId,
        SwapAmount(PosLong.MaxValue),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        tokensLockEpoch,
        List.empty
      )

      signedAllowSpendA <- Signed
        .forAsyncHasher[IO, AllowSpend](allowSpendTokenA, keyPair)
        .flatMap(_.toHashed[IO])
      signedAllowSpendB <- Signed
        .forAsyncHasher[IO, AllowSpend](allowSpendTokenB, keyPair)
        .flatMap(_.toHashed[IO])

      liquidityPoolUpdate = getFakeSignedUpdate(
        LiquidityPoolUpdate(
          CurrencyId(destinationAddress),
          sourceAddress,
          signedAllowSpendA.hash,
          signedAllowSpendB.hash,
          tokenAId,
          tokenBId,
          tokenAAmount,
          tokenBAmount,
          EpochProgress.MaxValue,
          None
        )
      )

      allowSpends = SortedMap(
        tokenAId.get.value.some ->
          SortedMap(
            ownerAddress -> SortedSet(signedAllowSpendA.signed)
          ),
        tokenBId.get.value.some ->
          SortedMap(
            ownerAddress -> SortedSet(signedAllowSpendB.signed)
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

      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      liquidityPoolValidations = LiquidityPoolValidations.make[IO](config)
      liquidityPoolCombinerService = LiquidityPoolCombinerService.make[IO](liquidityPoolValidations, jsonBase64BinaryCodec)
      liquidityPoolResponse <- liquidityPoolCombinerService.combineNew(
        liquidityPoolUpdate,
        state,
        currentEpoch,
        allowSpends,
        CurrencyId(destinationAddress)
      )

      liquidityPoolCalculatedState = liquidityPoolResponse.calculated.operations(LiquidityPool).asInstanceOf[LiquidityPoolCalculatedState]
    } yield
      expect.all(
        liquidityPoolCalculatedState.failed.toList.length === 1,
        liquidityPoolCalculatedState.failed.toList.head.expiringEpochProgress === EpochProgress(
          NonNegLong.unsafeFrom(currentEpoch.value.value + config.expirationEpochProgresses.failedOperations.value.value)
        ),
        liquidityPoolCalculatedState.failed.toList.head.reason == AllowSpendExpired(allowSpendTokenA)
      )
  }

  test("Failed because allowSpend source different than LPUpdate source") { implicit res =>
    implicit val (h, hs, sp) = res

    val tokenAId = CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some
    val tokenAAmount = PosLong.unsafeFrom(100L.toTokenAmountFormat)
    val tokenBId = CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some
    val tokenBAmount = PosLong.unsafeFrom(50L.toTokenAmountFormat)

    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")
    val destinationAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAP")
    val ammOnChainState = AmmOnChainState(SortedSet.empty, None)
    val ammCalculatedState = AmmCalculatedState()
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      keyPair2 <- KeyPairGenerator.makeKeyPair[IO]
      address2 = keyPair2.getPublic.toAddress
      allowSpendTokenA = AllowSpend(
        address2,
        destinationAddress,
        tokenAId,
        SwapAmount(PosLong.MaxValue),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        EpochProgress.MaxValue,
        List.empty
      )
      allowSpendTokenB = AllowSpend(
        address2,
        destinationAddress,
        tokenBId,
        SwapAmount(PosLong.MaxValue),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        EpochProgress.MaxValue,
        List.empty
      )

      signedAllowSpendA <- Signed
        .forAsyncHasher[IO, AllowSpend](allowSpendTokenA, keyPair2)
        .flatMap(_.toHashed[IO])
      signedAllowSpendB <- Signed
        .forAsyncHasher[IO, AllowSpend](allowSpendTokenB, keyPair2)
        .flatMap(_.toHashed[IO])

      liquidityPoolUpdate = getFakeSignedUpdate(
        LiquidityPoolUpdate(
          CurrencyId(destinationAddress),
          sourceAddress,
          signedAllowSpendA.hash,
          signedAllowSpendB.hash,
          tokenAId,
          tokenBId,
          tokenAAmount,
          tokenBAmount,
          EpochProgress.MaxValue,
          None
        )
      )

      allowSpends = SortedMap(
        tokenAId.get.value.some ->
          SortedMap(
            ownerAddress -> SortedSet(signedAllowSpendA.signed)
          ),
        tokenBId.get.value.some ->
          SortedMap(
            ownerAddress -> SortedSet(signedAllowSpendB.signed)
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

      liquidityPoolValidations = LiquidityPoolValidations.make[IO](config)
      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      liquidityPoolCombinerService = LiquidityPoolCombinerService.make[IO](liquidityPoolValidations, jsonBase64BinaryCodec)

      futureEpoch = EpochProgress(NonNegLong.unsafeFrom(10L))

      liquidityPoolPendingSpendActionResponse <- liquidityPoolCombinerService.combineNew(
        liquidityPoolUpdate,
        state,
        futureEpoch,
        allowSpends,
        CurrencyId(destinationAddress)
      )

      liquidityPoolCalculatedState = liquidityPoolPendingSpendActionResponse.calculated
        .operations(LiquidityPool)
        .asInstanceOf[LiquidityPoolCalculatedState]

    } yield
      expect.all(
        liquidityPoolCalculatedState.failed.toList.length === 1,
        liquidityPoolCalculatedState.failed.toList.head.expiringEpochProgress === EpochProgress(
          NonNegLong.unsafeFrom(futureEpoch.value.value + config.expirationEpochProgresses.failedOperations.value.value)
        ),
        liquidityPoolCalculatedState.failed.toList.head.reason == SourceAddressBetweenUpdateAndAllowSpendDifferent(liquidityPoolUpdate)
      )
  }

  test("Test failure lp creation - duplicated allow spend") { implicit res =>
    implicit val (h, hs, sp) = res

    val tokenAId = CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some
    val tokenAAmount = PosLong.unsafeFrom(100L.toTokenAmountFormat)
    val tokenBId = CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some
    val tokenBAmount = PosLong.unsafeFrom(50L.toTokenAmountFormat)

    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")
    val destinationAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAP")
    val ammOnChainState = AmmOnChainState(SortedSet.empty, None)
    val ammCalculatedState = AmmCalculatedState()
    val state = DataState(ammOnChainState, ammCalculatedState)
    val futureEpoch = EpochProgress(NonNegLong.unsafeFrom(10L))

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      allowSpendTokenA = AllowSpend(
        ownerAddress,
        destinationAddress,
        tokenAId,
        SwapAmount(PosLong.MaxValue),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        EpochProgress.MaxValue,
        List.empty
      )
      allowSpendTokenB = AllowSpend(
        ownerAddress,
        destinationAddress,
        tokenBId,
        SwapAmount(PosLong.MaxValue),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        EpochProgress.MaxValue,
        List.empty
      )

      signedAllowSpendA <- Signed
        .forAsyncHasher[IO, AllowSpend](allowSpendTokenA, keyPair)
        .flatMap(_.toHashed[IO])
      signedAllowSpendB <- Signed
        .forAsyncHasher[IO, AllowSpend](allowSpendTokenB, keyPair)
        .flatMap(_.toHashed[IO])

      liquidityPoolUpdate = getFakeSignedUpdate(
        LiquidityPoolUpdate(
          CurrencyId(destinationAddress),
          sourceAddress,
          signedAllowSpendA.hash,
          signedAllowSpendB.hash,
          tokenAId,
          tokenBId,
          tokenAAmount,
          tokenBAmount,
          EpochProgress.MaxValue,
          None
        )
      )

      allowSpends = SortedMap(
        tokenAId.get.value.some ->
          SortedMap(
            ownerAddress -> SortedSet(signedAllowSpendA.signed)
          ),
        tokenBId.get.value.some ->
          SortedMap(
            ownerAddress -> SortedSet(signedAllowSpendB.signed)
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

      liquidityPoolValidations = LiquidityPoolValidations.make[IO](config)
      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      liquidityPoolCombinerService = LiquidityPoolCombinerService.make[IO](liquidityPoolValidations, jsonBase64BinaryCodec)

      liquidityPoolPendingSpendActionResponse <- liquidityPoolCombinerService.combineNew(
        liquidityPoolUpdate,
        state,
        futureEpoch,
        allowSpends,
        CurrencyId(destinationAddress)
      )

      liquidityPoolPendingSpendActionResponse2 <- liquidityPoolCombinerService.combineNew(
        liquidityPoolUpdate,
        liquidityPoolPendingSpendActionResponse,
        futureEpoch,
        allowSpends,
        CurrencyId(destinationAddress)
      )

      lpCalculatedState = liquidityPoolPendingSpendActionResponse2.calculated
        .operations(LiquidityPool)
        .asInstanceOf[LiquidityPoolCalculatedState]
    } yield
      expect.all(
        lpCalculatedState.failed.toList.length === 1,
        lpCalculatedState.failed.toList.head.expiringEpochProgress === EpochProgress(
          NonNegLong.unsafeFrom(futureEpoch.value.value + config.expirationEpochProgresses.failedOperations.value.value)
        ),
        lpCalculatedState.failed.toList.head.reason == DuplicatedAllowSpend(liquidityPoolUpdate)
      )
  }

}
