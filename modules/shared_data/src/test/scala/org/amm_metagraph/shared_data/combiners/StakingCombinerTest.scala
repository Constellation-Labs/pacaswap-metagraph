package org.amm_metagraph.shared_data.combiners

import cats.effect.{IO, Resource}
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.ext.cats.effect.ResourceIO
import io.constellationnetwork.json.JsonSerializer
import io.constellationnetwork.schema._
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact.SpendAction
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap._
import io.constellationnetwork.security._
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.key.ops.PublicKeyOps
import io.constellationnetwork.security.signature.Signed

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.{NonNegLong, PosLong}
import monocle.syntax.all._
import org.amm_metagraph.shared_data.DummyL0Context.buildL0NodeContext
import org.amm_metagraph.shared_data.Shared._
import org.amm_metagraph.shared_data.calculated_state.CalculatedStateService
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.services.combiners.StakingCombinerService
import org.amm_metagraph.shared_data.services.pricing.PricingService
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, StakingUpdate}
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.Staking.{StakingReference, getPendingSpendActionStakingUpdates}
import org.amm_metagraph.shared_data.types.States.OperationType.Staking
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.codecs
import org.amm_metagraph.shared_data.types.codecs.JsonWithBase64BinaryCodec
import org.amm_metagraph.shared_data.validations.Errors._
import org.amm_metagraph.shared_data.validations.StakingValidations
import weaver.MutableIOSuite

object StakingCombinerTest extends MutableIOSuite {

  type Res = (Hasher[IO], codecs.HasherSelector[IO], SecurityProvider[IO])

  override def sharedResource: Resource[IO, Res] = for {
    sp <- SecurityProvider.forAsync[IO]
    implicit0(j: JsonSerializer[IO]) <- JsonSerializer.forSync[IO].asResource
    h = Hasher.forJson[IO]
    hs = codecs.HasherSelector.forSync(h, h)
  } yield (h, hs, sp)

  test("Test successful staking - single provider") { implicit res =>
    implicit val (h, hs, sp) = res

    // 100.0 tokens = 10000000000 in fixed-point
    val primaryToken = TokenInformation(
      CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some,
      PosLong.unsafeFrom(toFixedPoint(100.0))
    )

    // 50.0 tokens = 5000000000 in fixed-point
    val pairToken = TokenInformation(
      CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some,
      PosLong.unsafeFrom(toFixedPoint(50.0))
    )

    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")
    val destinationAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAP")

    val (poolId, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val ammOnChainState = AmmOnChainState(SortedSet.empty, None)
    val ammCalculatedState = AmmCalculatedState(
      SortedMap(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      allowSpendTokenA = AllowSpend(
        ownerAddress,
        destinationAddress,
        primaryToken.identifier,
        SwapAmount(PosLong.unsafeFrom(toFixedPoint(200.0))),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        EpochProgress.MaxValue,
        List.empty
      )
      allowSpendTokenB = AllowSpend(
        ownerAddress,
        destinationAddress,
        pairToken.identifier,
        SwapAmount(PosLong.unsafeFrom(toFixedPoint(100.0))),
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

      stakingUpdate = getFakeSignedUpdate(
        StakingUpdate(
          CurrencyId(destinationAddress),
          sourceAddress,
          signedAllowSpendA.hash,
          signedAllowSpendB.hash,
          primaryToken.identifier,
          PosLong.unsafeFrom(toFixedPoint(100.0)),
          pairToken.identifier,
          StakingReference.empty,
          EpochProgress.MaxValue
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
      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService = PricingService.make[IO](config, calculatedStateService)

      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      stakingValidations = StakingValidations.make[IO](config, jsonBase64BinaryCodec)
      stakingCombinerService = StakingCombinerService.make[IO](config, pricingService, stakingValidations, jsonBase64BinaryCodec)

      stakeResponsePendingSpendActionResponse <- stakingCombinerService.combineNew(
        stakingUpdate,
        state,
        EpochProgress.MinValue,
        allowSpends,
        CurrencyId(destinationAddress)
      )

      spendActions = stakeResponsePendingSpendActionResponse.sharedArtifacts.map(_.asInstanceOf[SpendAction]).toList
      pending = getPendingSpendActionStakingUpdates(stakeResponsePendingSpendActionResponse.calculated)

      stakeResponseConfirmedResponse <- stakingCombinerService.combinePendingSpendAction(
        PendingSpendAction(stakingUpdate, pending.head.updateHash, spendActions.head, pending.head.pricingTokenInfo),
        stakeResponsePendingSpendActionResponse,
        EpochProgress.MinValue,
        spendActions,
        SnapshotOrdinal.MinValue,
        CurrencyId(destinationAddress)
      )

      oldLiquidityPool = liquidityPoolCalculatedState.confirmed.value(poolId)
      updatedLiquidityPool = stakeResponseConfirmedResponse.calculated
        .operations(OperationType.LiquidityPool)
        .asInstanceOf[LiquidityPoolCalculatedState]
        .confirmed
        .value(poolId)

      stakingSpendAction = stakeResponsePendingSpendActionResponse.sharedArtifacts.toList.collect {
        case action: artifact.SpendAction => action
      }

    } yield
      expect.all(
        oldLiquidityPool.tokenA.amount.value === toFixedPoint(100.0),
        oldLiquidityPool.tokenB.amount.value === toFixedPoint(50.0),
        oldLiquidityPool.poolShares.totalShares.value === toFixedPoint(1),
        oldLiquidityPool.poolShares.addressShares(ownerAddress).value.value.value === toFixedPoint(1),
        updatedLiquidityPool.tokenA.amount.value === toFixedPoint(200.0),
        updatedLiquidityPool.tokenB.amount.value === toFixedPoint(100.0),
        updatedLiquidityPool.poolShares.totalShares.value === toFixedPoint(2.0),
        updatedLiquidityPool.poolShares.addressShares(ownerAddress).value.value.value === toFixedPoint(2.0),
        stakingSpendAction.size === 1
      )
  }

  test("Test successful staking - multiple providers") { implicit res =>
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

    val secondProviderAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Kt")

    val (poolId, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(
      primaryToken,
      pairToken,
      ownerAddress,
      Some(secondProviderAddress -> ShareAmount(Amount(PosLong.unsafeFrom(toFixedPoint(1.0)))))
    )

    val ammOnChainState = AmmOnChainState(SortedSet.empty, None)
    val ammCalculatedState = AmmCalculatedState(
      SortedMap(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      allowSpendTokenA = AllowSpend(
        ownerAddress,
        destinationAddress,
        primaryToken.identifier,
        SwapAmount(PosLong.unsafeFrom(toFixedPoint(200.0))),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        EpochProgress.MaxValue,
        List.empty
      )
      allowSpendTokenB = AllowSpend(
        ownerAddress,
        destinationAddress,
        pairToken.identifier,
        SwapAmount(PosLong.unsafeFrom(toFixedPoint(200.0))),
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

      stakingUpdate = getFakeSignedUpdate(
        StakingUpdate(
          CurrencyId(destinationAddress),
          sourceAddress,
          signedAllowSpendA.hash,
          signedAllowSpendB.hash,
          primaryToken.identifier,
          PosLong.unsafeFrom(toFixedPoint(100.0)),
          pairToken.identifier,
          StakingReference.empty,
          EpochProgress.MaxValue
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
      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService = PricingService.make[IO](config, calculatedStateService)
      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      stakingValidations = StakingValidations.make[IO](config, jsonBase64BinaryCodec)
      stakingCombinerService = StakingCombinerService.make[IO](config, pricingService, stakingValidations, jsonBase64BinaryCodec)

      stakeResponsePendingSpendActionResponse <- stakingCombinerService.combineNew(
        stakingUpdate,
        state,
        EpochProgress.MinValue,
        allowSpends,
        CurrencyId(destinationAddress)
      )
      spendActions = stakeResponsePendingSpendActionResponse.sharedArtifacts.map(_.asInstanceOf[SpendAction]).toList
      pending = getPendingSpendActionStakingUpdates(stakeResponsePendingSpendActionResponse.calculated)

      stakeResponseConfirmedResponse <- stakingCombinerService.combinePendingSpendAction(
        PendingSpendAction(stakingUpdate, pending.head.updateHash, spendActions.head, pending.head.pricingTokenInfo),
        stakeResponsePendingSpendActionResponse,
        EpochProgress.MinValue,
        spendActions,
        SnapshotOrdinal.MinValue,
        CurrencyId(destinationAddress)
      )

      oldLiquidityPool = liquidityPoolCalculatedState.confirmed.value(poolId)
      updatedLiquidityPool = stakeResponseConfirmedResponse.calculated
        .operations(OperationType.LiquidityPool)
        .asInstanceOf[LiquidityPoolCalculatedState]
        .confirmed
        .value(poolId)

    } yield
      expect.all(
        oldLiquidityPool.poolShares.addressShares.size === 2,
        oldLiquidityPool.poolShares.addressShares(ownerAddress).value.value.value === toFixedPoint(1.0),
        oldLiquidityPool.poolShares.addressShares(secondProviderAddress).value.value.value === toFixedPoint(1.0),
        updatedLiquidityPool.poolShares.addressShares.size === 2,
        updatedLiquidityPool.poolShares.addressShares(ownerAddress).value.value.value === toFixedPoint(3.0),
        updatedLiquidityPool.poolShares.addressShares(secondProviderAddress).value.value.value === toFixedPoint(1.0)
      )
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

      stakingUpdate = getFakeSignedUpdate(
        StakingUpdate(
          CurrencyId(destinationAddress),
          sourceAddress,
          signedAllowSpendA.hash,
          signedAllowSpendB.hash,
          primaryToken.identifier,
          100L.toPosLongUnsafe,
          pairToken.identifier,
          StakingReference.empty,
          futureEpoch
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

      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService = PricingService.make[IO](config, calculatedStateService)

      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      stakingValidations = StakingValidations.make[IO](config, jsonBase64BinaryCodec)
      stakingCombinerService = StakingCombinerService.make[IO](config, pricingService, stakingValidations, jsonBase64BinaryCodec)

      stakeResponse <- stakingCombinerService.combineNew(
        stakingUpdate,
        state,
        futureEpoch,
        allowSpends,
        CurrencyId(destinationAddress)
      )
      stakingCalculatedState = stakeResponse.calculated.operations(Staking).asInstanceOf[StakingCalculatedState]
    } yield
      expect.all(
        stakingCalculatedState.failed.toList.length === 1,
        stakingCalculatedState.failed.toList.head.expiringEpochProgress === EpochProgress(
          NonNegLong.unsafeFrom(futureEpoch.value.value + config.expirationEpochProgresses.failedOperations.value.value)
        ),
        stakingCalculatedState.failed.toList.head.reason == AmountGreaterThanAllowSpendLimit(allowSpendTokenA)
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
        SwapAmount(PosLong(100)),
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

      stakingUpdate = getFakeSignedUpdate(
        StakingUpdate(
          CurrencyId(destinationAddress),
          sourceAddress,
          signedAllowSpendA.hash,
          signedAllowSpendB.hash,
          primaryToken.identifier,
          100L.toPosLongUnsafe,
          pairToken.identifier,
          StakingReference.empty,
          futureEpoch
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

      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService = PricingService.make[IO](config, calculatedStateService)

      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      stakingValidations = StakingValidations.make[IO](config, jsonBase64BinaryCodec)
      stakingCombinerService = StakingCombinerService.make[IO](config, pricingService, stakingValidations, jsonBase64BinaryCodec)

      stakeResponse <- stakingCombinerService.combineNew(
        stakingUpdate,
        state,
        futureEpoch,
        allowSpends,
        CurrencyId(destinationAddress)
      )
      stakingCalculatedState = stakeResponse.calculated.operations(Staking).asInstanceOf[StakingCalculatedState]
    } yield
      expect.all(
        stakingCalculatedState.failed.toList.length === 1,
        stakingCalculatedState.failed.toList.head.expiringEpochProgress === EpochProgress(
          NonNegLong.unsafeFrom(futureEpoch.value.value + config.expirationEpochProgresses.failedOperations.value.value)
        ),
        stakingCalculatedState.failed.toList.head.reason == AllowSpendExpired(allowSpendTokenA)
      )
  }

  test("Test failure staking - duplicated staking") { implicit res =>
    implicit val (h, hs, sp) = res

    // 100.0 tokens = 10000000000 in fixed-point
    val primaryToken = TokenInformation(
      CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some,
      PosLong.unsafeFrom(toFixedPoint(100.0))
    )

    // 50.0 tokens = 5000000000 in fixed-point
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
        SwapAmount(PosLong.unsafeFrom(toFixedPoint(200.0))),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        EpochProgress.MaxValue,
        List.empty
      )
      allowSpendTokenB = AllowSpend(
        ownerAddress,
        destinationAddress,
        pairToken.identifier,
        SwapAmount(PosLong.unsafeFrom(toFixedPoint(100.0))),
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

      stakingUpdate = getFakeSignedUpdate(
        StakingUpdate(
          CurrencyId(destinationAddress),
          sourceAddress,
          signedAllowSpendA.hash,
          signedAllowSpendB.hash,
          primaryToken.identifier,
          PosLong.unsafeFrom(toFixedPoint(100.0)),
          pairToken.identifier,
          StakingReference.empty,
          futureEpoch
        )
      )

      stakingUpdate2 = getFakeSignedUpdate(
        StakingUpdate(
          CurrencyId(destinationAddress),
          sourceAddress,
          signedAllowSpendA.hash,
          signedAllowSpendB.hash,
          primaryToken.identifier,
          PosLong.unsafeFrom(toFixedPoint(10.0)),
          pairToken.identifier,
          StakingReference.empty,
          futureEpoch
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
      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService = PricingService.make[IO](config, calculatedStateService)

      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      stakingValidations = StakingValidations.make[IO](config, jsonBase64BinaryCodec)
      stakingCombinerService = StakingCombinerService.make[IO](config, pricingService, stakingValidations, jsonBase64BinaryCodec)

      stakeResponsePendingSpendActionResponse <- stakingCombinerService.combineNew(
        stakingUpdate,
        state,
        futureEpoch,
        allowSpends,
        CurrencyId(destinationAddress)
      )

      stakeResponsePendingSpendActionResponse2 <- stakingCombinerService.combineNew(
        stakingUpdate2,
        stakeResponsePendingSpendActionResponse,
        futureEpoch,
        allowSpends,
        CurrencyId(destinationAddress)
      )

      stakingCalculatedState = stakeResponsePendingSpendActionResponse2.calculated.operations(Staking).asInstanceOf[StakingCalculatedState]
    } yield
      expect.all(
        stakingCalculatedState.failed.toList.length === 1,
        stakingCalculatedState.failed.toList.head.expiringEpochProgress === EpochProgress(
          NonNegLong.unsafeFrom(futureEpoch.value.value + config.expirationEpochProgresses.failedOperations.value.value)
        ),
        stakingCalculatedState.failed.toList.head.reason == TransactionAlreadyExists(stakingUpdate2)
      )
  }

  test("Test failed because of allowSpendEpochBufferDelay") { implicit res =>
    implicit val (h, hs, sp) = res

    // 100.0 tokens = 10000000000 in fixed-point
    val primaryToken = TokenInformation(
      CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some,
      PosLong.unsafeFrom(toFixedPoint(100.0))
    )

    // 50.0 tokens = 5000000000 in fixed-point
    val pairToken = TokenInformation(
      CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some,
      PosLong.unsafeFrom(toFixedPoint(50.0))
    )

    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")
    val destinationAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAP")

    val (poolId, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val ammOnChainState = AmmOnChainState(SortedSet.empty, None)
    val ammCalculatedState = AmmCalculatedState(
      SortedMap(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)
    val bufferDelay = EpochProgress(NonNegLong.unsafeFrom(1L))
    val tokenEpoch = EpochProgress(NonNegLong.unsafeFrom(10L))
    val currentEpoch = EpochProgress(NonNegLong.unsafeFrom(tokenEpoch.value.value + bufferDelay.value.value + 1))

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      allowSpendTokenA = AllowSpend(
        ownerAddress,
        destinationAddress,
        primaryToken.identifier,
        SwapAmount(PosLong.unsafeFrom(toFixedPoint(200.0))),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        tokenEpoch,
        List.empty
      )
      allowSpendTokenB = AllowSpend(
        ownerAddress,
        destinationAddress,
        pairToken.identifier,
        SwapAmount(PosLong.unsafeFrom(toFixedPoint(100.0))),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        tokenEpoch,
        List.empty
      )

      signedAllowSpendA <- Signed
        .forAsyncHasher[IO, AllowSpend](allowSpendTokenA, keyPair)
        .flatMap(_.toHashed[IO])
      signedAllowSpendB <- Signed
        .forAsyncHasher[IO, AllowSpend](allowSpendTokenB, keyPair)
        .flatMap(_.toHashed[IO])

      stakingUpdate = getFakeSignedUpdate(
        StakingUpdate(
          CurrencyId(destinationAddress),
          sourceAddress,
          signedAllowSpendA.hash,
          signedAllowSpendB.hash,
          primaryToken.identifier,
          PosLong.unsafeFrom(toFixedPoint(100.0)),
          pairToken.identifier,
          StakingReference.empty,
          currentEpoch
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
      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService = PricingService.make[IO](config, calculatedStateService)

      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      stakingValidations = StakingValidations.make[IO](config.copy(allowSpendEpochBufferDelay = bufferDelay), jsonBase64BinaryCodec)
      stakingCombinerService = StakingCombinerService.make[IO](config, pricingService, stakingValidations, jsonBase64BinaryCodec)
      stakeResponse <- stakingCombinerService.combineNew(
        stakingUpdate,
        state,
        currentEpoch,
        allowSpends,
        CurrencyId(destinationAddress)
      )
      stakingCalculatedState = stakeResponse.calculated.operations(Staking).asInstanceOf[StakingCalculatedState]
    } yield
      expect.all(
        stakingCalculatedState.failed.toList.length === 1,
        stakingCalculatedState.failed.toList.head.expiringEpochProgress === EpochProgress(
          NonNegLong.unsafeFrom(currentEpoch.value.value + config.expirationEpochProgresses.failedOperations.value.value)
        ),
        stakingCalculatedState.failed.toList.head.reason == AllowSpendExpired(allowSpendTokenA)
      )
  }

  test("Test successful staking - single provider - clean state") { implicit res =>
    implicit val (h, hs, sp) = res

    // 100.0 tokens = 10000000000 in fixed-point
    val primaryToken = TokenInformation(
      CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some,
      PosLong.unsafeFrom(toFixedPoint(100.0))
    )

    // 50.0 tokens = 5000000000 in fixed-point
    val pairToken = TokenInformation(
      CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some,
      PosLong.unsafeFrom(toFixedPoint(50.0))
    )

    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")
    val destinationAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAP")

    val (poolId, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val ammOnChainState = AmmOnChainState(SortedSet.empty, None)
    val ammCalculatedState = AmmCalculatedState(
      SortedMap(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      allowSpendTokenA = AllowSpend(
        ownerAddress,
        destinationAddress,
        primaryToken.identifier,
        SwapAmount(PosLong.unsafeFrom(toFixedPoint(200.0))),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        EpochProgress.MaxValue,
        List.empty
      )
      allowSpendTokenB = AllowSpend(
        ownerAddress,
        destinationAddress,
        pairToken.identifier,
        SwapAmount(PosLong.unsafeFrom(toFixedPoint(100.0))),
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

      stakingUpdate = getFakeSignedUpdate(
        StakingUpdate(
          CurrencyId(destinationAddress),
          sourceAddress,
          signedAllowSpendA.hash,
          signedAllowSpendB.hash,
          primaryToken.identifier,
          PosLong.unsafeFrom(toFixedPoint(100.0)),
          pairToken.identifier,
          StakingReference.empty,
          EpochProgress.MaxValue
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
      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService = PricingService.make[IO](config, calculatedStateService)

      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      stakingValidations = StakingValidations.make[IO](config, jsonBase64BinaryCodec)
      stakingCombinerService = StakingCombinerService.make[IO](config, pricingService, stakingValidations, jsonBase64BinaryCodec)

      stakeResponsePendingSpendActionResponse <- stakingCombinerService.combineNew(
        stakingUpdate,
        state,
        EpochProgress.MinValue,
        allowSpends,
        CurrencyId(destinationAddress)
      )

      spendActions = stakeResponsePendingSpendActionResponse.sharedArtifacts.map(_.asInstanceOf[SpendAction]).toList
      pending = getPendingSpendActionStakingUpdates(stakeResponsePendingSpendActionResponse.calculated)

      stakeResponseConfirmedResponse <- stakingCombinerService.combinePendingSpendAction(
        PendingSpendAction(stakingUpdate, pending.head.updateHash, spendActions.head),
        stakeResponsePendingSpendActionResponse,
        EpochProgress.MinValue,
        spendActions,
        SnapshotOrdinal.MinValue,
        CurrencyId(destinationAddress)
      )

      stakeCleanupResponse = stakingCombinerService.cleanupExpiredOperations(
        stakeResponseConfirmedResponse,
        EpochProgress.MaxValue
      )

      stakeConfirmedState = stakeResponseConfirmedResponse.calculated
        .operations(OperationType.Staking)
        .asInstanceOf[StakingCalculatedState]
        .confirmed
        .value

      stakeCleanupState = stakeCleanupResponse.calculated
        .operations(OperationType.Staking)
        .asInstanceOf[StakingCalculatedState]
        .confirmed
        .value

    } yield
      expect.all(
        stakeConfirmedState.nonEmpty,
        stakeCleanupState.nonEmpty,
        stakeCleanupState.head._2.values.isEmpty
      )
  }

  test("Failed because allowSpend source different than StakingUpdate source") { implicit res =>
    implicit val (h, hs, sp) = res

    // 100.0 tokens = 10000000000 in fixed-point
    val primaryToken = TokenInformation(
      CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some,
      PosLong.unsafeFrom(toFixedPoint(100.0))
    )

    // 50.0 tokens = 5000000000 in fixed-point
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
    val tokenEpoch = EpochProgress(NonNegLong.unsafeFrom(10L))

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      keyPair2 <- KeyPairGenerator.makeKeyPair[IO]
      address2 = keyPair2.getPublic.toAddress

      allowSpendTokenA = AllowSpend(
        address2,
        destinationAddress,
        primaryToken.identifier,
        SwapAmount(PosLong.unsafeFrom(toFixedPoint(200.0))),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        EpochProgress.MaxValue,
        List.empty
      )
      allowSpendTokenB = AllowSpend(
        address2,
        destinationAddress,
        pairToken.identifier,
        SwapAmount(PosLong.unsafeFrom(toFixedPoint(100.0))),
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

      stakingUpdate = getFakeSignedUpdate(
        StakingUpdate(
          CurrencyId(destinationAddress),
          sourceAddress,
          signedAllowSpendA.hash,
          signedAllowSpendB.hash,
          primaryToken.identifier,
          PosLong.unsafeFrom(toFixedPoint(100.0)),
          pairToken.identifier,
          StakingReference.empty,
          tokenEpoch
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
      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService = PricingService.make[IO](config, calculatedStateService)

      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      stakingValidations = StakingValidations.make[IO](config, jsonBase64BinaryCodec)
      stakingCombinerService = StakingCombinerService.make[IO](config, pricingService, stakingValidations, jsonBase64BinaryCodec)

      stakeResponsePendingSpendActionResponse <- stakingCombinerService.combineNew(
        stakingUpdate,
        state,
        tokenEpoch,
        allowSpends,
        CurrencyId(destinationAddress)
      )

      stakingCalculatedState = stakeResponsePendingSpendActionResponse.calculated.operations(Staking).asInstanceOf[StakingCalculatedState]

    } yield
      expect.all(
        stakingCalculatedState.failed.toList.length === 1,
        stakingCalculatedState.failed.toList.head.expiringEpochProgress === EpochProgress(
          NonNegLong.unsafeFrom(tokenEpoch.value.value + config.expirationEpochProgresses.failedOperations.value.value)
        ),
        stakingCalculatedState.failed.toList.head.reason == SourceAddressBetweenUpdateAndAllowSpendDifferent(stakingUpdate)
      )
  }

}
