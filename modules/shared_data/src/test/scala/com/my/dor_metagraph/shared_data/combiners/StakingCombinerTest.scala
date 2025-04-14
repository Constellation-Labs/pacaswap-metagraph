package com.my.dor_metagraph.shared_data.combiners

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
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.signature.signature.{Signature, SignatureProof}

import com.my.dor_metagraph.shared_data.DummyL0Context.buildL0NodeContext
import com.my.dor_metagraph.shared_data.Shared._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.{NonNegLong, PosLong}
import org.amm_metagraph.shared_data.calculated_state.CalculatedStateService
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.services.combiners.StakingCombinerService
import org.amm_metagraph.shared_data.services.pricing.PricingService
import org.amm_metagraph.shared_data.types.DataUpdates.StakingUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.Staking.StakingReference
import org.amm_metagraph.shared_data.types.States.OperationType.Staking
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.codecs
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
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
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

      stakingCombinerService = StakingCombinerService.make[IO](config, pricingService)
      stakeResponsePendingSpendActionResponse <- stakingCombinerService.combineNew(
        stakingUpdate,
        state,
        EpochProgress.MinValue,
        allowSpends,
        CurrencyId(destinationAddress)
      )

      spendActions = stakeResponsePendingSpendActionResponse.sharedArtifacts.map(_.asInstanceOf[SpendAction]).toList

      stakeResponseConfirmedResponse <- stakingCombinerService.combinePendingSpendAction(
        PendingSpendAction(stakingUpdate, spendActions.head),
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

    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
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
      stakingCombinerService = StakingCombinerService.make[IO](config, pricingService)

      stakeResponsePendingSpendActionResponse <- stakingCombinerService.combineNew(
        stakingUpdate,
        state,
        EpochProgress.MinValue,
        allowSpends,
        CurrencyId(destinationAddress)
      )
      spendActions = stakeResponsePendingSpendActionResponse.sharedArtifacts.map(_.asInstanceOf[SpendAction]).toList

      stakeResponseConfirmedResponse <- stakingCombinerService.combinePendingSpendAction(
        PendingSpendAction(stakingUpdate, spendActions.head),
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
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
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
      stakingCombinerService = StakingCombinerService.make[IO](config, pricingService)

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
          NonNegLong.unsafeFrom(futureEpoch.value.value + config.failedOperationsExpirationEpochProgresses.value.value)
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
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
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
      stakingCombinerService = StakingCombinerService.make[IO](config, pricingService)

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
          NonNegLong.unsafeFrom(futureEpoch.value.value + config.failedOperationsExpirationEpochProgresses.value.value)
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
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
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

      stakingCombinerService = StakingCombinerService.make[IO](config, pricingService)
      stakeResponsePendingSpendActionResponse <- stakingCombinerService.combineNew(
        stakingUpdate,
        state,
        futureEpoch,
        allowSpends,
        CurrencyId(destinationAddress)
      )

      stakeResponsePendingSpendActionResponse2 <- stakingCombinerService.combineNew(
        stakingUpdate,
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
          NonNegLong.unsafeFrom(futureEpoch.value.value + config.failedOperationsExpirationEpochProgresses.value.value)
        ),
        stakingCalculatedState.failed.toList.head.reason == DuplicatedStakingRequest(stakingUpdate)
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
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
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

      stakingCombinerService = StakingCombinerService.make[IO](config.copy(allowSpendEpochBufferDelay = bufferDelay), pricingService)
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
          NonNegLong.unsafeFrom(currentEpoch.value.value + config.failedOperationsExpirationEpochProgresses.value.value)
        ),
        stakingCalculatedState.failed.toList.head.reason == AllowSpendExpired(allowSpendTokenA)
      )
  }
}
