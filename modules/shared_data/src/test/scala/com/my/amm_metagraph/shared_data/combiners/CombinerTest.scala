package com.my.amm_metagraph.shared_data.combiners

import cats.effect.{IO, Resource}
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.currency.dataApplication.DataState
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

import com.my.amm_metagraph.shared_data.DummyL0Context.buildL0NodeContext
import com.my.amm_metagraph.shared_data.Shared._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.{NonNegLong, PosLong}
import org.amm_metagraph.shared_data.calculated_state.CalculatedStateService
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.services.combiners._
import org.amm_metagraph.shared_data.services.pricing.PricingService
import org.amm_metagraph.shared_data.storages.GlobalSnapshotsStorage
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.codecs.{HasherSelector, JsonWithBase64BinaryCodec}
import org.amm_metagraph.shared_data.validations._
import weaver.MutableIOSuite

object CombinerTest extends MutableIOSuite {
  type Res = (Hasher[IO], HasherSelector[IO], SecurityProvider[IO])

  override def sharedResource: Resource[IO, Res] = for {
    sp <- SecurityProvider.forAsync[IO]
    implicit0(j: JsonSerializer[IO]) <- JsonSerializer.forSync[IO].asResource
    h = Hasher.forJson[IO]
    hs = HasherSelector.forSync(h, h)
  } yield (h, hs, sp)

  test("Test combiner - confirmed") { implicit res =>
    implicit val (h, hs, sp) = res

    val tokenAId = CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some
    val tokenAAmount = PosLong.unsafeFrom(100L.toTokenAmountFormat)
    val tokenBId = CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some
    val tokenBAmount = PosLong.unsafeFrom(50L.toTokenAmountFormat)

    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")
    val destinationAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAP")

    val ammOnChainState = AmmOnChainState(Set.empty)
    val ammCalculatedState = AmmCalculatedState(Map.empty)
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

      allowSpendsLp = SortedMap(
        tokenAId.get.value.some ->
          SortedMap(
            ownerAddress -> SortedSet(signedAllowSpendA.signed)
          ),
        tokenBId.get.value.some ->
          SortedMap(
            ownerAddress -> SortedSet(signedAllowSpendB.signed)
          )
      )

      context1 = buildL0NodeContext(
        keyPair,
        allowSpendsLp,
        EpochProgress.MinValue,
        SnapshotOrdinal.MinValue,
        SortedMap.empty,
        EpochProgress.MinValue,
        SnapshotOrdinal.MinValue,
        destinationAddress
      )

      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      globalSnapshotService <- GlobalSnapshotsStorage.make[IO]

      liquidityPoolValidations = LiquidityPoolValidations.make[IO](config)
      stakingValidations = StakingValidations.make[IO](config)
      swapValidations = SwapValidations.make[IO](config)
      withdrawalValidations = WithdrawalValidations.make[IO](config)

      pricingService = PricingService.make[IO](config, calculatedStateService)
      governanceCombinerService = GovernanceCombinerService.make[IO](config)
      liquidityPoolCombinerService = LiquidityPoolCombinerService.make[IO](liquidityPoolValidations, jsonBase64BinaryCodec)
      stakingCombinerService = StakingCombinerService.make[IO](pricingService, stakingValidations, jsonBase64BinaryCodec)
      swapCombinerService = SwapCombinerService.make[IO](config, pricingService, swapValidations, jsonBase64BinaryCodec)
      withdrawalCombinerService = WithdrawalCombinerService.make[IO](config, pricingService, withdrawalValidations, jsonBase64BinaryCodec)

      combinerService = L0CombinerService
        .make[IO](
          globalSnapshotService,
          governanceCombinerService,
          liquidityPoolCombinerService,
          stakingCombinerService,
          swapCombinerService,
          withdrawalCombinerService
        )

      combineResponsePendingSpendAction <- combinerService.combine(
        state,
        List(liquidityPoolUpdate)
      )(context1)
      spendActions = combineResponsePendingSpendAction.sharedArtifacts.collect { case spendAction: SpendAction => spendAction }

      context2 = buildL0NodeContext(
        keyPair,
        SortedMap.empty,
        EpochProgress(NonNegLong.unsafeFrom(2L)),
        SnapshotOrdinal(NonNegLong.unsafeFrom(2L)),
        SortedMap.empty,
        EpochProgress.MinValue,
        SnapshotOrdinal.MinValue,
        destinationAddress,
        SortedMap(ownerAddress -> spendActions.toList).some
      )

      t <- context2.getLastSynchronizedGlobalSnapshot
      _ <- globalSnapshotService.set(t.get)

      combineResponseConfirmed <- combinerService.combine(
        combineResponsePendingSpendAction,
        List.empty
      )(context2)

      pendingLiquidityPoolCalculatedState = combineResponsePendingSpendAction.calculated
        .operations(OperationType.LiquidityPool)
        .asInstanceOf[LiquidityPoolCalculatedState]

      confirmedLiquidityPoolCalculatedState = combineResponseConfirmed.calculated
        .operations(OperationType.LiquidityPool)
        .asInstanceOf[LiquidityPoolCalculatedState]
    } yield
      expect.all(
        pendingLiquidityPoolCalculatedState.pending.size === 1 &&
          confirmedLiquidityPoolCalculatedState.pending.size === 0 &&
          pendingLiquidityPoolCalculatedState.confirmed.value.size === 0 &&
          confirmedLiquidityPoolCalculatedState.confirmed.value.size === 1
      )
  }

  test("Test combiner - pendingAllowSpend") { implicit res =>
    implicit val (h, hs, sp) = res

    val tokenAId = CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some
    val tokenAAmount = PosLong.unsafeFrom(100L.toTokenAmountFormat)
    val tokenBId = CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some
    val tokenBAmount = PosLong.unsafeFrom(50L.toTokenAmountFormat)

    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")
    val ammOnChainState = AmmOnChainState(Set.empty)
    val ammCalculatedState = AmmCalculatedState(Map.empty)
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]

      allowSpendTokenA = AllowSpend(
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc"),
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb"),
        tokenAId,
        SwapAmount(PosLong.MaxValue),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        EpochProgress.MaxValue,
        List.empty
      )
      allowSpendTokenB = AllowSpend(
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc"),
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb"),
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

      allowSpends = SortedMap.empty[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]]

      context1 = buildL0NodeContext(
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
      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      globalSnapshotService <- GlobalSnapshotsStorage.make[IO]

      liquidityPoolValidations = LiquidityPoolValidations.make[IO](config)
      stakingValidations = StakingValidations.make[IO](config)
      swapValidations = SwapValidations.make[IO](config)
      withdrawalValidations = WithdrawalValidations.make[IO](config)
      governanceValidations = GovernanceValidations.make[IO]

      pricingService = PricingService.make[IO](config, calculatedStateService)
      governanceCombinerService = GovernanceCombinerService.make[IO](config)
      liquidityPoolCombinerService = LiquidityPoolCombinerService.make[IO](liquidityPoolValidations, jsonBase64BinaryCodec)
      stakingCombinerService = StakingCombinerService.make[IO](pricingService, stakingValidations, jsonBase64BinaryCodec)
      swapCombinerService = SwapCombinerService.make[IO](config, pricingService, swapValidations, jsonBase64BinaryCodec)
      withdrawalCombinerService = WithdrawalCombinerService.make[IO](config, pricingService, withdrawalValidations, jsonBase64BinaryCodec)

      combinerService = L0CombinerService
        .make[IO](
          globalSnapshotService,
          governanceCombinerService,
          liquidityPoolCombinerService,
          stakingCombinerService,
          swapCombinerService,
          withdrawalCombinerService
        )

      combineResponsePendingSpendAction <- combinerService.combine(
        state,
        List(liquidityPoolUpdate)
      )(context1)

      context2 = buildL0NodeContext(
        keyPair,
        allowSpends,
        EpochProgress(NonNegLong.unsafeFrom(2L)),
        SnapshotOrdinal(NonNegLong.unsafeFrom(2L)),
        SortedMap.empty,
        EpochProgress.MinValue,
        SnapshotOrdinal.MinValue,
        ownerAddress,
        None
      )

      t <- context2.getLastSynchronizedGlobalSnapshot
      _ <- globalSnapshotService.set(t.get)

      stillPendingResponse <- combinerService.combine(
        combineResponsePendingSpendAction,
        List.empty
      )(context2)

      pendingLiquidityPoolCalculatedState = combineResponsePendingSpendAction.calculated
        .operations(OperationType.LiquidityPool)
        .asInstanceOf[LiquidityPoolCalculatedState]

      confirmedLiquidityPoolCalculatedState = stillPendingResponse.calculated
        .operations(OperationType.LiquidityPool)
        .asInstanceOf[LiquidityPoolCalculatedState]
    } yield
      expect.all(
        pendingLiquidityPoolCalculatedState.pending.collect { case PendingAllowSpend(update, _, _) => update }.size === 1 &&
          confirmedLiquidityPoolCalculatedState.pending.collect { case PendingAllowSpend(update, _, _) => update }.size === 1 &&
          pendingLiquidityPoolCalculatedState.confirmed.value.size === 0 &&
          confirmedLiquidityPoolCalculatedState.confirmed.value.size === 0
      )
  }

  test("Test combiner - pendingSpendAction") { implicit res =>
    implicit val (h, hs, sp) = res

    val tokenAId = CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some
    val tokenAAmount = PosLong.unsafeFrom(100L.toTokenAmountFormat)
    val tokenBId = CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some
    val tokenBAmount = PosLong.unsafeFrom(50L.toTokenAmountFormat)

    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")
    val destinationAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAP")

    val ammOnChainState = AmmOnChainState(Set.empty)
    val ammCalculatedState = AmmCalculatedState(Map.empty)
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

      context1 = buildL0NodeContext(
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
      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      globalSnapshotService <- GlobalSnapshotsStorage.make[IO]

      liquidityPoolValidations = LiquidityPoolValidations.make[IO](config)
      stakingValidations = StakingValidations.make[IO](config)
      swapValidations = SwapValidations.make[IO](config)
      withdrawalValidations = WithdrawalValidations.make[IO](config)

      pricingService = PricingService.make[IO](config, calculatedStateService)
      governanceCombinerService = GovernanceCombinerService.make[IO](config)
      liquidityPoolCombinerService = LiquidityPoolCombinerService.make[IO](liquidityPoolValidations, jsonBase64BinaryCodec)
      stakingCombinerService = StakingCombinerService.make[IO](pricingService, stakingValidations, jsonBase64BinaryCodec)
      swapCombinerService = SwapCombinerService.make[IO](config, pricingService, swapValidations, jsonBase64BinaryCodec)
      withdrawalCombinerService = WithdrawalCombinerService.make[IO](config, pricingService, withdrawalValidations, jsonBase64BinaryCodec)

      combinerService = L0CombinerService
        .make[IO](
          globalSnapshotService,
          governanceCombinerService,
          liquidityPoolCombinerService,
          stakingCombinerService,
          swapCombinerService,
          withdrawalCombinerService
        )

      combineResponsePendingSpendAction <- combinerService.combine(
        state,
        List(liquidityPoolUpdate)
      )(context1)

      context2 = buildL0NodeContext(
        keyPair,
        SortedMap.empty,
        EpochProgress(NonNegLong.unsafeFrom(2L)),
        SnapshotOrdinal(NonNegLong.unsafeFrom(2L)),
        SortedMap.empty,
        EpochProgress.MinValue,
        SnapshotOrdinal.MinValue,
        destinationAddress,
        None
      )

      t <- context2.getLastSynchronizedGlobalSnapshot
      _ <- globalSnapshotService.set(t.get)

      stillPendingResponse <- combinerService.combine(
        combineResponsePendingSpendAction,
        List.empty
      )(context2)

      pendingLiquidityPoolCalculatedState = combineResponsePendingSpendAction.calculated
        .operations(OperationType.LiquidityPool)
        .asInstanceOf[LiquidityPoolCalculatedState]

      confirmedLiquidityPoolCalculatedState = stillPendingResponse.calculated
        .operations(OperationType.LiquidityPool)
        .asInstanceOf[LiquidityPoolCalculatedState]
    } yield
      expect.all(
        pendingLiquidityPoolCalculatedState.pending.collect { case PendingSpendAction(update, _, _, _) => update }.size === 1 &&
          confirmedLiquidityPoolCalculatedState.pending.collect { case PendingSpendAction(update, _, _, _) => update }.size === 1 &&
          pendingLiquidityPoolCalculatedState.confirmed.value.size === 0 &&
          confirmedLiquidityPoolCalculatedState.confirmed.value.size === 0
      )
  }
}
