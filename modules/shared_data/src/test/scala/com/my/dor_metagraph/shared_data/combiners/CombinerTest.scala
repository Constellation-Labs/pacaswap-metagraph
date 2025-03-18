package com.my.dor_metagraph.shared_data.combiners

import cats.data.NonEmptySet
import cats.effect.{IO, Resource}
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.currency.dataApplication.DataState
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
import eu.timepit.refined.types.all.{NonNegLong, PosDouble, PosLong}
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.app.ApplicationConfig._
import org.amm_metagraph.shared_data.calculated_state.CalculatedStateService
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.services.combiners.{L0CombinerService, _}
import org.amm_metagraph.shared_data.services.pricing.PricingService
import org.amm_metagraph.shared_data.storages.GlobalSnapshotsStorage
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.codecs.{HasherSelector, JsonWithBase64BinaryCodec}
import weaver.MutableIOSuite

object CombinerTest extends MutableIOSuite {
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

  override def sharedResource: Resource[IO, Res] = for {
    sp <- SecurityProvider.forAsync[IO]
    implicit0(j: JsonSerializer[IO]) <- JsonSerializer.forSync[IO].asResource
    h = Hasher.forJson[IO]
    hs = HasherSelector.forSync(h, h)
  } yield (h, hs, sp)

  private def toFixedPoint(decimal: Double): Long = (decimal * 1e8).toLong

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

  def getFakeSignedUpdate[A <: AmmUpdate](update: A): Signed[A] =
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

  test("Test combiner - confirmed") { implicit res =>
    implicit val (h, hs, sp) = res

    val tokenAId = CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some
    val tokenAAmount = PosLong.unsafeFrom(100L.toTokenAmountFormat)
    val tokenBId = CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some
    val tokenBAmount = PosLong.unsafeFrom(50L.toTokenAmountFormat)

    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")
    val ammOnChainState = AmmOnChainState(List.empty)
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
          sourceAddress,
          signedAllowSpendA.hash,
          signedAllowSpendB.hash,
          tokenAId,
          tokenBId,
          tokenAAmount,
          tokenBAmount,
          EpochProgress.MaxValue
        )
      )

      allowSpendsLp = SortedMap(
        tokenAId.get.value.some ->
          SortedMap(
            Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc") -> SortedSet(signedAllowSpendA.signed)
          ),
        tokenBId.get.value.some ->
          SortedMap(
            Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc") -> SortedSet(signedAllowSpendB.signed)
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
        ownerAddress
      )

      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      globalSnapshotService <- GlobalSnapshotsStorage.make[IO]

      pricingService = PricingService.make[IO](calculatedStateService)
      governanceCombinerService = GovernanceCombinerService.make[IO](config)
      liquidityPoolCombinerService = LiquidityPoolCombinerService.make[IO](config)
      stakingCombinerService = StakingCombinerService.make[IO](config, pricingService)
      swapCombinerService = SwapCombinerService.make[IO](config, pricingService, jsonBase64BinaryCodec)
      withdrawalCombinerService = WithdrawalCombinerService.make[IO](config)

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
        ownerAddress,
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
    val ammOnChainState = AmmOnChainState(List.empty)
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
          sourceAddress,
          signedAllowSpendA.hash,
          signedAllowSpendB.hash,
          tokenAId,
          tokenBId,
          tokenAAmount,
          tokenBAmount,
          EpochProgress.MaxValue
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

      pricingService = PricingService.make[IO](calculatedStateService)
      governanceCombinerService = GovernanceCombinerService.make[IO](config)
      liquidityPoolCombinerService = LiquidityPoolCombinerService.make[IO](config)
      stakingCombinerService = StakingCombinerService.make[IO](config, pricingService)
      swapCombinerService = SwapCombinerService.make[IO](config, pricingService, jsonBase64BinaryCodec)
      withdrawalCombinerService = WithdrawalCombinerService.make[IO](config)

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
        pendingLiquidityPoolCalculatedState.pending.collect { case PendingAllowSpend(update) => update }.size === 1 &&
          confirmedLiquidityPoolCalculatedState.pending.collect { case PendingAllowSpend(update) => update }.size === 1 &&
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
    val ammOnChainState = AmmOnChainState(List.empty)
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
          sourceAddress,
          signedAllowSpendA.hash,
          signedAllowSpendB.hash,
          tokenAId,
          tokenBId,
          tokenAAmount,
          tokenBAmount,
          EpochProgress.MaxValue
        )
      )

      allowSpends = SortedMap(
        tokenAId.get.value.some ->
          SortedMap(
            Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc") -> SortedSet(signedAllowSpendA.signed)
          ),
        tokenBId.get.value.some ->
          SortedMap(
            Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc") -> SortedSet(signedAllowSpendB.signed)
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
        ownerAddress
      )

      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      globalSnapshotService <- GlobalSnapshotsStorage.make[IO]

      pricingService = PricingService.make[IO](calculatedStateService)
      governanceCombinerService = GovernanceCombinerService.make[IO](config)
      liquidityPoolCombinerService = LiquidityPoolCombinerService.make[IO](config)
      stakingCombinerService = StakingCombinerService.make[IO](config, pricingService)
      swapCombinerService = SwapCombinerService.make[IO](config, pricingService, jsonBase64BinaryCodec)
      withdrawalCombinerService = WithdrawalCombinerService.make[IO](config)

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
        pendingLiquidityPoolCalculatedState.pending.collect { case PendingSpendAction(update, _) => update }.size === 1 &&
          confirmedLiquidityPoolCalculatedState.pending.collect { case PendingSpendAction(update, _) => update }.size === 1 &&
          pendingLiquidityPoolCalculatedState.confirmed.value.size === 0 &&
          confirmedLiquidityPoolCalculatedState.confirmed.value.size === 0
      )
  }
}
