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
import com.my.dor_metagraph.shared_data.combiners.StakingCombinerTest.buildLiquidityPoolCalculatedState
import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.{NonNegLong, PosLong}
import eu.timepit.refined.types.numeric.PosDouble
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.app.ApplicationConfig._
import org.amm_metagraph.shared_data.combiners.LiquidityPoolCombiner.{combineNewLiquidityPool, combinePendingSpendActionLiquidityPool}
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.LiquidityPool.{TokenInformation, buildLiquidityPoolUniqueIdentifier}
import org.amm_metagraph.shared_data.types.States.OperationType.LiquidityPool
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.codecs.HasherSelector
import weaver.MutableIOSuite

object LiquidityPoolCombinerTest extends MutableIOSuite {

  type Res = (Hasher[IO], HasherSelector[IO], SecurityProvider[IO])

  private def toFixedPoint(decimal: Double): Long = (decimal * 1e8).toLong
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

  def getFakeSignedUpdate(
    update: LiquidityPoolUpdate
  ): Signed[LiquidityPoolUpdate] =
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

  test("Successfully create a liquidity pool - confirmed") { implicit res =>
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

      liquidityPoolPendingSpendActionResponse <- combineNewLiquidityPool[IO](
        config,
        state,
        liquidityPoolUpdate,
        EpochProgress.MinValue,
        allowSpends
      )

      spendActions = liquidityPoolPendingSpendActionResponse.sharedArtifacts.map(_.asInstanceOf[SpendAction]).toList
      poolId <- buildLiquidityPoolUniqueIdentifier(tokenAId, tokenBId)

      liquidityPoolConfirmedResponse <- combinePendingSpendActionLiquidityPool[IO](
        config,
        liquidityPoolPendingSpendActionResponse,
        PendingSpendAction(liquidityPoolUpdate, spendActions.head),
        EpochProgress.MinValue,
        spendActions
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
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(Map.empty)
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      allowSpendTokenA = AllowSpend(
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc"),
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb"),
        none,
        SwapAmount(PosLong.MaxValue),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        EpochProgress.MaxValue,
        List.empty
      )
      allowSpendTokenB = AllowSpend(
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc"),
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb"),
        none,
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
        tokenBId ->
          SortedMap(
            Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc") -> SortedSet(signedAllowSpendB.signed)
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

      liquidityPoolPendingSpendActionResponse <- combineNewLiquidityPool[IO](
        config,
        state,
        liquidityPoolUpdate,
        EpochProgress.MinValue,
        allowSpends
      )
      spendActions = liquidityPoolPendingSpendActionResponse.sharedArtifacts.map(_.asInstanceOf[SpendAction]).toList
      poolId <- buildLiquidityPoolUniqueIdentifier(tokenAId, tokenBId)

      liquidityPoolConfirmedResponse <- combinePendingSpendActionLiquidityPool[IO](
        config,
        liquidityPoolPendingSpendActionResponse,
        PendingSpendAction(liquidityPoolUpdate, spendActions.head),
        EpochProgress.MinValue,
        spendActions
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
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc"),
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb"),
        none,
        SwapAmount(PosLong(1)),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        EpochProgress.MinValue,
        List.empty
      )
      allowSpendTokenB = AllowSpend(
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc"),
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb"),
        none,
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
          signedAllowSpendA.hash,
          signedAllowSpendB.hash,
          primaryToken.identifier,
          pairToken.identifier,
          primaryToken.amount,
          pairToken.amount,
          EpochProgress.MaxValue
        )
      )

      allowSpends = SortedMap(
        primaryToken.identifier.get.value.some ->
          SortedMap(
            Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc") -> SortedSet(signedAllowSpendA.signed)
          ),
        pairToken.identifier.get.value.some ->
          SortedMap(
            Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc") -> SortedSet(signedAllowSpendB.signed)
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

      liquidityPoolResponse <- combineNewLiquidityPool[IO](
        config,
        state,
        liquidityPoolUpdate,
        futureEpoch,
        allowSpends
      )
      liquidityPoolCalculatedState = liquidityPoolResponse.calculated.operations(LiquidityPool).asInstanceOf[LiquidityPoolCalculatedState]
    } yield
      expect.all(
        liquidityPoolCalculatedState.failed.toList.length === 1,
        liquidityPoolCalculatedState.failed.toList.head.expiringEpochProgress === EpochProgress(
          NonNegLong.unsafeFrom(futureEpoch.value.value + config.failedOperationsExpirationEpochProgresses.value.value)
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
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc"),
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb"),
        none,
        SwapAmount(PosLong.MaxValue),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        EpochProgress.MinValue,
        List.empty
      )
      allowSpendTokenB = AllowSpend(
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc"),
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb"),
        none,
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
          signedAllowSpendA.hash,
          signedAllowSpendB.hash,
          primaryToken.identifier,
          pairToken.identifier,
          primaryToken.amount,
          pairToken.amount,
          EpochProgress.MaxValue
        )
      )

      allowSpends = SortedMap(
        primaryToken.identifier.get.value.some ->
          SortedMap(
            Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc") -> SortedSet(signedAllowSpendA.signed)
          ),
        pairToken.identifier.get.value.some ->
          SortedMap(
            Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc") -> SortedSet(signedAllowSpendB.signed)
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

      liquidityPoolResponse <- combineNewLiquidityPool[IO](
        config,
        state,
        liquidityPoolUpdate,
        futureEpoch,
        allowSpends
      )
      liquidityPoolCalculatedState = liquidityPoolResponse.calculated.operations(LiquidityPool).asInstanceOf[LiquidityPoolCalculatedState]
    } yield
      expect.all(
        liquidityPoolCalculatedState.failed.toList.length === 1,
        liquidityPoolCalculatedState.failed.toList.head.expiringEpochProgress === EpochProgress(
          NonNegLong.unsafeFrom(futureEpoch.value.value + config.failedOperationsExpirationEpochProgresses.value.value)
        ),
        liquidityPoolCalculatedState.failed.toList.head.reason == AllowSpendExpired(allowSpendTokenA)
      )
  }
}
