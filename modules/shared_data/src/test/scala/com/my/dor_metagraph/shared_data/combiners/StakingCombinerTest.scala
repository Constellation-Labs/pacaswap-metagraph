package com.my.dor_metagraph.shared_data.combiners

import cats.data.NonEmptySet
import cats.effect.{IO, Resource}
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.ext.cats.effect.ResourceIO
import io.constellationnetwork.json.JsonSerializer
import io.constellationnetwork.schema.ID.Id
import io.constellationnetwork.schema._
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact.SpendTransaction
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap._
import io.constellationnetwork.security._
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.hex.Hex
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.signature.signature.{Signature, SignatureProof}

import com.my.dor_metagraph.shared_data.DummyL0Context.buildL0NodeContext
import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.{NonNegLong, PosLong}
import eu.timepit.refined.types.numeric.PosDouble
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.app.ApplicationConfig.{Dev, Governance, VotingWeightMultipliers}
import org.amm_metagraph.shared_data.combiners.StakingCombiner.combineStaking
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.types.DataUpdates.StakingUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.Staking.StakingReference
import org.amm_metagraph.shared_data.types.States.OperationType.Staking
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.codecs
import weaver.MutableIOSuite

object StakingCombinerTest extends MutableIOSuite {

  type Res = (Hasher[IO], codecs.HasherSelector[IO], SecurityProvider[IO])

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
    )
  )

  override def sharedResource: Resource[IO, Res] = for {
    sp <- SecurityProvider.forAsync[IO]
    implicit0(j: JsonSerializer[IO]) <- JsonSerializer.forSync[IO].asResource
    h = Hasher.forJson[IO]
    hs = codecs.HasherSelector.forSync(h, h)
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
    update: StakingUpdate
  ): Signed[StakingUpdate] =
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

    val ownerAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Ks")

    val (poolId, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      allowSpendTokenA = AllowSpend(
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc"),
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb"),
        primaryToken.identifier,
        SwapAmount(PosLong.unsafeFrom(toFixedPoint(200.0))),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        EpochProgress.MaxValue,
        List.empty
      )
      allowSpendTokenB = AllowSpend(
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc"),
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb"),
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
          signedAllowSpendA.hash,
          signedAllowSpendB.hash,
          primaryToken.identifier,
          PosLong.unsafeFrom(toFixedPoint(100.0)),
          pairToken.identifier,
          StakingReference.empty,
          EpochProgress.MaxValue
        )
      )

      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(
        keyPair,
        SortedMap(
          primaryToken.identifier.get.value.some ->
            SortedMap(
              Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc") -> SortedSet(signedAllowSpendA.signed)
            ),
          pairToken.identifier.get.value.some ->
            SortedMap(
              Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc") -> SortedSet(signedAllowSpendB.signed)
            )
        ),
        EpochProgress.MinValue,
        ownerAddress
      )

      stakeResponse <- combineStaking[IO](
        config,
        state,
        stakingUpdate,
        ownerAddress,
        SnapshotOrdinal.MinValue,
        EpochProgress.MinValue
      )

      oldLiquidityPool = liquidityPoolCalculatedState.confirmed.value(poolId)
      updatedLiquidityPool = stakeResponse.calculated
        .operations(OperationType.LiquidityPool)
        .asInstanceOf[LiquidityPoolCalculatedState]
        .confirmed
        .value(poolId)

      stakingSpendTransactions = stakeResponse.sharedArtifacts.collect {
        case action: artifact.SpendAction => action.output
      }.collect {
        case transaction: SpendTransaction => transaction
      }

    } yield
      expect.all(
        oldLiquidityPool.tokenA.amount.value === toFixedPoint(100.0),
        oldLiquidityPool.tokenB.amount.value === toFixedPoint(50.0),
        oldLiquidityPool.poolShares.totalShares.value === toFixedPoint(1),
        oldLiquidityPool.poolShares.addressShares(ownerAddress).value.value.value === toFixedPoint(1),
        updatedLiquidityPool.tokenA.amount.value === toFixedPoint(200.0),
        updatedLiquidityPool.tokenB.amount.value === toFixedPoint(100.0),
        updatedLiquidityPool.poolShares.totalShares.value === toFixedPoint(1.5),
        updatedLiquidityPool.poolShares.addressShares(ownerAddress).value.value.value === toFixedPoint(1.5),
        stakingSpendTransactions.size === 2
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
    val ownerAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Ks")
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
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc"),
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb"),
        none,
        SwapAmount(PosLong.unsafeFrom(toFixedPoint(50.0))),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        EpochProgress.MaxValue,
        List.empty
      )
      allowSpendTokenB = AllowSpend(
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc"),
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb"),
        none,
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
          signedAllowSpendA.hash,
          signedAllowSpendB.hash,
          primaryToken.identifier,
          PosLong.unsafeFrom(toFixedPoint(100.0)),
          pairToken.identifier,
          StakingReference.empty,
          EpochProgress.MaxValue
        )
      )

      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(
        keyPair,
        SortedMap(
          primaryToken.identifier.get.value.some ->
            SortedMap(
              Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc") -> SortedSet(signedAllowSpendA.signed)
            ),
          pairToken.identifier.get.value.some ->
            SortedMap(
              Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc") -> SortedSet(signedAllowSpendB.signed)
            )
        ),
        EpochProgress.MinValue,
        ownerAddress
      )

      stakeResponse <- combineStaking[IO](
        config,
        state,
        stakingUpdate,
        ownerAddress,
        SnapshotOrdinal.MinValue,
        EpochProgress.MinValue
      )

      oldLiquidityPool = liquidityPoolCalculatedState.confirmed.value(poolId)
      updatedLiquidityPool = stakeResponse.calculated
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
        updatedLiquidityPool.poolShares.addressShares(ownerAddress).value.value.value === toFixedPoint(2.0),
        updatedLiquidityPool.poolShares.addressShares(secondProviderAddress).value.value.value === toFixedPoint(1.0)
      )
  }

  test("Throws error if liquidity pool does not exists") { implicit res =>
    implicit val (h, hs, sp) = res

    val primaryToken = TokenInformation(CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some, 100L)
    val pairToken = TokenInformation(CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some, 50L)
    val ownerAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Ks")

    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(Map.empty)
    val state = DataState(ammOnChainState, ammCalculatedState)
    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      allowSpendTokenA = AllowSpend(
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc"),
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb"),
        none,
        SwapAmount(PosLong.MinValue),
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
      stakingUpdate = getFakeSignedUpdate(
        StakingUpdate(
          signedAllowSpendA.hash,
          signedAllowSpendB.hash,
          primaryToken.identifier,
          100L.toPosLongUnsafe,
          pairToken.identifier,
          StakingReference.empty,
          EpochProgress.MaxValue
        )
      )

      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(
        keyPair,
        SortedMap(
          primaryToken.identifier.get.value.some ->
            SortedMap(
              Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc") -> SortedSet(signedAllowSpendA.signed)
            ),
          pairToken.identifier.get.value.some ->
            SortedMap(
              Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc") -> SortedSet(signedAllowSpendB.signed)
            )
        ),
        EpochProgress.MinValue,
        ownerAddress
      )

      result <- combineStaking[IO](
        config,
        state,
        stakingUpdate,
        ownerAddress,
        SnapshotOrdinal.MinValue,
        EpochProgress.MinValue
      ).attempt.map {
        case Left(e: IllegalStateException) =>
          expect(e.getMessage == "Liquidity Pool does not exist")
        case Left(e) =>
          failure(s"Unexpected exception: $e")
        case Right(_) =>
          failure("Expected exception was not thrown")
      }
    } yield result
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

    val ownerAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Ks")

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

      stakingUpdate = getFakeSignedUpdate(
        StakingUpdate(
          signedAllowSpendA.hash,
          signedAllowSpendB.hash,
          primaryToken.identifier,
          100L.toPosLongUnsafe,
          pairToken.identifier,
          StakingReference.empty,
          futureEpoch
        )
      )

      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(
        keyPair,
        SortedMap(
          primaryToken.identifier.get.value.some ->
            SortedMap(
              Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc") -> SortedSet(signedAllowSpendA.signed)
            ),
          pairToken.identifier.get.value.some ->
            SortedMap(
              Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc") -> SortedSet(signedAllowSpendB.signed)
            )
        ),
        EpochProgress.MinValue,
        ownerAddress
      )

      stakeResponse <- combineStaking[IO](
        config,
        state,
        stakingUpdate,
        ownerAddress,
        SnapshotOrdinal.MinValue,
        futureEpoch
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

    val ownerAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Ks")

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
        SwapAmount(PosLong(100)),
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

      stakingUpdate = getFakeSignedUpdate(
        StakingUpdate(
          signedAllowSpendA.hash,
          signedAllowSpendB.hash,
          primaryToken.identifier,
          100L.toPosLongUnsafe,
          pairToken.identifier,
          StakingReference.empty,
          futureEpoch
        )
      )

      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(
        keyPair,
        SortedMap(
          primaryToken.identifier.get.value.some ->
            SortedMap(
              Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc") -> SortedSet(signedAllowSpendA.signed)
            ),
          pairToken.identifier.get.value.some ->
            SortedMap(
              Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc") -> SortedSet(signedAllowSpendB.signed)
            )
        ),
        EpochProgress.MinValue,
        ownerAddress
      )

      stakeResponse <- combineStaking[IO](
        config,
        state,
        stakingUpdate,
        ownerAddress,
        SnapshotOrdinal.MinValue,
        futureEpoch
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
}
