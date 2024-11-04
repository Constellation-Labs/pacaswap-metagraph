package com.my.dor_metagraph.shared_data.combiners

import cats.effect.{IO, Resource}
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.ext.cats.effect.ResourceIO
import io.constellationnetwork.json.JsonSerializer
import io.constellationnetwork.schema._
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap._
import io.constellationnetwork.security._
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed

import com.my.dor_metagraph.shared_data.DummyL0Context.buildL0NodeContext
import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.PosLong
import org.amm_metagraph.shared_data.Utils._
import org.amm_metagraph.shared_data.combiners.StakingCombiner.combineStaking
import org.amm_metagraph.shared_data.types.DataUpdates.StakingUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool.{LiquidityPool, LiquidityProviders, TokenInformation}
import org.amm_metagraph.shared_data.types.States._
import weaver.MutableIOSuite

object StakingCombinerTest extends MutableIOSuite {

  type Res = (Hasher[IO], SecurityProvider[IO])

  override def sharedResource: Resource[IO, Res] = for {
    sp <- SecurityProvider.forAsync[IO]
    implicit0(j: JsonSerializer[IO]) <- JsonSerializer.forSync[IO].asResource
    h = Hasher.forJson[IO]
  } yield (h, sp)

  def buildLiquidityPoolCalculatedState(
    tokenA: TokenInformation,
    tokenB: TokenInformation,
    owner: Address
  ): (String, LiquidityPoolCalculatedState) = {
    val primaryAddressAsString = tokenA.identifier.fold("")(address => address.value.value)
    val pairAddressAsString = tokenB.identifier.fold("")(address => address.value.value)
    val poolId = s"$primaryAddressAsString-$pairAddressAsString"
    val liquidityPool = LiquidityPool(
      poolId,
      tokenA.copy(amount = tokenA.amount.value.toTokenAmountFormat.toPosLongUnsafe),
      tokenB.copy(amount = tokenB.amount.value.toTokenAmountFormat.toPosLongUnsafe),
      owner,
      (tokenA.amount.value * tokenB.amount.value).toDouble,
      0.3,
      math.sqrt(tokenA.amount.value.toDouble * tokenB.amount.value.toDouble).toTokenAmountFormat,
      LiquidityProviders(Map(owner -> math.sqrt(tokenA.amount.value.toDouble * tokenB.amount.value.toDouble).toTokenAmountFormat))
    )
    (poolId, LiquidityPoolCalculatedState(Map(poolId -> liquidityPool)))
  }

  test("Test combining successfully when liquidity pool exists") { implicit res =>
    implicit val (h, sp) = res

    val primaryToken = TokenInformation(CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some, 100L)
    val pairToken = TokenInformation(CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some, 50L)
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

      stakingUpdate = StakingUpdate(
        signedAllowSpendA.hash,
        signedAllowSpendB.hash,
        primaryToken.identifier,
        100L.toPosLongUnsafe,
        pairToken.identifier
      )

      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(
        keyPair,
        SortedMap(
          Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc") -> SortedSet(signedAllowSpendA.signed, signedAllowSpendB.signed)
        )
      )

      stakeResponse <- combineStaking[IO](
        state,
        stakingUpdate,
        ownerAddress,
        SnapshotOrdinal.MinValue
      )

      stakingCalculatedState = stakeResponse.calculated.ammState(OperationType.Staking).asInstanceOf[StakingCalculatedState]
      addressStakedResponse = stakingCalculatedState.addresses(ownerAddress)

      oldLiquidityPoolCalculatedState = state.calculated.ammState(OperationType.LiquidityPool).asInstanceOf[LiquidityPoolCalculatedState]
      oldLiquidityPool = oldLiquidityPoolCalculatedState.liquidityPools(poolId)

      updatedLiquidityPoolCalculatedState = stakeResponse.calculated
        .ammState(OperationType.LiquidityPool)
        .asInstanceOf[LiquidityPoolCalculatedState]
      updatedLiquidityPool = updatedLiquidityPoolCalculatedState.liquidityPools(poolId)

      stakingSpendTransactions = stakeResponse.sharedArtifacts.collect {
        case transaction: artifact.SpendTransaction => transaction
      }.collect {
        case transaction: artifact.PendingSpendTransaction => transaction
      }
      tokenASpendTransaction = stakingSpendTransactions.find(_.allowSpendRef === signedAllowSpendA.hash)
      tokenBSpendTransaction = stakingSpendTransactions.find(_.allowSpendRef === signedAllowSpendB.hash)

    } yield
      expect.eql(100L.toTokenAmountFormat, addressStakedResponse.tokenA.amount.value) &&
        expect.eql(primaryToken.identifier.get, addressStakedResponse.tokenA.identifier.get) &&
        expect.eql(50L.toTokenAmountFormat, addressStakedResponse.tokenB.amount.value) &&
        expect.eql(pairToken.identifier.get, addressStakedResponse.tokenB.identifier.get) &&
        expect.eql(100L.toTokenAmountFormat, oldLiquidityPool.tokenA.amount.value) &&
        expect.eql(200L.toTokenAmountFormat, updatedLiquidityPool.tokenA.amount.value) &&
        expect.eql(50L.toTokenAmountFormat, oldLiquidityPool.tokenB.amount.value) &&
        expect.eql(100L.toTokenAmountFormat, updatedLiquidityPool.tokenB.amount.value) &&
        expect.eql(5000d, oldLiquidityPool.k) &&
        expect.eql(20000d, updatedLiquidityPool.k) &&
        expect.eql(allowSpendTokenA.amount.value.value, tokenASpendTransaction.get.amount.value.value) &&
        expect.eql(allowSpendTokenB.amount.value.value, tokenBSpendTransaction.get.amount.value.value)
  }

  test("Throws error if liquidity pool does not exists") { implicit res =>
    implicit val (h, sp) = res

    val primaryToken = TokenInformation(CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some, 100L)
    val pairToken = TokenInformation(CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some, 50L)
    val ownerAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Ks")

    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(Map.empty)
    val state = DataState(ammOnChainState, ammCalculatedState)
    val stakingUpdate = StakingUpdate(
      Hash.empty,
      Hash.empty,
      primaryToken.identifier,
      100L.toPosLongUnsafe,
      pairToken.identifier
    )
    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(keyPair, SortedMap.empty)
      result <- combineStaking[IO](
        state,
        stakingUpdate,
        ownerAddress,
        SnapshotOrdinal.MinValue
      ).attempt.map {
        case Left(e: IllegalStateException) =>
          expect(e.getMessage == "Liquidity Pool does not exists")
        case Left(e) =>
          failure(s"Unexpected exception: $e")
        case Right(_) =>
          failure("Expected exception was not thrown")
      }
    } yield result
  }
}
