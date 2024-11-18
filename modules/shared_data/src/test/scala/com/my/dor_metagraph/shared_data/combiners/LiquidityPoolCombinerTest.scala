package com.my.dor_metagraph.shared_data.combiners

import cats.data.NonEmptySet
import cats.effect.{IO, Resource}
import cats.syntax.all._
import com.my.dor_metagraph.shared_data.DummyL0Context.buildL0NodeContext
import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.PosLong
import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.ext.cats.effect.ResourceIO
import io.constellationnetwork.json.JsonSerializer
import io.constellationnetwork.schema.ID.Id
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap._
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.hex.Hex
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.signature.signature.{Signature, SignatureProof}
import io.constellationnetwork.security.{Hasher, KeyPairGenerator, SecurityProvider}
import org.amm_metagraph.shared_data.Utils.{LongOps, buildLiquidityPoolUniqueIdentifier}
import org.amm_metagraph.shared_data.combiners.LiquidityPoolCombiner.combineLiquidityPool
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.States._
import weaver.MutableIOSuite

import scala.collection.immutable.{SortedMap, SortedSet}

object LiquidityPoolCombinerTest extends MutableIOSuite {

  type Res = (Hasher[IO], SecurityProvider[IO])

  override def sharedResource: Resource[IO, Res] = for {
    sp <- SecurityProvider.forAsync[IO]
    implicit0(j: JsonSerializer[IO]) <- JsonSerializer.forSync[IO].asResource
    h = Hasher.forJson[IO]
  } yield (h, sp)

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
  test("Successfully create a liquidity pool") { implicit res =>
    implicit val (h, sp) = res

    val tokenAId = CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some
    val tokenAAmount = PosLong(100L)
    val tokenBId = CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some
    val tokenBAmount = PosLong(50L)

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

      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(
        keyPair,
        SortedMap(
          Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc") -> SortedSet(signedAllowSpendA.signed, signedAllowSpendB.signed)
        )
      )

      liquidityPoolResponse <- combineLiquidityPool[IO](
        state,
        liquidityPoolUpdate,
        ownerAddress
      )
      poolId <- buildLiquidityPoolUniqueIdentifier(tokenAId, tokenBId)
      updatedLiquidityPoolCalculatedState = liquidityPoolResponse.calculated
        .confirmedOperations(OperationType.LiquidityPool)
        .asInstanceOf[LiquidityPoolCalculatedState]
      updatedLiquidityPool = updatedLiquidityPoolCalculatedState.liquidityPools(poolId.value)
    } yield
      expect.eql(100L.toTokenAmountFormat, updatedLiquidityPool.tokenA.amount.value) &&
        expect.eql(tokenAId.get, updatedLiquidityPool.tokenA.identifier.get) &&
        expect.eql(50L.toTokenAmountFormat, updatedLiquidityPool.tokenB.amount.value) &&
        expect.eql(tokenBId.get, updatedLiquidityPool.tokenB.identifier.get) &&
        expect.eql(5000d, updatedLiquidityPool.k)
  }

  test("Successfully create a liquidity pool - L0Token - DAG") { implicit res =>
    implicit val (h, sp) = res

    val tokenAId = CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some
    val tokenAAmount = PosLong(100L)
    val tokenBId = none
    val tokenBAmount = PosLong(50L)

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

      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(
        keyPair,
        SortedMap(
          Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc") -> SortedSet(signedAllowSpendA.signed, signedAllowSpendB.signed)
        )
      )

      liquidityPoolResponse <- combineLiquidityPool[IO](
        state,
        liquidityPoolUpdate,
        ownerAddress
      )

      poolId <- buildLiquidityPoolUniqueIdentifier(tokenAId, tokenBId)
      updatedLiquidityPoolCalculatedState = liquidityPoolResponse.calculated
        .confirmedOperations(OperationType.LiquidityPool)
        .asInstanceOf[LiquidityPoolCalculatedState]
      updatedLiquidityPool = updatedLiquidityPoolCalculatedState.liquidityPools(poolId.value)
    } yield
      expect.eql(100L.toTokenAmountFormat, updatedLiquidityPool.tokenA.amount.value) &&
        expect.eql(tokenAId.get, updatedLiquidityPool.tokenA.identifier.get) &&
        expect.eql(50L.toTokenAmountFormat, updatedLiquidityPool.tokenB.amount.value) &&
        expect.eql(tokenBId.isEmpty, updatedLiquidityPool.tokenB.identifier.isEmpty) &&
        expect.eql(5000d, updatedLiquidityPool.k)
  }
}
