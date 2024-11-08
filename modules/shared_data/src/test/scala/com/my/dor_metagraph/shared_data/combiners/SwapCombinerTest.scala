package com.my.dor_metagraph.shared_data.combiners

import cats.data.NonEmptySet
import cats.effect.{IO, Resource}
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.ext.cats.effect.ResourceIO
import io.constellationnetwork.json.JsonSerializer
import io.constellationnetwork.schema.ID.Id
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap._
import io.constellationnetwork.schema.{SnapshotOrdinal, artifact}
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.hex.Hex
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.signature.signature.{Signature, SignatureProof}
import io.constellationnetwork.security.{Hasher, KeyPairGenerator, SecurityProvider}

import com.my.dor_metagraph.shared_data.DummyL0Context.buildL0NodeContext
import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.PosLong
import org.amm_metagraph.shared_data.Utils._
import org.amm_metagraph.shared_data.combiners.SwapCombiner.combineSwap
import org.amm_metagraph.shared_data.types.DataUpdates.SwapUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._
import weaver.MutableIOSuite

object SwapCombinerTest extends MutableIOSuite {
  type Res = (Hasher[IO], SecurityProvider[IO])

  override def sharedResource: Resource[IO, Res] = for {
    sp <- SecurityProvider.forAsync[IO]
    implicit0(j: JsonSerializer[IO]) <- JsonSerializer.forSync[IO].asResource
    h = Hasher.forJson[IO]
  } yield (h, sp)

  def buildLiquidityPoolCalculatedState(
    tokenA: TokenInformation,
    tokenB: TokenInformation,
    owner: Address,
    feeRate: Long
  ): (String, LiquidityPoolCalculatedState) = {
    val primaryAddressAsString = tokenA.identifier.fold("")(address => address.value.value)
    val pairAddressAsString = tokenB.identifier.fold("")(address => address.value.value)
    val poolId = org.amm_metagraph.shared_data.types.LiquidityPool.PoolId(s"$primaryAddressAsString-$pairAddressAsString")
    val liquidityPool = LiquidityPool(
      poolId,
      tokenA,
      tokenB,
      owner,
      tokenA.amount.value.fromTokenAmountFormat * tokenB.amount.value.fromTokenAmountFormat,
      feeRate.toDouble / 100d,
      math.sqrt(tokenA.amount.value.toDouble * tokenB.amount.value.toDouble).toTokenAmountFormat,
      LiquidityProviders(Map(owner -> math.sqrt(tokenA.amount.value.toDouble * tokenB.amount.value.toDouble).toTokenAmountFormat))
    )
    (poolId.value, LiquidityPoolCalculatedState(Map(poolId.value -> liquidityPool)))
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

  test("Test swap successfully when liquidity pool exists 3% fee") { implicit res =>
    implicit val (h, sp) = res

    val primaryToken =
      TokenInformation(CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some, 1000000L.toTokenAmountFormat.toPosLongUnsafe)
    val pairToken =
      TokenInformation(CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some, 2000000L.toTokenAmountFormat.toPosLongUnsafe)
    val ownerAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Ks")

    val (poolId, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress, 3L)
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
          ownerAddress,
          primaryToken.identifier,
          pairToken.identifier,
          0L,
          signedAllowSpend.hash,
          SwapAmount(100000L),
          SwapAmount(100000L),
          EpochProgress.MaxValue,
          none,
          none,
          none
        )
      )

      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(
        keyPair,
        SortedMap(
          ownerAddress -> SortedSet(signedAllowSpend.signed)
        )
      )

      stakeResponse <- combineSwap[IO](
        state,
        swapUpdate,
        SnapshotOrdinal.MinValue
      )

      swapCalculatedState = stakeResponse.calculated.operations(OperationType.Swap).asInstanceOf[SwapCalculatedState]
      addressSwapResponse = swapCalculatedState.confirmed(ownerAddress).head

      oldLiquidityPoolCalculatedState = state.calculated.operations(OperationType.LiquidityPool).asInstanceOf[LiquidityPoolCalculatedState]
      oldLiquidityPool = oldLiquidityPoolCalculatedState.liquidityPools(poolId)

      updatedLiquidityPoolCalculatedState = stakeResponse.calculated
        .operations(OperationType.LiquidityPool)
        .asInstanceOf[LiquidityPoolCalculatedState]
      updatedLiquidityPool = updatedLiquidityPoolCalculatedState.liquidityPools(poolId)

      swapSpendTransactions = stakeResponse.sharedArtifacts.collect {
        case transaction: artifact.SpendTransaction => transaction
      }.collect {
        case transaction: artifact.PendingSpendTransaction => transaction
      }

      spendTransaction = swapSpendTransactions.find(_.allowSpendRef === signedAllowSpend.hash)
    } yield
      expect.eql(1100000L, addressSwapResponse.fromToken.amount.value.fromTokenAmountFormat) &&
        expect.eql(primaryToken.identifier.get, addressSwapResponse.fromToken.identifier.get) &&
        expect.eql(1823154.05651777, addressSwapResponse.toToken.amount.value.fromTokenAmountFormat) &&
        expect.eql(1000000L.toTokenAmountFormat, oldLiquidityPool.tokenA.amount.value) &&
        expect.eql(2000000L.toTokenAmountFormat, oldLiquidityPool.tokenB.amount.value) &&
        expect.eql(1100000L.toTokenAmountFormat, updatedLiquidityPool.tokenA.amount.value) &&
        expect.eql(1823154.05651777.toTokenAmountFormat, updatedLiquidityPool.tokenB.amount.value) &&
        expect.eql(allowSpend.amount.value.value, spendTransaction.get.amount.value.value)
  }

  test("Test swap successfully when liquidity pool exists - 10% fee") { implicit res =>
    implicit val (h, sp) = res

    val primaryToken =
      TokenInformation(CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some, 1000000L.toTokenAmountFormat.toPosLongUnsafe)
    val pairToken =
      TokenInformation(CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some, 2000000L.toTokenAmountFormat.toPosLongUnsafe)
    val ownerAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Ks")

    val (poolId, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress, 10L)
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
          ownerAddress,
          primaryToken.identifier,
          pairToken.identifier,
          0L,
          signedAllowSpend.hash,
          SwapAmount(100000L),
          SwapAmount(100000L),
          EpochProgress.MaxValue,
          none,
          none,
          none
        )
      )

      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(
        keyPair,
        SortedMap(
          ownerAddress -> SortedSet(signedAllowSpend.signed)
        )
      )
      stakeResponse <- combineSwap[IO](
        state,
        swapUpdate,
        SnapshotOrdinal.MinValue
      )

      swapCalculatedState = stakeResponse.calculated.operations(OperationType.Swap).asInstanceOf[SwapCalculatedState]
      addressSwapResponse = swapCalculatedState.confirmed(ownerAddress).head

      oldLiquidityPoolCalculatedState = state.calculated.operations(OperationType.LiquidityPool).asInstanceOf[LiquidityPoolCalculatedState]
      oldLiquidityPool = oldLiquidityPoolCalculatedState.liquidityPools(poolId)

      updatedLiquidityPoolCalculatedState = stakeResponse.calculated
        .operations(OperationType.LiquidityPool)
        .asInstanceOf[LiquidityPoolCalculatedState]
      updatedLiquidityPool = updatedLiquidityPoolCalculatedState.liquidityPools(poolId)

      swapSpendTransactions = stakeResponse.sharedArtifacts.collect {
        case transaction: artifact.SpendTransaction => transaction
      }.collect {
        case transaction: artifact.PendingSpendTransaction => transaction
      }

      spendTransaction = swapSpendTransactions.find(_.allowSpendRef === signedAllowSpend.hash)
    } yield
      expect.eql(1100000L, addressSwapResponse.fromToken.amount.value.fromTokenAmountFormat) &&
        expect.eql(primaryToken.identifier.get, addressSwapResponse.fromToken.identifier.get) &&
        expect.eql(1834862.3853211, addressSwapResponse.toToken.amount.value.fromTokenAmountFormat) &&
        expect.eql(1000000L.toTokenAmountFormat, oldLiquidityPool.tokenA.amount.value) &&
        expect.eql(2000000L.toTokenAmountFormat, oldLiquidityPool.tokenB.amount.value) &&
        expect.eql(1100000L.toTokenAmountFormat, updatedLiquidityPool.tokenA.amount.value) &&
        expect.eql(1834862.3853211.toTokenAmountFormat, updatedLiquidityPool.tokenB.amount.value) &&
        expect.eql(allowSpend.amount.value.value, spendTransaction.get.amount.value.value)
  }

  test("Test swap - pending swap without allow spend") { implicit res =>
    implicit val (h, sp) = res

    val primaryToken =
      TokenInformation(CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some, 1000000L.toTokenAmountFormat.toPosLongUnsafe)
    val pairToken =
      TokenInformation(CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some, 2000000L.toTokenAmountFormat.toPosLongUnsafe)
    val ownerAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Ks")

    val (_, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress, 10L)
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
          ownerAddress,
          primaryToken.identifier,
          pairToken.identifier,
          0L,
          signedAllowSpend.hash,
          SwapAmount(100000L),
          SwapAmount(100000L),
          EpochProgress.MaxValue,
          none,
          none,
          none
        )
      )

      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(
        keyPair,
        SortedMap.empty
      )

      stakeResponse <- combineSwap[IO](
        state,
        swapUpdate,
        SnapshotOrdinal.MinValue
      )

      swapCalculatedState = stakeResponse.calculated.operations(OperationType.Swap).asInstanceOf[SwapCalculatedState]
      confirmedSwapResponse = swapCalculatedState.confirmed.get(ownerAddress)
      pendingSwapResponse = swapCalculatedState.pending.get(ownerAddress)

    } yield
      expect.eql(none, confirmedSwapResponse) &&
        expect.eql(1, pendingSwapResponse.get.size) &&
        expect.eql(swapUpdate.value.allowSpendReference, pendingSwapResponse.get.head.value.allowSpendReference)
  }

  test("Test swap - ignore swap that exceed limit epoch progress") { implicit res =>
    implicit val (h, sp) = res

    val primaryToken =
      TokenInformation(CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some, 1000000L.toTokenAmountFormat.toPosLongUnsafe)
    val pairToken =
      TokenInformation(CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some, 2000000L.toTokenAmountFormat.toPosLongUnsafe)
    val ownerAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Ks")

    val (_, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress, 10L)
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
          ownerAddress,
          primaryToken.identifier,
          pairToken.identifier,
          0L,
          signedAllowSpend.hash,
          SwapAmount(100000L),
          SwapAmount(100000L),
          EpochProgress.MinValue,
          none,
          none,
          none
        )
      )

      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(
        keyPair,
        SortedMap.empty,
        EpochProgress.MaxValue
      )

      stakeResponse <- combineSwap[IO](
        state,
        swapUpdate,
        SnapshotOrdinal.MinValue
      )

      swapCalculatedState = stakeResponse.calculated.operations(OperationType.Swap).asInstanceOf[SwapCalculatedState]
      confirmedSwapResponse = swapCalculatedState.confirmed.get(ownerAddress)
      pendingSwapResponse = swapCalculatedState.pending.get(ownerAddress)

    } yield
      expect.eql(none, confirmedSwapResponse) &&
        expect.eql(none, pendingSwapResponse)
  }
}
