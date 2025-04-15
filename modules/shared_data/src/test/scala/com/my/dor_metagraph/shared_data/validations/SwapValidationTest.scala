package com.my.dor_metagraph.shared_data.validations

import cats.Eq
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, NonEmptySet}
import cats.effect.{IO, Resource}
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext, L1NodeContext}
import io.constellationnetwork.ext.cats.effect.ResourceIO
import io.constellationnetwork.json.JsonSerializer
import io.constellationnetwork.schema.ID.Id
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap._
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.hex.Hex
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.signature.signature.{Signature, SignatureProof}
import io.constellationnetwork.security.{Hasher, KeyPairGenerator, SecurityProvider}

import com.my.dor_metagraph.shared_data.DummyL0Context.buildL0NodeContext
import com.my.dor_metagraph.shared_data.DummyL1Context.buildL1NodeContext
import com.my.dor_metagraph.shared_data.Shared._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.{NonNegLong, PosLong}
import eu.timepit.refined.types.numeric.PosDouble
import org.amm_metagraph.shared_data.FeeDistributor
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.app.ApplicationConfig._
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.types.DataUpdates.SwapUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.Swap.SwapReference
import org.amm_metagraph.shared_data.types.codecs
import org.amm_metagraph.shared_data.validations.{Errors, ValidationService}
import weaver.MutableIOSuite

object SwapValidationTest extends MutableIOSuite {
  type Res = (Hasher[IO], codecs.HasherSelector[IO], SecurityProvider[IO])

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
    ),
    PosLong.unsafeFrom((100 * 1e8).toLong)
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
    owner: Address
  ): (String, LiquidityPoolCalculatedState) = {
    val primaryAddressAsString = tokenA.identifier.fold("")(address => address.value.value)
    val pairAddressAsString = tokenB.identifier.fold("")(address => address.value.value)
    val poolId = org.amm_metagraph.shared_data.types.LiquidityPool.PoolId(s"$primaryAddressAsString-$pairAddressAsString")
    val liquidityPool = LiquidityPool(
      poolId,
      tokenA,
      tokenB,
      owner,
      BigInt(tokenA.amount.value) * BigInt(tokenB.amount.value),
      PoolShares(
        1.toTokenAmountFormat.toPosLongUnsafe,
        Map(owner -> ShareAmount(Amount(PosLong.unsafeFrom(1e8.toLong)))),
        Map(owner -> 0L.toNonNegLongUnsafe)
      ),
      FeeDistributor.empty
    )
    (
      poolId.value,
      LiquidityPoolCalculatedState.empty.copy(confirmed =
        ConfirmedLiquidityPoolCalculatedState.empty.copy(value = Map(poolId.value -> liquidityPool))
      )
    )
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

  implicit val eqDataApplicationValidationErrorOr: Eq[DataApplicationValidationErrorOr[Unit]] =
    Eq.fromUniversalEquals

  test("Validate update successfully") { implicit res =>
    implicit val (_, hs, sp) = res
    val primaryToken = TokenInformation(CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some, 100L)
    val pairToken = TokenInformation(CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some, 50L)

    val context: L1NodeContext[IO] = buildL1NodeContext(
      ammMetagraphId
    )

    val stakingUpdate = SwapUpdate(
      ammMetagraphIdAsCurrencyId,
      sourceAddress,
      primaryToken.identifier,
      pairToken.identifier,
      Hash.empty,
      SwapAmount(100000L.toTokenAmountFormat.toPosLongUnsafe),
      SwapAmount(100000L.toTokenAmountFormat.toPosLongUnsafe),
      EpochProgress.MinValue,
      SwapReference.empty
    )

    val validationService = ValidationService.make[IO](config)
    for {
      response <- validationService.validateUpdate(stakingUpdate)(context)
    } yield expect.eql(Valid(()), response)
  }

  test("Validate data successfully") { implicit res =>
    implicit val (h, hs, sp) = res
    val primaryToken =
      TokenInformation(CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some, 1000000L.toTokenAmountFormat.toPosLongUnsafe)
    val pairToken =
      TokenInformation(CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some, 2000000L.toTokenAmountFormat.toPosLongUnsafe)
    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")

    val (_, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      allowSpend = AllowSpend(
        ownerAddress,
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb"),
        primaryToken.identifier,
        SwapAmount(PosLong.MinValue),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        EpochProgress.MaxValue,
        List.empty
      )
      signedAllowSpend <- Signed
        .forAsyncHasher[IO, AllowSpend](allowSpend, keyPair)
        .flatMap(_.toHashed[IO])

      swapUpdate = SwapUpdate(
        CurrencyId(ownerAddress),
        ownerAddress,
        primaryToken.identifier,
        pairToken.identifier,
        signedAllowSpend.hash,
        SwapAmount(100000L.toTokenAmountFormat.toPosLongUnsafe),
        SwapAmount(100000L.toTokenAmountFormat.toPosLongUnsafe),
        EpochProgress.MinValue,
        SwapReference.empty
      )
      fakeSignedUpdate = getFakeSignedUpdate(swapUpdate)

      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(
        keyPair,
        SortedMap(
          ownerAddress.some ->
            SortedMap(
              ownerAddress -> SortedSet(signedAllowSpend.signed)
            )
        ),
        EpochProgress.MinValue,
        SnapshotOrdinal.MinValue,
        SortedMap.empty,
        EpochProgress.MinValue,
        SnapshotOrdinal.MinValue,
        ownerAddress
      )

      validationService = ValidationService.make[IO](config)
      response <- validationService.validateData(NonEmptyList.one(fakeSignedUpdate), state)
    } yield expect.eql(Valid(()), response)
  }

  test("Validate data fail - Liquidity pool does not exists") { implicit res =>
    implicit val (h, hs, sp) = res
    val primaryToken =
      TokenInformation(CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some, 1000000L.toTokenAmountFormat.toPosLongUnsafe)
    val pairToken =
      TokenInformation(CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some, 2000000L.toTokenAmountFormat.toPosLongUnsafe)
    val ownerAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Gs")

    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(Map.empty)
    val state = DataState(ammOnChainState, ammCalculatedState)
    val stakingUpdate = SwapUpdate(
      CurrencyId(ownerAddress),
      sourceAddress,
      primaryToken.identifier,
      pairToken.identifier,
      Hash.empty,
      SwapAmount(100000L.toTokenAmountFormat.toPosLongUnsafe),
      SwapAmount(100000L.toTokenAmountFormat.toPosLongUnsafe),
      EpochProgress.MinValue,
      SwapReference.empty
    )
    val fakeSignedUpdate = getFakeSignedUpdate(stakingUpdate)

    val validationService = ValidationService.make[IO](config)
    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(
        keyPair,
        SortedMap.empty,
        EpochProgress.MinValue,
        SnapshotOrdinal.MinValue,
        SortedMap.empty,
        EpochProgress.MinValue,
        SnapshotOrdinal.MinValue,
        ownerAddress
      )
      response <- validationService.validateData(NonEmptyList.one(fakeSignedUpdate), state)
    } yield {
      val expectedError = response match {
        case Invalid(errors) if errors.exists {
              case Errors.SwapLiquidityPoolDoesNotExists | Errors.SwapLiquidityPoolNotEnoughTokens => true
              case _                                                                               => false
            } =>
          true
        case _ => false
      }
      expect(expectedError)
    }
  }

  test("Validate data fail - Not enough tokens") { implicit res =>
    implicit val (h, hs, sp) = res
    val primaryToken =
      TokenInformation(CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some, 1L.toTokenAmountFormat.toPosLongUnsafe)
    val pairToken =
      TokenInformation(CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some, 2L.toTokenAmountFormat.toPosLongUnsafe)
    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")

    val (_, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)
    val stakingUpdate = SwapUpdate(
      CurrencyId(ownerAddress),
      sourceAddress,
      primaryToken.identifier,
      pairToken.identifier,
      Hash.empty,
      SwapAmount(100000L.toTokenAmountFormat.toPosLongUnsafe),
      SwapAmount(100000L.toTokenAmountFormat.toPosLongUnsafe),
      EpochProgress.MinValue,
      SwapReference.empty
    )
    val fakeSignedUpdate = getFakeSignedUpdate(stakingUpdate)

    val validationService = ValidationService.make[IO](config)
    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(
        keyPair,
        SortedMap.empty,
        EpochProgress.MinValue,
        SnapshotOrdinal.MinValue,
        SortedMap.empty,
        EpochProgress.MinValue,
        SnapshotOrdinal.MinValue,
        ownerAddress
      )
      response <- validationService.validateData(NonEmptyList.one(fakeSignedUpdate), state)
    } yield {
      val expectedError = response match {
        case Invalid(errors) if errors.exists {
              case Errors.SwapLiquidityPoolNotEnoughTokens => true
              case _                                       => false
            } =>
          true
        case _ => false
      }
      expect(expectedError)
    }
  }
}
