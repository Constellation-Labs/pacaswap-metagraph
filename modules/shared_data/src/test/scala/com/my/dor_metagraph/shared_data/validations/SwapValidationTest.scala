package com.my.dor_metagraph.shared_data.validations

import cats.Eq
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, NonEmptySet}
import cats.effect.{IO, Resource}
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.ext.cats.effect.ResourceIO
import io.constellationnetwork.json.JsonSerializer
import io.constellationnetwork.schema.ID.Id
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
import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.PosLong
import eu.timepit.refined.types.numeric.PosDouble
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.app.ApplicationConfig.{Dev, Governance, VotingWeightMultipliers}
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.types.DataUpdates.SwapUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.validations.{Errors, ValidationService}
import weaver.MutableIOSuite

object SwapValidationTest extends MutableIOSuite {
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
    val poolId = org.amm_metagraph.shared_data.types.LiquidityPool.PoolId(s"$primaryAddressAsString-$pairAddressAsString")
    val liquidityPool = LiquidityPool(
      poolId,
      tokenA,
      tokenB,
      owner,
      tokenA.amount.value.fromTokenAmountFormat * tokenB.amount.value.fromTokenAmountFormat,
      PoolShares(1.toTokenAmountFormat.toPosLongUnsafe, Map(owner -> ShareAmount(Amount(PosLong.unsafeFrom(1e8.toLong)))))
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

  implicit val eqDataApplicationValidationErrorOr: Eq[DataApplicationValidationErrorOr[Unit]] =
    Eq.fromUniversalEquals

  test("Validate update successfully") { implicit res =>
    implicit val (h, sp) = res
    val primaryToken = TokenInformation(CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some, 100L)
    val pairToken = TokenInformation(CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some, 50L)
    val ownerAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Gs")

    val stakingUpdate = SwapUpdate(
      ownerAddress,
      primaryToken.identifier,
      pairToken.identifier,
      Hash.empty,
      SwapAmount(100000L.toTokenAmountFormat.toPosLongUnsafe),
      SwapAmount(100000L.toTokenAmountFormat.toPosLongUnsafe),
      EpochProgress.MinValue,
      none,
      none,
      none
    )

    val config = ApplicationConfig(
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

    for {
      validationService <- ValidationService.make[IO](config)
      response <- validationService.validateUpdate(stakingUpdate)
    } yield expect.eql(Valid(()), response)
  }

  test("Validate data successfully") { implicit res =>
    implicit val (h, sp) = res
    val primaryToken =
      TokenInformation(CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some, 1000000L.toTokenAmountFormat.toPosLongUnsafe)
    val pairToken =
      TokenInformation(CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some, 2000000L.toTokenAmountFormat.toPosLongUnsafe)
    val ownerAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Ks")

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
        ownerAddress,
        primaryToken.identifier,
        pairToken.identifier,
        signedAllowSpend.hash,
        SwapAmount(100000L.toTokenAmountFormat.toPosLongUnsafe),
        SwapAmount(100000L.toTokenAmountFormat.toPosLongUnsafe),
        EpochProgress.MinValue,
        none,
        Some(1000L),
        Some(20000L)
      )
      fakeSignedUpdate = getFakeSignedUpdate(swapUpdate)

      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(
        keyPair,
        SortedMap(
          ownerAddress -> SortedSet(signedAllowSpend.signed)
        )
      )

      config = ApplicationConfig(
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

      validationService <- ValidationService.make[IO](config)
      response <- validationService.validateData(NonEmptyList.one(fakeSignedUpdate), state)
    } yield expect.eql(Valid(()), response)
  }

  test("Validate data fail - Liquidity pool does not exists") { implicit res =>
    implicit val (h, sp) = res
    val primaryToken =
      TokenInformation(CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some, 1000000L.toTokenAmountFormat.toPosLongUnsafe)
    val pairToken =
      TokenInformation(CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some, 2000000L.toTokenAmountFormat.toPosLongUnsafe)
    val ownerAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Gs")

    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(Map.empty)
    val state = DataState(ammOnChainState, ammCalculatedState)
    val stakingUpdate = SwapUpdate(
      ownerAddress,
      primaryToken.identifier,
      pairToken.identifier,
      Hash.empty,
      SwapAmount(100000L.toTokenAmountFormat.toPosLongUnsafe),
      SwapAmount(100000L.toTokenAmountFormat.toPosLongUnsafe),
      EpochProgress.MinValue,
      none,
      none,
      none
    )
    val fakeSignedUpdate = getFakeSignedUpdate(stakingUpdate)
    val config = ApplicationConfig(
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

    for {
      validationService <- ValidationService.make[IO](config)
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(keyPair, SortedMap.empty)
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
    implicit val (h, sp) = res
    val primaryToken =
      TokenInformation(CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some, 1L.toTokenAmountFormat.toPosLongUnsafe)
    val pairToken =
      TokenInformation(CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some, 2L.toTokenAmountFormat.toPosLongUnsafe)
    val ownerAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Ks")

    val (_, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)
    val stakingUpdate = SwapUpdate(
      ownerAddress,
      primaryToken.identifier,
      pairToken.identifier,
      Hash.empty,
      SwapAmount(100000L.toTokenAmountFormat.toPosLongUnsafe),
      SwapAmount(100000L.toTokenAmountFormat.toPosLongUnsafe),
      EpochProgress.MinValue,
      none,
      Some(1000L),
      Some(20000L)
    )
    val fakeSignedUpdate = getFakeSignedUpdate(stakingUpdate)
    val config = ApplicationConfig(
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

    for {
      validationService <- ValidationService.make[IO](config)
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(keyPair, SortedMap.empty)
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
