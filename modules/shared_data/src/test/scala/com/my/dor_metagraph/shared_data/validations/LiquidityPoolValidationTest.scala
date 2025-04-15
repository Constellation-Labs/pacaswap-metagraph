package com.my.dor_metagraph.shared_data.validations

import cats.Eq
import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import cats.effect.{IO, Resource}
import cats.syntax.all._

import scala.collection.immutable.SortedMap

import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext, L1NodeContext}
import io.constellationnetwork.ext.cats.effect.ResourceIO
import io.constellationnetwork.json.JsonSerializer
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.CurrencyId
import io.constellationnetwork.security.hash.Hash
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
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.codecs
import org.amm_metagraph.shared_data.validations.{Errors, ValidationService}
import weaver.MutableIOSuite

object LiquidityPoolValidationTest extends MutableIOSuite {
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

  implicit val eqDataApplicationValidationErrorOr: Eq[DataApplicationValidationErrorOr[Unit]] =
    Eq.fromUniversalEquals

  test("Validate update successfully") { implicit res =>
    implicit val (h, hs, sp) = res

    val primaryToken = TokenInformation(CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some, 100L)
    val pairToken = TokenInformation(CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some, 50L)
    val context: L1NodeContext[IO] = buildL1NodeContext(
      ammMetagraphId
    )

    val liquidityPoolUpdate = LiquidityPoolUpdate(
      ammMetagraphIdAsCurrencyId,
      sourceAddress,
      Hash.empty,
      Hash.empty,
      primaryToken.identifier,
      pairToken.identifier,
      primaryToken.amount,
      pairToken.amount,
      EpochProgress.MaxValue,
      None
    )

    val validationService = ValidationService.make[IO](config)
    for {
      response <- validationService.validateUpdate(liquidityPoolUpdate)(context)
    } yield expect.eql(Valid(()), response)
  }

  test("Validate update fail - both tokens none") { implicit res =>
    implicit val (h, hs, sp) = res

    val primaryToken = TokenInformation(none, 100L)
    val pairToken = TokenInformation(none, 50L)
    val context: L1NodeContext[IO] = buildL1NodeContext(
      ammMetagraphId
    )

    val liquidityPoolUpdate = LiquidityPoolUpdate(
      ammMetagraphIdAsCurrencyId,
      sourceAddress,
      Hash.empty,
      Hash.empty,
      primaryToken.identifier,
      pairToken.identifier,
      primaryToken.amount,
      pairToken.amount,
      EpochProgress.MaxValue,
      None
    )

    val validationService = ValidationService.make[IO](config)
    for {
      response <- validationService.validateUpdate(liquidityPoolUpdate)(context)
    } yield {
      val expectedError = response match {
        case Invalid(errors) if errors.exists(_.isInstanceOf[Errors.LiquidityPoolNotEnoughInformation.type]) =>
          true
        case _ =>
          false
      }
      expect(expectedError)
    }
  }

  test("Validate data successfully - L0 token - L0 token") { implicit res =>
    implicit val (h, hs, sp) = res

    val primaryToken = TokenInformation(CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some, 100L)
    val pairToken = TokenInformation(CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some, 50L)
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(Map.empty)
    val state = DataState(ammOnChainState, ammCalculatedState)
    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")

    val liquidityPoolUpdate = LiquidityPoolUpdate(
      CurrencyId(ownerAddress),
      sourceAddress,
      Hash.empty,
      Hash.empty,
      primaryToken.identifier,
      pairToken.identifier,
      primaryToken.amount,
      pairToken.amount,
      EpochProgress.MaxValue,
      None
    )

    val fakeSignedUpdate = getFakeSignedUpdate(liquidityPoolUpdate)

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
    } yield expect.eql(Valid(()), response)
  }

  test("Validate data successfully - L0 token - DAG") { implicit res =>
    implicit val (h, hs, sp) = res

    val primaryToken = TokenInformation(CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some, 100L)
    val pairToken = TokenInformation(none, 50L)
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(Map.empty)
    val state = DataState(ammOnChainState, ammCalculatedState)
    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")

    val liquidityPoolUpdate = LiquidityPoolUpdate(
      CurrencyId(ownerAddress),
      sourceAddress,
      Hash.empty,
      Hash.empty,
      primaryToken.identifier,
      pairToken.identifier,
      primaryToken.amount,
      pairToken.amount,
      EpochProgress.MaxValue,
      None
    )

    val fakeSignedUpdate = getFakeSignedUpdate(liquidityPoolUpdate)

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
    } yield expect.eql(Valid(()), response)
  }

  test("Validate data fail - both tokens none") { implicit res =>
    implicit val (h, hs, sp) = res

    val primaryToken = TokenInformation(none, 100L)
    val pairToken = TokenInformation(none, 50L)
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(Map.empty)
    val state = DataState(ammOnChainState, ammCalculatedState)
    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")

    val liquidityPoolUpdate = LiquidityPoolUpdate(
      CurrencyId(ownerAddress),
      sourceAddress,
      Hash.empty,
      Hash.empty,
      primaryToken.identifier,
      pairToken.identifier,
      primaryToken.amount,
      pairToken.amount,
      EpochProgress.MaxValue,
      None
    )

    val fakeSignedUpdate = getFakeSignedUpdate(liquidityPoolUpdate)

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
        case Invalid(errors) if errors.exists(_.isInstanceOf[Errors.LiquidityPoolNotEnoughInformation.type]) =>
          true
        case _ =>
          false
      }
      expect(expectedError)
    }
  }
  test("Validate data fail - pool already exists") { implicit res =>
    implicit val (h, hs, sp) = res

    val primaryToken = TokenInformation(CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some, 100L)
    val pairToken = TokenInformation(CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some, 50L)
    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")

    val (_, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    val liquidityPoolUpdate = LiquidityPoolUpdate(
      CurrencyId(ownerAddress),
      sourceAddress,
      Hash.empty,
      Hash.empty,
      primaryToken.identifier,
      pairToken.identifier,
      primaryToken.amount,
      pairToken.amount,
      EpochProgress.MaxValue,
      None
    )

    val fakeSignedUpdate = getFakeSignedUpdate(liquidityPoolUpdate)

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
    } yield expect(response == Errors.LiquidityPoolAlreadyExists.invalidNec)
  }

  test("Validate update fail - fees as 0 on mainnet environment") { implicit res =>
    implicit val (h, hs, sp) = res

    val primaryToken = TokenInformation(CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some, 100L)
    val pairToken = TokenInformation(CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some, 50L)
    val context: L1NodeContext[IO] = buildL1NodeContext(
      ammMetagraphId
    )

    val liquidityPoolUpdate = LiquidityPoolUpdate(
      ammMetagraphIdAsCurrencyId,
      sourceAddress,
      Hash.empty,
      Hash.empty,
      primaryToken.identifier,
      pairToken.identifier,
      primaryToken.amount,
      pairToken.amount,
      EpochProgress.MaxValue,
      FeeDistributor.empty.some
    )

    val validationService = ValidationService.make[IO](config.copy(environment = Mainnet))
    for {
      response <- validationService.validateUpdate(liquidityPoolUpdate)(context)
    } yield expect(response == Errors.FeePercentageTotalMustBeGreaterThanZero.invalidNec)
  }

  test("Validate data fail - fees as 0 on mainnet environment") { implicit res =>
    implicit val (h, hs, sp) = res

    val primaryToken = TokenInformation(CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some, 100L)
    val pairToken = TokenInformation(CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some, 50L)
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(Map.empty)
    val state = DataState(ammOnChainState, ammCalculatedState)
    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")

    val liquidityPoolUpdate = LiquidityPoolUpdate(
      CurrencyId(ownerAddress),
      sourceAddress,
      Hash.empty,
      Hash.empty,
      primaryToken.identifier,
      pairToken.identifier,
      primaryToken.amount,
      pairToken.amount,
      EpochProgress.MaxValue,
      FeeDistributor.empty.some
    )

    val fakeSignedUpdate = getFakeSignedUpdate(liquidityPoolUpdate)

    val validationService = ValidationService.make[IO](config.copy(environment = Mainnet))
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
    } yield expect(response == Errors.FeePercentageTotalMustBeGreaterThanZero.invalidNec)
  }

}
