package org.amm_metagraph.shared_data.validations

import cats.effect.{IO, Resource}
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.currency.dataApplication.L0NodeContext
import io.constellationnetwork.ext.cats.effect.ResourceIO
import io.constellationnetwork.json.JsonSerializer
import io.constellationnetwork.schema.ID.Id
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.CurrencyId
import io.constellationnetwork.security._
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.hex.Hex

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.PosLong
import org.amm_metagraph.shared_data.DummyL0Context.buildL0NodeContext
import org.amm_metagraph.shared_data.Shared._
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, WithdrawalUpdate}
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.Withdrawal.WithdrawalReference
import org.amm_metagraph.shared_data.types.codecs
import org.amm_metagraph.shared_data.types.codecs.JsonWithBase64BinaryCodec
import org.amm_metagraph.shared_data.validations.Errors._
import org.amm_metagraph.shared_data.validations.SwapValidationTest.expect
import org.amm_metagraph.shared_data.validations.WithdrawalValidations
import weaver.MutableIOSuite

object WithdrawalValidationsTest extends MutableIOSuite {
  type Res = (Hasher[IO], codecs.HasherSelector[IO], SecurityProvider[IO])

  override def sharedResource: Resource[IO, Res] = for {
    sp <- SecurityProvider.forAsync[IO]
    implicit0(j: JsonSerializer[IO]) <- JsonSerializer.forSync[IO].asResource
    h = Hasher.forJson[IO]
    hs = codecs.HasherSelector.forSync(h, h)
  } yield (h, hs, sp)

  test("Validation passes for valid withdrawal request") { implicit res =>
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

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]

      signerAddress <- Id(
        Hex(
          "db2faf200159ca3c47924bf5f3bda4f45d681a39f9490053ecf98d788122f7a7973693570bd242e10ab670748e86139847eb682a53c7c5c711b832517ce34860"
        )
      ).toAddress[IO]

      liquidityPoolCalculatedState = {
        val (_, state) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, signerAddress)
        state
      }

      ammCalculatedState = AmmCalculatedState(
        SortedMap(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
      )

      withdrawalUpdate = getFakeSignedUpdate(
        WithdrawalUpdate(
          CurrencyId(ownerAddress),
          sourceAddress,
          primaryToken.identifier,
          pairToken.identifier,
          ShareAmount(Amount(PosLong.unsafeFrom(toFixedPoint(0.5)))),
          none,
          none,
          none,
          none,
          WithdrawalReference.empty,
          EpochProgress.MaxValue
        )
      )

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

      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      withdrawalValidations = WithdrawalValidations.make[IO](config, jsonBase64BinaryCodec)
      result <- withdrawalValidations.l0Validations(
        withdrawalUpdate,
        ammCalculatedState,
        EpochProgress.MaxValue
      )

    } yield
      result match {
        case Left(_)  => expect(false)
        case Right(_) => expect(true)
      }
  }

  test("Validation fails when liquidity pool does not exist") { implicit res =>
    implicit val (h, hs, sp) = res

    val ammCalculatedState = AmmCalculatedState()
    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]

      withdrawalUpdate = getFakeSignedUpdate(
        WithdrawalUpdate(
          CurrencyId(ownerAddress),
          sourceAddress,
          Some(CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb"))),
          Some(CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5"))),
          ShareAmount(Amount(PosLong.unsafeFrom(toFixedPoint(0.5)))),
          none,
          none,
          none,
          none,
          WithdrawalReference.empty,
          EpochProgress.MaxValue
        )
      )

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

      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      withdrawalValidations = WithdrawalValidations.make[IO](config, jsonBase64BinaryCodec)
      result <- withdrawalValidations.l0Validations(
        withdrawalUpdate,
        ammCalculatedState,
        EpochProgress.MaxValue
      )

    } yield
      result match {
        case Left(value) =>
          value.reason match {
            case InvalidLiquidityPool() => expect(true)
            case _                      => expect(false)
          }
        case Right(_) => expect(false)
      }
  }

  test("Validation fails when trying to withdraw more shares than owned") { implicit res =>
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
    val ammCalculatedState = AmmCalculatedState(
      SortedMap(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]

      withdrawalUpdate = getFakeSignedUpdate(
        WithdrawalUpdate(
          CurrencyId(ownerAddress),
          sourceAddress,
          primaryToken.identifier,
          pairToken.identifier,
          ShareAmount(Amount(PosLong.unsafeFrom(toFixedPoint(2.0)))), // More than owned
          none,
          none,
          none,
          none,
          WithdrawalReference.empty,
          EpochProgress.MaxValue
        )
      )

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

      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      withdrawalValidations = WithdrawalValidations.make[IO](config, jsonBase64BinaryCodec)
      result <- withdrawalValidations.l0Validations(
        withdrawalUpdate,
        ammCalculatedState,
        EpochProgress.MaxValue
      )

    } yield
      result match {
        case Left(value) =>
          value.reason match {
            case NotEnoughShares() => expect(true)
            case _                 => expect(false)
          }
        case Right(_) => expect(false)
      }
  }

  test("Validation fails when withdrawal is for all LP shares") { implicit res =>
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
    val ammCalculatedState = AmmCalculatedState(
      SortedMap(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]

      withdrawalUpdate = getFakeSignedUpdate(
        WithdrawalUpdate(
          CurrencyId(ownerAddress),
          ownerAddress,
          primaryToken.identifier,
          pairToken.identifier,
          ShareAmount(Amount(PosLong.unsafeFrom(toFixedPoint(1.0)))),
          none,
          none,
          none,
          none,
          WithdrawalReference.empty,
          EpochProgress.MaxValue
        )
      )

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

      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      withdrawalValidations = WithdrawalValidations.make[IO](config, jsonBase64BinaryCodec)
      result <- withdrawalValidations.l0Validations(
        withdrawalUpdate,
        ammCalculatedState,
        EpochProgress.MaxValue
      )

    } yield
      result match {
        case Left(value) =>
          value.reason match {
            case WithdrawalAllLPSharesError() => expect(true)
            case _                            => expect(false)
          }
        case Right(_) => expect(false)
      }
  }

  test("Validation fails when withdrawal is already pending") { implicit res =>
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

    val withdrawalUpdate = getFakeSignedUpdate(
      WithdrawalUpdate(
        CurrencyId(ownerAddress),
        sourceAddress,
        primaryToken.identifier,
        pairToken.identifier,
        ShareAmount(Amount(PosLong.unsafeFrom(toFixedPoint(0.5)))),
        none,
        none,
        none,
        none,
        WithdrawalReference.empty,
        EpochProgress.MaxValue
      )
    )

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]

      signerAddress <- Id(
        Hex(
          "db2faf200159ca3c47924bf5f3bda4f45d681a39f9490053ecf98d788122f7a7973693570bd242e10ab670748e86139847eb682a53c7c5c711b832517ce34860"
        )
      ).toAddress[IO]

      (_, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, signerAddress)

      ammCalculatedState = AmmCalculatedState(
        SortedMap(
          OperationType.LiquidityPool -> liquidityPoolCalculatedState,
          OperationType.Withdrawal -> WithdrawalCalculatedState.empty.copy(pending =
            SortedSet(PendingAllowSpend(withdrawalUpdate, Hash.empty))
          )
        )
      )

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

      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      withdrawalValidations = WithdrawalValidations.make[IO](config, jsonBase64BinaryCodec)
      result <- withdrawalValidations.l0Validations(
        withdrawalUpdate,
        ammCalculatedState,
        EpochProgress.MaxValue
      )

    } yield
      result match {
        case Left(value) =>
          value.reason match {
            case WithdrawalNotPendingError() => expect(true)
            case _                           => expect(false)
          }
        case Right(_) => expect(false)
      }
  }

}
