package com.my.dor_metagraph.shared_data.validations

import cats.data.NonEmptySet
import cats.effect.{IO, Resource}
import cats.syntax.all._

import scala.collection.immutable.SortedMap

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
import io.constellationnetwork.security.hex.Hex
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.signature.signature.{Signature, SignatureProof}

import com.my.dor_metagraph.shared_data.DummyL0Context.buildL0NodeContext
import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.PosLong
import org.amm_metagraph.shared_data.refined.PosLongOps
import org.amm_metagraph.shared_data.types.DataUpdates.WithdrawalUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.Withdrawal.WithdrawalReference
import org.amm_metagraph.shared_data.types.codecs
import org.amm_metagraph.shared_data.validations.Errors.{LiquidityPoolDoesNotExists, WithdrawalAlreadyPending, WithdrawalInsufficientShares}
import org.amm_metagraph.shared_data.validations.WithdrawalValidations
import weaver.MutableIOSuite

object WithdrawalValidationsTest extends MutableIOSuite {
  type Res = (Hasher[IO], codecs.HasherSelector[IO], SecurityProvider[IO])
  val sourceAddress: Address = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")

  override def sharedResource: Resource[IO, Res] = for {
    sp <- SecurityProvider.forAsync[IO]
    implicit0(j: JsonSerializer[IO]) <- JsonSerializer.forSync[IO].asResource
    h = Hasher.forJson[IO]
    hs = codecs.HasherSelector.forSync(h, h)
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

  def getFakeSignedUpdate(
    update: WithdrawalUpdate
  ): Signed[WithdrawalUpdate] =
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
        Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
      )

      withdrawalUpdate = getFakeSignedUpdate(
        WithdrawalUpdate(
          sourceAddress,
          primaryToken.identifier,
          pairToken.identifier,
          ShareAmount(Amount(PosLong.unsafeFrom(toFixedPoint(0.5)))),
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

      result <- WithdrawalValidations.withdrawalValidationsL0[IO](
        withdrawalUpdate,
        ammCalculatedState
      )

    } yield expect(result.isValid)
  }

  test("Validation fails when liquidity pool does not exist") { implicit res =>
    implicit val (h, hs, sp) = res

    val ammCalculatedState = AmmCalculatedState(Map.empty)
    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]

      withdrawalUpdate = getFakeSignedUpdate(
        WithdrawalUpdate(
          sourceAddress,
          Some(CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb"))),
          Some(CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5"))),
          ShareAmount(Amount(PosLong.unsafeFrom(toFixedPoint(0.5)))),
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

      result <- WithdrawalValidations.withdrawalValidationsL0[IO](
        withdrawalUpdate,
        ammCalculatedState
      )

    } yield expect(result.isInvalid) && expect(result.swap.exists(_.head == LiquidityPoolDoesNotExists))
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
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]

      withdrawalUpdate = getFakeSignedUpdate(
        WithdrawalUpdate(
          sourceAddress,
          primaryToken.identifier,
          pairToken.identifier,
          ShareAmount(Amount(PosLong.unsafeFrom(toFixedPoint(2.0)))), // More than owned
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

      result <- WithdrawalValidations.withdrawalValidationsL0[IO](
        withdrawalUpdate,
        ammCalculatedState
      )

    } yield expect(result.isInvalid) && expect(result.swap.exists(_.head == WithdrawalInsufficientShares))
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
        sourceAddress,
        primaryToken.identifier,
        pairToken.identifier,
        ShareAmount(Amount(PosLong.unsafeFrom(toFixedPoint(0.5)))),
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
        Map(
          OperationType.LiquidityPool -> liquidityPoolCalculatedState,
          OperationType.Withdrawal -> WithdrawalCalculatedState.empty.copy(pending = Set(PendingAllowSpend(withdrawalUpdate)))
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

      result <- WithdrawalValidations.withdrawalValidationsL0[IO](
        withdrawalUpdate,
        ammCalculatedState
      )

    } yield expect(result.isInvalid) && expect(result.swap.exists(_.head == WithdrawalAlreadyPending))
  }

}
