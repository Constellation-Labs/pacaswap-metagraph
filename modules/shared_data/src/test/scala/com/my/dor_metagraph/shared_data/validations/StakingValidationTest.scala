package com.my.dor_metagraph.shared_data.validations

import cats.Eq
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, NonEmptySet}
import cats.effect.{IO, Resource}
import cats.syntax.all._
import eu.timepit.refined.auto._
import org.amm_metagraph.shared_data.Utils.{DoubleOps, LongOps, PosLongOps}
import org.amm_metagraph.shared_data.types.DataUpdates.StakingUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool.{LiquidityPool, LiquidityProviders, TokenInformation}
import org.amm_metagraph.shared_data.types.Staking.StakingCalculatedStateAddress
import org.amm_metagraph.shared_data.types.States.{AmmCalculatedState, AmmOnChainState, LiquidityPoolCalculatedState, OperationType, StakingCalculatedState}
import org.amm_metagraph.shared_data.validations.ValidationService
import org.amm_metagraph.shared_data.validations.Errors
import org.tessellation.currency.dataApplication.DataState
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import org.tessellation.schema.ID.Id
import org.tessellation.schema.SnapshotOrdinal
import org.tessellation.schema.address.Address
import org.tessellation.security.SecurityProvider
import org.tessellation.security.hex.Hex
import org.tessellation.security.signature.Signed
import org.tessellation.security.signature.signature.{Signature, SignatureProof}
import weaver.MutableIOSuite

object StakingValidationTest extends MutableIOSuite {
  type Res = SecurityProvider[IO]

  override def sharedResource: Resource[IO, SecurityProvider[IO]] =
    SecurityProvider.forAsync[IO]

  def buildLiquidityPoolCalculatedState(
    tokenA : TokenInformation,
    tokenB : TokenInformation,
    owner  : Address,
    feeRate: Long
  ): (String, LiquidityPoolCalculatedState) = {
    val primaryAddressAsString = tokenA.identifier.fold("")(address => address.value.value)
    val pairAddressAsString = tokenB.identifier.fold("")(address => address.value.value)
    val poolId = s"$primaryAddressAsString-$pairAddressAsString"
    val liquidityPool = LiquidityPool(
      poolId,
      tokenA,
      tokenB,
      owner,
      tokenA.amount.value.fromTokenAmountFormat * tokenB.amount.value.fromTokenAmountFormat,
      (feeRate.toDouble / 100D),
      math.sqrt(tokenA.amount.value.toDouble * tokenB.amount.value.toDouble).toTokenAmountFormat,
      LiquidityProviders(Map(owner -> math.sqrt(tokenA.amount.value.toDouble * tokenB.amount.value.toDouble).toTokenAmountFormat))
    )
    (poolId, LiquidityPoolCalculatedState(Map(poolId -> liquidityPool)))
  }

  def getFakeSignedUpdate(
    update: StakingUpdate
  ): Signed[StakingUpdate] =
    Signed(
      update,
      NonEmptySet.one(
        SignatureProof(
          Id(Hex("db2faf200159ca3c47924bf5f3bda4f45d681a39f9490053ecf98d788122f7a7973693570bd242e10ab670748e86139847eb682a53c7c5c711b832517ce34860")),
          Signature(Hex("3045022100fb26702e976a6569caa3507140756fee96b5ba748719abe1b812b17f7279a3dc0220613db28d5c5a30d7353383358b653aa29772151ccf352a2e67a26a74e49eac57"))
        )
      )
    )

  implicit val eqDataApplicationValidationErrorOr: Eq[DataApplicationValidationErrorOr[Unit]] =
    Eq.fromUniversalEquals

  test("Validate update successfully") { implicit res =>
    val primaryToken = TokenInformation(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb").some, 100L)
    val pairToken = TokenInformation(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5").some, 50L)

    val stakingUpdate = StakingUpdate(
      "fake-primary-txn-1",
      "fake-pair-txn-1",
      primaryToken.identifier,
      100L.toPosLongUnsafe,
      pairToken.identifier
    )
    for {
      validationService <- ValidationService.make[IO]
      response <- validationService.validateUpdate(stakingUpdate)
    } yield
      expect.eql(Valid(()), response)
  }

  test("Validate data successfully") { implicit res =>
    val primaryToken = TokenInformation(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb").some, 100L)
    val pairToken = TokenInformation(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5").some, 50L)
    val ownerAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Ks")

    val (_, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress, 0)
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)
    val stakingUpdate = StakingUpdate(
      "fake-primary-txn-1",
      "fake-pair-txn-1",
      primaryToken.identifier,
      100L.toPosLongUnsafe,
      pairToken.identifier
    )

    val fakeSignedUpdate = getFakeSignedUpdate(stakingUpdate)
    for {
      validationService <- ValidationService.make[IO]
      response <- validationService.validateData(NonEmptyList.one(fakeSignedUpdate), state)
    } yield
      expect.eql(Valid(()), response)
  }

  test("Validate data fails - Liquidity pool does not exist") { implicit res =>
    val primaryToken = TokenInformation(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb").some, 100L)
    val pairToken = TokenInformation(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5").some, 50L)

    val ammOnChainState = AmmOnChainState(List.empty) // No pools initialized
    val ammCalculatedState = AmmCalculatedState(Map.empty)
    val state = DataState(ammOnChainState, ammCalculatedState)

    val stakingUpdate = StakingUpdate(
      "fake-primary-txn-1",
      "fake-pair-txn-1",
      primaryToken.identifier,
      100L.toPosLongUnsafe,
      pairToken.identifier
    )

    val fakeSignedUpdate = getFakeSignedUpdate(stakingUpdate)

    for {
      validationService <- ValidationService.make[IO]
      response <- validationService.validateData(NonEmptyList.one(fakeSignedUpdate), state)
    } yield {
      val expectedError = response match {
        case Invalid(errors) if errors.exists(_.isInstanceOf[Errors.StakingLiquidityPoolDoesNotExists.type]) =>
          true
        case _ =>
          false
      }
      expect(expectedError)
    }
  }

  test("Validate data fails - Txn already exists") { implicit res =>
    val primaryToken = TokenInformation(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb").some, 100L)
    val pairToken = TokenInformation(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5").some, 50L)
    val ownerAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Ks")
    val signerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")

    val (_, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress, 0)
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(
      Map(
        OperationType.LiquidityPool -> liquidityPoolCalculatedState,
        OperationType.Staking -> StakingCalculatedState(Map(signerAddress -> StakingCalculatedStateAddress(
          "fake-primary-txn-1",
          "fake-pair-txn-1",
          primaryToken,
          pairToken,
          SnapshotOrdinal.MinValue,
          none
        )))
      )
    )
    val state = DataState(ammOnChainState, ammCalculatedState)
    val stakingUpdate = StakingUpdate(
      "fake-primary-txn-1",
      "fake-pair-txn-1",
      primaryToken.identifier,
      100L.toPosLongUnsafe,
      pairToken.identifier
    )

    val fakeSignedUpdate = getFakeSignedUpdate(stakingUpdate)
    for {
      validationService <- ValidationService.make[IO]
      response <- validationService.validateData(NonEmptyList.one(fakeSignedUpdate), state)
    } yield {
      val expectedError = response match {
        case Invalid(errors) if errors.exists(_.isInstanceOf[Errors.StakingTransactionAlreadyExists.type]) =>
          true
        case _ =>
          false
      }
      expect(expectedError)
    }
  }

}
