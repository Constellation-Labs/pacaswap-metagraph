package com.my.dor_metagraph.shared_data.validations

import cats.Eq
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, NonEmptySet}
import cats.effect.{IO, Resource}
import cats.syntax.all._
import com.my.dor_metagraph.shared_data.validations.LiquidityPoolValidationTest.expect
import eu.timepit.refined.auto._
import org.amm_metagraph.shared_data.Utils.{DoubleOps, LongOps, PosLongOps}
import org.amm_metagraph.shared_data.types.DataUpdates.SwapUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool.{LiquidityPool, LiquidityProviders, TokenInformation}
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.validations.{Errors, ValidationService}
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

object SwapValidationTest extends MutableIOSuite {
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
    update: SwapUpdate
  ): Signed[SwapUpdate] =
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
    val metagraphAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Gs")

    val stakingUpdate = SwapUpdate(
      primaryToken.identifier,
      pairToken.identifier,
      metagraphAddress,
      0L,
      "",
      "",
      100000L.toPosLongUnsafe,
      100000L.toPosLongUnsafe,
      SnapshotOrdinal.unsafeApply(20),
      none,
      1L.toPosLongUnsafe,
      20000.toPosLongUnsafe
    )

    for {
      validationService <- ValidationService.make[IO]
      response <- validationService.validateUpdate(stakingUpdate)
    } yield
      expect.eql(Valid(()), response)
  }

  test("Validate data successfully") { implicit res =>
    val primaryToken = TokenInformation(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb").some, 1000000L.toTokenAmountFormat.toPosLongUnsafe)
    val pairToken = TokenInformation(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5").some, 2000000L.toTokenAmountFormat.toPosLongUnsafe)
    val ownerAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Ks")
    val metagraphAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Gs")

    val (poolId, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress, 3L)
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)
    val stakingUpdate = SwapUpdate(
      primaryToken.identifier,
      pairToken.identifier,
      metagraphAddress,
      0L,
      "",
      "",
      100000L.toPosLongUnsafe,
      100000L.toPosLongUnsafe,
      SnapshotOrdinal.unsafeApply(20),
      none,
      1L.toPosLongUnsafe,
      20000.toPosLongUnsafe
    )
    val fakeSignedUpdate = getFakeSignedUpdate(stakingUpdate)
    for {
      validationService <- ValidationService.make[IO]
      response <- validationService.validateData(NonEmptyList.one(fakeSignedUpdate), state)
    } yield
      expect.eql(Valid(()), response)
  }

  test("Validate data fail - Liquidity pool does not exists") { implicit res =>
    val primaryToken = TokenInformation(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb").some, 1000000L.toTokenAmountFormat.toPosLongUnsafe)
    val pairToken = TokenInformation(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5").some, 2000000L.toTokenAmountFormat.toPosLongUnsafe)
    val metagraphAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Gs")

    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(Map.empty)
    val state = DataState(ammOnChainState, ammCalculatedState)
    val stakingUpdate = SwapUpdate(
      primaryToken.identifier,
      pairToken.identifier,
      metagraphAddress,
      0L,
      "",
      "",
      100000L.toPosLongUnsafe,
      100000L.toPosLongUnsafe,
      SnapshotOrdinal.unsafeApply(20),
      none,
      1L.toPosLongUnsafe,
      20000.toPosLongUnsafe
    )
    val fakeSignedUpdate = getFakeSignedUpdate(stakingUpdate)
    for {
      validationService <- ValidationService.make[IO]
      response <- validationService.validateData(NonEmptyList.one(fakeSignedUpdate), state)
    } yield {
      val expectedError = response match {
        case Invalid(errors) if errors.exists {
          case Errors.SwapLiquidityPoolDoesNotExists | Errors.SwapLiquidityPoolNotEnoughTokens => true
          case _ => false
        } => true
        case _ => false
      }
      expect(expectedError)
    }
  }

  test("Validate data fail - Not enough tokens") { implicit res =>
    val primaryToken = TokenInformation(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb").some, 1L.toTokenAmountFormat.toPosLongUnsafe)
    val pairToken = TokenInformation(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5").some, 2L.toTokenAmountFormat.toPosLongUnsafe)
    val ownerAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Ks")
    val metagraphAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Gs")

    val (_, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress, 3L)
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)
    val stakingUpdate = SwapUpdate(
      primaryToken.identifier,
      pairToken.identifier,
      metagraphAddress,
      0L,
      "",
      "",
      100000L.toTokenAmountFormat.toPosLongUnsafe,
      100000L.toTokenAmountFormat.toPosLongUnsafe,
      SnapshotOrdinal.unsafeApply(20),
      none,
      1000L.toPosLongUnsafe,
      20000.toPosLongUnsafe
    )
    val fakeSignedUpdate = getFakeSignedUpdate(stakingUpdate)
    for {
      validationService <- ValidationService.make[IO]
      response <- validationService.validateData(NonEmptyList.one(fakeSignedUpdate), state)
    } yield {
      val expectedError = response match {
        case Invalid(errors) if errors.exists {
          case Errors.SwapLiquidityPoolNotEnoughTokens => true
          case _ => false
        } => true
        case _ => false
      }
      expect(expectedError)
    }
  }
}
