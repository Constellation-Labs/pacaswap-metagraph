package org.amm_metagraph.shared_data.validations

import cats.Eq
import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import cats.effect.{IO, Resource}
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}
import scala.concurrent.duration.DurationInt

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

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.{NonNegLong, PosLong}
import eu.timepit.refined.types.numeric._
import org.amm_metagraph.shared_data.DummyL0Context.buildL0NodeContext
import org.amm_metagraph.shared_data.DummyL1Context.buildL1NodeContext
import org.amm_metagraph.shared_data.FeeDistributor
import org.amm_metagraph.shared_data.Shared._
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.app.ApplicationConfig._
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.codecs
import org.amm_metagraph.shared_data.types.codecs.JsonWithBase64BinaryCodec
import org.amm_metagraph.shared_data.validations.Errors._
import org.amm_metagraph.shared_data.validations.SwapValidationTest.expect
import org.amm_metagraph.shared_data.validations._
import weaver.MutableIOSuite

object LiquidityPoolValidationTest extends MutableIOSuite {
  type Res = (Hasher[IO], codecs.HasherSelector[IO], SecurityProvider[IO])

  private val config = ApplicationConfig(
    ExpirationEpochProgresses(
      EpochProgress(NonNegLong.unsafeFrom(30L)),
      EpochProgress(NonNegLong.unsafeFrom(30L))
    ),
    "NodeValidators",
    Dev,
    Governance(
      VotingPowerMultipliers(Seq.empty)
    ),
    Rewards(
      Amount.empty,
      Amount.empty,
      NonNegLong.MinValue,
      NonNegLong.MinValue,
      NonNegLong.MinValue,
      EpochProgress.MinValue,
      Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb"),
      rewardCalculationInterval = NonNegLong(100),
      rewardWithdrawDelay = EpochProgress(NonNegLong(10L)),
      availableRewardsPerSnapshot = NonNegInt(100),
      nodeValidatorConfig = NodeValidatorConfig(Seq.empty)
    ),
    TokenLimits(
      NonNegLong.unsafeFrom((100 * 1e8).toLong),
      NonNegLong.unsafeFrom((9223372036854775000L * 1e8).toLong)
    ),
    EpochProgress(NonNegLong.unsafeFrom(0L)),
    EpochMetadata(43.seconds, 30L),
    TokenLockLimitsConfig(
      PosInt.unsafeFrom(10),
      PosLong.unsafeFrom(toFixedPoint(1000))
    )
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

    for {
      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      liquidityPoolValidations = LiquidityPoolValidations.make[IO](config, jsonBase64BinaryCodec)
      stakingValidations = StakingValidations.make[IO](config, jsonBase64BinaryCodec)
      swapValidations = SwapValidations.make[IO](config, jsonBase64BinaryCodec)
      withdrawalValidations = WithdrawalValidations.make[IO](config, jsonBase64BinaryCodec)
      governanceValidations = GovernanceValidations.make[IO](config, jsonBase64BinaryCodec)
      rewardWithdrawValidations = RewardWithdrawValidations.make[IO](config, jsonBase64BinaryCodec)

      validationService = ValidationService.make[IO](
        config,
        liquidityPoolValidations,
        stakingValidations,
        swapValidations,
        withdrawalValidations,
        governanceValidations,
        rewardWithdrawValidations
      )

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

    for {
      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      liquidityPoolValidations = LiquidityPoolValidations.make[IO](config, jsonBase64BinaryCodec)
      stakingValidations = StakingValidations.make[IO](config, jsonBase64BinaryCodec)
      swapValidations = SwapValidations.make[IO](config, jsonBase64BinaryCodec)
      withdrawalValidations = WithdrawalValidations.make[IO](config, jsonBase64BinaryCodec)
      governanceValidations = GovernanceValidations.make[IO](config, jsonBase64BinaryCodec)
      rewardWithdrawValidations = RewardWithdrawValidations.make[IO](config, jsonBase64BinaryCodec)

      validationService = ValidationService.make[IO](
        config,
        liquidityPoolValidations,
        stakingValidations,
        swapValidations,
        withdrawalValidations,
        governanceValidations,
        rewardWithdrawValidations
      )
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
    val ammOnChainState = AmmOnChainState.empty
    val ammCalculatedState = AmmCalculatedState()
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
      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      liquidityPoolValidations = LiquidityPoolValidations.make[IO](config, jsonBase64BinaryCodec)
      stakingValidations = StakingValidations.make[IO](config, jsonBase64BinaryCodec)
      swapValidations = SwapValidations.make[IO](config, jsonBase64BinaryCodec)
      withdrawalValidations = WithdrawalValidations.make[IO](config, jsonBase64BinaryCodec)
      governanceValidations = GovernanceValidations.make[IO](config, jsonBase64BinaryCodec)
      rewardWithdrawValidations = RewardWithdrawValidations.make[IO](config, jsonBase64BinaryCodec)

      validationService = ValidationService.make[IO](
        config,
        liquidityPoolValidations,
        stakingValidations,
        swapValidations,
        withdrawalValidations,
        governanceValidations,
        rewardWithdrawValidations
      )
      response <- validationService.validateData(NonEmptyList.one(fakeSignedUpdate), state)
    } yield expect.eql(Valid(()), response)
  }

  test("Validate data successfully - L0 token - DAG") { implicit res =>
    implicit val (h, hs, sp) = res

    val primaryToken = TokenInformation(CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some, 100L)
    val pairToken = TokenInformation(none, 50L)
    val ammOnChainState = AmmOnChainState.empty
    val ammCalculatedState = AmmCalculatedState()
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
      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      liquidityPoolValidations = LiquidityPoolValidations.make[IO](config, jsonBase64BinaryCodec)
      stakingValidations = StakingValidations.make[IO](config, jsonBase64BinaryCodec)
      swapValidations = SwapValidations.make[IO](config, jsonBase64BinaryCodec)
      withdrawalValidations = WithdrawalValidations.make[IO](config, jsonBase64BinaryCodec)
      governanceValidations = GovernanceValidations.make[IO](config, jsonBase64BinaryCodec)
      rewardWithdrawValidations = RewardWithdrawValidations.make[IO](config, jsonBase64BinaryCodec)

      validationService = ValidationService.make[IO](
        config,
        liquidityPoolValidations,
        stakingValidations,
        swapValidations,
        withdrawalValidations,
        governanceValidations,
        rewardWithdrawValidations
      )
      response <- validationService.validateData(NonEmptyList.one(fakeSignedUpdate), state)
    } yield expect.eql(Valid(()), response)
  }

  test("Validate data fail - pool already exists") { implicit res =>
    implicit val (h, hs, sp) = res

    val primaryToken = TokenInformation(CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some, 100L)
    val pairToken = TokenInformation(CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some, 50L)
    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")

    val (_, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val ammOnChainState = AmmOnChainState.empty
    val ammCalculatedState = AmmCalculatedState(
      SortedMap(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
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
      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      liquidityPoolValidations = LiquidityPoolValidations.make[IO](config, jsonBase64BinaryCodec)
      response <- liquidityPoolValidations.l0Validations(fakeSignedUpdate, state.calculated, EpochProgress.MaxValue)
    } yield
      response match {
        case Left(value) =>
          value.reason match {
            case InvalidLiquidityPool() => expect(true)
            case _                      => expect(false)
          }
        case Right(_) => expect(false)
      }
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

    for {
      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      liquidityPoolValidations = LiquidityPoolValidations.make[IO](config.copy(environment = Mainnet), jsonBase64BinaryCodec)
      stakingValidations = StakingValidations.make[IO](config, jsonBase64BinaryCodec)
      swapValidations = SwapValidations.make[IO](config, jsonBase64BinaryCodec)
      withdrawalValidations = WithdrawalValidations.make[IO](config, jsonBase64BinaryCodec)
      governanceValidations = GovernanceValidations.make[IO](config, jsonBase64BinaryCodec)
      rewardWithdrawValidations = RewardWithdrawValidations.make[IO](config, jsonBase64BinaryCodec)

      validationService = ValidationService.make[IO](
        config,
        liquidityPoolValidations,
        stakingValidations,
        swapValidations,
        withdrawalValidations,
        governanceValidations,
        rewardWithdrawValidations
      )
      response <- validationService.validateUpdate(liquidityPoolUpdate)(context)
    } yield expect(response == Errors.FeePercentageTotalMustBeGreaterThanZero.invalidNec)
  }

}
