package org.amm_metagraph.shared_data.validations

import cats.Eq
import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import cats.effect.{IO, Resource}
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}
import scala.concurrent.duration.DurationInt
import scala.tools.nsc.tasty.SafeEq

import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext, L1NodeContext}
import io.constellationnetwork.ext.cats.effect.ResourceIO
import io.constellationnetwork.json.JsonSerializer
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap._
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hasher, KeyPairGenerator, SecurityProvider}

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.{NonNegLong, PosLong}
import eu.timepit.refined.types.numeric.PosDouble
import org.amm_metagraph.shared_data.DummyL0Context.buildL0NodeContext
import org.amm_metagraph.shared_data.DummyL1Context.buildL1NodeContext
import org.amm_metagraph.shared_data.Shared._
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.app.ApplicationConfig._
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, StakingUpdate}
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.Staking._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.codecs
import org.amm_metagraph.shared_data.types.codecs.JsonWithBase64BinaryCodec
import org.amm_metagraph.shared_data.validations.Errors._
import org.amm_metagraph.shared_data.validations.LiquidityPoolValidationTest.config
import org.amm_metagraph.shared_data.validations._
import weaver.MutableIOSuite

object StakingValidationTest extends MutableIOSuite {
  type Res = (Hasher[IO], codecs.HasherSelector[IO], SecurityProvider[IO])

  private val config = ApplicationConfig(
    ExpirationEpochProgresses(
      EpochProgress(NonNegLong.unsafeFrom(30L)),
      EpochProgress(NonNegLong.unsafeFrom(30L))
    ),
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
      Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb"),
      rewardCalculationInterval = NonNegLong(100),
      rewardWithdrawDelay = EpochProgress(NonNegLong(10L))
    ),
    TokenLimits(
      NonNegLong.unsafeFrom((100 * 1e8).toLong),
      NonNegLong.unsafeFrom((9223372036854775000L * 1e8).toLong)
    ),
    EpochProgress(NonNegLong.unsafeFrom(0L)),
    EpochMetadata(43.seconds, 30L)
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

    val stakingUpdate = StakingUpdate(
      ammMetagraphIdAsCurrencyId,
      sourceAddress,
      Hash.empty,
      Hash.empty,
      primaryToken.identifier,
      100L.toPosLongUnsafe,
      pairToken.identifier,
      StakingReference.empty,
      EpochProgress.MaxValue
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
      response <- validationService.validateUpdate(stakingUpdate)(context)
    } yield expect.eql(Valid(()), response)
  }

  test("Validate data successfully") { implicit res =>
    implicit val (h, hs, sp) = res

    val primaryToken = TokenInformation(CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some, 100L)
    val pairToken = TokenInformation(CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some, 50L)
    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")

    val (_, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val ammOnChainState = AmmOnChainState(SortedSet.empty, None)
    val ammCalculatedState = AmmCalculatedState(
      SortedMap(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      allowSpendTokenA = AllowSpend(
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc"),
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb"),
        primaryToken.identifier,
        SwapAmount(PosLong.MinValue),
        AllowSpendFee(PosLong.MinValue),
        AllowSpendReference(AllowSpendOrdinal.first, Hash.empty),
        EpochProgress.MaxValue,
        List.empty
      )
      allowSpendTokenB = AllowSpend(
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc"),
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb"),
        pairToken.identifier,
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

      stakingUpdate = StakingUpdate(
        CurrencyId(ownerAddress),
        sourceAddress,
        signedAllowSpendA.hash,
        signedAllowSpendB.hash,
        primaryToken.identifier,
        100L.toPosLongUnsafe,
        pairToken.identifier,
        StakingReference.empty,
        EpochProgress.MaxValue
      )

      fakeSignedUpdate = getFakeSignedUpdate(stakingUpdate)
      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(
        keyPair,
        SortedMap(
          primaryToken.identifier.get.value.some ->
            SortedMap(
              Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc") -> SortedSet(signedAllowSpendA.signed)
            ),
          pairToken.identifier.get.value.some ->
            SortedMap(
              Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMc") -> SortedSet(signedAllowSpendB.signed)
            )
        ),
        EpochProgress.MinValue,
        SnapshotOrdinal.MinValue,
        SortedMap.empty,
        EpochProgress.MinValue,
        SnapshotOrdinal.MinValue,
        ownerAddress
      )

      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      stakingValidations = StakingValidations.make[IO](config, jsonBase64BinaryCodec)

      response <- stakingValidations.l0Validations(fakeSignedUpdate, state.calculated, EpochProgress.MaxValue)
    } yield
      response match {
        case Left(_)      => expect(false)
        case Right(value) => expect(true)
      }
  }

  test("Validate data fails - Liquidity pool does not exist") { implicit res =>
    implicit val (h, hs, sp) = res

    val primaryToken = TokenInformation(CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some, 100L)
    val pairToken = TokenInformation(CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some, 50L)

    val ammOnChainState = AmmOnChainState(SortedSet.empty, None) // No pools initialized
    val ammCalculatedState = AmmCalculatedState()
    val state = DataState(ammOnChainState, ammCalculatedState)
    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")
    val stakingUpdate = StakingUpdate(
      ammMetagraphIdAsCurrencyId,
      sourceAddress,
      Hash.empty,
      Hash.empty,
      primaryToken.identifier,
      100L.toPosLongUnsafe,
      pairToken.identifier,
      StakingReference.empty,
      EpochProgress.MaxValue
    )

    val fakeSignedUpdate = getFakeSignedUpdate(stakingUpdate)

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
      stakingValidations = StakingValidations.make[IO](config, jsonBase64BinaryCodec)
      response <- stakingValidations.l0Validations(fakeSignedUpdate, state.calculated, EpochProgress.MaxValue)
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

  test("Validate data fails - Txn already exists") { implicit res =>
    implicit val (h, hs, sp) = res
    val primaryToken = TokenInformation(CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some, 100L)
    val pairToken = TokenInformation(CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some, 50L)
    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")
    val signerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")

    val (_, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val ammOnChainState = AmmOnChainState(SortedSet.empty, None)
    val ammCalculatedState = AmmCalculatedState(
      SortedMap(
        OperationType.LiquidityPool -> liquidityPoolCalculatedState,
        OperationType.Staking -> StakingCalculatedState(
          ConfirmedStakingCalculatedState(
            SortedMap(
              signerAddress ->
                StakingCalculatedStateInfo(
                  StakingReference.empty,
                  SortedSet(
                    StakingCalculatedStateValue(
                      EpochProgress.MaxValue,
                      StakingCalculatedStateAddress(
                        ownerAddress,
                        Hash.empty,
                        Hash("allowSpendA"),
                        Hash("allowSpendB"),
                        primaryToken,
                        pairToken,
                        StakingReference.empty
                      )
                    )
                  )
                )
            )
          ),
          SortedSet.empty,
          SortedSet.empty
        )
      )
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    val stakingUpdate = StakingUpdate(
      ammMetagraphIdAsCurrencyId,
      sourceAddress,
      Hash("allowSpendA"),
      Hash("allowSpendB"),
      primaryToken.identifier,
      100L.toPosLongUnsafe,
      pairToken.identifier,
      StakingReference.empty,
      EpochProgress.MaxValue
    )

    val fakeSignedUpdate = getFakeSignedUpdate(stakingUpdate)

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
      stakingValidations = StakingValidations.make[IO](config, jsonBase64BinaryCodec)
      response <- stakingValidations.l0Validations(fakeSignedUpdate, state.calculated, EpochProgress.MaxValue)
    } yield
      response match {
        case Left(value) =>
          value.reason match {
            case TransactionAlreadyExists(_) => expect(true)
            case _                           => expect(false)
          }
        case Right(_) => expect(false)
      }
  }

  test("Validate update fail - different AMM metagraph ID provided") { implicit res =>
    implicit val (h, hs, sp) = res

    val primaryToken = TokenInformation(CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some, 100L)
    val pairToken = TokenInformation(CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some, 50L)

    val context: L1NodeContext[IO] = buildL1NodeContext(
      ammMetagraphId
    )

    val stakingUpdate = StakingUpdate(
      primaryToken.identifier.get,
      sourceAddress,
      Hash.empty,
      Hash.empty,
      primaryToken.identifier,
      100L.toPosLongUnsafe,
      pairToken.identifier,
      StakingReference.empty,
      EpochProgress.MaxValue
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
      response <- validationService.validateUpdate(stakingUpdate)(context)
    } yield {
      val expectedError = response match {
        case Invalid(errors) if errors.exists(_.isInstanceOf[Errors.InvalidAMMMetagraphId.type]) =>
          true
        case _ =>
          false
      }
      expect(expectedError)
    }
  }

}
