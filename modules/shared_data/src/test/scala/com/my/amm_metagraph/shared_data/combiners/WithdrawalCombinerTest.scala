package com.my.amm_metagraph.shared_data.combiners

import cats.effect.{IO, Resource}
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}
import scala.concurrent.duration.DurationInt

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.ext.cats.effect.ResourceIO
import io.constellationnetwork.json.JsonSerializer
import io.constellationnetwork.schema._
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact.SpendAction
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap._
import io.constellationnetwork.security._

import com.my.amm_metagraph.shared_data.DummyL0Context.buildL0NodeContext
import com.my.amm_metagraph.shared_data.Shared._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.{NonNegLong, PosDouble, PosLong}
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.app.ApplicationConfig._
import org.amm_metagraph.shared_data.calculated_state.CalculatedStateService
import org.amm_metagraph.shared_data.services.combiners.WithdrawalCombinerService
import org.amm_metagraph.shared_data.services.pricing.PricingService
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, WithdrawalUpdate}
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States.OperationType.Withdrawal
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.Withdrawal.{WithdrawalReference, getPendingSpendActionWithdrawalUpdates}
import org.amm_metagraph.shared_data.types.codecs.{HasherSelector, JsonWithBase64BinaryCodec}
import org.amm_metagraph.shared_data.validations.Errors._
import org.amm_metagraph.shared_data.validations.WithdrawalValidations
import weaver.MutableIOSuite

object WithdrawalCombinerTest extends MutableIOSuite {
  type Res = (Hasher[IO], SecurityProvider[IO], HasherSelector[IO])

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
      NonNegLong.unsafeFrom((1 * 1e8).toLong),
      NonNegLong.unsafeFrom((9223372036854775000L * 1e8).toLong)
    ),
    EpochProgress(NonNegLong.unsafeFrom(0L)),
    EpochMetadata(43.seconds)
  )

  override def sharedResource: Resource[IO, Res] = for {
    sp <- SecurityProvider.forAsync[IO]
    implicit0(j: JsonSerializer[IO]) <- JsonSerializer.forSync[IO].asResource
    h = Hasher.forJson[IO]
    hs = HasherSelector.forSync(h, h)
  } yield (h, sp, hs)

  test("Test successful withdrawal - single provider") { implicit res =>
    implicit val (h, sp, hs) = res

    // 100.0 tokens = 10000000000 in fixed-point
    val primaryToken = TokenInformation(
      CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some,
      PosLong.unsafeFrom(toFixedPoint(100.0))
    )
    // 50.0 tokens = 5000000000 in fixed-point
    val pairToken = TokenInformation(
      CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some,
      PosLong.unsafeFrom(toFixedPoint(50.0))
    )
    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")

    val (poolId, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val ammOnChainState = AmmOnChainState(Set.empty, None)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]

      // Withdraw 0.5 shares = 50000000 in fixed-point
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

      reference <- WithdrawalReference.of(withdrawalUpdate)

      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(
        keyPair,
        SortedMap.empty,
        EpochProgress.MaxValue,
        SnapshotOrdinal.MinValue,
        SortedMap.empty,
        EpochProgress.MaxValue,
        SnapshotOrdinal.MinValue,
        ownerAddress
      )
      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService = PricingService.make[IO](config, calculatedStateService)

      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      withdrawalValidations = WithdrawalValidations.make[IO](config)
      withdrawalCombinerService = WithdrawalCombinerService.make[IO](config, pricingService, withdrawalValidations, jsonBase64BinaryCodec)

      withdrawalResponsePendingSpendActionResponse <- withdrawalCombinerService.combineNew(
        withdrawalUpdate,
        state,
        EpochProgress.MinValue,
        SortedMap.empty,
        CurrencyId(ownerAddress)
      )

      spendActions = withdrawalResponsePendingSpendActionResponse.sharedArtifacts.map(_.asInstanceOf[SpendAction]).toList
      pending = withdrawalResponsePendingSpendActionResponse.calculated.operations(OperationType.Withdrawal).pending.head
      pendingActions = getPendingSpendActionWithdrawalUpdates(withdrawalResponsePendingSpendActionResponse.calculated)

      withdrawalResponseConfirmedResponse <- withdrawalCombinerService.combinePendingSpendAction(
        PendingSpendAction(withdrawalUpdate, pendingActions.head.updateHash, spendActions.head, pending.pricingTokenInfo),
        withdrawalResponsePendingSpendActionResponse.copy(sharedArtifacts = SortedSet.empty),
        EpochProgress.MinValue,
        spendActions,
        SnapshotOrdinal.MinValue
      )

      updatedLiquidityPool = withdrawalResponseConfirmedResponse.calculated
        .operations(OperationType.LiquidityPool)
        .asInstanceOf[LiquidityPoolCalculatedState]
        .confirmed
        .value(poolId)

      withdrawalSpendTransactions = withdrawalResponsePendingSpendActionResponse.sharedArtifacts.collect {
        case action: artifact.SpendAction => action
      }.flatMap(_.spendTransactions.toList)

      withdrawalCalculatedState = withdrawalResponseConfirmedResponse.calculated.operations
        .getOrElse(Withdrawal, WithdrawalCalculatedState.empty)
        .asInstanceOf[WithdrawalCalculatedState]

    } yield
      expect.all(
        withdrawalCalculatedState.pending.isEmpty,
        withdrawalCalculatedState.failed.isEmpty,
        withdrawalCalculatedState.confirmed.value.map { case (_, s) => s }.exists(_.values.head.value.parent === reference),
        withdrawalResponseConfirmedResponse.sharedArtifacts.isEmpty,
        // Should have 50.0 tokens remaining = 5000000000
        updatedLiquidityPool.tokenA.amount.value === toFixedPoint(50.0),
        // Should have 25.0 tokens remaining = 2500000000
        updatedLiquidityPool.tokenB.amount.value === toFixedPoint(25.0),
        // Should have 0.5 shares remaining = 50000000
        updatedLiquidityPool.poolShares.totalShares.value === toFixedPoint(0.5),
        updatedLiquidityPool.poolShares.addressShares(ownerAddress).value.value.value === toFixedPoint(0.5),
        withdrawalSpendTransactions.size === 2
      )
  }

  test("Test successful withdrawal - multiple providers") { implicit res =>
    implicit val (h, sp, hs) = res

    val primaryToken = TokenInformation(
      CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some,
      PosLong.unsafeFrom(toFixedPoint(100.0))
    )
    val pairToken = TokenInformation(
      CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some,
      PosLong.unsafeFrom(toFixedPoint(50.0))
    )
    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")
    val secondProviderAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Kt")

    val (poolId, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(
      primaryToken,
      pairToken,
      ownerAddress,
      Some(secondProviderAddress -> ShareAmount(Amount(PosLong.unsafeFrom(toFixedPoint(1.0)))))
    )

    val ammOnChainState = AmmOnChainState(Set.empty, None)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]

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

      reference <- WithdrawalReference.of(withdrawalUpdate)

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

      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService = PricingService.make[IO](config, calculatedStateService)

      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      withdrawalValidations = WithdrawalValidations.make[IO](config)
      withdrawalCombinerService = WithdrawalCombinerService.make[IO](config, pricingService, withdrawalValidations, jsonBase64BinaryCodec)

      withdrawalResponsePendingSpendActionResponse <- withdrawalCombinerService.combineNew(
        withdrawalUpdate,
        state,
        EpochProgress.MinValue,
        SortedMap.empty,
        CurrencyId(ownerAddress)
      )

      spendActions = withdrawalResponsePendingSpendActionResponse.sharedArtifacts.map(_.asInstanceOf[SpendAction]).toList
      pending = withdrawalResponsePendingSpendActionResponse.calculated.operations(OperationType.Withdrawal).pending.head
      pendingActions = getPendingSpendActionWithdrawalUpdates(withdrawalResponsePendingSpendActionResponse.calculated)

      withdrawalResponseConfirmedResponse <- withdrawalCombinerService.combinePendingSpendAction(
        PendingSpendAction(withdrawalUpdate, pendingActions.head.updateHash, spendActions.head, pending.pricingTokenInfo),
        withdrawalResponsePendingSpendActionResponse.copy(sharedArtifacts = SortedSet.empty),
        EpochProgress.MinValue,
        spendActions,
        SnapshotOrdinal.MinValue
      )

      updatedLiquidityPool = withdrawalResponseConfirmedResponse.calculated
        .operations(OperationType.LiquidityPool)
        .asInstanceOf[LiquidityPoolCalculatedState]
        .confirmed
        .value(poolId)

      withdrawalCalculatedState = withdrawalResponseConfirmedResponse.calculated.operations
        .getOrElse(Withdrawal, WithdrawalCalculatedState.empty)
        .asInstanceOf[WithdrawalCalculatedState]

    } yield
      expect.all(
        withdrawalCalculatedState.pending.isEmpty,
        withdrawalCalculatedState.failed.isEmpty,
        withdrawalCalculatedState.confirmed.value.map { case (_, s) => s }.exists(_.values.head.value.parent === reference),
        withdrawalResponseConfirmedResponse.sharedArtifacts.isEmpty,
        updatedLiquidityPool.poolShares.addressShares.size === 2,
        updatedLiquidityPool.poolShares.addressShares(ownerAddress).value.value.value === toFixedPoint(0.5),
        updatedLiquidityPool.poolShares.addressShares(secondProviderAddress).value.value.value === toFixedPoint(1.0)
      )
  }

  test("Test withdrawal fails when liquidity pool does not exist") { implicit res =>
    implicit val (h, sp, hs) = res

    val ammOnChainState = AmmOnChainState(Set.empty, None)
    val ammCalculatedState = AmmCalculatedState(Map.empty)
    val state = DataState(ammOnChainState, ammCalculatedState)
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

      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService = PricingService.make[IO](config, calculatedStateService)

      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      withdrawalValidations = WithdrawalValidations.make[IO](config)
      withdrawalCombinerService = WithdrawalCombinerService.make[IO](config, pricingService, withdrawalValidations, jsonBase64BinaryCodec)

      result <- withdrawalCombinerService
        .combineNew(
          withdrawalUpdate,
          state,
          EpochProgress.MinValue,
          SortedMap.empty,
          CurrencyId(ownerAddress)
        )
        .attempt
        .map {
          case Left(e: IllegalStateException) =>
            expect(e.getMessage === "Liquidity Pool does not exist")
          case Left(e) =>
            failure(s"Unexpected exception: $e")
          case Right(_) =>
            failure("Expected exception was not thrown")
        }
    } yield result
  }

  test("Test withdrawal with minimum amount") { implicit res =>
    implicit val (h, sp, hs) = res

    val primaryToken = TokenInformation(
      CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some,
      PosLong.unsafeFrom(toFixedPoint(1000.0))
    )
    val pairToken = TokenInformation(
      CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some,
      PosLong.unsafeFrom(toFixedPoint(50.0))
    )
    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")

    val (poolId, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val ammOnChainState = AmmOnChainState(Set.empty, None)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]

      // Try to withdraw 0.0000001 shares (1 in fixed-point)
      withdrawalUpdate = getFakeSignedUpdate(
        WithdrawalUpdate(
          CurrencyId(ownerAddress),
          sourceAddress,
          primaryToken.identifier,
          pairToken.identifier,
          ShareAmount(Amount(PosLong.unsafeFrom(1L))),
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

      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService = PricingService.make[IO](config, calculatedStateService)

      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      withdrawalValidations = WithdrawalValidations.make[IO](config)
      withdrawalCombinerService = WithdrawalCombinerService.make[IO](config, pricingService, withdrawalValidations, jsonBase64BinaryCodec)

      withdrawalResponsePendingSpendActionResponse <- withdrawalCombinerService.combineNew(
        withdrawalUpdate,
        state,
        EpochProgress.MinValue,
        SortedMap.empty,
        CurrencyId(ownerAddress)
      )

      spendActions = withdrawalResponsePendingSpendActionResponse.sharedArtifacts.map(_.asInstanceOf[SpendAction]).toList
      pending = getPendingSpendActionWithdrawalUpdates(withdrawalResponsePendingSpendActionResponse.calculated)

      withdrawalResponseConfirmedResponse <- withdrawalCombinerService.combinePendingSpendAction(
        PendingSpendAction(withdrawalUpdate, pending.head.updateHash, spendActions.head),
        withdrawalResponsePendingSpendActionResponse,
        EpochProgress.MinValue,
        spendActions,
        SnapshotOrdinal.MinValue
      )

      updatedLiquidityPool = withdrawalResponseConfirmedResponse.calculated
        .operations(OperationType.LiquidityPool)
        .asInstanceOf[LiquidityPoolCalculatedState]
        .confirmed
        .value(poolId)

    } yield
      expect.all(
        updatedLiquidityPool.poolShares.totalShares.value === (toFixedPoint(1.0) - 1),
        updatedLiquidityPool.poolShares.addressShares(ownerAddress).value.value.value === (toFixedPoint(1.0) - 1)
      )
  }

  test("Test withdrawal of all shares") { implicit res =>
    implicit val (h, sp, hs) = res

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
    val ammOnChainState = AmmOnChainState(Set.empty, None)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]

      withdrawalUpdate = getFakeSignedUpdate(
        WithdrawalUpdate(
          CurrencyId(ownerAddress),
          sourceAddress,
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
        EpochProgress.MaxValue,
        SnapshotOrdinal.MinValue,
        SortedMap.empty,
        EpochProgress.MaxValue,
        SnapshotOrdinal.MinValue,
        ownerAddress
      )

      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService = PricingService.make[IO](config, calculatedStateService)

      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      withdrawalValidations = WithdrawalValidations.make[IO](config)
      withdrawalCombinerService = WithdrawalCombinerService.make[IO](config, pricingService, withdrawalValidations, jsonBase64BinaryCodec)

      result <- withdrawalCombinerService
        .combineNew(
          withdrawalUpdate,
          state,
          EpochProgress(NonNegLong.unsafeFrom(100L)),
          SortedMap.empty,
          CurrencyId(ownerAddress)
        )
    } yield
      expect.all(
        result.calculated.operations(Withdrawal).pending.isEmpty,
        result.calculated.operations(Withdrawal).failed.nonEmpty,
        result.sharedArtifacts.isEmpty
      )
  }

  test("Test withdrawal fail and return balances to the pool") { implicit res =>
    implicit val (h, sp, hs) = res

    // 100.0 tokens = 10000000000 in fixed-point
    val primaryToken = TokenInformation(
      CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some,
      PosLong.unsafeFrom(toFixedPoint(100.0))
    )
    // 50.0 tokens = 5000000000 in fixed-point
    val pairToken = TokenInformation(
      CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some,
      PosLong.unsafeFrom(toFixedPoint(50.0))
    )
    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")

    val (poolId, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val ammOnChainState = AmmOnChainState(Set.empty, None)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]

      // Withdraw 0.5 shares = 50000000 in fixed-point
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
        EpochProgress.MaxValue,
        SnapshotOrdinal.MinValue,
        SortedMap.empty,
        EpochProgress.MaxValue,
        SnapshotOrdinal.MinValue,
        ownerAddress
      )
      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService = PricingService.make[IO](config, calculatedStateService)

      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      withdrawalValidations = WithdrawalValidations.make[IO](config)
      withdrawalCombinerService = WithdrawalCombinerService.make[IO](config, pricingService, withdrawalValidations, jsonBase64BinaryCodec)

      withdrawalResponsePendingSpendActionResponse <- withdrawalCombinerService.combineNew(
        withdrawalUpdate,
        state,
        EpochProgress.MinValue,
        SortedMap.empty,
        CurrencyId(ownerAddress)
      )

      spendActions = withdrawalResponsePendingSpendActionResponse.sharedArtifacts.map(_.asInstanceOf[SpendAction]).toList
      pending = withdrawalResponsePendingSpendActionResponse.calculated.operations(OperationType.Withdrawal).pending.head
      pendingActions = getPendingSpendActionWithdrawalUpdates(withdrawalResponsePendingSpendActionResponse.calculated)

      withdrawalResponseConfirmedResponse <- withdrawalCombinerService.combinePendingSpendAction(
        PendingSpendAction(
          withdrawalUpdate.copy(value = withdrawalUpdate.value.copy(maxValidGsEpochProgress = EpochProgress.MinValue)),
          pendingActions.head.updateHash,
          spendActions.head,
          pending.pricingTokenInfo
        ),
        withdrawalResponsePendingSpendActionResponse.copy(sharedArtifacts = SortedSet.empty),
        EpochProgress.MaxValue,
        spendActions,
        SnapshotOrdinal.MinValue
      )

      lpsAfterSuccessNew = withdrawalResponsePendingSpendActionResponse.calculated
        .operations(OperationType.LiquidityPool)
        .confirmed
        .asInstanceOf[ConfirmedLiquidityPoolCalculatedState]
      lpsAfterFailurePending = withdrawalResponseConfirmedResponse.calculated
        .operations(OperationType.LiquidityPool)
        .confirmed
        .asInstanceOf[ConfirmedLiquidityPoolCalculatedState]

      initialLp = liquidityPoolCalculatedState.confirmed.value(poolId)
      lpAfterSuccessNew = lpsAfterSuccessNew.value(poolId)
      lpAfterFailurePending = lpsAfterFailurePending.value(poolId)

    } yield
      expect.all(
        initialLp.tokenA.amount.value === lpAfterFailurePending.tokenA.amount.value,
        initialLp.tokenB.amount.value === lpAfterFailurePending.tokenB.amount.value,
        lpAfterSuccessNew.tokenA.amount.value < initialLp.tokenA.amount.value,
        lpAfterSuccessNew.tokenB.amount.value < initialLp.tokenB.amount.value
      )
  }

  test("Test successful withdrawal - single provider - cleanup provider") { implicit res =>
    implicit val (h, sp, hs) = res

    // 100.0 tokens = 10000000000 in fixed-point
    val primaryToken = TokenInformation(
      CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some,
      PosLong.unsafeFrom(toFixedPoint(100.0))
    )
    // 50.0 tokens = 5000000000 in fixed-point
    val pairToken = TokenInformation(
      CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some,
      PosLong.unsafeFrom(toFixedPoint(50.0))
    )
    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")

    val (poolId, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val ammOnChainState = AmmOnChainState(Set.empty, None)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]

      // Withdraw 0.5 shares = 50000000 in fixed-point
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

      reference <- WithdrawalReference.of(withdrawalUpdate)

      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(
        keyPair,
        SortedMap.empty,
        EpochProgress.MaxValue,
        SnapshotOrdinal.MinValue,
        SortedMap.empty,
        EpochProgress.MaxValue,
        SnapshotOrdinal.MinValue,
        ownerAddress
      )
      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService = PricingService.make[IO](config, calculatedStateService)

      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      withdrawalValidations = WithdrawalValidations.make[IO](config)
      withdrawalCombinerService = WithdrawalCombinerService.make[IO](config, pricingService, withdrawalValidations, jsonBase64BinaryCodec)

      withdrawalResponsePendingSpendActionResponse <- withdrawalCombinerService.combineNew(
        withdrawalUpdate,
        state,
        EpochProgress.MinValue,
        SortedMap.empty,
        CurrencyId(ownerAddress)
      )

      spendActions = withdrawalResponsePendingSpendActionResponse.sharedArtifacts.map(_.asInstanceOf[SpendAction]).toList
      pending = withdrawalResponsePendingSpendActionResponse.calculated.operations(OperationType.Withdrawal).pending.head
      pendingActions = getPendingSpendActionWithdrawalUpdates(withdrawalResponsePendingSpendActionResponse.calculated)

      withdrawalResponseConfirmedResponse <- withdrawalCombinerService.combinePendingSpendAction(
        PendingSpendAction(withdrawalUpdate, pendingActions.head.updateHash, spendActions.head, pending.pricingTokenInfo),
        withdrawalResponsePendingSpendActionResponse.copy(sharedArtifacts = SortedSet.empty),
        EpochProgress.MinValue,
        spendActions,
        SnapshotOrdinal.MinValue
      )

      withdrawalCleanupResponse = withdrawalCombinerService.cleanupExpiredOperations(
        withdrawalResponseConfirmedResponse,
        EpochProgress.MaxValue
      )

      withdrawalConfirmedState = withdrawalResponseConfirmedResponse.calculated
        .operations(OperationType.Withdrawal)
        .asInstanceOf[WithdrawalCalculatedState]
        .confirmed
        .value

      withdrawalCleanupState = withdrawalCleanupResponse.calculated
        .operations(OperationType.Withdrawal)
        .asInstanceOf[WithdrawalCalculatedState]
        .confirmed
        .value

    } yield
      expect.all(
        withdrawalConfirmedState.nonEmpty,
        withdrawalCleanupState.nonEmpty,
        withdrawalCleanupState.head._2.values.isEmpty
      )
  }

  test("Test fail withdrawal - tokenA min amount not reached") { implicit res =>
    implicit val (h, sp, hs) = res

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
    val ammOnChainState = AmmOnChainState(Set.empty, None)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      withdrawalUpdate = getFakeSignedUpdate(
        WithdrawalUpdate(
          CurrencyId(ownerAddress),
          sourceAddress,
          primaryToken.identifier,
          pairToken.identifier,
          ShareAmount(Amount(PosLong.unsafeFrom(toFixedPoint(0.5)))),
          SwapAmount(PosLong.MaxValue).some,
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
        EpochProgress.MaxValue,
        SnapshotOrdinal.MinValue,
        SortedMap.empty,
        EpochProgress.MaxValue,
        SnapshotOrdinal.MinValue,
        ownerAddress
      )
      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService = PricingService.make[IO](config, calculatedStateService)

      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      withdrawalValidations = WithdrawalValidations.make[IO](config)
      withdrawalCombinerService = WithdrawalCombinerService.make[IO](config, pricingService, withdrawalValidations, jsonBase64BinaryCodec)

      futureEpoch = EpochProgress(NonNegLong.unsafeFrom(10L))

      withdrawalResponsePendingSpendActionResponse <- withdrawalCombinerService.combineNew(
        withdrawalUpdate,
        state,
        futureEpoch,
        SortedMap.empty,
        CurrencyId(ownerAddress)
      )

      withdrawalCalculatedState = withdrawalResponsePendingSpendActionResponse.calculated
        .operations(OperationType.Withdrawal)
        .asInstanceOf[WithdrawalCalculatedState]

    } yield
      expect.all(
        withdrawalCalculatedState.failed.toList.length === 1,
        withdrawalCalculatedState.failed.toList.head.expiringEpochProgress === EpochProgress(
          NonNegLong.unsafeFrom(futureEpoch.value.value + config.expirationEpochProgresses.failedOperations.value.value)
        ),
        withdrawalCalculatedState.failed.toList.head.reason == WithdrawalLessThanMinAmount()
      )
  }

  test("Test fail withdrawal - tokenB min amount not reached") { implicit res =>
    implicit val (h, sp, hs) = res

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
    val ammOnChainState = AmmOnChainState(Set.empty, None)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      withdrawalUpdate = getFakeSignedUpdate(
        WithdrawalUpdate(
          CurrencyId(ownerAddress),
          sourceAddress,
          primaryToken.identifier,
          pairToken.identifier,
          ShareAmount(Amount(PosLong.unsafeFrom(toFixedPoint(0.5)))),
          none,
          SwapAmount(PosLong.MaxValue).some,
          none,
          none,
          WithdrawalReference.empty,
          EpochProgress.MaxValue
        )
      )

      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(
        keyPair,
        SortedMap.empty,
        EpochProgress.MaxValue,
        SnapshotOrdinal.MinValue,
        SortedMap.empty,
        EpochProgress.MaxValue,
        SnapshotOrdinal.MinValue,
        ownerAddress
      )
      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService = PricingService.make[IO](config, calculatedStateService)

      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      withdrawalValidations = WithdrawalValidations.make[IO](config)
      withdrawalCombinerService = WithdrawalCombinerService.make[IO](config, pricingService, withdrawalValidations, jsonBase64BinaryCodec)

      futureEpoch = EpochProgress(NonNegLong.unsafeFrom(10L))

      withdrawalResponsePendingSpendActionResponse <- withdrawalCombinerService.combineNew(
        withdrawalUpdate,
        state,
        futureEpoch,
        SortedMap.empty,
        CurrencyId(ownerAddress)
      )

      withdrawalCalculatedState = withdrawalResponsePendingSpendActionResponse.calculated
        .operations(OperationType.Withdrawal)
        .asInstanceOf[WithdrawalCalculatedState]

    } yield
      expect.all(
        withdrawalCalculatedState.failed.toList.length === 1,
        withdrawalCalculatedState.failed.toList.head.expiringEpochProgress === EpochProgress(
          NonNegLong.unsafeFrom(futureEpoch.value.value + config.expirationEpochProgresses.failedOperations.value.value)
        ),
        withdrawalCalculatedState.failed.toList.head.reason == WithdrawalLessThanMinAmount()
      )
  }

  test("Test fail withdrawal - tokenA max amount exceeds limit") { implicit res =>
    implicit val (h, sp, hs) = res

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
    val ammOnChainState = AmmOnChainState(Set.empty, None)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      withdrawalUpdate = getFakeSignedUpdate(
        WithdrawalUpdate(
          CurrencyId(ownerAddress),
          sourceAddress,
          primaryToken.identifier,
          pairToken.identifier,
          ShareAmount(Amount(PosLong.unsafeFrom(toFixedPoint(0.5)))),
          none,
          none,
          SwapAmount(PosLong.MinValue).some,
          none,
          WithdrawalReference.empty,
          EpochProgress.MaxValue
        )
      )

      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(
        keyPair,
        SortedMap.empty,
        EpochProgress.MaxValue,
        SnapshotOrdinal.MinValue,
        SortedMap.empty,
        EpochProgress.MaxValue,
        SnapshotOrdinal.MinValue,
        ownerAddress
      )
      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService = PricingService.make[IO](config, calculatedStateService)

      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      withdrawalValidations = WithdrawalValidations.make[IO](config)
      withdrawalCombinerService = WithdrawalCombinerService.make[IO](config, pricingService, withdrawalValidations, jsonBase64BinaryCodec)

      futureEpoch = EpochProgress(NonNegLong.unsafeFrom(10L))

      withdrawalResponsePendingSpendActionResponse <- withdrawalCombinerService.combineNew(
        withdrawalUpdate,
        state,
        futureEpoch,
        SortedMap.empty,
        CurrencyId(ownerAddress)
      )

      withdrawalCalculatedState = withdrawalResponsePendingSpendActionResponse.calculated
        .operations(OperationType.Withdrawal)
        .asInstanceOf[WithdrawalCalculatedState]

    } yield
      expect.all(
        withdrawalCalculatedState.failed.toList.length === 1,
        withdrawalCalculatedState.failed.toList.head.expiringEpochProgress === EpochProgress(
          NonNegLong.unsafeFrom(futureEpoch.value.value + config.expirationEpochProgresses.failedOperations.value.value)
        ),
        withdrawalCalculatedState.failed.toList.head.reason == WithdrawalHigherThanMaxAmount()
      )
  }

  test("Test fail withdrawal - tokenB max amount exceeds limit") { implicit res =>
    implicit val (h, sp, hs) = res

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
    val ammOnChainState = AmmOnChainState(Set.empty, None)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
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
          SwapAmount(PosLong.MinValue).some,
          WithdrawalReference.empty,
          EpochProgress.MaxValue
        )
      )

      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(
        keyPair,
        SortedMap.empty,
        EpochProgress.MaxValue,
        SnapshotOrdinal.MinValue,
        SortedMap.empty,
        EpochProgress.MaxValue,
        SnapshotOrdinal.MinValue,
        ownerAddress
      )
      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService = PricingService.make[IO](config, calculatedStateService)

      jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      withdrawalValidations = WithdrawalValidations.make[IO](config)
      withdrawalCombinerService = WithdrawalCombinerService.make[IO](config, pricingService, withdrawalValidations, jsonBase64BinaryCodec)

      futureEpoch = EpochProgress(NonNegLong.unsafeFrom(10L))

      withdrawalResponsePendingSpendActionResponse <- withdrawalCombinerService.combineNew(
        withdrawalUpdate,
        state,
        futureEpoch,
        SortedMap.empty,
        CurrencyId(ownerAddress)
      )

      withdrawalCalculatedState = withdrawalResponsePendingSpendActionResponse.calculated
        .operations(OperationType.Withdrawal)
        .asInstanceOf[WithdrawalCalculatedState]

    } yield
      expect.all(
        withdrawalCalculatedState.failed.toList.length === 1,
        withdrawalCalculatedState.failed.toList.head.expiringEpochProgress === EpochProgress(
          NonNegLong.unsafeFrom(futureEpoch.value.value + config.expirationEpochProgresses.failedOperations.value.value)
        ),
        withdrawalCalculatedState.failed.toList.head.reason == WithdrawalHigherThanMaxAmount()
      )
  }
}
