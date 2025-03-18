package com.my.dor_metagraph.shared_data.combiners

import cats.data.NonEmptySet
import cats.effect.{IO, Resource}
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.ext.cats.effect.ResourceIO
import io.constellationnetwork.json.JsonSerializer
import io.constellationnetwork.schema.ID.Id
import io.constellationnetwork.schema._
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact.{SpendAction, SpendTransaction}
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap._
import io.constellationnetwork.security._
import io.constellationnetwork.security.hex.Hex
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.signature.signature.{Signature, SignatureProof}

import com.my.dor_metagraph.shared_data.DummyL0Context.buildL0NodeContext
import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.{NonNegLong, PosDouble, PosLong}
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.app.ApplicationConfig._
import org.amm_metagraph.shared_data.refined.PosLongOps
import org.amm_metagraph.shared_data.services.combiners.WithdrawalCombinerService
import org.amm_metagraph.shared_data.types.DataUpdates.WithdrawalUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States.OperationType.Withdrawal
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.Withdrawal.WithdrawalReference
import org.amm_metagraph.shared_data.types.codecs.HasherSelector
import weaver.MutableIOSuite

object WithdrawalCombinerTest extends MutableIOSuite {
  type Res = (Hasher[IO], SecurityProvider[IO], HasherSelector[IO])
  val sourceAddress: Address = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")

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
    )
  )

  override def sharedResource: Resource[IO, Res] = for {
    sp <- SecurityProvider.forAsync[IO]
    implicit0(j: JsonSerializer[IO]) <- JsonSerializer.forSync[IO].asResource
    h = Hasher.forJson[IO]
    hs = HasherSelector.forSync(h, h)
  } yield (h, sp, hs)

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

  def getFakeSignedUpdate(update: WithdrawalUpdate): Signed[WithdrawalUpdate] =
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
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]

      // Withdraw 0.5 shares = 50000000 in fixed-point
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

      withdrawalCombinerService = WithdrawalCombinerService.make[IO](config)

      withdrawalResponsePendingSpendActionResponse <- withdrawalCombinerService.combineNew(
        withdrawalUpdate,
        state,
        EpochProgress.MinValue,
        SortedMap.empty,
        CurrencyId(ownerAddress)
      )

      spendActions = withdrawalResponsePendingSpendActionResponse.sharedArtifacts.map(_.asInstanceOf[SpendAction]).toList

      withdrawalResponseConfirmedResponse <- withdrawalCombinerService.combinePendingSpendAction(
        PendingSpendAction(withdrawalUpdate, spendActions.head),
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
        withdrawalCalculatedState.confirmed.value.flatMap { case (_, s) => s }.exists(_.parent === reference),
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

    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]

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

      withdrawalCombinerService = WithdrawalCombinerService.make[IO](config)

      withdrawalResponsePendingSpendActionResponse <- withdrawalCombinerService.combineNew(
        withdrawalUpdate,
        state,
        EpochProgress.MinValue,
        SortedMap.empty,
        CurrencyId(ownerAddress)
      )

      spendActions = withdrawalResponsePendingSpendActionResponse.sharedArtifacts.map(_.asInstanceOf[SpendAction]).toList

      withdrawalResponseConfirmedResponse <- withdrawalCombinerService.combinePendingSpendAction(
        PendingSpendAction(withdrawalUpdate, spendActions.head),
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

      withdrawalCalculatedState = withdrawalResponseConfirmedResponse.calculated.operations
        .getOrElse(Withdrawal, WithdrawalCalculatedState.empty)
        .asInstanceOf[WithdrawalCalculatedState]

    } yield
      expect.all(
        withdrawalCalculatedState.pending.isEmpty,
        withdrawalCalculatedState.failed.isEmpty,
        withdrawalCalculatedState.confirmed.value.flatMap { case (_, s) => s }.exists(_.parent === reference),
        withdrawalResponseConfirmedResponse.sharedArtifacts.isEmpty,
        updatedLiquidityPool.poolShares.addressShares.size === 2,
        updatedLiquidityPool.poolShares.addressShares(ownerAddress).value.value.value === toFixedPoint(0.5),
        updatedLiquidityPool.poolShares.addressShares(secondProviderAddress).value.value.value === toFixedPoint(1.0)
      )
  }

  test("Test withdrawal fails when liquidity pool does not exist") { implicit res =>
    implicit val (h, sp, hs) = res

    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(Map.empty)
    val state = DataState(ammOnChainState, ammCalculatedState)
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

      withdrawalCombinerService = WithdrawalCombinerService.make[IO](config)

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
      PosLong.unsafeFrom(toFixedPoint(100.0))
    )
    val pairToken = TokenInformation(
      CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some,
      PosLong.unsafeFrom(toFixedPoint(50.0))
    )
    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")

    val (poolId, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]

      // Try to withdraw 0.0000001 shares (1 in fixed-point)
      withdrawalUpdate = getFakeSignedUpdate(
        WithdrawalUpdate(
          sourceAddress,
          primaryToken.identifier,
          pairToken.identifier,
          ShareAmount(Amount(PosLong.unsafeFrom(1L))),
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

      withdrawalCombinerService = WithdrawalCombinerService.make[IO](config)

      withdrawalResponsePendingSpendActionResponse <- withdrawalCombinerService.combineNew(
        withdrawalUpdate,
        state,
        EpochProgress.MinValue,
        SortedMap.empty,
        CurrencyId(ownerAddress)
      )

      spendActions = withdrawalResponsePendingSpendActionResponse.sharedArtifacts.map(_.asInstanceOf[SpendAction]).toList

      withdrawalResponseConfirmedResponse <- withdrawalCombinerService.combinePendingSpendAction(
        PendingSpendAction(withdrawalUpdate, spendActions.head),
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
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]

      withdrawalUpdate = getFakeSignedUpdate(
        WithdrawalUpdate(
          sourceAddress,
          primaryToken.identifier,
          pairToken.identifier,
          ShareAmount(Amount(PosLong.unsafeFrom(toFixedPoint(1.0)))),
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

      withdrawalCombinerService = WithdrawalCombinerService.make[IO](config)

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
}
