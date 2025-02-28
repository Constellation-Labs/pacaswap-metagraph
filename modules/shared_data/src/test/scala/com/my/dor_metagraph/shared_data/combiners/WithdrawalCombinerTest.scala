package com.my.dor_metagraph.shared_data.combiners

import cats.data.NonEmptySet
import cats.effect.{IO, Resource}
import cats.syntax.all._

import scala.collection.immutable.SortedMap

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.ext.cats.effect.ResourceIO
import io.constellationnetwork.json.JsonSerializer
import io.constellationnetwork.schema.ID.Id
import io.constellationnetwork.schema._
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact.SpendTransaction
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap._
import io.constellationnetwork.security._
import io.constellationnetwork.security.hex.Hex
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.signature.signature.{Signature, SignatureProof}

import com.my.dor_metagraph.shared_data.DummyL0Context.buildL0NodeContext
import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.PosLong
import eu.timepit.refined.types.numeric.NonNegLong
import org.amm_metagraph.shared_data.combiners.WithdrawalCombiner.combineWithdrawal
import org.amm_metagraph.shared_data.refined.PosLongOps
import org.amm_metagraph.shared_data.types.DataUpdates.WithdrawalUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States.OperationType.Withdrawal
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.Withdrawal.WithdrawalReference
import weaver.MutableIOSuite

object WithdrawalCombinerTest extends MutableIOSuite {
  type Res = (Hasher[IO], SecurityProvider[IO])

  override def sharedResource: Resource[IO, Res] = for {
    sp <- SecurityProvider.forAsync[IO]
    implicit0(j: JsonSerializer[IO]) <- JsonSerializer.forSync[IO].asResource
    h = Hasher.forJson[IO]
  } yield (h, sp)

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

    // Initial shares for owner - 1.0 share = 100000000
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
    implicit val (h, sp) = res

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
    val ownerAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Ks")

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
          primaryToken.identifier,
          pairToken.identifier,
          ShareAmount(Amount(PosLong.unsafeFrom(toFixedPoint(0.5)))),
          WithdrawalReference.empty,
          EpochProgress.MaxValue
        )
      )

      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(keyPair, SortedMap.empty, EpochProgress.MinValue, ownerAddress)

      withdrawalResponse <- combineWithdrawal[IO](
        state,
        withdrawalUpdate,
        ownerAddress,
        SnapshotOrdinal.MinValue
      )

      updatedLiquidityPool = withdrawalResponse.calculated
        .operations(OperationType.LiquidityPool)
        .asInstanceOf[LiquidityPoolCalculatedState]
        .confirmed
        .value(poolId)

      withdrawalSpendTransactions = withdrawalResponse.sharedArtifacts.collect {
        case action: artifact.SpendAction => action.output
      }.collect {
        case transaction: SpendTransaction => transaction
      }

    } yield
      expect.all(
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
    implicit val (h, sp) = res

    val primaryToken = TokenInformation(
      CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some,
      PosLong.unsafeFrom(toFixedPoint(100.0))
    )
    val pairToken = TokenInformation(
      CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some,
      PosLong.unsafeFrom(toFixedPoint(50.0))
    )
    val ownerAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Ks")
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
          primaryToken.identifier,
          pairToken.identifier,
          ShareAmount(Amount(PosLong.unsafeFrom(toFixedPoint(0.5)))),
          WithdrawalReference.empty,
          EpochProgress.MaxValue
        )
      )

      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(keyPair, SortedMap.empty, EpochProgress.MinValue, ownerAddress)

      withdrawalResponse <- combineWithdrawal[IO](
        state,
        withdrawalUpdate,
        secondProviderAddress,
        SnapshotOrdinal.MinValue
      )

      updatedLiquidityPool = withdrawalResponse.calculated
        .operations(OperationType.LiquidityPool)
        .asInstanceOf[LiquidityPoolCalculatedState]
        .confirmed
        .value(poolId)

    } yield
      expect.all(
        updatedLiquidityPool.poolShares.addressShares.size === 2,
        updatedLiquidityPool.poolShares.addressShares(ownerAddress).value.value.value === toFixedPoint(1.0),
        updatedLiquidityPool.poolShares.addressShares(secondProviderAddress).value.value.value === toFixedPoint(0.5)
      )
  }

//  test("Test withdrawal fails when trying to withdraw more shares than owned") { implicit res =>
//    implicit val (h, sp) = res
//
//    val primaryToken = TokenInformation(
//      CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some,
//      PosLong.unsafeFrom(toFixedPoint(100.0))
//    )
//    val pairToken = TokenInformation(
//      CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some,
//      PosLong.unsafeFrom(toFixedPoint(50.0))
//    )
//    val ownerAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Ks")
//
//    val (_, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
//    val ammOnChainState = AmmOnChainState(List.empty)
//    val ammCalculatedState = AmmCalculatedState(
//      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
//    )
//    val state = DataState(ammOnChainState, ammCalculatedState)
//
//    for {
//      keyPair <- KeyPairGenerator.makeKeyPair[IO]
//
//      // Try to withdraw 2.0 shares when only 1.0 is owned
//      withdrawalUpdate = getFakeSignedUpdate(
//        WithdrawalUpdate(
//          primaryToken.identifier,
//          pairToken.identifier,
//          ShareAmount(Amount(PosLong.unsafeFrom(toFixedPoint(2.0)))),
//          EpochProgress.MaxValue
//        )
//      )
//
//      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(keyPair, SortedMap.empty)
//
//      result <- combineWithdrawal[IO](
//        state,
//        withdrawalUpdate,
//        ownerAddress,
//        SnapshotOrdinal.MinValue
//      ).attempt
//
//    } yield expect(result.isLeft)
//  }

  test("Test withdrawal fails when liquidity pool does not exist") { implicit res =>
    implicit val (h, sp) = res

    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(Map.empty)
    val state = DataState(ammOnChainState, ammCalculatedState)
    val ownerAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Ks")

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]

      withdrawalUpdate = getFakeSignedUpdate(
        WithdrawalUpdate(
          Some(CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb"))),
          Some(CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5"))),
          ShareAmount(Amount(PosLong.unsafeFrom(toFixedPoint(0.5)))),
          WithdrawalReference.empty,
          EpochProgress.MaxValue
        )
      )

      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(keyPair, SortedMap.empty, EpochProgress.MinValue, ownerAddress)

      result <- combineWithdrawal[IO](
        state,
        withdrawalUpdate,
        Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Ks"),
        SnapshotOrdinal.MinValue
      ).attempt.map {
        case Left(e: IllegalStateException) =>
          expect(e.getMessage == "Liquidity Pool does not exist")
        case Left(e) =>
          failure(s"Unexpected exception: $e")
        case Right(_) =>
          failure("Expected exception was not thrown")
      }
    } yield result
  }

  test("Test withdrawal with minimum amount") { implicit res =>
    implicit val (h, sp) = res

    val primaryToken = TokenInformation(
      CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some,
      PosLong.unsafeFrom(toFixedPoint(100.0))
    )
    val pairToken = TokenInformation(
      CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some,
      PosLong.unsafeFrom(toFixedPoint(50.0))
    )
    val ownerAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Ks")

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
          primaryToken.identifier,
          pairToken.identifier,
          ShareAmount(Amount(PosLong.unsafeFrom(1L))),
          WithdrawalReference.empty,
          EpochProgress.MaxValue
        )
      )

      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(keyPair, SortedMap.empty, EpochProgress.MinValue, ownerAddress)

      withdrawalResponse <- combineWithdrawal[IO](
        state,
        withdrawalUpdate,
        ownerAddress,
        SnapshotOrdinal.MinValue
      )

      updatedLiquidityPool = withdrawalResponse.calculated
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
    implicit val (h, sp) = res

    val primaryToken = TokenInformation(
      CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some,
      PosLong.unsafeFrom(toFixedPoint(100.0))
    )
    val pairToken = TokenInformation(
      CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some,
      PosLong.unsafeFrom(toFixedPoint(50.0))
    )
    val ownerAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Ks")

    val (poolId, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]

      // Withdraw all shares (1.0)
      withdrawalUpdate = getFakeSignedUpdate(
        WithdrawalUpdate(
          primaryToken.identifier,
          pairToken.identifier,
          ShareAmount(Amount(PosLong.unsafeFrom(toFixedPoint(1.0)))),
          WithdrawalReference.empty,
          EpochProgress.MaxValue
        )
      )

      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(keyPair, SortedMap.empty, EpochProgress.MinValue, ownerAddress)

      result <- combineWithdrawal[IO](
        state,
        withdrawalUpdate,
        ownerAddress,
        SnapshotOrdinal.MinValue
      ).attempt.map {
        case Left(e: IllegalArgumentException) =>
          expect(e.getMessage.contains("Predicate failed"))
        case Left(e) =>
          failure(s"Unexpected exception: $e")
        case Right(_) =>
          failure("Expected exception was not thrown")
      }
    } yield result
  }

  test("Test withdrawal respects epoch progress") { implicit res =>
    implicit val (h, sp) = res

    val primaryToken = TokenInformation(
      CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some,
      PosLong.unsafeFrom(toFixedPoint(100.0))
    )
    val pairToken = TokenInformation(
      CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some,
      PosLong.unsafeFrom(toFixedPoint(50.0))
    )
    val ownerAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Ks")

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
          primaryToken.identifier,
          pairToken.identifier,
          ShareAmount(Amount(PosLong.unsafeFrom(toFixedPoint(0.5)))),
          WithdrawalReference.empty,
          EpochProgress.MinValue // Set to minimum to test epoch progress validation
        )
      )

      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(keyPair, SortedMap.empty, EpochProgress.MinValue, ownerAddress)

      result <- combineWithdrawal[IO](
        state,
        withdrawalUpdate,
        ownerAddress,
        SnapshotOrdinal(NonNegLong.MaxValue) // Set to maximum to ensure epoch progress validation fails
      )

    } yield expect(result.calculated.operations(Withdrawal).pending.isEmpty)
  }
}
