package com.my.dor_metagraph.shared_data.pricing

import cats.data.NonEmptySet
import cats.effect.IO
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.DataState
import io.constellationnetwork.schema.ID.Id
import io.constellationnetwork.schema._
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap._
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.hex.Hex
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.signature.signature.{Signature, SignatureProof}

import com.my.dor_metagraph.shared_data.combiners.SwapCombinerTest.{getFakeSignedUpdate, toFixedPoint}
import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.PosLong
import org.amm_metagraph.shared_data.calculated_state.CalculatedStateService
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.services.pricing.PricingService
import org.amm_metagraph.shared_data.types.DataUpdates.{StakingUpdate, SwapUpdate}
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.Staking.StakingReference
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.Swap.SwapReference
import weaver.SimpleIOSuite

object PricingTest extends SimpleIOSuite {
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

  def getFakeSignedUpdate[A](
    update: A
  ): Signed[A] =
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

  test("Test successfully getSwapQuote - small impact") {
    val primaryToken = TokenInformation(
      CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some,
      PosLong.unsafeFrom(toFixedPoint(1000.0))
    )

    val pairToken = TokenInformation(
      CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some,
      PosLong.unsafeFrom(toFixedPoint(500.0))
    )

    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")

    val (_, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService = PricingService.make[IO](calculatedStateService)
      swapQuoteResponse <- pricingService.getSwapQuote(
        primaryToken.identifier,
        pairToken.identifier,
        Amount(PosLong.unsafeFrom(toFixedPoint(1.0))),
        Percentage.unsafeFrom(BigDecimal(3L))
      )

    } yield
      expect.all(
        swapQuoteResponse.isRight,
        swapQuoteResponse.toOption.get.slippagePercent == Percentage.unsafeFrom(BigDecimal(3L)),
        swapQuoteResponse.toOption.get.rate === 0.4995005,
        swapQuoteResponse.toOption.get.priceImpactPercent === BigDecimal(0.10),
        swapQuoteResponse.toOption.get.estimatedReceived === BigInt(49950050L),
        swapQuoteResponse.toOption.get.minimumReceived === BigInt(48451548L)
      )
  }

  test("Test successfully getSwapQuote - large impact") {
    val primaryToken = TokenInformation(
      CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some,
      PosLong.unsafeFrom(toFixedPoint(1000.0))
    )

    val pairToken = TokenInformation(
      CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some,
      PosLong.unsafeFrom(toFixedPoint(500.0))
    )

    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")

    val (_, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService = PricingService.make[IO](calculatedStateService)
      swapQuoteResponse <- pricingService.getSwapQuote(
        primaryToken.identifier,
        pairToken.identifier,
        Amount(PosLong.unsafeFrom(toFixedPoint(955.0))),
        Percentage.unsafeFrom(BigDecimal(3L))
      )

    } yield
      expect.all(
        swapQuoteResponse.isRight,
        swapQuoteResponse.toOption.get.slippagePercent == Percentage.unsafeFrom(BigDecimal(3L)),
        swapQuoteResponse.toOption.get.rate.setScale(4, BigDecimal.RoundingMode.HALF_UP) === 0.2558,
        swapQuoteResponse.toOption.get.priceImpactPercent === BigDecimal(48.85),
        swapQuoteResponse.toOption.get.estimatedReceived === BigInt(24424552430L),
        swapQuoteResponse.toOption.get.minimumReceived === BigInt(23691815857L)
      )
  }

  test("Fail to getSwapQuote pool not exists") {
    val primaryToken = TokenInformation(
      CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some,
      PosLong.unsafeFrom(toFixedPoint(1000.00))
    )

    val pairToken = TokenInformation(
      CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some,
      PosLong.unsafeFrom(toFixedPoint(500.0))
    )

    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")

    val (_, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService = PricingService.make[IO](calculatedStateService)
      swapQuoteResponse <- pricingService.getSwapQuote(
        None,
        pairToken.identifier,
        Amount(PosLong.unsafeFrom(toFixedPoint(100.0))),
        Percentage.unsafeFrom(BigDecimal(3L))
      )

    } yield {
      val expectedError = "Liquidity pool does not exist"
      expect.all(
        swapQuoteResponse match {
          case Left(message) => message.contains(expectedError)
          case _             => false
        }
      )
    }
  }

  test("Test successfully getLiquidityPoolPrices - small difference") {
    val primaryToken = TokenInformation(
      CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some,
      PosLong.unsafeFrom(toFixedPoint(200.0))
    )

    val pairToken = TokenInformation(
      CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some,
      PosLong.unsafeFrom(toFixedPoint(100.0))
    )

    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")

    val (_, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      poolId <- buildLiquidityPoolUniqueIdentifier(primaryToken.identifier, pairToken.identifier)
      pricingService = PricingService.make[IO](calculatedStateService)
      liquidityPoolPrices <- pricingService.getLiquidityPoolPrices(
        poolId
      )

    } yield
      expect.all(
        liquidityPoolPrices.isRight,
        liquidityPoolPrices.toOption.get._1 === toFixedPoint(1.0),
        liquidityPoolPrices.toOption.get._2 === toFixedPoint(2.0)
      )
  }

  test("Test successfully getLiquidityPoolPrices - large difference") {
    val primaryToken = TokenInformation(
      CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some,
      PosLong.unsafeFrom(toFixedPoint(500.0))
    )

    val pairToken = TokenInformation(
      CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some,
      PosLong.unsafeFrom(toFixedPoint(1500.0))
    )

    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")

    val (_, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      poolId <- buildLiquidityPoolUniqueIdentifier(primaryToken.identifier, pairToken.identifier)
      pricingService = PricingService.make[IO](calculatedStateService)
      liquidityPoolPrices <- pricingService.getLiquidityPoolPrices(
        poolId
      )

    } yield
      expect.all(
        liquidityPoolPrices.isRight,
        liquidityPoolPrices.toOption.get._1 === toFixedPoint(3.0),
        liquidityPoolPrices.toOption.get._2 === 0
      )
  }

  test("Test successfully getSwapTokenInfo") {
    val primaryToken = TokenInformation(
      CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some,
      PosLong.unsafeFrom(toFixedPoint(1000.0))
    )

    val pairToken = TokenInformation(
      CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some,
      PosLong.unsafeFrom(toFixedPoint(1000.0))
    )

    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")

    val (_, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      poolId <- buildLiquidityPoolUniqueIdentifier(primaryToken.identifier, pairToken.identifier)
      pricingService = PricingService.make[IO](calculatedStateService)

      swapUpdate = getFakeSignedUpdate[SwapUpdate](
        SwapUpdate(
          primaryToken.identifier,
          pairToken.identifier,
          Hash.empty,
          SwapAmount(PosLong.unsafeFrom(toFixedPoint(45.0))),
          SwapAmount(PosLong.unsafeFrom(toFixedPoint(50.0))),
          EpochProgress.MaxValue,
          PosLong.unsafeFrom(toFixedPoint(0.4)).some,
          PosLong.unsafeFrom(toFixedPoint(0.6)).some,
          SwapReference.empty
        )
      )

      swapTokenInfo <- pricingService.getSwapTokenInfo(
        swapUpdate,
        poolId
      )

    } yield
      expect.all(
        swapTokenInfo.isRight,
        swapTokenInfo.toOption.get.effectivePrice.value.value === 95238095L,
        swapTokenInfo.toOption.get.receivedAmount.value.value === 4761904762L
      )
  }

  test("Test successfully getStakingTokenInfo") {
    val primaryToken = TokenInformation(
      CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some,
      PosLong.unsafeFrom(toFixedPoint(2000.0))
    )

    val pairToken = TokenInformation(
      CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some,
      PosLong.unsafeFrom(toFixedPoint(4000.0))
    )

    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")

    val (_, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val ammOnChainState = AmmOnChainState(List.empty)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      poolId <- buildLiquidityPoolUniqueIdentifier(primaryToken.identifier, pairToken.identifier)
      pricingService = PricingService.make[IO](calculatedStateService)

      stakingUpdate = getFakeSignedUpdate[StakingUpdate](
        StakingUpdate(
          Hash.empty,
          Hash.empty,
          primaryToken.identifier,
          PosLong.unsafeFrom(toFixedPoint(100.0)),
          pairToken.identifier,
          StakingReference.empty,
          EpochProgress.MinValue
        )
      )

      stakingTokenInfo <- pricingService.getStakingTokenInfo(
        stakingUpdate,
        poolId
      )

    } yield
      expect.all(
        stakingTokenInfo.isRight,
        stakingTokenInfo.toOption.get.primaryTokenInformation.amount.value === toFixedPoint(100.0),
        stakingTokenInfo.toOption.get.pairTokenInformation.amount.value === toFixedPoint(200.0),
        stakingTokenInfo.toOption.get.newlyIssuedShares === 4761904L
      )
  }
}
