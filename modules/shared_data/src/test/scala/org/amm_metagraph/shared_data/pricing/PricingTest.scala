package org.amm_metagraph.shared_data.pricing

import cats.effect.IO
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.currency.dataApplication.DataState
import io.constellationnetwork.schema._
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap._
import io.constellationnetwork.security.hash.Hash

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.PosLong
import org.amm_metagraph.shared_data.Shared._
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
    val ammOnChainState = AmmOnChainState(SortedSet.empty, None)
    val ammCalculatedState = AmmCalculatedState(
      SortedMap(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService = PricingService.make[IO](config, calculatedStateService)
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
    val ammOnChainState = AmmOnChainState(SortedSet.empty, None)
    val ammCalculatedState = AmmCalculatedState(
      SortedMap(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService = PricingService.make[IO](config, calculatedStateService)
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
    val ammOnChainState = AmmOnChainState(SortedSet.empty, None)
    val ammCalculatedState = AmmCalculatedState(
      SortedMap(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService = PricingService.make[IO](config, calculatedStateService)
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
    val ammOnChainState = AmmOnChainState(SortedSet.empty, None)
    val ammCalculatedState = AmmCalculatedState(
      SortedMap(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      poolId <- buildLiquidityPoolUniqueIdentifier(primaryToken.identifier, pairToken.identifier)
      pricingService = PricingService.make[IO](config, calculatedStateService)
      liquidityPoolPrices <- pricingService.getLiquidityPoolPrices(
        poolId
      )

    } yield
      expect.all(
        liquidityPoolPrices.isRight,
        liquidityPoolPrices.toOption.get._1 === toFixedPoint(0.5),
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
    val ammOnChainState = AmmOnChainState(SortedSet.empty, None)
    val ammCalculatedState = AmmCalculatedState(
      SortedMap(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      poolId <- buildLiquidityPoolUniqueIdentifier(primaryToken.identifier, pairToken.identifier)
      pricingService = PricingService.make[IO](config, calculatedStateService)
      liquidityPoolPrices <- pricingService.getLiquidityPoolPrices(
        poolId
      )

    } yield
      expect.all(
        liquidityPoolPrices.isRight,
        liquidityPoolPrices.toOption.get._1 === toFixedPoint(3.0),
        liquidityPoolPrices.toOption.get._2 === toFixedPoint(0.33333333)
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
    val ammOnChainState = AmmOnChainState(SortedSet.empty, None)
    val ammCalculatedState = AmmCalculatedState(
      SortedMap(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      poolId <- buildLiquidityPoolUniqueIdentifier(primaryToken.identifier, pairToken.identifier)
      pricingService = PricingService.make[IO](config, calculatedStateService)

      swapUpdate = getFakeSignedUpdate[SwapUpdate](
        SwapUpdate(
          CurrencyId(ownerAddress),
          sourceAddress,
          primaryToken.identifier,
          pairToken.identifier,
          Hash.empty,
          SwapAmount(PosLong.unsafeFrom(toFixedPoint(50.0))),
          SwapAmount(PosLong.unsafeFrom(toFixedPoint(45.0))),
          none,
          EpochProgress.MaxValue,
          SwapReference.empty
        )
      )

      swapTokenInfo <- pricingService.getSwapTokenInfo(
        swapUpdate,
        Hash.empty,
        poolId,
        EpochProgress.MaxValue
      )

    } yield
      expect.all(
        swapTokenInfo.isRight,
        swapTokenInfo.toOption.get.netReceived.value.value === 4761904762L
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
    val ammOnChainState = AmmOnChainState(SortedSet.empty, None)
    val ammCalculatedState = AmmCalculatedState(
      SortedMap(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      poolId <- buildLiquidityPoolUniqueIdentifier(primaryToken.identifier, pairToken.identifier)
      pricingService = PricingService.make[IO](config, calculatedStateService)

      stakingUpdate = getFakeSignedUpdate[StakingUpdate](
        StakingUpdate(
          CurrencyId(ownerAddress),
          sourceAddress,
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
        Hash.empty,
        poolId,
        EpochProgress.MaxValue
      )

    } yield
      expect.all(
        stakingTokenInfo.isRight,
        stakingTokenInfo.toOption.get.primaryTokenInformation.amount.value === toFixedPoint(100.0),
        stakingTokenInfo.toOption.get.pairTokenInformation.amount.value === toFixedPoint(200.0),
        stakingTokenInfo.toOption.get.newlyIssuedShares === 5000000L
      )
  }

  test("Test successfully getSwapQuote and getReverseSwapQuote should return the same info") {
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
    val ammOnChainState = AmmOnChainState(SortedSet.empty, None)
    val ammCalculatedState = AmmCalculatedState(
      SortedMap(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService = PricingService.make[IO](config, calculatedStateService)
      swapQuoteResponse <- pricingService.getSwapQuote(
        primaryToken.identifier,
        pairToken.identifier,
        Amount(PosLong.unsafeFrom(toFixedPoint(1.0))),
        Percentage.unsafeFrom(BigDecimal(3L))
      )
      reverseSwapQuoteResponse <- pricingService.getReverseSwapQuote(
        primaryToken.identifier,
        pairToken.identifier,
        Amount(PosLong.unsafeFrom(swapQuoteResponse.toOption.get.estimatedReceived.toLong)),
        Percentage.unsafeFrom(BigDecimal(3L))
      )

    } yield
      expect.all(
        swapQuoteResponse.isRight,
        swapQuoteResponse.toOption.get.slippagePercent == Percentage.unsafeFrom(BigDecimal(3L)),
        swapQuoteResponse.toOption.get.rate === 0.4995005,
        swapQuoteResponse.toOption.get.priceImpactPercent === BigDecimal(0.10),
        swapQuoteResponse.toOption.get.estimatedReceived === BigInt(49950050L),
        swapQuoteResponse.toOption.get.minimumReceived === BigInt(48451548L),
        swapQuoteResponse.toOption.get.slippagePercent == reverseSwapQuoteResponse.toOption.get.slippagePercent,
        swapQuoteResponse.toOption.get.estimatedReceived.toLong == reverseSwapQuoteResponse.toOption.get.desiredOutputAmount.value.value,
        swapQuoteResponse.toOption.get.amount.value.value == reverseSwapQuoteResponse.toOption.get.requiredInputAmount.toLong
      )
  }
}
