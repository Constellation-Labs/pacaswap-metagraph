package org.amm_metagraph.shared_data.pricing

import cats.data.EitherT
import cats.effect.{Async, IO}
import cats.syntax.all._

import scala.collection.immutable.SortedMap
import scala.collection.mutable
import scala.util.Random

import io.constellationnetwork.currency.dataApplication.DataState
import io.constellationnetwork.schema._
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap._
import io.constellationnetwork.security.Hashed
import io.constellationnetwork.security.hash.{Hash, ProofsHash}

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.{NonNegLong, PosLong}
import monocle.Monocle.toAppliedFocusOps
import org.amm_metagraph.shared_data.FeeDistributor
import org.amm_metagraph.shared_data.FeeDistributor.FeePercentages
import org.amm_metagraph.shared_data.Shared._
import org.amm_metagraph.shared_data.calculated_state.CalculatedStateService
import org.amm_metagraph.shared_data.services.pricing.PricingService
import org.amm_metagraph.shared_data.types.DataUpdates.{StakingUpdate, SwapUpdate, WithdrawalUpdate}
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.Staking.StakingReference
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.Swap.SwapReference
import org.amm_metagraph.shared_data.types.Withdrawal.WithdrawalReference
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import weaver.{Expectations, SimpleIOSuite}

object RandomPricingTest extends SimpleIOSuite {
  def logger[F[_]: Async]: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

  test("Random liquidity ops preserve accounting") {

    val N = 10000
    val rnd = new Random(3145L)

    def createNewAddress(): Address = {
      val arr = new Array[Byte](40)
      rnd.nextBytes(arr)
      Address.fromBytes(arr)
    }

    val accounting = mutable.Map.empty[Address, Long].withDefaultValue(0)

    def trackAdd(runner: PriceRunner[IO], addr: Address, primary: Boolean, amount: Double): IO[Unit] =
      for {
        amount <- runner.addShares(addr, primary, amount).value.flatMap {
          case Right((lp, shares)) =>
            logger[IO].trace(s"$addr: ADDED $shares shares --  $lp") >> shares.pure[IO]
          case Left(_) =>
            logger[IO].trace("ADD ERROR") >> 0L.pure[IO]
        }
        _ <- (accounting(addr) = accounting(addr) + amount).pure[IO]
      } yield ()

    def trackWithdraw(runner: PriceRunner[IO], addr: Address, forceAll: Boolean = false): IO[Unit] =
      for {
        sharesOpt <- runner.sharesForAddress(addr)
        _ <- sharesOpt match {
          case Some(shares) if shares.value.value.value > 0 =>
            val totalShares = shares.value.value.value
            val withdrawSharesAmount =
              if (rnd.nextBoolean() || forceAll) totalShares
              else rnd.between(1, totalShares.toInt).toLong

            runner.withdraw(addr, withdrawSharesAmount).value.flatMap {
              case Right((_, _)) =>
                logger[IO].trace(s"$addr: WITHDRAWN $withdrawSharesAmount shares from $totalShares") >>
                  IO(accounting(addr) -= withdrawSharesAmount)
              case Left(_) =>
                logger[IO].trace(s"$addr: WITHDRAWN ERROR") >> IO.unit
            }

          case _ => IO.unit
        }
      } yield ()

    def doRandomAction(runner: PriceRunner[IO]): IO[Unit] =
      rnd.nextInt(3) match {
        // --- Swap ---
        case 0 =>
          val swapValue = rnd.between(-50.0, 50.0)
          runner.doSwap(swapValue).value.flatMap {
            case Right(_) => logger[IO].trace(s"SWAP: $swapValue") >> IO.unit
            case Left(_)  => logger[IO].trace("SWAP ERROR") >> IO.unit
          }

        // --- Add liquidity ---
        case 1 =>
          val addr =
            if (accounting.isEmpty || rnd.nextBoolean()) createNewAddress()
            else accounting.keys.toVector(rnd.nextInt(accounting.size))
          trackAdd(runner, addr, rnd.nextBoolean(), rnd.between(1.0, 200.0))

        // --- Withdraw ---
        case 2 =>
          if (accounting.isEmpty) IO.unit
          else {
            val addr = accounting.keys.toVector(rnd.nextInt(accounting.size))
            trackWithdraw(runner, addr)
          }
      }

    val res: EitherT[IO, Any, Expectations] = for {
      runner <- EitherT.liftF(PriceRunner.make[IO](1000.0, 100.0))
      _ <- EitherT.liftF((1 to N).toList.traverse_ { _ =>
        for {
          _ <- doRandomAction(runner)
          lp <- runner.getLiquidityPool
          _ <- logger[IO].trace(s"$accounting")
          values <- accounting.keys.toList.traverse(address => runner.getTotalValueBySharesForAddress(address).map(address -> _))
          _ <- logger[IO].trace(s"$values")
          _ <- logger[IO].trace(s"${toDoublePoint(lp.tokenA.amount)}:${toDoublePoint(lp.tokenB.amount)}")
          _ <- logger[IO].trace(s"=========")
        } yield ()
      })
      _ <- EitherT.liftF(accounting.keys.toList.traverse_ { address =>
        trackWithdraw(runner, address, forceAll = true)
      })

      lp <- EitherT.liftF(runner.getLiquidityPool)
      _ <- EitherT.liftF(logger[IO].trace(s"${toDoublePoint(lp.tokenA.amount)}:${toDoublePoint(lp.tokenB.amount)}"))
      allSum = accounting.values.sum
      _ <- EitherT.liftF(logger[IO].trace(s"$accounting"))
      actualShares <- EitherT.liftF(
        accounting.keys.toList.traverse(address => runner.getTotalValueBySharesForAddress(address).map(_._1)).map(_.sum)
      )
    } yield expect.all(allSum == 0, actualShares == 0)

    res.value.map {
      case Left(e)      => failure(s"Test failed with error: $e")
      case Right(value) => value
    }
  }
}

case class PriceRunner[F[_]: Async](
  owner: Address,
  poolId: PoolId,
  tokenA: Option[CurrencyId],
  tokenB: Option[CurrencyId],
  calculatedStateService: CalculatedStateService[F],
  pricingService: PricingService[F]
) {
  def getLiquidityPool: F[LiquidityPool] =
    calculatedStateService.get.map(state => getLiquidityPoolCalculatedState(state.state).confirmed.value(poolId.value))

  def sharesForAddress(address: Address): F[Option[ShareAmount]] =
    getLiquidityPool.map(_.poolShares.addressShares.get(address))

  def getTotalValueBySharesForAddress(address: Address): F[(Long, Double, Double)] =
    for {
      liquidityPool <- getLiquidityPool
      totalShares = liquidityPool.poolShares.totalShares.value.toDouble
      sharesForAddressOpt <- sharesForAddress(address).map(_.map(_.value.value.value))
      percentage = sharesForAddressOpt match {
        case Some(value) if value > 0.0 => BigDecimal(value) / totalShares
        case _                          => BigDecimal(0)
      }
    } yield
      (
        sharesForAddressOpt.getOrElse(0),
        (toDoublePoint(liquidityPool.tokenA.amount) * percentage).toDouble,
        (toDoublePoint(liquidityPool.tokenB.amount) * percentage).toDouble
      )

  def totalShares: F[Long] = getLiquidityPool.map(_.poolShares.totalShares)

  def doSwap(swap: Double): EitherT[F, FailedCalculatedState, LiquidityPool] = {
    val pair = if (swap > 0) (tokenA, tokenB) else (tokenB, tokenA)
    val amount = SwapAmount(PosLong.unsafeFrom(Math.abs(toFixedPoint(swap))))

    val swapUpdate = getFakeSignedUpdate[SwapUpdate](
      SwapUpdate(
        CurrencyId(owner),
        sourceAddress,
        pair._1,
        pair._2,
        Hash.empty,
        amount,
        amount,
        none,
        EpochProgress.MaxValue,
        SwapReference.empty
      )
    )

    for {
      updatedTokenInformation <- EitherT(
        pricingService.getSwapTokenInfo(
          swapUpdate,
          Hash.empty,
          poolId,
          EpochProgress.MaxValue
        )
      )

      liquidityPool <- EitherT.liftF(getLiquidityPool)
      liquidityPoolUpdated <- EitherT.fromEither[F](
        pricingService.getUpdatedLiquidityPoolDueNewSwap(
          Hashed[SwapUpdate](swapUpdate, Hash.empty, ProofsHash("")),
          liquidityPool,
          updatedTokenInformation.primaryTokenInformationUpdated,
          updatedTokenInformation.pairTokenInformationUpdated,
          updatedTokenInformation.grossReceived,
          CurrencyId(owner)
        )
      )

      state <- EitherT.liftF[F, FailedCalculatedState, AmmCalculatedState](calculatedStateService.get.map(_.state))
      liquidityPoolsCalculatedState = getLiquidityPoolCalculatedState(state)
      newLiquidityPoolState =
        liquidityPoolsCalculatedState
          .focus(_.confirmed.value)
          .modify(_.updated(poolId.value, liquidityPoolUpdated))

      updatedCalculatedState = state
        .focus(_.operations)
        .modify(_.updated(OperationType.LiquidityPool, newLiquidityPoolState))

      _ <- EitherT.liftF(calculatedStateService.update(SnapshotOrdinal.MinValue, updatedCalculatedState))
    } yield liquidityPoolUpdated

  }

  def addShares(address: Address, primary: Boolean, amount: Double): EitherT[F, FailedCalculatedState, (LiquidityPool, Long)] = {

    val (token, otherToken) = if (primary) (tokenA, tokenB) else (tokenB, tokenA)
    val stakingUpdate = getFakeSignedUpdate[StakingUpdate](
      StakingUpdate(
        CurrencyId(owner),
        address,
        Hash.empty,
        Hash.empty,
        token,
        PosLong.unsafeFrom(toFixedPoint(amount)),
        otherToken,
        StakingReference.empty,
        EpochProgress.MinValue
      )
    )

    for {
      stakingTokenInfo <- EitherT(
        pricingService.getStakingTokenInfo(
          stakingUpdate,
          Hash.empty,
          poolId,
          EpochProgress.MaxValue
        )
      )

      liquidityPool <- EitherT.liftF(getLiquidityPool)
      liquidityPoolUpdated <- EitherT.fromEither[F](
        pricingService.getUpdatedLiquidityPoolDueStaking(
          liquidityPool,
          stakingUpdate,
          Hash.empty,
          address,
          stakingTokenInfo,
          EpochProgress.MinValue
        )
      )

      state <- EitherT.liftF[F, FailedCalculatedState, AmmCalculatedState](calculatedStateService.get.map(_.state))
      liquidityPoolsCalculatedState = getLiquidityPoolCalculatedState(state)
      newLiquidityPoolState =
        liquidityPoolsCalculatedState
          .focus(_.confirmed.value)
          .modify(_.updated(poolId.value, liquidityPoolUpdated))

      updatedCalculatedState = state
        .focus(_.operations)
        .modify(_.updated(OperationType.LiquidityPool, newLiquidityPoolState))

      _ <- EitherT.liftF(calculatedStateService.update(SnapshotOrdinal.MinValue, updatedCalculatedState))
    } yield (liquidityPoolUpdated, stakingTokenInfo.newlyIssuedShares)
  }

  def withdraw(address: Address, amount: Long): EitherT[F, FailedCalculatedState, (LiquidityPool, WithdrawalTokenInfo)] = {
    val withdrawUpdate = getFakeSignedUpdate[WithdrawalUpdate](
      WithdrawalUpdate(
        CurrencyId(owner),
        address,
        tokenA,
        tokenB,
        ShareAmount(Amount(NonNegLong.unsafeFrom(amount))),
        None,
        None,
        None,
        None,
        WithdrawalReference.empty,
        EpochProgress.MinValue
      )
    )

    for {
      liquidityPool <- EitherT.liftF(getLiquidityPool)

      withdrawalAmounts <- EitherT.fromEither[F](
        pricingService.getWithdrawalTokenInfo(
          withdrawUpdate,
          Hash.empty,
          liquidityPool,
          EpochProgress.MinValue
        )
      )

      updatedPool <- EitherT.fromEither[F](
        pricingService.getUpdatedLiquidityPoolDueNewWithdrawal(
          withdrawUpdate,
          Hash.empty,
          liquidityPool,
          withdrawalAmounts,
          EpochProgress.MinValue
        )
      )

      state <- EitherT.liftF[F, FailedCalculatedState, AmmCalculatedState](calculatedStateService.get.map(_.state))
      liquidityPoolsCalculatedState = getLiquidityPoolCalculatedState(state)
      newLiquidityPoolState = liquidityPoolsCalculatedState
        .focus(_.confirmed.value)
        .modify(_.updated(poolId.value, updatedPool))

      updatedCalculatedState = state
        .focus(_.operations)
        .modify(_.updated(OperationType.LiquidityPool, newLiquidityPoolState))

      _ <- EitherT.liftF(calculatedStateService.update(SnapshotOrdinal.MinValue, updatedCalculatedState))
    } yield (updatedPool, withdrawalAmounts)
  }
}

object PriceRunner {
  def make[F[_]: Async](
    tokenAAmount: Double,
    tokenBAmount: Double,
    ownerAddress: Address = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ"),
    additionalProvider: Option[(Address, ShareAmount)] = None,
    fees: FeePercentages = FeeDistributor.empty
  ): F[PriceRunner[F]] = {
    val primaryToken = TokenInformation(
      CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some,
      PosLong.unsafeFrom(toFixedPoint(tokenAAmount))
    )

    val pairToken = TokenInformation(
      CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some,
      PosLong.unsafeFrom(toFixedPoint(tokenBAmount))
    )

    val (poolId, liquidityPoolCalculatedState) =
      buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress, additionalProvider, fees)
    val ammOnChainState = AmmOnChainState.empty
    val ammCalculatedState = AmmCalculatedState(
      SortedMap(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      calculatedStateService <- CalculatedStateService.make[F]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, state.calculated)
      pricingService = PricingService.make[F](config, calculatedStateService)
    } yield
      PriceRunner[F](ownerAddress, PoolId(poolId), primaryToken.identifier, pairToken.identifier, calculatedStateService, pricingService)
  }
}
