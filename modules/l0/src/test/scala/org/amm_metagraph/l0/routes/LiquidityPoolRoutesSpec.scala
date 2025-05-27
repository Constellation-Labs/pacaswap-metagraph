package org.amm_metagraph.l0.routes

import cats.Eq
import cats.effect.{IO, Resource}
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.ext.cats.effect.ResourceIO
import io.constellationnetwork.json.JsonSerializer
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.swap.CurrencyId
import io.constellationnetwork.security.{Hasher, SecurityProvider}

import eu.timepit.refined.auto._
import org.amm_metagraph.l0.Shared._
import org.amm_metagraph.l0.custom_routes.LiquidityPoolRoutes
import org.amm_metagraph.l0.custom_routes.Pagination.PaginationParams
import org.amm_metagraph.shared_data.calculated_state.CalculatedStateService
import org.amm_metagraph.shared_data.services.pricing.PricingService
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.codecs
import weaver.MutableIOSuite

object LiquidityPoolRoutesSpec extends MutableIOSuite {
  type Res = (Hasher[IO], codecs.HasherSelector[IO], SecurityProvider[IO])

  override def sharedResource: Resource[IO, Res] = for {
    sp <- SecurityProvider.forAsync[IO]
    implicit0(j: JsonSerializer[IO]) <- JsonSerializer.forSync[IO].asResource
    h = Hasher.forJson[IO]
    hs = codecs.HasherSelector.forSync(h, h)
  } yield (h, hs, sp)

  implicit val eqDataApplicationValidationErrorOr: Eq[DataApplicationValidationErrorOr[Unit]] =
    Eq.fromUniversalEquals

  test("Should get all LP correctly") { implicit res =>
    val primaryToken = TokenInformation(CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some, 100L)
    val pairToken = TokenInformation(CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some, 50L)
    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")

    val (_, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )

    for {
      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, ammCalculatedState)
      pricingService = PricingService.make[IO](config, calculatedStateService)
      liquidityPoolRoutes = LiquidityPoolRoutes(calculatedStateService, pricingService)
      paginationParams = PaginationParams(10, 0)
      (liquidityPools, _) <- liquidityPoolRoutes.getLiquidityPools(paginationParams, none)
    } yield
      expect.all(
        liquidityPools.size === 1,
        liquidityPools.head.poolId === liquidityPoolCalculatedState.confirmed.value.head._2.poolId.value
      )
  }

  test("Should return empty pools when provided address don't have shares") { implicit res =>
    val primaryToken = TokenInformation(CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some, 100L)
    val pairToken = TokenInformation(CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some, 50L)
    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")

    val (_, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )

    for {
      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, ammCalculatedState)
      pricingService = PricingService.make[IO](config, calculatedStateService)
      liquidityPoolRoutes = LiquidityPoolRoutes(calculatedStateService, pricingService)
      paginationParams = PaginationParams(10, 0)
      (liquidityPools, _) <- liquidityPoolRoutes.getLiquidityPools(
        paginationParams,
        Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb").some
      )
    } yield
      expect.all(
        liquidityPools.isEmpty
      )
  }

  test("Should return LP when provided address have shares") { implicit res =>
    val primaryToken = TokenInformation(CurrencyId(Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")).some, 100L)
    val pairToken = TokenInformation(CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9w5")).some, 50L)
    val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")

    val (_, liquidityPoolCalculatedState) = buildLiquidityPoolCalculatedState(primaryToken, pairToken, ownerAddress)
    val ammCalculatedState = AmmCalculatedState(
      Map(OperationType.LiquidityPool -> liquidityPoolCalculatedState)
    )

    for {
      calculatedStateService <- CalculatedStateService.make[IO]
      _ <- calculatedStateService.update(SnapshotOrdinal.MinValue, ammCalculatedState)
      pricingService = PricingService.make[IO](config, calculatedStateService)
      liquidityPoolRoutes = LiquidityPoolRoutes(calculatedStateService, pricingService)
      paginationParams = PaginationParams(10, 0)
      (liquidityPools, _) <- liquidityPoolRoutes.getLiquidityPools(paginationParams, ownerAddress.some)
    } yield
      expect.all(
        liquidityPools.size === 1,
        liquidityPools.head.poolId === liquidityPoolCalculatedState.confirmed.value.head._2.poolId.value
      )
  }
}
