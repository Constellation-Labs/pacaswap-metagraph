package org.amm_metagraph.l0.custom_routes

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.swap.CurrencyId

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import io.circe.generic.auto._
import org.amm_metagraph.shared_data.calculated_state.CalculatedStateService
import org.amm_metagraph.shared_data.types.LiquidityPool.{LiquidityPool, getLiquidityPoolCalculatedState, getLiquidityPoolPrices}
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.Http4sDsl
import org.http4s.{HttpRoutes, Response}

object LiquidityPoolRoutes {

  @derive(encoder, decoder)
  case class TokenInfoResponse(
    id: Option[CurrencyId],
    amount: Long,
    price: Long
  )

  @derive(encoder, decoder)
  case class LiquidityPoolResponse(
    poolId: String,
    tokenA: TokenInfoResponse,
    tokenB: TokenInfoResponse,
    owner: Address,
    k: BigInt,
    totalShared: Long
  )

  object LiquidityPoolResponse {
    def from(pool: LiquidityPool): LiquidityPoolResponse = {
      val (priceTokenA, priceTokenB) = getLiquidityPoolPrices(pool)

      LiquidityPoolResponse(
        poolId = pool.poolId.value,
        tokenA = TokenInfoResponse(pool.tokenA.identifier, pool.tokenA.amount.value, priceTokenA),
        tokenB = TokenInfoResponse(pool.tokenB.identifier, pool.tokenB.amount.value, priceTokenB),
        owner = pool.owner,
        k = pool.k,
        totalShared = pool.poolShares.totalShares.value
      )
    }
  }
}

case class LiquidityPoolRoutes[F[_]: Async](
  calculatedStateService: CalculatedStateService[F]
) extends Http4sDsl[F] {
  import LiquidityPoolRoutes._
  import Pagination._
  import Responses._

  private def getLiquidityPools(
    pagination: PaginationParams
  ): F[(List[LiquidityPoolResponse], PaginationResponse)] =
    calculatedStateService.get.map { calculatedState =>
      val liquidityPoolCalculatedState = getLiquidityPoolCalculatedState(calculatedState.state)

      val allLiquidityPools = liquidityPoolCalculatedState.confirmed.value.values
      val filteredLPs = allLiquidityPools
        .slice(pagination.offset, pagination.offset + pagination.limit)
        .map(LiquidityPoolResponse.from)
        .toList

      val allLPsSize = allLiquidityPools.size
      (
        filteredLPs,
        PaginationResponse(allLPsSize, pagination.limit, pagination.offset, filteredLPs.length < allLPsSize)
      )
    }

  private def getLiquidityPoolByPoolId(
    poolId: String
  ): F[Option[LiquidityPoolResponse]] =
    calculatedStateService.get.map { calculatedState =>
      val liquidityPoolCalculatedState = getLiquidityPoolCalculatedState(calculatedState.state)

      liquidityPoolCalculatedState.confirmed.value.get(poolId).fold(none[LiquidityPoolResponse])(LiquidityPoolResponse.from(_).some)
    }

  private def handleGetLiquidityPools(
    maybeLimit: Option[String],
    maybeOffset: Option[String]
  ): F[Response[F]] = {
    val pagination = PaginationParams.fromStrings(maybeLimit, maybeOffset)

    getLiquidityPools(pagination).flatMap {
      case (data, meta) =>
        Ok(PaginatedResponse(data, meta))
    }
  }

  private def handleGetLiquidityPoolByPoolId(
    poolId: String
  ): F[Response[F]] =
    getLiquidityPoolByPoolId(poolId).flatMap { maybeLiquidityPool =>
      maybeLiquidityPool.fold(NotFound()) { liquidityPool =>
        Ok(SingleResponse(liquidityPool))
      }
    }

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ GET -> Root / "liquidity-pools" =>
      handleGetLiquidityPools(
        req.params.get("limit"),
        req.params.get("offset")
      )
    case GET -> Root / "liquidity-pools" / poolId =>
      handleGetLiquidityPoolByPoolId(
        poolId
      )
  }
}
