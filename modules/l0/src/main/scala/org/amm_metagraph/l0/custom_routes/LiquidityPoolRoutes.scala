package org.amm_metagraph.l0.custom_routes

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.ext.http4s.AddressVar
import io.constellationnetwork.schema.address.{Address, DAGAddressRefined}
import io.constellationnetwork.schema.swap.CurrencyId

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import eu.timepit.refined.refineV
import io.circe.generic.auto._
import org.amm_metagraph.shared_data.calculated_state.CalculatedStateService
import org.amm_metagraph.shared_data.services.pricing.PricingService
import org.amm_metagraph.shared_data.types.LiquidityPool._
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

  private object LiquidityPoolResponse {
    def from(pool: LiquidityPool, priceTokenA: Long, priceTokenB: Long): LiquidityPoolResponse =
      LiquidityPoolResponse(
        poolId = pool.poolId.value,
        tokenA = TokenInfoResponse(pool.tokenA.identifier, pool.tokenA.amount.value, priceTokenA),
        tokenB = TokenInfoResponse(pool.tokenB.identifier, pool.tokenB.amount.value, priceTokenB),
        owner = pool.owner,
        k = pool.k,
        totalShared = pool.poolShares.totalShares.value
      )
  }

  @derive(encoder, decoder)
  case class LiquidityPoolSharesResponse(
    poolId: PoolId,
    address: Address,
    shares: ShareAmount
  )

  private object LiquidityPoolSharesResponse {
    def from(pool: LiquidityPool, address: Address): Option[LiquidityPoolSharesResponse] =
      pool.poolShares.addressShares.get(address).map(shares => LiquidityPoolSharesResponse(pool.poolId, address, shares))
  }
}

case class LiquidityPoolRoutes[F[_]: Async](
  calculatedStateService: CalculatedStateService[F],
  pricingService: PricingService[F]
) extends Http4sDsl[F] {
  import LiquidityPoolRoutes._
  import Pagination._
  import Responses._

  def getLiquidityPools(
    pagination: PaginationParams,
    maybeAddress: Option[Address]
  ): F[(List[LiquidityPoolResponse], PaginationResponse)] = for {
    calculatedState <- calculatedStateService.get
    liquidityPoolCalculatedState = getLiquidityPoolCalculatedState(calculatedState.state)
    allLiquidityPools = liquidityPoolCalculatedState.confirmed.value.values

    allLiquidityPoolFiltered = maybeAddress match {
      case None          => allLiquidityPools
      case Some(address) => allLiquidityPools.filter(_.poolShares.addressShares.contains(address))
    }

    filteredLPs <- allLiquidityPoolFiltered
      .slice(pagination.offset, pagination.offset + pagination.limit)
      .toList
      .traverse { lp =>
        pricingService
          .getLiquidityPoolPrices(lp.poolId)
          .map {
            case Right((tokenAPrice, tokenBPrice)) if tokenAPrice > 0 && tokenBPrice > 0 =>
              Some(LiquidityPoolResponse.from(lp, tokenAPrice, tokenBPrice))
            case _ => None
          }
      }
      .map(_.flatten)

    allLpSize = allLiquidityPools.size
    response = (
      filteredLPs,
      PaginationResponse(allLpSize, pagination.limit, pagination.offset, filteredLPs.length < allLpSize)
    )

  } yield response

  private def getLiquidityPoolByPoolId(
    poolId: String
  ): F[Option[LiquidityPool]] =
    calculatedStateService.get.map { calculatedState =>
      getLiquidityPoolCalculatedState(calculatedState.state).confirmed.value.get(poolId)
    }

  private def handleGetLiquidityPools(
    maybeLimit: Option[String],
    maybeOffset: Option[String],
    addressString: Option[String]
  ): F[Response[F]] = {
    val pagination = PaginationParams.fromStrings(maybeLimit, maybeOffset)

    val maybeAddress = addressString match {
      case Some(addressStr) =>
        refineV[DAGAddressRefined](addressStr) match {
          case Right(validAddress) => Some(Address(validAddress))
          case Left(_)             => return BadRequest("Invalid address format")
        }
      case None => None
    }

    getLiquidityPools(pagination, maybeAddress).flatMap {
      case (data, meta) =>
        Ok(PaginatedResponse(data, meta))
    }
  }

  private def handleGetLiquidityPoolByPoolId(
    poolId: String
  ): F[Response[F]] =
    getLiquidityPoolByPoolId(poolId).flatMap {
      case Some(pool) =>
        pricingService
          .getLiquidityPoolPrices(pool.poolId)
          .flatMap {
            case Right((tokenAPrice, tokenBPrice)) if tokenAPrice > 0 && tokenBPrice > 0 =>
              Ok(SingleResponse(LiquidityPoolResponse.from(pool, tokenAPrice, tokenBPrice)))
            case _ =>
              NotFound()
          }
      case None =>
        NotFound()
    }

  private def handleGetLiquidityPoolShares(
    poolId: String,
    address: Address
  ): F[Response[F]] =
    getLiquidityPoolByPoolId(poolId).flatMap {
      case Some(pool) =>
        LiquidityPoolSharesResponse.from(pool, address) match {
          case Some(shares) => Ok(SingleResponse(shares))
          case None         => Ok(SingleResponse(LiquidityPoolSharesResponse(pool.poolId, address, ShareAmount.empty)))
        }
      case None =>
        NotFound()
    }

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ GET -> Root / "liquidity-pools" =>
      handleGetLiquidityPools(
        req.params.get("limit"),
        req.params.get("offset"),
        req.params.get("address")
      )
    case GET -> Root / "liquidity-pools" / poolId =>
      handleGetLiquidityPoolByPoolId(
        poolId
      )
    case GET -> Root / "liquidity-pools" / poolId / "shares" / AddressVar(address) =>
      handleGetLiquidityPoolShares(
        poolId,
        address
      )
  }
}
