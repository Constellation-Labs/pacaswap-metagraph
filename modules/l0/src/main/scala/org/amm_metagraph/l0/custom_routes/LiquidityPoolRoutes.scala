package org.amm_metagraph.l0.custom_routes

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.ext.http4s.{AddressVar, HashVar}
import io.constellationnetwork.schema.address.{Address, DAGAddressRefined}
import io.constellationnetwork.schema.swap.CurrencyId
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import eu.timepit.refined.refineV
import eu.timepit.refined.types.numeric.PosLong
import io.circe.generic.auto._
import io.circe.refined._
import org.amm_metagraph.shared_data.FeeDistributor.FeePercentages
import org.amm_metagraph.shared_data.calculated_state.CalculatedStateService
import org.amm_metagraph.shared_data.services.pricing.PricingService
import org.amm_metagraph.shared_data.types.DataUpdates.LiquidityPoolUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States.StateTransitionType._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.codecs.HasherSelector
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
    totalShared: Long,
    poolFees: FeePercentages
  )

  private object LiquidityPoolResponse {
    def from(pool: LiquidityPool, priceTokenA: Long, priceTokenB: Long): LiquidityPoolResponse =
      LiquidityPoolResponse(
        poolId = pool.poolId.value,
        tokenA = TokenInfoResponse(pool.tokenA.identifier, pool.tokenA.amount.value, priceTokenA),
        tokenB = TokenInfoResponse(pool.tokenB.identifier, pool.tokenB.amount.value, priceTokenB),
        owner = pool.owner,
        k = pool.k,
        totalShared = pool.poolShares.totalShares.value,
        poolFees = pool.poolFees
      )
  }

  @derive(encoder, decoder)
  case class LiquidityPoolStateResponse(
    owner: Option[Address],
    tokenAId: Option[CurrencyId],
    tokenBId: Option[CurrencyId],
    tokenAAmount: Option[PosLong],
    tokenBAmount: Option[PosLong],
    poolFees: Option[FeePercentages],
    state: StateTransitionType
  )

  object LiquidityPoolStateResponse {
    def from(update: Signed[LiquidityPoolUpdate], stateTransitionType: StateTransitionType): LiquidityPoolStateResponse =
      LiquidityPoolStateResponse(
        update.source.some,
        update.tokenAId,
        update.tokenBId,
        update.tokenAAmount.some,
        update.tokenBAmount.some,
        update.poolFees,
        stateTransitionType
      )
    def from(liquidityPool: LiquidityPool, stateTransitionType: StateTransitionType): LiquidityPoolStateResponse =
      LiquidityPoolStateResponse(
        liquidityPool.owner.some,
        liquidityPool.tokenA.identifier,
        liquidityPool.tokenB.identifier,
        liquidityPool.tokenA.amount.some,
        liquidityPool.tokenB.amount.some,
        liquidityPool.poolFees.some,
        stateTransitionType
      )
    def from(stateTransitionType: StateTransitionType): LiquidityPoolStateResponse =
      LiquidityPoolStateResponse(
        none,
        none,
        none,
        none,
        none,
        none,
        stateTransitionType
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

case class LiquidityPoolRoutes[F[_]: Async: HasherSelector](
  calculatedStateService: CalculatedStateService[F],
  pricingService: PricingService[F]
) extends Http4sDsl[F] {
  import LiquidityPoolRoutes._
  import Pagination._
  import Responses._

  def getLiquidityPools(
    pagination: PaginationParams,
    maybeAddress: Option[Address],
    maybeTokenId: Option[Address],
    shouldSearchDAG: Boolean
  ): F[(List[LiquidityPoolResponse], PaginationResponse)] = for {
    calculatedState <- calculatedStateService.get
    liquidityPools = extractLiquidityPools(calculatedState.state)

    filteredPools = applyFilters(liquidityPools, maybeAddress, maybeTokenId, shouldSearchDAG)
    paginatedPools = applyPagination(filteredPools, pagination)

    poolResponses <- enrichWithPricing(paginatedPools)
    paginationResponse = createPaginationResponse(filteredPools.size, pagination, poolResponses.size)

  } yield (poolResponses, paginationResponse)

  private def extractLiquidityPools(state: AmmCalculatedState): List[LiquidityPool] = {
    val liquidityPoolState = getLiquidityPoolCalculatedState(state)
    liquidityPoolState.confirmed.value.values.toList
  }

  private def applyFilters(
    pools: List[LiquidityPool],
    maybeAddress: Option[Address],
    maybeTokenId: Option[Address],
    shouldSearchDAG: Boolean
  ): List[LiquidityPool] = {
    val addressFiltered = filterByAddress(pools, maybeAddress)
    filterByToken(addressFiltered, maybeTokenId, shouldSearchDAG)
  }

  private def filterByAddress(
    pools: List[LiquidityPool],
    maybeAddress: Option[Address]
  ): List[LiquidityPool] =
    maybeAddress.fold(pools) { address =>
      pools.filter(_.poolShares.addressShares.contains(address))
    }

  private def filterByToken(
    pools: List[LiquidityPool],
    maybeTokenId: Option[Address],
    shouldSearchDAG: Boolean
  ): List[LiquidityPool] =
    if (shouldSearchDAG) {
      pools.filter(pool => pool.tokenA.identifier.isEmpty || pool.tokenB.identifier.isEmpty)
    } else {
      maybeTokenId.fold(pools) { tokenId =>
        pools.filter(pool =>
          pool.tokenA.identifier.exists(_.value === tokenId) ||
            pool.tokenB.identifier.exists(_.value === tokenId)
        )
      }
    }

  private def applyPagination(
    pools: List[LiquidityPool],
    pagination: PaginationParams
  ): List[LiquidityPool] =
    pools
      .slice(pagination.offset, pagination.offset + pagination.limit)

  private def enrichWithPricing(pools: List[LiquidityPool]): F[List[LiquidityPoolResponse]] =
    pools.traverse(enrichPoolWithPricing).map(_.flatten)

  private def enrichPoolWithPricing(pool: LiquidityPool): F[Option[LiquidityPoolResponse]] =
    pricingService
      .getLiquidityPoolPrices(pool.poolId)
      .map {
        case Right((tokenAPrice, tokenBPrice)) =>
          Some(LiquidityPoolResponse.from(pool, tokenAPrice, tokenBPrice))
        case Left(_) => None
      }

  private def createPaginationResponse(
    totalCount: Int,
    pagination: PaginationParams,
    returnedCount: Int
  ): PaginationResponse =
    PaginationResponse(
      total = totalCount,
      limit = pagination.limit,
      offset = pagination.offset,
      has_more = returnedCount < totalCount
    )

  private def getLiquidityPoolByPoolId(
    poolId: String
  ): F[Option[LiquidityPool]] = for {
    calculatedState <- calculatedStateService.get
    poolIdFormatted = poolId
      .split("-")
      .filter(_ != null)
      .sorted
      .mkString("-")

    confirmedLpCalculatedState = getLiquidityPoolCalculatedState(calculatedState.state).confirmed
    result = confirmedLpCalculatedState.value.get(poolIdFormatted)
  } yield result

  private def handleGetLiquidityPools(
    maybeLimit: Option[String],
    maybeOffset: Option[String],
    addressString: Option[String],
    tokenIdString: Option[String]
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

    val (shouldSearchDAG, maybeTokenId) = tokenIdString match {
      case Some(tokenIdStr) if tokenIdStr === "DAG" =>
        (true, none)
      case Some(tokenIdStr) =>
        refineV[DAGAddressRefined](tokenIdStr) match {
          case Right(validAddress) => (false, Some(Address(validAddress)))
          case Left(_)             => return BadRequest("Invalid address format")
        }
      case None => (false, none)
    }

    getLiquidityPools(pagination, maybeAddress, maybeTokenId, shouldSearchDAG).flatMap {
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

  private def getLiquidityPoolByHash(liquidityPoolHash: Hash): F[Response[F]] =
    for {
      calculatedState <- calculatedStateService.get
      liquidityPoolState = getLiquidityPoolCalculatedState(calculatedState.state)
      result <- findLiquidityPoolByHash(liquidityPoolState, liquidityPoolHash)
    } yield result

  private def findLiquidityPoolByHash(
    state: LiquidityPoolCalculatedState,
    hash: Hash
  ): F[Response[F]] =
    state.pending.collectFirst {
      case pending: PendingAllowSpend[LiquidityPoolUpdate] if pending.updateHash === hash =>
        SingleResponse(LiquidityPoolStateResponse.from(pending.update, PendingAllowSpends))
    }.orElse {
      state.pending.collectFirst {
        case pending: PendingSpendAction[LiquidityPoolUpdate] if pending.updateHash === hash =>
          SingleResponse(LiquidityPoolStateResponse.from(pending.update, PendingSpendTransactions))
      }
    }.orElse {
      state.confirmed.value.collectFirst {
        case (_, liquidityPool) if liquidityPool.updateHash === hash =>
          SingleResponse(LiquidityPoolStateResponse.from(liquidityPool, Confirmed))
      }
    }.orElse {
      state.failed.collectFirst {
        case signed if signed.updateHash === hash =>
          SingleResponse(LiquidityPoolStateResponse.from(Failed))
      }
    }.map(Ok(_))
      .getOrElse(NotFound())

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "liquidity-pools" / HashVar(lpHashString) / "state" => getLiquidityPoolByHash(lpHashString)
    case req @ GET -> Root / "liquidity-pools" =>
      handleGetLiquidityPools(
        req.params.get("limit"),
        req.params.get("offset"),
        req.params.get("address"),
        req.params.get("tokenId")
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
