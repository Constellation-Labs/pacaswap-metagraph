package org.amm_metagraph.shared_data.types

import cats.effect.Async

import io.constellationnetwork.currency.dataApplication.DataState
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.swap.CurrencyId

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import eu.timepit.refined.types.numeric.PosLong
import io.circe.refined._
import org.amm_metagraph.shared_data.types.States._

object LiquidityPool {
  @derive(encoder, decoder)
  case class TokenInformation(
    identifier: Option[CurrencyId],
    amount: PosLong
  )

  @derive(encoder, decoder)
  case class LiquidityProviders(
    providers: Map[Address, Long]
  )

  @derive(encoder, decoder)
  case class LiquidityPool(
    poolId: String,
    tokenA: TokenInformation,
    tokenB: TokenInformation,
    owner: Address,
    k: Double,
    feeRate: Double,
    totalLiquidity: Long,
    liquidityProviders: LiquidityProviders
  )

  def getLiquidityPools[F[_]: Async](state: DataState[AmmOnChainState, AmmCalculatedState]): Map[String, LiquidityPool] =
    state.calculated.ammState.get(OperationType.LiquidityPool).fold(Map.empty[String, LiquidityPool]) {
      case liquidityPoolsCalculatedState: LiquidityPoolCalculatedState => liquidityPoolsCalculatedState.liquidityPools
      case _                                                           => Map.empty
    }
}
