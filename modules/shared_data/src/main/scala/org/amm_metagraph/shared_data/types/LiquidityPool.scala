package org.amm_metagraph.shared_data.types

import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.swap.CurrencyId
import io.constellationnetwork.security.signature.Signed

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import eu.timepit.refined.types.numeric.PosLong
import io.circe.refined._
import io.estatico.newtype.macros.newtype
import org.amm_metagraph.shared_data.types.DataUpdates.LiquidityPoolUpdate
import org.amm_metagraph.shared_data.types.States._

object LiquidityPool {
  @derive(encoder, decoder)
  @newtype
  case class PoolId(value: String)

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
    poolId: PoolId,
    tokenA: TokenInformation,
    tokenB: TokenInformation,
    owner: Address,
    k: Double,
    totalLiquidity: Long,
    liquidityProviders: LiquidityProviders
  )

  def getLiquidityPools(state: AmmCalculatedState): Map[String, LiquidityPool] =
    state.confirmedOperations.get(OperationType.LiquidityPool).fold(Map.empty[String, LiquidityPool]) {
      case liquidityPoolsCalculatedState: LiquidityPoolCalculatedState => liquidityPoolsCalculatedState.liquidityPools
      case _                                                           => Map.empty
    }

  def getLiquidityPoolCalculatedState(
    calculatedState: AmmCalculatedState
  ): LiquidityPoolCalculatedState =
    calculatedState.confirmedOperations
      .get(OperationType.Staking)
      .collect { case t: LiquidityPoolCalculatedState => t }
      .getOrElse(LiquidityPoolCalculatedState.empty)

  def getPendingLiquidityPoolUpdates(
    state: AmmCalculatedState
  ): Set[Signed[LiquidityPoolUpdate]] =
    state.pendingUpdates.collect {
      case pendingUpdate @ Signed(liquidityPoolUpdate: LiquidityPoolUpdate, _) =>
        Signed(liquidityPoolUpdate, pendingUpdate.proofs)
    }
}
