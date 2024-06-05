package org.amm_metagraph.shared_data.types

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import eu.timepit.refined.types.numeric.PosLong
import io.circe.refined._
import org.tessellation.schema.address.Address


object LiquidityPool {
  @derive(encoder, decoder)
  case class TokenInformation(
    identifier: Option[Address],
    amount    : PosLong,
  )

  @derive(encoder, decoder)
  case class LiquidityProviders(
    providers: Map[Address, Long]
  )

  @derive(encoder, decoder)
  case class LiquidityPool(
    poolId            : String,
    tokenA            : TokenInformation,
    tokenB            : TokenInformation,
    owner             : Address,
    k                 : Double,
    feeRate           : Double,
    totalLiquidity    : Long,
    liquidityProviders: LiquidityProviders
  )

}
