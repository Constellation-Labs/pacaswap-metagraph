package org.amm_metagraph.shared_data.types

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import eu.timepit.refined.types.numeric.PosLong
import io.circe.refined._
import org.amm_metagraph.shared_data.types.LiquidityPool.TokenInformation
import org.tessellation.currency.dataApplication.DataUpdate
import org.tessellation.schema.SnapshotOrdinal
import org.tessellation.schema.address.Address


object DataUpdates {
  @derive(encoder, decoder)
  sealed trait AmmUpdate extends DataUpdate

  @derive(decoder, encoder)
  case class LiquidityPoolUpdate(
    tokenA : TokenInformation,
    tokenB : TokenInformation,
    feeRate: Double,
  ) extends AmmUpdate

  @derive(decoder, encoder)
  case class StakingUpdate(
    primaryAllowSpendReferenceTxnId: String,
    pairAllowSpendReferenceTxnId   : String,
    primaryTokenId                 : Option[Address],
    primaryTokenAmount             : PosLong,
    pairTokenId                    : Option[Address],
  ) extends AmmUpdate

  @derive(decoder, encoder)
  case class SwapUpdate(
    swapFromToken      : Option[Address],
    swapToToken        : Option[Address],
    metagraphAddress   : Address,
    fee                : Long,
    reference          : String,
    allowSpendReference: String,
    minAmount          : PosLong,
    maxAmount          : PosLong,
    maxValidGsOrdinal  : SnapshotOrdinal,
    poolId             : Option[String],
    minPrice           : PosLong,
    maxPrice           : PosLong
  ) extends AmmUpdate


  @derive(decoder, encoder)
  case class WithdrawUpdate(
    amount: PosLong
  ) extends AmmUpdate
}
