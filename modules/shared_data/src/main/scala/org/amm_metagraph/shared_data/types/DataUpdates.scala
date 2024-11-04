package org.amm_metagraph.shared_data.types

import io.constellationnetwork.currency.dataApplication.DataUpdate
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.swap._
import io.constellationnetwork.security.hash.Hash

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import eu.timepit.refined.types.numeric.PosLong
import io.circe.refined._
import org.amm_metagraph.shared_data.types.LiquidityPool.TokenInformation

object DataUpdates {
  @derive(encoder, decoder)
  sealed trait AmmUpdate extends DataUpdate

  @derive(decoder, encoder)
  case class LiquidityPoolUpdate(
    tokenA: TokenInformation,
    tokenB: TokenInformation,
    feeRate: Double
  ) extends AmmUpdate

  @derive(decoder, encoder)
  case class StakingUpdate(
    tokenAAllowSpend: Hash,
    tokenBAllowSpend: Hash,
    tokenAId: Option[CurrencyId],
    tokenAAmount: PosLong,
    tokenBId: Option[CurrencyId]
  ) extends AmmUpdate

  @derive(decoder, encoder)
  case class SwapUpdate(
    swapFromToken: Option[CurrencyId],
    swapToToken: Option[CurrencyId],
    metagraphAddress: CurrencyId,
    fee: Long,
    reference: String,
    allowSpendReference: String,
    minAmount: PosLong,
    maxAmount: PosLong,
    maxValidGsOrdinal: SnapshotOrdinal,
    poolId: Option[String],
    minPrice: PosLong,
    maxPrice: PosLong
  ) extends AmmUpdate

  @derive(decoder, encoder)
  case class WithdrawUpdate(
    amount: PosLong
  ) extends AmmUpdate
}
