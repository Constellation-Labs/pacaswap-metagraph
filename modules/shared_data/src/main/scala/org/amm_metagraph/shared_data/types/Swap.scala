package org.amm_metagraph.shared_data.types

import io.constellationnetwork.schema.SnapshotOrdinal

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import eu.timepit.refined.types.numeric.PosLong
import io.circe.refined._
import org.amm_metagraph.shared_data.types.LiquidityPool.TokenInformation

object Swap {

  @derive(encoder, decoder)
  case class SwapCalculatedStateLastReference(
    fromToken: TokenInformation,
    toToken: TokenInformation,
    fee: Long,
    reference: String,
    allowSpendReference: String,
    minAmount: PosLong,
    maxAmount: PosLong,
    maxValidGsOrdinal: SnapshotOrdinal,
    poolId: Option[String],
    minPrice: PosLong,
    maxPrice: PosLong,
    ordinal: SnapshotOrdinal
  )

  @derive(encoder, decoder)
  case class SwapCalculatedStateAddress(
    fromToken: TokenInformation,
    toToken: TokenInformation,
    fee: Long,
    reference: String,
    allowSpendReference: String,
    minAmount: PosLong,
    maxAmount: PosLong,
    maxValidGsOrdinal: SnapshotOrdinal,
    poolId: Option[String],
    minPrice: PosLong,
    maxPrice: PosLong,
    ordinal: SnapshotOrdinal,
    lastSwapUpdate: Option[SwapCalculatedStateLastReference]
  )

}
