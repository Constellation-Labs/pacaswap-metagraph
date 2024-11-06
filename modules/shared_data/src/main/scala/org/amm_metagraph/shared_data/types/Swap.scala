package org.amm_metagraph.shared_data.types

import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.SwapAmount
import io.constellationnetwork.security.hash.Hash

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import eu.timepit.refined.types.numeric.{NonNegLong, PosLong}
import io.circe.refined._
import org.amm_metagraph.shared_data.types.LiquidityPool._

object Swap {

  @derive(encoder, decoder)
  case class SwapCalculatedStateLastReference(
    fromToken: TokenInformation,
    toToken: TokenInformation,
    fee: NonNegLong,
    reference: String,
    allowSpendReference: Hash,
    minAmount: SwapAmount,
    maxAmount: SwapAmount,
    maxValidGsEpochProgress: EpochProgress,
    poolId: Option[PoolId],
    minPrice: Option[PosLong],
    maxPrice: Option[PosLong],
    ordinal: SnapshotOrdinal
  )

  @derive(encoder, decoder)
  case class SwapCalculatedStateAddress(
    fromToken: TokenInformation,
    toToken: TokenInformation,
    fee: NonNegLong,
    reference: String,
    allowSpendReference: Hash,
    minAmount: SwapAmount,
    maxAmount: SwapAmount,
    maxValidGsEpochProgress: EpochProgress,
    poolId: Option[PoolId],
    minPrice: Option[PosLong],
    maxPrice: Option[PosLong],
    ordinal: SnapshotOrdinal,
    lastSwapUpdate: Option[SwapCalculatedStateLastReference]
  )

}
