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
import org.amm_metagraph.shared_data.types.States.{AmmCalculatedState, OperationType, SwapCalculatedState}

object Swap {
  @derive(encoder, decoder)
  case class SwapCalculatedStateAddress(
    fromToken: TokenInformation,
    toToken: TokenInformation,
    fee: NonNegLong,
    allowSpendReference: Hash,
    minAmount: SwapAmount,
    maxAmount: SwapAmount,
    maxValidGsEpochProgress: EpochProgress,
    poolId: Option[PoolId],
    minPrice: Option[PosLong],
    maxPrice: Option[PosLong],
    ordinal: SnapshotOrdinal
  )

  def getSwapCalculatedState(
    calculatedState: AmmCalculatedState
  ): SwapCalculatedState =
    calculatedState.operations
      .get(OperationType.Swap)
      .collect { case t: SwapCalculatedState => t }
      .getOrElse(SwapCalculatedState.empty)
}
