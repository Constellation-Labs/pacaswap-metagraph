package org.amm_metagraph.shared_data.types

import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.SwapAmount
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import eu.timepit.refined.types.numeric.PosLong
import io.circe.refined._
import org.amm_metagraph.shared_data.types.DataUpdates.SwapUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States.{AmmCalculatedState, OperationType, SwapCalculatedState}

object Swap {
  @derive(encoder, decoder)
  case class SwapCalculatedStateAddress(
    sourceAddress: Address,
    fromToken: TokenInformation,
    toToken: TokenInformation,
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
    calculatedState.confirmedOperations
      .get(OperationType.Swap)
      .collect { case t: SwapCalculatedState => t }
      .getOrElse(SwapCalculatedState.empty)

  def getPendingSwapUpdates(
    state: AmmCalculatedState
  ): Set[Signed[SwapUpdate]] =
    state.pendingUpdates.collect {
      case pendingUpdate @ Signed(swapUpdate: SwapUpdate, _) =>
        Signed(swapUpdate, pendingUpdate.proofs)
    }
}
