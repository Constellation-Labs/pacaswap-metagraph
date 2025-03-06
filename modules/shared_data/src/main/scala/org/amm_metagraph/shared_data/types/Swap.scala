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
import org.amm_metagraph.shared_data.types.States._

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
    state: AmmCalculatedState
  ): SwapCalculatedState =
    state.operations
      .get(OperationType.Swap)
      .collect { case t: SwapCalculatedState => t }
      .getOrElse(SwapCalculatedState.empty)

  def getPendingAllowSpendsSwapUpdates(
    state: AmmCalculatedState
  ): Set[Signed[SwapUpdate]] =
    getSwapCalculatedState(state).pending.collect {
      case PendingAllowSpend(signedUpdate: Signed[SwapUpdate]) => signedUpdate
    }

  def getPendingSpendActionSwapUpdates(
    state: AmmCalculatedState
  ): Set[PendingSpendAction[SwapUpdate]] =
    getSwapCalculatedState(state).pending.collect {
      case pendingSpend: PendingSpendAction[SwapUpdate] => pendingSpend
    }
}
