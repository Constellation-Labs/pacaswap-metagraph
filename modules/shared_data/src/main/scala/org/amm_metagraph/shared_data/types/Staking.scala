package org.amm_metagraph.shared_data.types

import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.security.hash.Hash

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import org.amm_metagraph.shared_data.types.LiquidityPool.TokenInformation
import org.amm_metagraph.shared_data.types.States.{AmmCalculatedState, OperationType, StakingCalculatedState}

object Staking {
  @derive(encoder, decoder)
  case class StakingCalculatedStateLastReference(
    tokenAAllowSpend: Hash,
    tokenBAllowSpend: Hash,
    tokenA: TokenInformation,
    tokenB: TokenInformation,
    ordinal: SnapshotOrdinal
  )

  @derive(encoder, decoder)
  case class StakingCalculatedStateAddress(
    tokenAAllowSpend: Hash,
    tokenBAllowSpend: Hash,
    tokenA: TokenInformation,
    tokenB: TokenInformation,
    ordinal: SnapshotOrdinal,
    lastStakingUpdate: Option[StakingCalculatedStateLastReference]
  )

  def getStakingCalculatedState(
    calculatedState: AmmCalculatedState
  ): StakingCalculatedState =
    calculatedState.operations
      .get(OperationType.Staking)
      .collect { case t: StakingCalculatedState => t }
      .getOrElse(StakingCalculatedState.empty)
}
