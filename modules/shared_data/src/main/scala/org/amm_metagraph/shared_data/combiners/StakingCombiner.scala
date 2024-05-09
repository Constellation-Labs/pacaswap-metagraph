package org.amm_metagraph.shared_data.combiners

import cats.syntax.option.{catsSyntaxOptionId, none}
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, AmmUpdateProof, StakingUpdate}
import org.amm_metagraph.shared_data.types.States._
import org.tessellation.currency.dataApplication.DataState
import org.tessellation.schema.SnapshotOrdinal
import org.tessellation.schema.address.Address

object StakingCombiner {
  def combineStaking(
    acc                   : DataState[AmmOnChainState, AmmCalculatedState],
    stakingUpdate         : StakingUpdate,
    signerAddress         : Address,
    ammUpdateProof        : AmmUpdateProof,
    currentSnapshotOrdinal: SnapshotOrdinal
  ): DataState[AmmOnChainState, AmmCalculatedState] = {
    val addressInfo = acc.calculated.addresses.getOrElse(signerAddress, Map.empty[String, AmmOffChainState])
    val maybeLastStakingInfo = addressInfo.get(Staking) match {
      case Some(stakingState: StakingCalculatedState) =>
        StakingCalculatedStateLastReference(
          stakingState.txnId,
          stakingState.originAddress,
          stakingState.proof,
          stakingState.amount,
          stakingState.ordinal
        ).some

      case _ => none
    }

    val stakingCalculatedState = StakingCalculatedState(
      stakingUpdate.txnId,
      signerAddress,
      ammUpdateProof,
      stakingUpdate.amount,
      currentSnapshotOrdinal,
      maybeLastStakingInfo
    )

    val updatedAddressInfo = addressInfo + (Staking -> stakingCalculatedState)
    val updatedCalculatedState = acc.calculated.addresses.updated(signerAddress, updatedAddressInfo)
    val updates: List[AmmUpdate] = stakingUpdate :: acc.onChain.updates

    DataState(
      AmmOnChainState(updates),
      AmmCalculatedState(updatedCalculatedState)
    )
  }
}