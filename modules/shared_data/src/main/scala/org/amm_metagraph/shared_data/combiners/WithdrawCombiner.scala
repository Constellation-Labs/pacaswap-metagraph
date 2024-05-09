package org.amm_metagraph.shared_data.combiners

import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, AmmUpdateProof, WithdrawUpdate}
import org.amm_metagraph.shared_data.types.States._
import org.tessellation.currency.dataApplication.DataState
import org.tessellation.schema.address.Address

object WithdrawCombiner {
  def combineWithdraw(
    acc           : DataState[AmmOnChainState, AmmCalculatedState],
    withdrawUpdate: WithdrawUpdate,
    signerAddress : Address,
    ammUpdateProof: AmmUpdateProof,
  ): DataState[AmmOnChainState, AmmCalculatedState] = {
    val addressInfo = acc.calculated.addresses.getOrElse(signerAddress, Map.empty[String, AmmOffChainState])

    val withdrawCalculatedState = WithdrawCalculatedState(
      ammUpdateProof,
      withdrawUpdate.amount
    )

    val updatedAddressInfo = addressInfo + (Withdraw -> withdrawCalculatedState)
    val updatedCalculatedState = acc.calculated.addresses.updated(signerAddress, updatedAddressInfo)
    val updates: List[AmmUpdate] = withdrawUpdate :: acc.onChain.updates

    DataState(
      AmmOnChainState(updates),
      AmmCalculatedState(updatedCalculatedState)
    )
  }
}