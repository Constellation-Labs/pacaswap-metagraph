package org.amm_metagraph.shared_data.combiners

import io.constellationnetwork.currency.dataApplication.DataState
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.security.signature.Signed

import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, WithdrawUpdate}
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.Withdraw.WithdrawCalculatedStateAddress

object WithdrawCombiner {
  def combineWithdraw(
    acc: DataState[AmmOnChainState, AmmCalculatedState],
    signedWithdrawUpdate: Signed[WithdrawUpdate],
    signerAddress: Address
  ): DataState[AmmOnChainState, AmmCalculatedState] = {
    val withdrawUpdate = signedWithdrawUpdate.value

    val withdrawCalculatedStateAddresses =
      acc.calculated.confirmedOperations.get(OperationType.Withdraw).fold(Map.empty[Address, WithdrawCalculatedStateAddress]) {
        case stakingCalculatedState: WithdrawCalculatedState => stakingCalculatedState.addresses
        case _                                               => Map.empty
      }
    val withdrawCalculatedStateAddress = WithdrawCalculatedStateAddress(
      withdrawUpdate.amount
    )

    val updatedWithdrawCalculatedState = WithdrawCalculatedState(
      withdrawCalculatedStateAddresses.updated(signerAddress, withdrawCalculatedStateAddress)
    )
    val updatedState = acc.calculated.confirmedOperations.updated(OperationType.Withdraw, updatedWithdrawCalculatedState)
    val updates: List[AmmUpdate] = withdrawUpdate :: acc.onChain.updates

    DataState(
      AmmOnChainState(updates),
      AmmCalculatedState(updatedState)
    )
  }
}
