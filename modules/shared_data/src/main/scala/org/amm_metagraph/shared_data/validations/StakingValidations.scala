package org.amm_metagraph.shared_data.validations

import org.amm_metagraph.shared_data.types.DataUpdates.StakingUpdate
import org.amm_metagraph.shared_data.types.States.StakingCalculatedState
import org.amm_metagraph.shared_data.validations.Errors._
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import org.tessellation.schema.address.Address

object StakingValidations {
  def validateIfTransactionsIsAlreadyProcessed(
    stakingUpdate: StakingUpdate
  ): DataApplicationValidationErrorOr[Unit] =
    valid

  def validateIfTransactionAlreadyExists(
    stakingUpdate         : StakingUpdate,
    stakingCalculatedState: Option[StakingCalculatedState]
  ): DataApplicationValidationErrorOr[Unit] =
    StakingTransactionAlreadyExists.whenA(stakingCalculatedState.exists(_.txnId == stakingUpdate.txnId))

  def validateProvidedAddress(proofAddress: Address, address: Address): DataApplicationValidationErrorOr[Unit] =
    InvalidAddress.unlessA(proofAddress == address)
}
