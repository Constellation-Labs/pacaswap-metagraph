package org.amm_metagraph.shared_data.validations

import scala.collection.immutable.SortedSet

import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.schema.artifact.SpendTransaction
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed

import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.validations.Errors.DuplicatedOperation

object SharedValidations {
  def validateIfAllowSpendsAndSpendTransactionsAreDuplicated(
    allowSpendRef: Hash,
    existingPendingUpdates: Set[Signed[AmmUpdate]],
    existingSpendTransactions: SortedSet[SpendTransaction]
  ): DataApplicationValidationErrorOr[Unit] = {
    val updateAlreadyExists = existingPendingUpdates.exists(_.value match {
      case lpUpdate: LiquidityPoolUpdate => lpUpdate.tokenAAllowSpend == allowSpendRef || lpUpdate.tokenBAllowSpend == allowSpendRef
      case stakingUpdate: StakingUpdate =>
        stakingUpdate.tokenAAllowSpend == allowSpendRef || stakingUpdate.tokenBAllowSpend == allowSpendRef
      case swapUpdate: SwapUpdate => swapUpdate.allowSpendReference == allowSpendRef
    })

    val spendTransactionAlreadyExists = existingSpendTransactions.exists(t => t.allowSpendRef.contains(allowSpendRef))

    DuplicatedOperation.whenA(updateAlreadyExists || spendTransactionAlreadyExists)
  }
}
