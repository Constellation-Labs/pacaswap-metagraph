package org.amm_metagraph.shared_data.validations

import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed

import io.circe.Encoder
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.validations.Errors._

object SharedValidations {

  def validateHasSingleSignature[A](signed: Signed[A]): DataApplicationValidationErrorOr[Unit] = {
    val maxSignatureCount = 1L
    MultipleSignatures.unlessA(signed.proofs.size === maxSignatureCount)
  }

  def validateIfAllowSpendsAreDuplicated(
    allowSpendRef: Hash,
    existingPendingUpdates: Set[_ <: Signed[AmmUpdate]]
  ): DataApplicationValidationErrorOr[Unit] = {
    val updateAlreadyExists = existingPendingUpdates.exists(_.value match {
      case lpUpdate: LiquidityPoolUpdate => lpUpdate.tokenAAllowSpend == allowSpendRef || lpUpdate.tokenBAllowSpend == allowSpendRef
      case stakingUpdate: StakingUpdate =>
        stakingUpdate.tokenAAllowSpend == allowSpendRef || stakingUpdate.tokenBAllowSpend == allowSpendRef
      case swapUpdate: SwapUpdate => swapUpdate.allowSpendReference == allowSpendRef
      case _                      => false
    })

    DuplicatedOperation.whenA(updateAlreadyExists)
  }
}
