package org.amm_metagraph.shared_data.validations

import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.SortedSet

import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact.SpendTransaction
import io.constellationnetwork.security.SecurityProvider
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.key.ops.PublicKeyOps
import io.constellationnetwork.security.signature.Signed

import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.validations.Errors._

object SharedValidations {
  def isSignedExclusivelyByValidation[F[_]: Async: SecurityProvider, A](
    signed: Signed[A],
    signerAddress: Address
  ): F[DataApplicationValidationErrorOr[Unit]] =
    signed.proofs.existsM { proof =>
      proof.id.hex.toPublicKey.map { signerPk =>
        signerPk.toAddress =!= signerAddress
      }
    }.map(isNotSignedExclusively => NotSignedExclusivelyByAddressOwner.whenA(isNotSignedExclusively))

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
      case _                      => false
    })

    val spendTransactionAlreadyExists = existingSpendTransactions.exists(t => t.allowSpendRef.contains(allowSpendRef))

    DuplicatedOperation.whenA(updateAlreadyExists || spendTransactionAlreadyExists)
  }
}
