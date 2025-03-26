package org.amm_metagraph.shared_data.validations

import cats.effect.Async
import cats.syntax.all._
import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.security.SecurityProvider
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.validations.Errors._
import org.checkerframework.checker.units.qual.Current
import io.constellationnetwork.schema.swap.CurrencyId

object SharedValidations {
  private def isSignedExclusivelyBySourceValidation[F[_]: Async: SecurityProvider, A](
    signed: Signed[A],
    sourceAddress: Address
  ): F[DataApplicationValidationErrorOr[Unit]] =
    signed.proofs.head.id.toAddress.map(signerAddress => NotSignedExclusivelyBySourceAddress.unlessA(signerAddress === sourceAddress))

  private def validateHasSingleSignature[A](signed: Signed[A]): DataApplicationValidationErrorOr[Unit] = {
    val maxSignatureCount = 1L
    MultipleSignatures.unlessA(signed.proofs.size === maxSignatureCount)
  }

  def signatureValidations[F[_]: Async: SecurityProvider, A](
    signed: Signed[A],
    sourceAddress: Address
  ): F[DataApplicationValidationErrorOr[Unit]] = for {
    exclusivelySignedBySourceAddress <- isSignedExclusivelyBySourceValidation(signed, sourceAddress)
    singleSignatureValidation = validateHasSingleSignature(signed)
  } yield
    singleSignatureValidation
      .productR(exclusivelySignedBySourceAddress)

  def validateIfTokenIdsAreTheSame(
    tokenAId: Option[CurrencyId],
    tokenBId: Option[CurrencyId]
  ): DataApplicationValidationErrorOr[Unit] =
    TokenIdsAreTheSame.whenA(tokenAId === tokenBId)

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
