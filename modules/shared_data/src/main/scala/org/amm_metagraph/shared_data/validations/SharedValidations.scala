package org.amm_metagraph.shared_data.validations

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.CurrencyId
import io.constellationnetwork.security.SecurityProvider
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed

import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.States.FailedCalculatedState
import org.amm_metagraph.shared_data.validations.Errors._

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

  def failWith[A <: AmmUpdate](
    reason: FailedCalculatedStateReason,
    expireEpochProgress: EpochProgress,
    signedUpdate: Signed[A]
  ): Left[FailedCalculatedState, Signed[A]] =
    Left(FailedCalculatedState(reason, expireEpochProgress, signedUpdate))

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
    allAllowSpendsInUse: Set[Hash]
  ): DataApplicationValidationErrorOr[Unit] =
    DuplicatedOperation.whenA(allAllowSpendsInUse.contains(allowSpendRef))

  def validateAmmMetagraphId[A <: AmmUpdate](
    signed: A,
    currencyId: CurrencyId
  ): DataApplicationValidationErrorOr[Unit] =
    InvalidAMMMetagraphId.whenA(signed.metagraphId =!= currencyId)
}
