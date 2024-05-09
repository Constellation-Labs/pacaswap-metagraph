package org.amm_metagraph.shared_data.validations

import cats.data.NonEmptySet
import cats.effect.Async
import cats.implicits.catsSyntaxApplicativeId
import cats.syntax.apply._
import cats.syntax.flatMap.toFlatMapOps
import cats.syntax.functor.toFunctorOps
import org.amm_metagraph.shared_data.Utils.toAddress
import org.amm_metagraph.shared_data.types.DataUpdates.{StakingUpdate, WithdrawUpdate}
import org.amm_metagraph.shared_data.types.States.{AmmCalculatedState, AmmOffChainState, Staking, StakingCalculatedState}
import org.amm_metagraph.shared_data.validations.Errors.valid
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import org.tessellation.security.SecurityProvider
import org.tessellation.security.signature.signature.SignatureProof

object Validations {

  def stakingValidationsL1[F[_] : Async](
    stakingUpdate: StakingUpdate,
  ): F[DataApplicationValidationErrorOr[Unit]] = Async[F].delay {
    StakingValidations.validateIfTransactionsIsAlreadyProcessed(stakingUpdate)
  }

  def stakingValidationsL0[F[_] : Async](
    stakingUpdate: StakingUpdate,
    proofs       : NonEmptySet[SignatureProof],
    state        : AmmCalculatedState
  )(implicit sp: SecurityProvider[F]): F[DataApplicationValidationErrorOr[Unit]] = {
    for {
      address <- toAddress(proofs.head)
      addressCalculatedState = state.addresses.getOrElse(address, Map.empty[String, AmmOffChainState])
      transactionAlreadyExists = StakingValidations.validateIfTransactionAlreadyExists(
        stakingUpdate,
        addressCalculatedState.get(Staking).asInstanceOf[Option[StakingCalculatedState]]
      )
      stakingValidationsL1 <- stakingValidationsL1(stakingUpdate)
    } yield StakingValidations.validateProvidedAddress(address, stakingUpdate.originAddress)
      .productR(transactionAlreadyExists)
      .productR(stakingValidationsL1)
  }

  def withdrawValidationsL1[F[_] : Async](
    withdrawUpdate: WithdrawUpdate,
  ): F[DataApplicationValidationErrorOr[Unit]] =
    valid.pure

  def withdrawValidationsL0[F[_] : Async](
    withdrawUpdate: WithdrawUpdate,
    proofs        : NonEmptySet[SignatureProof],
    state         : AmmCalculatedState
  )(implicit sp: SecurityProvider[F]): F[DataApplicationValidationErrorOr[Unit]] = {
    for {
      withdrawValidationsL1 <- withdrawValidationsL1(withdrawUpdate)
    } yield withdrawValidationsL1
  }
}
