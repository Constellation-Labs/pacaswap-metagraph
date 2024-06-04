package org.amm_metagraph.shared_data.validations

import cats.data.NonEmptyList
import cats.effect.kernel.Async
import cats.syntax.all._
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.validations.LiquidityPoolValidations.{liquidityPoolValidationsL0, liquidityPoolValidationsL1}
import org.amm_metagraph.shared_data.validations.StakingValidations.{stakingValidationsL0, stakingValidationsL1}
import org.amm_metagraph.shared_data.validations.WithdrawValidations.{withdrawValidationsL0, withdrawValidationsL1}
import org.tessellation.currency.dataApplication.DataState
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import org.tessellation.security.SecurityProvider
import org.tessellation.security.signature.Signed

trait ValidationService[F[_]] {
  def validateUpdate(
    update: AmmUpdate
  ): F[DataApplicationValidationErrorOr[Unit]]

  def validateData(
    signedUpdates: NonEmptyList[Signed[AmmUpdate]],
    state        : DataState[AmmOnChainState, AmmCalculatedState]
  ): F[DataApplicationValidationErrorOr[Unit]]
}

object ValidationService {
  def make[F[_] : Async : SecurityProvider]: F[ValidationService[F]] = Async[F].delay {
    new ValidationService[F] {
      private def validateL1(update: AmmUpdate): F[DataApplicationValidationErrorOr[Unit]] = update match {
        case stakingUpdate: StakingUpdate => stakingValidationsL1(stakingUpdate)
        case withdrawUpdate: WithdrawUpdate => withdrawValidationsL1(withdrawUpdate)
        case liquidityPoolUpdate: LiquidityPoolUpdate => liquidityPoolValidationsL1(liquidityPoolUpdate)
      }

      private def validateL0(signedUpdate: Signed[AmmUpdate], state: AmmCalculatedState): F[DataApplicationValidationErrorOr[Unit]] = signedUpdate.value match {
        case stakingUpdate: StakingUpdate => stakingValidationsL0(stakingUpdate, state, signedUpdate.proofs)
        case withdrawUpdate: WithdrawUpdate => withdrawValidationsL0(withdrawUpdate, state)
        case liquidityPoolUpdate: LiquidityPoolUpdate => liquidityPoolValidationsL0(liquidityPoolUpdate, state)
      }

      override def validateUpdate(
        update: AmmUpdate
      ): F[DataApplicationValidationErrorOr[Unit]] = validateL1(update)

      override def validateData(
        signedUpdates: NonEmptyList[Signed[AmmUpdate]],
        state        : DataState[AmmOnChainState, AmmCalculatedState]
      ): F[DataApplicationValidationErrorOr[Unit]] = signedUpdates
        .traverse(validateL0(_, state.calculated))
        .map(_.combineAll)
    }
  }
}
