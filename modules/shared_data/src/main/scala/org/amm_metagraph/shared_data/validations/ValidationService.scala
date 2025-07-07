package org.amm_metagraph.shared_data.validations

import cats.data.NonEmptyList
import cats.effect.kernel.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext, L1NodeContext}
import io.constellationnetwork.security.SecurityProvider
import io.constellationnetwork.security.signature.Signed

import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.validations.Errors.valid
import org.amm_metagraph.shared_data.validations.SharedValidations.validateAmmMetagraphId

trait ValidationService[F[_]] {
  def validateUpdate(
    update: AmmUpdate
  )(implicit context: L1NodeContext[F]): F[DataApplicationValidationErrorOr[Unit]]

  def validateData(
    signedUpdates: NonEmptyList[Signed[AmmUpdate]],
    state: DataState[AmmOnChainState, AmmCalculatedState]
  )(implicit context: L0NodeContext[F]): F[DataApplicationValidationErrorOr[Unit]]
}

object ValidationService {
  def make[F[_]: Async: SecurityProvider](
    applicationConfig: ApplicationConfig,
    liquidityPoolValidations: LiquidityPoolValidations[F],
    stakingValidations: StakingValidations[F],
    swapValidations: SwapValidations[F],
    withdrawalValidations: WithdrawalValidations[F],
    governanceValidations: GovernanceValidations[F],
    rewardWithdrawValidations: RewardWithdrawValidations[F]
  ): ValidationService[F] =
    new ValidationService[F] {
      private def validateL1(
        update: AmmUpdate
      )(implicit context: L1NodeContext[F]): F[DataApplicationValidationErrorOr[Unit]] = for {
        ammMetagraphId <- context.getCurrencyId
        ammMetagraphIdV = validateAmmMetagraphId(update, ammMetagraphId)
        result <- update match {
          case stakingUpdate: StakingUpdate       => stakingValidations.l1Validations(stakingUpdate)
          case withdrawalUpdate: WithdrawalUpdate => withdrawalValidations.l1Validations(withdrawalUpdate)
          case liquidityPoolUpdate: LiquidityPoolUpdate =>
            liquidityPoolValidations.l1Validations(liquidityPoolUpdate)
          case swapUpdate: SwapUpdate => swapValidations.l1Validations(swapUpdate)
          case rewardAllocationVoteUpdate: RewardAllocationVoteUpdate =>
            governanceValidations.l1Validations(rewardAllocationVoteUpdate)
          case rewardWithdrawUpdate: RewardWithdrawUpdate =>
            rewardWithdrawValidations.rewardWithdrawValidationL1(rewardWithdrawUpdate)
        }
      } yield
        result
          .productR(ammMetagraphIdV)

      // The validations were migrated to combine function to populate FailedOperations state
      private def validateL0(
        signedUpdate: Signed[AmmUpdate],
        state: AmmCalculatedState
      )(implicit context: L0NodeContext[F]): F[DataApplicationValidationErrorOr[Unit]] =
        valid.pure

      override def validateUpdate(
        update: AmmUpdate
      )(implicit context: L1NodeContext[F]): F[DataApplicationValidationErrorOr[Unit]] = validateL1(update)

      override def validateData(
        signedUpdates: NonEmptyList[Signed[AmmUpdate]],
        state: DataState[AmmOnChainState, AmmCalculatedState]
      )(implicit context: L0NodeContext[F]): F[DataApplicationValidationErrorOr[Unit]] = signedUpdates
        .traverse(validateL0(_, state.calculated))
        .map(_.combineAll)
    }
}
