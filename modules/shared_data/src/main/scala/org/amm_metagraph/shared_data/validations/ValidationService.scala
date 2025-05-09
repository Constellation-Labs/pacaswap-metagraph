package org.amm_metagraph.shared_data.validations

import cats.data.{NonEmptyList, OptionT}
import cats.effect.kernel.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext, L1NodeContext}
import io.constellationnetwork.security.SecurityProvider
import io.constellationnetwork.security.signature.Signed

import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.codecs.HasherSelector
import org.amm_metagraph.shared_data.validations.RewardWithdrawValidations.{rewardWithdrawValidationL0, rewardWithdrawValidationL1}
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
    governanceValidations: GovernanceValidations[F]
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
            liquidityPoolValidations.l1Validations(applicationConfig, liquidityPoolUpdate)
          case swapUpdate: SwapUpdate => swapValidations.l1Validations(swapUpdate)
          case rewardAllocationVoteUpdate: RewardAllocationVoteUpdate =>
            governanceValidations.l1Validations(rewardAllocationVoteUpdate)
          case rewardWithdrawUpdate: RewardWithdrawUpdate =>
            rewardWithdrawValidationL1(rewardWithdrawUpdate)
        }
      } yield
        result
          .productR(ammMetagraphIdV)

      private def validateL0(
        signedUpdate: Signed[AmmUpdate],
        state: AmmCalculatedState
      )(implicit context: L0NodeContext[F]): F[DataApplicationValidationErrorOr[Unit]] =
        for {
          lastSyncGlobalSnapshot <- OptionT(context.getLastSynchronizedGlobalSnapshot).getOrRaise(
            new IllegalStateException("lastSyncGlobalSnapshot unavailable")
          )
          ammMetagraphId <- context.getCurrencyId
          ammMetagraphIdV = validateAmmMetagraphId(signedUpdate.value, ammMetagraphId)

          result <- signedUpdate.value match {
            case stakingUpdate: StakingUpdate => stakingValidations.l0Validations(Signed(stakingUpdate, signedUpdate.proofs), state)
            case withdrawalUpdate: WithdrawalUpdate =>
              withdrawalValidations.l0Validations(Signed(withdrawalUpdate, signedUpdate.proofs), state)
            case liquidityPoolUpdate: LiquidityPoolUpdate =>
              liquidityPoolValidations.l0Validations(applicationConfig, Signed(liquidityPoolUpdate, signedUpdate.proofs), state)
            case swapUpdate: SwapUpdate => swapValidations.l0Validations(Signed(swapUpdate, signedUpdate.proofs), state)
            case rewardAllocationVoteUpdate: RewardAllocationVoteUpdate =>
              governanceValidations.l0Validations(
                applicationConfig,
                Signed(rewardAllocationVoteUpdate, signedUpdate.proofs),
                state,
                lastSyncGlobalSnapshot.epochProgress
              )
            case rewardWithdrawUpdate: RewardWithdrawUpdate =>
              rewardWithdrawValidationL0(Signed(rewardWithdrawUpdate, signedUpdate.proofs), state)
          }
        } yield
          result
            .productR(ammMetagraphIdV)

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
