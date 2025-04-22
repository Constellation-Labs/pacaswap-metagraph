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
import org.amm_metagraph.shared_data.validations.GovernanceValidations.{rewardAllocationValidationsL0, rewardAllocationValidationsL1}
import org.amm_metagraph.shared_data.validations.LiquidityPoolValidations.{liquidityPoolValidationsL0, liquidityPoolValidationsL1}
import org.amm_metagraph.shared_data.validations.SharedValidations.validateAmmMetagraphId
import org.amm_metagraph.shared_data.validations.StakingValidations.{stakingValidationsL0, stakingValidationsL1}
import org.amm_metagraph.shared_data.validations.SwapValidations.{swapValidationsL0, swapValidationsL1}
import org.amm_metagraph.shared_data.validations.WithdrawalValidations.{withdrawalValidationsL0, withdrawalValidationsL1}

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
  def make[F[_]: Async: SecurityProvider: HasherSelector](
    applicationConfig: ApplicationConfig
  ): ValidationService[F] =
    new ValidationService[F] {
      private def validateL1(
        update: AmmUpdate
      )(implicit context: L1NodeContext[F]): F[DataApplicationValidationErrorOr[Unit]] = for {
        ammMetagraphId <- context.getCurrencyId
        ammMetagraphIdV = validateAmmMetagraphId(update, ammMetagraphId)
        result <- update match {
          case stakingUpdate: StakingUpdate       => stakingValidationsL1(stakingUpdate)
          case withdrawalUpdate: WithdrawalUpdate => withdrawalValidationsL1(withdrawalUpdate)
          case liquidityPoolUpdate: LiquidityPoolUpdate =>
            liquidityPoolValidationsL1(applicationConfig, liquidityPoolUpdate)
          case swapUpdate: SwapUpdate => swapValidationsL1(swapUpdate)
          case rewardAllocationVoteUpdate: RewardAllocationVoteUpdate =>
            rewardAllocationValidationsL1(rewardAllocationVoteUpdate)
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
            case stakingUpdate: StakingUpdate => stakingValidationsL0(Signed(stakingUpdate, signedUpdate.proofs), state)
            case withdrawalUpdate: WithdrawalUpdate =>
              withdrawalValidationsL0(Signed(withdrawalUpdate, signedUpdate.proofs), state)
            case liquidityPoolUpdate: LiquidityPoolUpdate =>
              liquidityPoolValidationsL0(applicationConfig, Signed(liquidityPoolUpdate, signedUpdate.proofs), state)
            case swapUpdate: SwapUpdate => swapValidationsL0(Signed(swapUpdate, signedUpdate.proofs), state)
            case rewardAllocationVoteUpdate: RewardAllocationVoteUpdate =>
              rewardAllocationValidationsL0(
                applicationConfig,
                Signed(rewardAllocationVoteUpdate, signedUpdate.proofs),
                state,
                lastSyncGlobalSnapshot.epochProgress
              )
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
