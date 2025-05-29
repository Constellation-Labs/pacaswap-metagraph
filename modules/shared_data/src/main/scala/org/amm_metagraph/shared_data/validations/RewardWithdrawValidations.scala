package org.amm_metagraph.shared_data.validations

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.security.SecurityProvider
import io.constellationnetwork.security.signature.Signed

import org.amm_metagraph.shared_data.types.DataUpdates.RewardWithdrawUpdate
import org.amm_metagraph.shared_data.types.RewardWithdraw.RewardWithdrawReference
import org.amm_metagraph.shared_data.types.Rewards.RewardInfo
import org.amm_metagraph.shared_data.types.States.{AmmCalculatedState, RewardWithdrawCalculatedState}
import org.amm_metagraph.shared_data.validations.Errors._
import org.amm_metagraph.shared_data.validations.SharedValidations.signatureValidations

trait RewardWithdrawValidations[F[_]] {
  def rewardWithdrawValidationL1(rewardWithdrawUpdate: RewardWithdrawUpdate): F[DataApplicationValidationErrorOr[Unit]]
  def rewardWithdrawValidationL0(
    signedRewardWithdrawUpdate: Signed[RewardWithdrawUpdate],
    state: AmmCalculatedState
  ): F[DataApplicationValidationErrorOr[Unit]]
}

object RewardWithdrawValidations {
  def make[F[_]: Async: SecurityProvider](): RewardWithdrawValidations[F] =
    new RewardWithdrawValidations[F] {
      override def rewardWithdrawValidationL1(rewardWithdrawUpdate: RewardWithdrawUpdate): F[DataApplicationValidationErrorOr[Unit]] =
        valid.pure[F]

      override def rewardWithdrawValidationL0(
        signedRewardWithdrawUpdate: Signed[RewardWithdrawUpdate],
        state: AmmCalculatedState
      ): F[DataApplicationValidationErrorOr[Unit]] = {
        val rewardUpdate = signedRewardWithdrawUpdate.value
        val rewardCalculatedState = state.rewards.withdraws
        val availableRewards = state.rewards.availableRewards

        for {
          signatures <- signatureValidations(signedRewardWithdrawUpdate, rewardUpdate.source)
          lastRef = lastRefValidation(rewardCalculatedState, signedRewardWithdrawUpdate)
          amount = amountValidation(availableRewards, signedRewardWithdrawUpdate)
        } yield
          signatures
            .productR(lastRef)
            .productR(amount)
      }

      private def lastRefValidation(
        rewardWithDrawCalculatedState: RewardWithdrawCalculatedState,
        signedRewardWithdraw: Signed[RewardWithdrawUpdate]
      ): DataApplicationValidationErrorOr[Unit] = {
        val lastConfirmed: Option[RewardWithdrawReference] =
          rewardWithDrawCalculatedState.confirmed.get(signedRewardWithdraw.value.source)

        lastConfirmed match {
          case Some(last) if signedRewardWithdraw.ordinal =!= last.ordinal.next || signedRewardWithdraw.parent =!= last =>
            InvalidRewardWithdrawParent.invalid
          case _ => valid
        }
      }

      private def amountValidation(
        available: RewardInfo,
        update: Signed[RewardWithdrawUpdate]
      ): DataApplicationValidationErrorOr[Unit] = {
        val substractionRes = available.subtractReward(update.source, update.rewardType, update.amount)
        substractionRes match {
          case Left(_)  => InvalidRewardWithdrawAmount.invalid
          case Right(_) => valid
        }
      }
    }
}
