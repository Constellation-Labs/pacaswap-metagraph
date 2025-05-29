package org.amm_metagraph.shared_data.services.combiners

import cats.data.EitherT
import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.security.signature.Signed

import monocle.syntax.all._
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.types.DataUpdates.RewardWithdrawUpdate
import org.amm_metagraph.shared_data.types.RewardWithdraw.RewardWithdrawReference
import org.amm_metagraph.shared_data.types.Rewards.RewardInfo
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.codecs.HasherSelector
import org.amm_metagraph.shared_data.validations.Errors._
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

trait RewardsWithdrawService[F[_]] {
  def combineNew(
    signedUpdate: Signed[RewardWithdrawUpdate],
    oldState: DataState[AmmOnChainState, AmmCalculatedState],
    globalEpochProgress: EpochProgress
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]]
}

object RewardsWithdrawService {
  def make[F[_]: Async: HasherSelector](rewardsConfig: ApplicationConfig.Rewards): RewardsWithdrawService[F] =
    new RewardsWithdrawService[F] {
      def logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("RewardsWithdrawService")

      override def combineNew(
        withdrawRequest: Signed[RewardWithdrawUpdate],
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        currentEpoch: EpochProgress
      )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
        val rewardsState = oldState.calculated.rewards
        val calculatedState: RewardWithdrawCalculatedState = rewardsState.withdraws
        val confirmed = calculatedState.confirmed
        val availableRewards: RewardInfo = rewardsState.availableRewards

        val combinedState: EitherT[F, FailedCalculatedState, DataState[AmmOnChainState, AmmCalculatedState]] =
          for {
            withdrawEpoch <- calculateWithdrawEpoch(currentEpoch, withdrawRequest)

            clearedPending = calculatedState.pending - currentEpoch // clear already distributed rewards
            pendingWithdraws = clearedPending.getOrElse(withdrawEpoch, RewardInfo.empty)
            (newAvailableRewards, newPendingRewards) <-
              moveAvailableToPending(currentEpoch, availableRewards, pendingWithdraws, withdrawRequest)
            newPending = clearedPending + (withdrawEpoch -> newPendingRewards)

            reference <- EitherT.liftF(HasherSelector[F].withCurrent(implicit hs => RewardWithdrawReference.of(withdrawRequest)))
            newConfirmedWithdrawEntry = withdrawRequest.source -> reference
            newConfirmed = confirmed + newConfirmedWithdrawEntry
            newWithdraws = calculatedState.copy(confirmed = newConfirmed, pending = newPending)
            newRewardsState = rewardsState.copy(withdraws = newWithdraws, availableRewards = newAvailableRewards)
          } yield oldState.focus(_.calculated.rewards).replace(newRewardsState)

        combinedState.valueOrF { failedCalculatedState: FailedCalculatedState =>
          handleFailedUpdate(oldState, failedCalculatedState)
        }
      }

      private def calculateWithdrawEpoch(
        currentSnapshotEpochProgress: EpochProgress,
        rewardWithdrawRequest: Signed[RewardWithdrawUpdate]
      ): EitherT[F, FailedCalculatedState, EpochProgress] =
        rewardsConfig.rewardWithdrawDelay
          .plus(currentSnapshotEpochProgress)
          .leftMap(_ => FailedCalculatedState(WrongRewardWithdrawEpoch, currentSnapshotEpochProgress, rewardWithdrawRequest))
          .toEitherT

      private def handleFailedUpdate(
        acc: DataState[AmmOnChainState, AmmCalculatedState],
        failedCalculatedState: FailedCalculatedState
      ): F[DataState[AmmOnChainState, AmmCalculatedState]] =
        logger.warn(s"Received incorrect RewardWithDraw update $failedCalculatedState") >> acc.pure[F]

      private def moveAvailableToPending(
        currentEpoch: EpochProgress,
        availableRewards: RewardInfo,
        pendingRewards: RewardInfo,
        update: Signed[RewardWithdrawUpdate]
      ) = {
        for {
          newAvailableRewards <- availableRewards.subtractReward(update.source, update.rewardType, update.amount)
          newPendingRewards <- pendingRewards.addReward(update.source, update.rewardType, update.amount)
        } yield (newAvailableRewards, newPendingRewards)
      }.leftMap { e =>
        val currentAmount = availableRewards.currentAmount(update.source, update.rewardType)
        FailedCalculatedState(RewardWithdrawAmountError(currentAmount, e.show), currentEpoch, update)
      }.toEitherT
    }
}
