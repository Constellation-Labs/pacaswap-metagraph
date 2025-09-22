package org.amm_metagraph.shared_data.services.combiners.operations

import cats.data.EitherT
import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed

import eu.timepit.refined.types.numeric.NonNegLong
import monocle.syntax.all._
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, RewardWithdrawUpdate}
import org.amm_metagraph.shared_data.types.ProcessedRewardWithdrawUpdate
import org.amm_metagraph.shared_data.types.RewardWithdraw.RewardWithdrawReference
import org.amm_metagraph.shared_data.types.Rewards.RewardInfo
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.codecs.{HasherSelector, JsonWithBase64BinaryCodec}
import org.amm_metagraph.shared_data.validations.Errors._
import org.amm_metagraph.shared_data.validations.RewardWithdrawValidations
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

trait RewardsWithdrawService[F[_]] {
  def combineNew(
    signedUpdate: Signed[RewardWithdrawUpdate],
    oldState: DataState[AmmOnChainState, AmmCalculatedState],
    currentMetagraphEpochProgress: EpochProgress,
    globalEpochProgress: EpochProgress
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]]
}

object RewardsWithdrawService {
  def make[F[_]: Async: HasherSelector](
    rewardsConfig: ApplicationConfig.Rewards,
    rewardWithdrawValidation: RewardWithdrawValidations[F],
    dataUpdateCodec: JsonWithBase64BinaryCodec[F, AmmUpdate]
  ): RewardsWithdrawService[F] =
    new RewardsWithdrawService[F] {
      val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)
      // Actual reward distribution done in Rewards Service.
      // Reward Service can't remove already distributed rewards because it can't change state directly.
      // Thus, we shall clear it here by assuming that rewards had been distributed already and we no longer
      // need information in Rewards Pending Calculated state about rewards "pendingClearDelay" epochs ago
      private val pendingClearDelay = EpochProgress(NonNegLong(10L))

      override def combineNew(
        withdrawRequest: Signed[RewardWithdrawUpdate],
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        currentMetagraphEpochProgress: EpochProgress,
        globalEpochProgress: EpochProgress
      )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
        val rewardsState = oldState.calculated.rewards
        val calculatedState: RewardWithdrawCalculatedState = rewardsState.withdraws
        val confirmed = calculatedState.confirmed
        val availableRewards: RewardInfo = rewardsState.availableRewards

        val combinedState: EitherT[F, FailedCalculatedState, DataState[AmmOnChainState, AmmCalculatedState]] =
          for {
            _ <- EitherT(
              rewardWithdrawValidation.rewardWithdrawValidationL0(
                withdrawRequest,
                oldState.calculated,
                globalEpochProgress
              )
            )

            updateHashed <- EitherT.liftF(HasherSelector[F].withCurrent(implicit hs => withdrawRequest.toHashed(dataUpdateCodec.serialize)))
            _ <- EitherT.liftF(logger.info(show"Received reward withdrawn request: ${withdrawRequest.value}"))
            withdrawEpoch <- calculateWithdrawEpoch(currentMetagraphEpochProgress, withdrawRequest, updateHashed.hash)

            removeFromPending = currentMetagraphEpochProgress.minus(pendingClearDelay).getOrElse(EpochProgress.MinValue)
            clearedPending = calculatedState.pending.removed(removeFromPending) // clear already distributed rewards
            pendingWithdraws = clearedPending.getOrElse(withdrawEpoch, RewardInfo.empty)
            (newAvailableRewards, newPendingRewards) <-
              moveAvailableToPending(currentMetagraphEpochProgress, availableRewards, pendingWithdraws, withdrawRequest, updateHashed.hash)
            newPending = clearedPending + (withdrawEpoch -> newPendingRewards)

            reference <- EitherT.liftF(HasherSelector[F].withCurrent(implicit hs => RewardWithdrawReference.of(withdrawRequest)))
            newConfirmedWithdrawEntry = withdrawRequest.source -> reference
            newConfirmed = confirmed + newConfirmedWithdrawEntry
            newWithdraws = calculatedState.copy(confirmed = newConfirmed, pending = newPending)
            newRewardsState = rewardsState.copy(withdraws = newWithdraws, availableRewards = newAvailableRewards)

            processedUpdate = ProcessedRewardWithdrawUpdate(withdrawRequest.source, withdrawRequest.amount, updateHashed.hash)
          } yield
            oldState
              .focus(_.calculated.rewards)
              .replace(newRewardsState)
              .focus(_.onChain.processedRewardWithdrawal)
              .modify(_ :+ processedUpdate) // that data will be cleared on next combine call at L0CombinerService

        combinedState.valueOrF { failedCalculatedState: FailedCalculatedState =>
          handleFailedUpdate(oldState, failedCalculatedState)
        }
      }

      private def calculateWithdrawEpoch(
        currentSnapshotEpochProgress: EpochProgress,
        rewardWithdrawRequest: Signed[RewardWithdrawUpdate],
        updateHash: Hash
      ): EitherT[F, FailedCalculatedState, EpochProgress] =
        rewardsConfig.rewardWithdrawDelay
          .plus(currentSnapshotEpochProgress)
          .leftMap(_ => FailedCalculatedState(WrongRewardWithdrawEpoch, currentSnapshotEpochProgress, updateHash, rewardWithdrawRequest))
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
        update: Signed[RewardWithdrawUpdate],
        updateHash: Hash
      ) = {
        for {
          newAvailableRewards <- availableRewards.subtractReward(update.source, update.rewardType, update.amount)
          newPendingRewards <- pendingRewards.addReward(update.source, update.rewardType, update.amount)
        } yield (newAvailableRewards, newPendingRewards)
      }.leftMap { e =>
        val currentAmount = availableRewards.currentAmount(update.source, update.rewardType)
        FailedCalculatedState(RewardWithdrawAmountError(currentAmount, e.show), currentEpoch, updateHash, update)
      }.toEitherT
    }
}
