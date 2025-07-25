package org.amm_metagraph.shared_data.services.combiners

import cats.data.EitherT
import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.currency.schema.currency.CurrencyIncrementalSnapshot
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.balance.{Amount, BalanceArithmeticError}
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.security.SecurityProvider
import io.constellationnetwork.security.signature.Signed

import derevo.cats.show
import derevo.derive
import eu.timepit.refined.types.all.NonNegLong
import monocle.syntax.all._
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.refined.Percentage._
import org.amm_metagraph.shared_data.rewards._
import org.amm_metagraph.shared_data.types.Governance._
import org.amm_metagraph.shared_data.types.LiquidityPool.getLiquidityPoolCalculatedState
import org.amm_metagraph.shared_data.types.Rewards.RewardInfo
import org.amm_metagraph.shared_data.types.States.{AmmCalculatedState, AmmOnChainState}
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

trait RewardsDistributionService[F[_]] {
  def updateRewardsDistribution(
    lastArtifact: Signed[CurrencyIncrementalSnapshot],
    state: DataState[AmmOnChainState, AmmCalculatedState],
    currentEpoch: EpochProgress
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]]
}

object RewardsDistributionService {
  def make[F[_]: Async: SecurityProvider](
    rewardCalculator: RewardCalculator[F],
    rewardsConfig: ApplicationConfig.Rewards
  ): RewardsDistributionService[F] =
    new RewardsDistributionService[F] {
      val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)
      private val interval = rewardsConfig.rewardCalculationInterval

      override def updateRewardsDistribution(
        lastArtifact: Signed[CurrencyIncrementalSnapshot],
        state: DataState[AmmOnChainState, AmmCalculatedState],
        currentEpoch: EpochProgress
      )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] =
        for {
          approvedValidators <- context.getMetagraphL0Seedlist.getOrElse(Set.empty).toList.traverse(_.peerId.toAddress)

          clearedOnChain <- state.focus(_.onChain.rewardsUpdate).replace(None).pure[F]
          withUpdateBuffer <- tryToUpdateRewardBuffer(lastArtifact, clearedOnChain, currentEpoch, approvedValidators)
          clearedEpoch <- withUpdateBuffer.focus(_.calculated.rewards.lastProcessedEpoch).replace(currentEpoch).pure[F]

          updatedRewards <- updateAvailableRewardsAndOnChainData(clearedEpoch, rewardsConfig)
        } yield updatedRewards

      private def updateAvailableRewardsAndOnChainData(
        state: DataState[AmmOnChainState, AmmCalculatedState],
        rewardsConfig: ApplicationConfig.Rewards
      ): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
        val rewardsBuffer = state.calculated.rewards.rewardsBuffer

        val rewardsTransactionCount = rewardsConfig.availableRewardsPerSnapshot.value

        val (rewardsToDistribute, rewardsToStay) = rewardsBuffer.data.splitAt(rewardsTransactionCount)
        val res = for {
          distributionInfo <- RewardInfo.make(rewardsToDistribute)
          mergedRewards <- state.calculated.rewards.availableRewards.addRewards(distributionInfo)
        } yield
          state
            .focus(_.calculated.rewards.rewardsBuffer.data)
            .replace(rewardsToStay)
            .focus(_.calculated.rewards.availableRewards)
            .replace(mergedRewards)
            .focus(_.onChain.rewardsUpdate)
            .replace(Option.when(distributionInfo.info.nonEmpty)(distributionInfo))

        EitherT
          .fromEither(res.leftMap(RewardMergingError))
          .foldF(
            error => logger.warn(show"Failed to update available rewards $error") >> state.pure[F],
            _.pure[F]
          )
      }

      private def tryToUpdateRewardBuffer(
        lastArtifact: Signed[CurrencyIncrementalSnapshot],
        state: DataState[AmmOnChainState, AmmCalculatedState],
        currentEpoch: EpochProgress,
        approvedValidators: Seq[Address]
      ): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
        val currentEpochNumber = currentEpoch.value.value
        val newEpoch = currentEpochNumber > state.calculated.rewards.lastProcessedEpoch.value.value
        val rewardDistributionEpoch = currentEpochNumber % interval.value == 0

        (newEpoch, rewardDistributionEpoch) match {
          case (false, _) =>
            logger.info(show"Skip reward distribution for epoch $currentEpochNumber. Epoch had been processed already") >>
              state.pure[F]
          case (true, false) =>
            logger.info(show"Skip reward distribution for epoch $currentEpochNumber. Not an epoch for distribution") >>
              state.pure[F]
          case (true, true) =>
            // We don't calculate rewards every epoch,
            // instead we skip "interval" epochs but increase rewards distribution to "interval".
            // Therefore, we get more or less the same value for rewards as we calculate them every epoch
            val rewardsMultiplier = interval.value
            for {
              _ <- logger.info(show"Start reward distribution. Current epoch $currentEpochNumber, interval ${interval.value}")
              res <- doUpdateRewardsBuffer(approvedValidators, lastArtifact, state, rewardsMultiplier, currentEpoch)
            } yield res
        }
      }

      private def doUpdateRewardsBuffer(
        approvedValidators: Seq[Address],
        lastArtifact: Signed[CurrencyIncrementalSnapshot],
        state: DataState[AmmOnChainState, AmmCalculatedState],
        rewardsMultiplier: Long,
        currentEpoch: EpochProgress
      ): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
        val calculatedState: AmmCalculatedState = state.calculated
        val res: EitherT[F, RewardDistributionError, DataState[AmmOnChainState, AmmCalculatedState]] =
          for {
            facilitators <- EitherT.liftF(lastArtifact.proofs.toList.traverse(_.id.toAddress[F]))
            distribution <- calculateEpochRewards(currentEpoch, approvedValidators, facilitators, calculatedState)
            updateRewardFun = (v: NonNegLong) => NonNegLong.from(v.value * rewardsMultiplier).getOrElse(v)
            multipliedRewards = distribution.updateAllRewards(updateRewardFun)

            distributionAsRewardInfo = RewardInfo.fromRewardDistribution(multipliedRewards)
            _ <- EitherT.liftF(logger.info(show"Calculated rewards for epoch ${currentEpoch.value.value} is $distributionAsRewardInfo"))
          } yield state.focus(_.calculated.rewards.rewardsBuffer.data).modify(_ ++ distributionAsRewardInfo.info.toSeq)

        res.foldF(
          error => logger.error(show"Failed to distribute rewards $error") >> state.pure[F],
          _.pure[F]
        )
      }

      private def calculateEpochRewards(
        currentEpoch: EpochProgress,
        approvedValidators: Seq[Address],
        validators: Seq[Address],
        state: AmmCalculatedState
      ): EitherT[F, RewardDistributionError, RewardDistribution] = {
        val frozenVotingWeights = state.allocations.frozenUsedUserVotes.votes
        val frozenGovernanceVotes = state.allocations.frozenUsedUserVotes.allocationVotes
        val currentLiquidityPools = getLiquidityPoolCalculatedState(state).confirmed.value
        def lpShow =
          currentLiquidityPools.map { case (id, lp) => id -> lp.poolShares.addressShares }.toList

        logger
          .info(
            show"Start reward calculation with parameters: $currentEpoch, $validators, $frozenVotingWeights, $frozenGovernanceVotes, $lpShow, $approvedValidators"
          )
          .asRight[RewardDistributionError]
          .toEitherT[F] >>
          EitherT(
            rewardCalculator.calculateEpochRewards(
              currentEpoch,
              validators,
              frozenVotingWeights,
              frozenGovernanceVotes,
              currentLiquidityPools,
              approvedValidators
            )
          ).leftMap(e => RewardCalculationError(e))
      }
    }

  @derive(show)
  sealed trait RewardDistributionError
  case class RewardCalculationError(error: RewardError) extends RewardDistributionError
  case class RewardMergingError(error: BalanceArithmeticError) extends RewardDistributionError

}
