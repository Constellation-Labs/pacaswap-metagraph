package org.amm_metagraph.shared_data.services.combiners

import cats.data.EitherT
import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.DataState
import io.constellationnetwork.currency.schema.currency.CurrencyIncrementalSnapshot
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.balance.BalanceArithmeticError
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.security.SecurityProvider
import io.constellationnetwork.security.signature.Signed

import derevo.cats.show
import derevo.derive
import eu.timepit.refined.types.all.NonNegLong
import monocle.syntax.all._
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.rewards._
import org.amm_metagraph.shared_data.types.Rewards.RewardInfo
import org.amm_metagraph.shared_data.types.States.{AmmCalculatedState, AmmOnChainState}
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

trait RewardsDistributionService[F[_]] {
  def updateRewardsDistribution(
    lastArtifact: Signed[CurrencyIncrementalSnapshot],
    state: DataState[AmmOnChainState, AmmCalculatedState],
    currentEpoch: EpochProgress
  ): F[DataState[AmmOnChainState, AmmCalculatedState]]
}

object RewardsDistributionService {
  def make[F[_]: Async: SecurityProvider](
    rewardCalculator: RewardCalculator[F],
    rewardsConfig: ApplicationConfig.Rewards
  ): RewardsDistributionService[F] =
    new RewardsDistributionService[F] {
      def logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("RewardsCombinerService")
      private val interval = rewardsConfig.rewardCalculationInterval

      override def updateRewardsDistribution(
        lastArtifact: Signed[CurrencyIncrementalSnapshot],
        state: DataState[AmmOnChainState, AmmCalculatedState],
        currentEpoch: EpochProgress
      ): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
        // Clean any existed reward distribution, we use them for indexer only
        val clearedState = state.focus(_.onChain.rewardsUpdate).replace(None)

        if (currentEpoch.value.value % interval.value == 0) {
          // We don't calculate rewards every epoch,
          // instead we skip "interval" epochs but increase rewards distribution to "interval".
          // Therefore, we get more or less the same value for rewards as we calculate them every epoch
          val rewardsMultiplier = interval.value
          doUpdateRewardState(lastArtifact, clearedState, rewardsMultiplier, currentEpoch)
        } else {
          clearedState.pure[F]
        }
      }

      private def doUpdateRewardState(
        lastArtifact: Signed[CurrencyIncrementalSnapshot],
        state: DataState[AmmOnChainState, AmmCalculatedState],
        rewardsMultiplier: Long,
        currentEpoch: EpochProgress
      ): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
        val calculatedState: AmmCalculatedState = state.calculated
        val res: EitherT[F, RewardDistributionError, DataState[AmmOnChainState, AmmCalculatedState]] =
          for {
            facilitators <- EitherT.liftF(lastArtifact.proofs.toList.traverse(_.id.toAddress[F]))
            votingPowers = calculatedState.votingWeights.toList.map { case (address, weight) => VotingPower(address, weight) }

            distribution <- calculateEpochRewards(currentEpoch, facilitators, votingPowers)
            updateRewardFun = (v: NonNegLong) => NonNegLong.from(v.value * rewardsMultiplier).getOrElse(v)
            multipliedRewards = distribution.updateAllRewards(updateRewardFun)

            distributionAsRewardInfo = RewardInfo.fromRewardDistribution(multipliedRewards)
            mergedRewards <- mergeRewards(calculatedState.rewards.availableRewards, distributionAsRewardInfo)
          } yield
            state
              .focus(_.calculated.rewards.availableRewards)
              .replace(mergedRewards)
              .focus(_.onChain.rewardsUpdate)
              .replace(distributionAsRewardInfo.some)

        res.foldF(
          error => logger.warn(show"Failed to distribute rewards $error") >> state.pure[F],
          _.pure[F]
        )
      }

      private def calculateEpochRewards(
        currentEpoch: EpochProgress,
        validators: List[Address],
        votingPowers: List[VotingPower]
      ): EitherT[F, RewardDistributionError, RewardDistribution] =
        EitherT(
          rewardCalculator.calculateEpochRewards(
            currentEpoch,
            validators,
            votingPowers
          )
        ).leftMap(e => RewardCalculationError(e))

      private def mergeRewards(
        currentRewards: RewardInfo,
        newRewards: RewardInfo
      ): EitherT[F, RewardDistributionError, RewardInfo] =
        currentRewards.addRewards(newRewards).toEitherT.leftMap(e => RewardMergingError(e))
    }

  @derive(show)
  sealed trait RewardDistributionError
  case class RewardCalculationError(error: RewardError) extends RewardDistributionError
  case class RewardMergingError(error: BalanceArithmeticError) extends RewardDistributionError

}
