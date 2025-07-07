package org.amm_metagraph.shared_data.rewards

import cats.data.EitherT
import cats.effect.kernel.Async
import cats.effect.std.Random
import cats.syntax.all._

import io.constellationnetwork.schema.AmountOps.AmountOps
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress

import derevo.cats.show
import derevo.derive
import eu.timepit.refined.auto._
import eu.timepit.refined.refineV
import eu.timepit.refined.types.numeric.NonNegLong
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.refined.{BigDecimalOps, LongOps, Percentage}
import org.amm_metagraph.shared_data.types.Governance._
import org.amm_metagraph.shared_data.types.LiquidityPool.LiquidityPool
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

@derive(show)
sealed trait RewardError extends Throwable
case class NegativeValueError(value: Long) extends RewardError
case class InvalidMonthError(month: Int) extends RewardError

case class RewardDistribution(
  nodeValidatorRewards: Map[Address, Amount],
  daoRewards: (Address, Amount),
  voteBasedRewards: Map[Address, Amount],
  governanceRewards: Map[Address, Amount]
) {
  def updateAllRewards(updateFun: NonNegLong => NonNegLong): RewardDistribution = {
    def updateEntry[T](keyValue: (T, Amount)) = keyValue._1 -> Amount(updateFun(keyValue._2.value))

    val newNodeValidatorRewards = nodeValidatorRewards.map(updateEntry)
    val newDaoRewards = updateEntry(daoRewards)
    val newVoteBasedRewards = voteBasedRewards.map(updateEntry)
    val newGovernanceRewards = governanceRewards.map(updateEntry)
    this.copy(
      nodeValidatorRewards = newNodeValidatorRewards,
      daoRewards = newDaoRewards,
      voteBasedRewards = newVoteBasedRewards,
      governanceRewards = newGovernanceRewards
    )
  }
}

case class VotingPower(
  address: Address,
  power: VotingWeight
)

trait RewardCalculator[F[_]] {
  def calculateEpochRewards(
    currentProgress: EpochProgress,
    validators: Seq[Address],
    frozenVotingPowers: Map[Address, VotingWeight],
    frozenGovernanceVotes: Map[AllocationId, Percentage],
    currentLiquidityPools: Map[String, LiquidityPool],
    approvedValidators: Seq[Address]
  ): F[Either[RewardError, RewardDistribution]]
}

object RewardCalculator {
  def make[F[_]: Async](config: ApplicationConfig.Rewards, epochData: ApplicationConfig.EpochMetadata): RewardCalculator[F] =
    new RewardCalculatorImpl(config, epochData)
}

class RewardCalculatorImpl[F[_]: Async](rewardConfig: ApplicationConfig.Rewards, epochData: ApplicationConfig.EpochMetadata)
    extends RewardCalculator[F] {
  def logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

  private def toAmount(value: Long): Either[RewardError, Amount] =
    NonNegLong
      .from(value)
      .bimap(
        _ => NegativeValueError(value),
        Amount(_)
      )

  private def epochProgressToMonth(progress: EpochProgress): Either[RewardError, Month] = {
    val month = ((progress.value.value / epochData.epochProgressOneDay / 30) % 12) + 1
    refineV[MonthRefinement](month.toInt).leftMap(_ => InvalidMonthError(month.toInt))
  }

  private def isLastEpochInYear(currentEpoch: EpochProgress): Boolean = {
    val epochsSinceStart = currentEpoch.value.value - rewardConfig.initialEpoch.value.value
    (epochsSinceStart + 1) % epochData.epochProgress1Year === 0
  }

  private def calculateNodeValidatorShare(
    perEpoch: BigDecimal,
    validators: Seq[Address]
  ): Either[RewardError, Map[Address, Amount]] =
    if (validators.isEmpty) {
      Map.empty[Address, Amount].asRight
    } else {
      val share = (perEpoch / validators.length).toAmountUnsafe
      validators.map(_ -> share).toMap.asRight
    }

  private def calculateVoteBasedShare(
    perEpoch: BigDecimal,
    approvedValidators: Seq[Address],
    frozenGovernanceVotes: Map[AllocationId, Percentage],
    currentLiquidityPools: Map[String, LiquidityPool]
  ): Either[RewardError, Map[Address, Amount]] = {
    val epochRewardsForAllocation = frozenGovernanceVotes.map {
      case (allocation, percentage) => allocation -> (percentage * perEpoch)
    }

    val voteBasedLpRewards = calculateLiquidityPoolsRewards(epochRewardsForAllocation, currentLiquidityPools)
    val voteBasedValidatorRewards = calculateValidatorsRewards(epochRewardsForAllocation, approvedValidators)

    val unifiedRewards = (voteBasedLpRewards.toList ++ voteBasedValidatorRewards.toList).groupMapReduce(_._1)(_._2)(_ + _).filter(_._2 > 0)
    val refinedRewards: Map[Address, Amount] =
      unifiedRewards.map {
        case (address, reward) => address -> reward.toAmountUnsafe // only positive here because it was checked on previous step
      }
    refinedRewards.asRight
  }

  private def calculateLiquidityPoolsRewards(
    epochRewardsForAllocation: Map[AllocationId, BigDecimal],
    currentLiquidityPools: Map[String, LiquidityPool]
  ): Map[Address, BigDecimal] = {
    val liquidityPoolAddressesRewards: Seq[(Address, BigDecimal)] =
      currentLiquidityPools.toSeq.flatMap {
        case (stringId, liquidityPool) =>
          val id = AllocationId(stringId, AllocationCategory.LiquidityPool)
          val totalRewardForLiquidityPool = epochRewardsForAllocation.getOrElse(id, BigDecimal(0))
          val totalInLp = liquidityPool.poolShares.totalShares
          liquidityPool.poolShares.addressShares.toSeq.map {
            case (address, shareAmount) =>
              val globalPercentage = shareAmount.value.toBigDecimal / totalInLp.value * totalRewardForLiquidityPool
              address -> globalPercentage
          }
      }

    liquidityPoolAddressesRewards.groupMapReduce(_._1)(_._2)(_ + _)
  }

  private def calculateValidatorsRewards(
    epochRewardsForAllocation: Map[AllocationId, BigDecimal],
    approvedValidators: Seq[Address]
  ): Map[Address, BigDecimal] =
    approvedValidators.map { address =>
      val allocationId = AllocationId(address.value.value, AllocationCategory.NodeOperator)
      val reward = epochRewardsForAllocation.getOrElse(allocationId, BigDecimal(0))
      address -> reward
    }.toMap

  private def calculateGovernanceRewards(
    votingPowers: Seq[VotingPower],
    currentProgress: EpochProgress
  ): EitherT[F, RewardError, Map[Address, Amount]] = {
    val basePerEpoch = rewardConfig.governancePool.value.value / epochData.epochProgress1Year
    val yearRemainder = if (isLastEpochInYear(currentProgress)) {
      rewardConfig.governancePool.value.value % epochData.epochProgress1Year
    } else 0L
    val totalPower = votingPowers.map(_.power.total.value).sum

    if (votingPowers.isEmpty || totalPower === 0L) {
      EitherT.pure[F, RewardError](Map.empty[Address, Amount])
    } else {
      val totalForEpoch = basePerEpoch + yearRemainder

      for {
        baseDistribution <- EitherT.fromEither[F](votingPowers.traverse { vote =>
          val share = (totalForEpoch * vote.power.total.value) / totalPower
          toAmount(share).map(vote.address -> _)
        }.map(_.toMap))

        distributedAmount = baseDistribution.values.map(_.value.value).sum
        remainder = totalForEpoch - distributedAmount

        finalDistribution <-
          if (remainder > 0) {
            for {
              random <- EitherT.liftF(Random.scalaUtilRandomSeedLong(currentProgress.value.value))
              maxPower = votingPowers.map(_.power.total.value).max
              highestPowerVoters = votingPowers.filter(_.power.total.value === maxPower)
              shuffled <- EitherT.liftF(random.shuffleList(highestPowerVoters.toList))
              luckyVoter = shuffled.head
              updatedAmount = baseDistribution(luckyVoter.address).value.value + remainder
              finalAmount <- EitherT.fromEither[F](toAmount(updatedAmount))
            } yield baseDistribution.updated(luckyVoter.address, finalAmount)
          } else {
            EitherT.pure[F, RewardError](baseDistribution)
          }
      } yield finalDistribution
    }
  }

  override def calculateEpochRewards(
    currentProgress: EpochProgress,
    validators: Seq[Address],
    frozenVotingPowers: Map[Address, VotingWeight],
    frozenGovernanceVotes: Map[AllocationId, Percentage],
    currentLiquidityPools: Map[String, LiquidityPool],
    approvedValidators: Seq[Address]
  ): F[Either[RewardError, RewardDistribution]] =
    (for {
      currentMonth <- EitherT.fromEither[F](epochProgressToMonth(currentProgress))

      epochTokens = rewardConfig.totalAnnualTokens.toBigDecimal / epochData.epochProgress1Year
      yearRemainder = rewardConfig.totalAnnualTokens.toBigDecimal % epochData.epochProgress1Year

      totalEpochTokens = epochTokens + (if (isLastEpochInYear(currentProgress)) yearRemainder else 0L)
      nodeValidatorsPerEpoch = totalEpochTokens * rewardConfig.nodeValidatorWeight.value / 100L
      daoPerEpoch = totalEpochTokens * rewardConfig.daoWeight.value / 100L
      voteBasedPerEpoch = totalEpochTokens * rewardConfig.voteBasedWeight.value / 100L

      _ <- EitherT.fromEither[F](
        logger.info(s"Going to distribute $totalEpochTokens as rewards for epoch $currentProgress in month $currentMonth").asRight
      )

      nodeValidatorDistribution <- EitherT.fromEither[F](calculateNodeValidatorShare(nodeValidatorsPerEpoch, validators))
      nodeValidatorDistributed = nodeValidatorDistribution.map(_._2.toBigDecimal).sum

      voteBasedDistribution <- EitherT.fromEither[F](
        calculateVoteBasedShare(voteBasedPerEpoch, approvedValidators, frozenGovernanceVotes, currentLiquidityPools)
      )
      voteBasedDistributed = voteBasedDistribution.map(_._2.toBigDecimal).sum

      allRemainders = totalEpochTokens - (nodeValidatorDistributed + daoPerEpoch + voteBasedDistributed)

      daoReward = (daoPerEpoch + allRemainders).toAmountUnsafe
      _ <- EitherT.fromEither[F](
        logger
          .info(s"Incentive distribution for epoch $currentProgress is: $nodeValidatorDistributed, $voteBasedDistribution, $daoReward")
          .asRight
      )

      governanceVotingPowers = frozenVotingPowers.map { case (address, vote) => VotingPower(address, vote) }.toList
      governanceDistribution <- calculateGovernanceRewards(governanceVotingPowers, currentProgress)
    } yield
      RewardDistribution(
        nodeValidatorRewards = nodeValidatorDistribution,
        daoRewards = rewardConfig.daoAddress -> daoReward,
        voteBasedRewards = voteBasedDistribution,
        governanceRewards = governanceDistribution
      )).value
}
