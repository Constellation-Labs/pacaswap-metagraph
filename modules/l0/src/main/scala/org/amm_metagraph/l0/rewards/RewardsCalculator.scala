package org.amm_metagraph.l0.rewards

import cats.data.EitherT
import cats.effect.kernel.Async
import cats.effect.std.Random
import cats.syntax.all._

import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress

import eu.timepit.refined.auto._
import eu.timepit.refined.refineV
import eu.timepit.refined.types.numeric.NonNegLong
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.epochProgress.{epochProgress1Year, epochProgressOneDay}
import org.amm_metagraph.shared_data.types.Governance.VotingWeight

sealed trait RewardError extends Throwable
case class NegativeValueError(value: Long) extends RewardError
case class InvalidMonthError(month: Int) extends RewardError

case class RewardDistribution(
  epochProgress: EpochProgress,
  month: Month,
  validatorRewards: Map[Address, Amount],
  daoRewards: (Address, Amount),
  votingRewards: Map[Address, Amount],
  governanceRewards: Map[Address, Amount]
)

case class VotingPower(
  address: Address,
  power: VotingWeight
)

object RewardCalculator {
  def make(config: ApplicationConfig.Rewards): RewardCalculator = new RewardCalculator(config)
}

class RewardCalculator(config: ApplicationConfig.Rewards) {
  private def toAmount(value: Long): Either[RewardError, Amount] =
    NonNegLong
      .from(value)
      .bimap(
        _ => NegativeValueError(value),
        Amount(_)
      )

  private def getMonthStartProgress(progress: EpochProgress): Long = {
    val monthNumber = progress.value.value / epochProgressOneDay / 30
    monthNumber * epochProgressOneDay * 30L
  }

  private def epochProgressToMonth(progress: EpochProgress): Either[RewardError, Month] = {
    val month = ((progress.value.value / epochProgressOneDay / 30) % 12) + 1
    refineV[MonthRefinement](month.toInt).leftMap(_ => InvalidMonthError(month.toInt))
  }

  private def isVoteFromCurrentMonth(currentProgress: EpochProgress, voteProgress: EpochProgress): Boolean = {
    val currentMonthStart = getMonthStartProgress(currentProgress)
    val nextMonthStart = currentMonthStart + epochProgressOneDay * 30
    voteProgress.value >= currentMonthStart && voteProgress.value < nextMonthStart
  }

  private def isLastEpochInYear(currentEpoch: EpochProgress): Boolean = {
    val epochsSinceStart = currentEpoch.value.value - config.initialEpoch.value.value
    (epochsSinceStart + 1) % epochProgress1Year === 0
  }

  private def calculateValidatorShare(
    perEpoch: Amount,
    validators: List[Address]
  ): Either[RewardError, (Map[Address, Amount], Amount)] =
    if (validators.isEmpty) {
      (Map.empty[Address, Amount], Amount.empty).asRight
    } else {
      val share = perEpoch.value.value / validators.length
      val remainder = perEpoch.value.value % validators.length
      (toAmount(share), toAmount(remainder)).mapN {
        case (s, r) =>
          (validators.map(_ -> s).toMap, r)
      }
    }

  private def calculateVotingShare(
    perEpoch: Amount,
    votingPowers: List[VotingPower],
    currentProgress: EpochProgress
  ): Either[RewardError, (Map[Address, Amount], Amount)] = {
    val votersOfCurrentMonth = votingPowers.filter { vp =>
      vp.power.info.exists(info => isVoteFromCurrentMonth(currentProgress, info.votedAtEpochProgress))
    }

    val totalVotingPower = votersOfCurrentMonth.map(_.power.total.value).sum

    if (totalVotingPower === 0L) {
      (Map.empty[Address, Amount], Amount.empty).asRight
    } else {
      votersOfCurrentMonth.traverse { vote =>
        toAmount((perEpoch.value.value * vote.power.total.value) / totalVotingPower)
          .map(reward => vote.address -> reward)
      }.flatMap { distribution =>
        val distributionMap = distribution.toMap
        val sum = distribution.map { case (_, amount) => amount.value.value }.sum
        toAmount(perEpoch.value.value - sum).map { remainder =>
          (distributionMap, remainder)
        }
      }
    }
  }

  private def calculateGovernanceRewards[F[_]: Async](
    votingPowers: List[VotingPower],
    currentProgress: EpochProgress
  ): F[Either[RewardError, Map[Address, Amount]]] = {
    val basePerEpoch = config.governancePool.value.value / epochProgress1Year
    val yearRemainder = if (isLastEpochInYear(currentProgress)) {
      config.governancePool.value.value % epochProgress1Year
    } else 0L

    (if (votingPowers.isEmpty) {
       EitherT.pure[F, RewardError](Map.empty[Address, Amount])
     } else {
       val totalPower = votingPowers.map(_.power.total.value).sum
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
               shuffled <- EitherT.liftF(random.shuffleList(highestPowerVoters))
               luckyVoter = shuffled.head
               updatedAmount = baseDistribution(luckyVoter.address).value.value + remainder
               finalAmount <- EitherT.fromEither[F](toAmount(updatedAmount))
             } yield baseDistribution.updated(luckyVoter.address, finalAmount)
           } else {
             EitherT.pure[F, RewardError](baseDistribution)
           }
       } yield finalDistribution
     }).value
  }

  def calculateEpochRewards[F[_]: Async](
    currentProgress: EpochProgress,
    validators: List[Address],
    votingPowers: List[VotingPower]
  ): F[Either[RewardError, RewardDistribution]] =
    (for {
      currentMonth <- EitherT.fromEither[F](epochProgressToMonth(currentProgress))

      epochTokens <- EitherT.fromEither[F](toAmount(config.totalAnnualTokens.value.value / epochProgress1Year))
      yearRemainder = config.totalAnnualTokens.value.value % epochProgress1Year

      totalEpochTokens <- EitherT.fromEither[F](
        toAmount(
          epochTokens.value.value + (if (isLastEpochInYear(currentProgress)) yearRemainder else 0L)
        )
      )

      validatorsPerEpoch <- EitherT.fromEither[F](toAmount(totalEpochTokens.value.value * config.validatorWeight.value / 100L))
      daoPerEpoch <- EitherT.fromEither[F](toAmount(totalEpochTokens.value.value * config.daoWeight.value / 100L))
      votingPerEpoch <- EitherT.fromEither[F](toAmount(totalEpochTokens.value.value * config.votingWeight.value / 100L))

      (validatorDistribution, validatorRemainder) <- EitherT.fromEither[F](calculateValidatorShare(validatorsPerEpoch, validators))
      (votingDistribution, votingRemainder) <- EitherT.fromEither[F](calculateVotingShare(votingPerEpoch, votingPowers, currentProgress))

      allRemainders = (totalEpochTokens.value.value -
        (validatorsPerEpoch.value.value + daoPerEpoch.value.value + votingPerEpoch.value.value)) +
        validatorRemainder.value.value + votingRemainder.value.value

      daoReward <- EitherT.fromEither[F](toAmount(daoPerEpoch.value.value + allRemainders))
      governanceDistribution <- EitherT(calculateGovernanceRewards(votingPowers, currentProgress))

    } yield
      RewardDistribution(
        epochProgress = currentProgress,
        month = currentMonth,
        validatorRewards = validatorDistribution,
        daoRewards = config.daoAddress -> daoReward,
        votingRewards = votingDistribution,
        governanceRewards = governanceDistribution
      )).value
}
