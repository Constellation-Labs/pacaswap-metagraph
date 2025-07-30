package org.amm_metagraph.shared_data.rewards

import cats.data.EitherT
import cats.effect.kernel.Async
import cats.effect.std.Random
import cats.syntax.all._

import io.constellationnetwork.schema.AmountOps.AmountOps
import io.constellationnetwork.schema.address.{Address, DAGAddressRefined}
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.CurrencyId

import derevo.cats.show
import derevo.derive
import eu.timepit.refined.auto._
import eu.timepit.refined.refineV
import eu.timepit.refined.types.numeric.NonNegLong
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.app.ApplicationConfig.{LpRewardInfo, TokenPairStrings}
import org.amm_metagraph.shared_data.refined.{BigDecimalOps, Percentage}
import org.amm_metagraph.shared_data.rewards.LiquidityPoolRewardConfiguration._
import org.amm_metagraph.shared_data.types.Governance._
import org.amm_metagraph.shared_data.types.LiquidityPool.LiquidityPool
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.{Logger, SelfAwareStructuredLogger}

@derive(show)
sealed trait RewardError extends Throwable
case class NegativeValueError(value: Long, description: String) extends RewardError
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

case class VotingPowerForAddress(
  address: Address,
  power: VotingPower
)

trait RewardCalculator[F[_]] {
  def calculateEpochRewards(
    currentProgress: EpochProgress,
    validators: Seq[Address],
    frozenVotingPowers: Map[Address, VotingPower],
    frozenGovernanceVotes: Map[AllocationId, Percentage],
    currentLiquidityPools: Map[String, LiquidityPool],
    approvedValidators: Seq[Address]
  ): F[Either[RewardError, RewardDistribution]]
}

object RewardCalculator {
  def make[F[_]: Async: Logger](config: ApplicationConfig.Rewards, epochData: ApplicationConfig.EpochMetadata): F[RewardCalculator[F]] =
    EitherT
      .fromEither[F](config.nodeValidatorConfig.LiquidityPoolsConfig.parsed)
      .valueOrF { error =>
        implicitly[Logger[F]].error(
          show"Failed to parse voted based liquidity pool config due: $error. Empty config will be used instead!"
        ) >>
          LiquidityPoolRewardConfiguration.empty.pure[F]
      }
      .map(lpConfig => new RewardCalculatorImpl(config, epochData, lpConfig): RewardCalculator[F])
}

class RewardCalculatorImpl[F[_]: Async](
  rewardConfig: ApplicationConfig.Rewards,
  epochData: ApplicationConfig.EpochMetadata,
  voteBasedLpConfig: LiquidityPoolRewardConfiguration
) extends RewardCalculator[F] {

  val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

  private def toAmount(value: BigDecimal, description: String): Either[RewardError, Amount] =
    NonNegLong
      .from(value.floor.toLong)
      .bimap(
        _ => NegativeValueError(value.floor.toLong, description),
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
    validators: Seq[Address],
    currentEpoch: EpochProgress,
    addressToLpInfo: Map[Address, Seq[LpInfo]]
  ): Either[RewardError, Map[Address, Amount]] = {
    def isLPConfigured(address: Address): Boolean =
      voteBasedLpConfig.contains(currentEpoch, addressToLpInfo.getOrElse(address, Seq.empty))

    val eligibleValidators = validators.filter(isLPConfigured)
    if (eligibleValidators.isEmpty) {
      Map.empty[Address, Amount].asRight
    } else {
      val share = (perEpoch / eligibleValidators.length).toAmountUnsafe
      eligibleValidators.map(_ -> share).toMap.asRight
    }
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

    val voteBasedValidatorRewards =
      calculateValidatorsRewards(epochRewardsForAllocation, approvedValidators)

    val unifiedRewards = (voteBasedLpRewards.toList ++ voteBasedValidatorRewards.toList).groupMapReduce(_._1)(_._2)(_ + _).filter(_._2 > 0)
    val refinedRewards: Map[Address, Amount] =
      unifiedRewards.map {
        case (address, reward) => address -> reward.toAmountUnsafe // only positive here because it was checked on previous step
      }
    refinedRewards.asRight
  }

  private def buildAddressToLP(currentLiquidityPools: Map[String, LiquidityPool]): Map[Address, Seq[LpInfo]] = {
    val addressToLpInfo: Seq[(Address, LpInfo)] = currentLiquidityPools.values.flatMap { lp =>
      val tokenA = lp.tokenA.identifier
      val tokenB = lp.tokenB.identifier

      lp.poolShares.addressShares.map { case (address, shareAmount) => address -> LpInfo(shareAmount.value, tokenA, tokenB) }
    }.toSeq

    addressToLpInfo.groupMapReduce(_._1)(e => Seq(e._2))(_ ++ _)
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
  ): Map[Address, BigDecimal] = {
    val validatorsTotalReward = epochRewardsForAllocation.collect {
      case (AllocationId(_, AllocationCategory.NodeOperator), value) => value
    }.sum

    approvedValidators match {
      case Nil => Map.empty
      case nonEmptyValidatorsList =>
        val rewardPerValidator = validatorsTotalReward / nonEmptyValidatorsList.size
        nonEmptyValidatorsList.map(_ -> rewardPerValidator).toMap
    }
  }

  private def calculateGovernanceRewards(
    votingPowers: Seq[VotingPowerForAddress],
    currentProgress: EpochProgress
  ): EitherT[F, RewardError, Map[Address, Amount]] = {
    val basePerEpoch = BigDecimal(rewardConfig.governancePool.value.value / epochData.epochProgress1Year)
    val yearRemainder = if (isLastEpochInYear(currentProgress)) {
      rewardConfig.governancePool.value.value % epochData.epochProgress1Year
    } else 0L
    val totalPower = votingPowers.map(v => BigDecimal(v.power.total.value)).sum

    if (votingPowers.isEmpty || totalPower === BigDecimal(0L)) {
      EitherT.pure[F, RewardError](Map.empty[Address, Amount])
    } else {
      val totalForEpoch = basePerEpoch + yearRemainder

      for {
        baseDistribution <- EitherT.fromEither[F](votingPowers.traverse { vote =>
          val share = (totalForEpoch * vote.power.total.value) / totalPower
          toAmount(share, s"Share is negative: $totalForEpoch, ${vote.power.total.value}, $totalPower").map(vote.address -> _)
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
              finalAmount <- EitherT.fromEither[F](toAmount(updatedAmount, s"Final amount is negative: $totalForEpoch, $distributedAmount"))
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
    frozenVotingPowers: Map[Address, VotingPower],
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

      addressToLpInfo = buildAddressToLP(currentLiquidityPools)
      nodeValidatorDistribution <- EitherT.fromEither[F](
        calculateNodeValidatorShare(nodeValidatorsPerEpoch, validators, currentProgress, addressToLpInfo)
      )
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

      governanceVotingPowers = frozenVotingPowers.map { case (address, vote) => VotingPowerForAddress(address, vote) }.toList
      governanceDistribution <- calculateGovernanceRewards(governanceVotingPowers, currentProgress)
    } yield
      RewardDistribution(
        nodeValidatorRewards = nodeValidatorDistribution,
        daoRewards = rewardConfig.daoAddress -> daoReward,
        voteBasedRewards = voteBasedDistribution,
        governanceRewards = governanceDistribution
      )).value
}

case class LpInfo(shareMount: Amount, tokenA: Option[CurrencyId], tokenB: Option[CurrencyId])

case class LiquidityPoolRewardConfiguration(configurations: Seq[ParsedLpConfig]) {
  def contains(epoch: EpochProgress, lpInfo: Seq[LpInfo]): Boolean =
    lpInfo.exists(contains(epoch, _))

  def contains(epoch: EpochProgress, lpInfo: LpInfo): Boolean =
    configurations.exists { config =>
      config.tokenPairs.exists(_.contains(lpInfo.tokenA, lpInfo.tokenB)) &&
      config.start <= epoch &&
      config.end >= epoch &&
      config.minimumShares <= lpInfo.shareMount
    }
}

object LiquidityPoolRewardConfiguration {
  val empty: LiquidityPoolRewardConfiguration = LiquidityPoolRewardConfiguration(Seq.empty)

  sealed trait ParsedToken
  case class ParsedTokenReal(token: CurrencyId) extends ParsedToken
  case object ParsedTokenDag extends ParsedToken
  case object ParsedTokenAny extends ParsedToken

  case class ParsedTokenPair(tokenA: ParsedToken, tokenB: ParsedToken) {
    def contains(rightCurrencyOpt: Option[CurrencyId], leftCurrencyOpt: Option[CurrencyId]): Boolean = {
      def matches(currencyOpt: Option[CurrencyId], parsed: ParsedToken): Boolean =
        (currencyOpt, parsed) match {
          case (_, ParsedTokenAny)                 => true
          case (None, ParsedTokenDag)              => true
          case (Some(id), ParsedTokenReal(realId)) => id == realId
          case _                                   => false
        }

      (matches(rightCurrencyOpt, tokenA) && matches(leftCurrencyOpt, tokenB)) ||
      (matches(rightCurrencyOpt, tokenB) && matches(leftCurrencyOpt, tokenA))
    }
  }

  implicit class TokenPairOps(tokenPair: TokenPairStrings) {
    private val DagLiteral: String = "DAG"
    private val AnyLiteral: String = "*"

    private def parseToken(tokenString: String): Either[String, ParsedToken] =
      tokenString match {
        case DagLiteral => ParsedTokenDag.asRight
        case AnyLiteral => ParsedTokenAny.asRight
        case _          => refineV[DAGAddressRefined](tokenString).map(address => ParsedTokenReal(CurrencyId(Address(address))))
      }

    def parsed: Either[String, ParsedTokenPair] = {
      val parsedA = parseToken(tokenPair.tokenA)
      val parsedB = parseToken(tokenPair.tokenB)

      (parsedA, parsedB).mapN { case (idA, idB) => ParsedTokenPair(idA, idB) }
    }
  }

  case class ParsedLpConfig(start: EpochProgress, end: EpochProgress, minimumShares: Amount, tokenPairs: Seq[ParsedTokenPair])

  implicit class LpRewardInfoOps(lpRewardInfo: LpRewardInfo) {
    def parsed: Either[String, ParsedLpConfig] =
      lpRewardInfo.tokenPairs.toList.traverse(token => token.parsed).map { parsedTokens =>
        ParsedLpConfig(
          lpRewardInfo.startEpoch,
          lpRewardInfo.endEpoch.getOrElse(EpochProgress.MaxValue),
          lpRewardInfo.minimumShares,
          parsedTokens
        )
      }
  }

  implicit class LpRewardConfigOps(configs: Seq[LpRewardInfo]) {
    def parsed: Either[String, LiquidityPoolRewardConfiguration] =
      configs.toList.traverse(c => c.parsed).map { parsedConfig =>
        LiquidityPoolRewardConfiguration(parsedConfig)
      }
  }
}
