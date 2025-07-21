package org.amm_metagraph.shared_data.rewards

import cats.Functor
import cats.data.EitherT
import cats.effect.IO
import cats.syntax.all._

import scala.collection.immutable.SortedSet
import scala.concurrent.duration.DurationInt

import io.constellationnetwork.schema.AmountOps.AmountOps
import io.constellationnetwork.schema.address.{Address, DAGAddressRefined}
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.CurrencyId
import io.constellationnetwork.schema.tokenLock._
import io.constellationnetwork.security.hash.Hash

import eu.timepit.refined.refineV
import eu.timepit.refined.types.all.NonNegLong
import eu.timepit.refined.types.numeric.{NonNegInt, PosLong}
import org.amm_metagraph.shared_data.FeeDistributor.FeePercentages
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.app.ApplicationConfig.{LpRewardInfo, NodeValidatorConfig, TokenPairStrings}
import org.amm_metagraph.shared_data.refined.Percentage
import org.amm_metagraph.shared_data.types.Governance._
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import weaver._

object RewardsCalculatorSpec extends SimpleIOSuite {
  private def toAmount(value: Long): Amount = Amount(NonNegLong.unsafeFrom(value))
  private def toFixedPoint(decimal: Double): Long = (decimal * 1e8).toLong

  def whenSuccessF[F[_], E, A](
    fa: F[Either[E, A]]
  )(f: A => Expectations)(
    implicit pos: SourceLocation,
    F: Functor[F]
  ): F[Expectations] =
    fa.map {
      case Right(value) => f(value)
      case Left(error)  => failure(s"Expected Right, but got Left: $error")
    }

  implicit val logger: SelfAwareStructuredLogger[IO] = Slf4jLogger.getLoggerFromName[IO](this.getClass.getName)

  val validatorProportion: NonNegLong = NonNegLong(5L)
  val daoProportion: NonNegLong = NonNegLong(20L)
  val votingProportion: NonNegLong = NonNegLong(75L)

  val lp1TokenA = "DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb"
  val lp1TokenB = "DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ"

  val rewardsConfig: ApplicationConfig.Rewards =
    ApplicationConfig.Rewards(
      totalAnnualTokens = toAmount(toFixedPoint(120000000)),
      governancePool = toAmount(toFixedPoint(20000000)),
      nodeValidatorWeight = validatorProportion,
      daoWeight = daoProportion,
      voteBasedWeight = votingProportion,
      initialEpoch = EpochProgress.MinValue,
      daoAddress = address("DAG7coCMRPJah33MMcfAEZVeB1vYn3vDRe6WqeGU"),
      rewardCalculationInterval = NonNegLong(100L),
      rewardWithdrawDelay = EpochProgress(NonNegLong(10L)),
      rewardTransactionsPerSnapshot = NonNegInt(100),
      nodeValidatorConfig = NodeValidatorConfig(
        Seq(
          LpRewardInfo(
            EpochProgress.MinValue,
            None,
            Amount(NonNegLong.unsafeFrom(1)),
            Seq(TokenPairStrings(lp1TokenA, lp1TokenB))
          )
        )
      )
    )

  val epochData: ApplicationConfig.EpochMetadata = ApplicationConfig.EpochMetadata(1.day, 10L)

  private def address(str: String) = Address(refineV[DAGAddressRefined].unsafeFrom(str))

  val validatorA: Address = address("DAG7coCMRPJah33MMcfAEZVeB1vYn3vDRe6WqeGU")
  val validatorB: Address = address("DAG0y4eLqhhXUafeE3mgBstezPTnr8L3tZjAtMWB")
  val validatorC: Address = address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")
  val validatorD: Address = address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")
  val validators: List[Address] = List(validatorA, validatorB, validatorC)

  val approvedValidators: List[Address] = List(validatorB, validatorC, validatorD)

  val voterA: Address = address("DAG8Yy2enxizZdWoipKKZg6VXwk7rY2Z54mJqUdC")
  val voterB: Address = address("DAG07tqNLYW8jHU9emXcRTT3CfgCUoumwcLghopd")
  val voterC: Address = address("DAG0y4eLqhhXUafeE3mgBstezPTnr8L3tZjAtMWB")

  val lp1Id = "lp1"
  val lp2Id = "lp2"

  val lpParticipant1 = validatorA
  val lpParticipant2 = validatorB
  val lpParticipant3 = validatorD
  val lpParticipant4 = address("DAG6kfTqFxLLPLopHqR43CeQrcvJ5k3eXgYSeELt")

  val lpParticipant1Lp1Share = 1000000L
  val lpParticipant2Lp1Share = 2000000L
  val lpParticipant3Lp1Share = 3000000L
  val lp1Shares: Map[Address, Long] = Map(
    lpParticipant1 -> lpParticipant1Lp1Share,
    lpParticipant2 -> lpParticipant2Lp1Share,
    lpParticipant3 -> lpParticipant3Lp1Share
  )

  val lpParticipant4Lp2Share = 1000000L
  val lpParticipant3Lp2Share = 2000000L
  val lpParticipant2Lp2Share = 3000000L
  val lp2Shares: Map[Address, Long] = Map(
    lpParticipant4 -> lpParticipant4Lp2Share,
    lpParticipant3 -> lpParticipant3Lp2Share,
    lpParticipant2 -> lpParticipant2Lp2Share
  )

  val currentLiquidityPools: Map[String, LiquidityPool] = Map(
    createLp(lp1Id, lp1Shares, lp1TokenA.some, lp1TokenB.some),
    createLp(lp2Id, lp2Shares, lp1TokenB.some, lp1TokenA.some)
  )

  val frozenGovernanceVotes: Map[AllocationId, Percentage] = Map(
    AllocationId(lp1Id, AllocationCategory.LiquidityPool) -> Percentage.unsafeFrom(0.1),
    AllocationId(lp2Id, AllocationCategory.LiquidityPool) -> Percentage.unsafeFrom(0.7),
    AllocationId(validatorC.value.value, AllocationCategory.NodeOperator) -> Percentage.unsafeFrom(0.15),
    AllocationId(validatorD.value.value, AllocationCategory.NodeOperator) -> Percentage.unsafeFrom(0.05)
  )

  def toCurrency(token: Option[String]) =
    token.flatMap(tokenString => refineV[DAGAddressRefined](tokenString).map(address => CurrencyId(Address(address))).toOption)

  def createLp(id: String, addressShares: Map[Address, Long], tokenA: Option[String], tokenB: Option[String]): (String, LiquidityPool) = {
    val poolShares = PoolShares(
      totalShares = PosLong.unsafeFrom(addressShares.values.sum),
      addressShares = addressShares.view.mapValues(v => ShareAmount(Amount(NonNegLong.unsafeFrom(v)))).toMap
    )

    val tokenACurrency = toCurrency(tokenA)
    val tokenBCurrency = toCurrency(tokenB)

    val lp = LiquidityPool(
      updateHash = Hash.empty,
      poolId = PoolId(id),
      tokenA = TokenInformation(tokenACurrency, PosLong(1)),
      tokenB = TokenInformation(tokenBCurrency, PosLong(1)),
      owner = addressShares.head._1,
      k = BigInt(0),
      poolShares = poolShares,
      poolFees = FeePercentages(Percentage.zero, Percentage.zero, Percentage.zero)
    )
    id -> lp
  }

  def frozenVotingPowers(epochProgress: EpochProgress): Map[Address, VotingWeight] =
    createVotingPowers(epochProgress)

  def createVotingWeightInfo(epochProgress: EpochProgress): VotingWeightInfo =
    VotingWeightInfo(
      NonNegLong(1000L),
      TokenLock(
        source = voterA,
        amount = TokenLockAmount(PosLong(1000L)),
        fee = TokenLockFee(NonNegLong.MinValue),
        parent = TokenLockReference.empty,
        currencyId = None,
        unlockEpoch = Some(epochProgress)
      ),
      epochProgress
    )

  def createVotingPowers(currentProgress: EpochProgress): Map[Address, VotingWeight] = Map(
    voterA -> VotingWeight(NonNegLong(4000L), SortedSet(createVotingWeightInfo(currentProgress))),
    voterB -> VotingWeight(NonNegLong(1000L), SortedSet(createVotingWeightInfo(currentProgress)))
  )

  def createCalculator(config: ApplicationConfig.Rewards = rewardsConfig): IO[RewardCalculator[IO]] =
    RewardCalculator.make(config, epochData)

  def approximatelyEqual(a: Double, b: Double, percentTolerance: Double): Boolean = {
    val tolerance = percentTolerance / 100.0 * math.max(a, b)
    math.abs(a - b) <= tolerance
  }

  test("total rewards per epoch should match configuration") {
    val currentProgress = EpochProgress(NonNegLong.unsafeFrom(epochData.epochProgress1Month))

    val result: EitherT[IO, RewardError, RewardDistribution] = EitherT {
      createCalculator().flatMap(
        _.calculateEpochRewards(
          currentProgress,
          validators,
          frozenVotingPowers(currentProgress),
          frozenGovernanceVotes,
          currentLiquidityPools,
          approvedValidators
        )
      )
    }

    whenSuccessF(result.value) { rewards =>
      val expectedPerEpoch = rewardsConfig.totalAnnualTokens.value.value / epochData.epochProgress1Year

      val totalRewards =
        rewards.nodeValidatorRewards.values.map(_.value.value).sum +
          rewards.daoRewards._2.value.value +
          rewards.voteBasedRewards.values.map(_.value.value).sum

      expect(totalRewards === expectedPerEpoch)
    }
  }

  test("Total distribution proportions shall be according config file") {
    val currentProgress = EpochProgress(NonNegLong.unsafeFrom(epochData.epochProgress1Month))

    val result: EitherT[IO, RewardError, RewardDistribution] = EitherT {
      createCalculator().flatMap(
        _.calculateEpochRewards(
          currentProgress,
          validators,
          frozenVotingPowers(currentProgress),
          frozenGovernanceVotes,
          currentLiquidityPools,
          approvedValidators
        )
      )
    }

    whenSuccessF(result.value) { rewards =>
      val totalRewards =
        rewards.nodeValidatorRewards.values.map(_.toBigDecimal).sum +
          rewards.daoRewards._2.toBigDecimal +
          rewards.voteBasedRewards.values.map(_.toBigDecimal).sum

      val validators = rewards.nodeValidatorRewards.values.map(_.toBigDecimal).sum
      val dao = rewards.daoRewards._2.toBigDecimal
      val voting = rewards.voteBasedRewards.values.map(_.toBigDecimal).sum

      expect(approximatelyEqual((validators / totalRewards).doubleValue, validatorProportion.value.doubleValue / 100, 0.1)) &&
      expect(approximatelyEqual((dao / totalRewards).doubleValue, daoProportion.value.doubleValue / 100, 0.1)) &&
      expect(approximatelyEqual((voting / totalRewards).doubleValue, votingProportion.value.doubleValue / 100, 0.1))
    }
  }

  test("validator rewards should be distributed equally") {
    val currentProgress = EpochProgress(NonNegLong.unsafeFrom(epochData.epochProgress1Month))

    val result: EitherT[IO, RewardError, RewardDistribution] = EitherT {
      createCalculator().flatMap(
        _.calculateEpochRewards(
          currentProgress,
          validators,
          frozenVotingPowers(currentProgress),
          frozenGovernanceVotes,
          currentLiquidityPools,
          approvedValidators
        )
      )
    }

    whenSuccessF(result.value) { rewards =>
      expect(
        rewards.nodeValidatorRewards.values.map(_.toBigDecimal).toSet.size == 1 &&
          rewards.nodeValidatorRewards.values.size > 1
      )
    }
  }

  test("Node validator rewards should be distributed taking into account liquidity pools") {
    val currentProgress = EpochProgress(NonNegLong.unsafeFrom(epochData.epochProgress1Month))

    val result: EitherT[IO, RewardError, RewardDistribution] = EitherT {
      createCalculator().flatMap(
        _.calculateEpochRewards(
          currentProgress,
          validators,
          frozenVotingPowers(currentProgress),
          frozenGovernanceVotes,
          currentLiquidityPools,
          approvedValidators
        )
      )
    }

    whenSuccessF(result.value) { rewards =>
      expect(
        rewards.nodeValidatorRewards.keys.toSet == Set(lpParticipant1, lpParticipant2)
      )
    }
  }

  test("Node validator rewards should be distributed taking into account liquidity pools if config have ANY for tokenA literal") {
    val currentProgress = EpochProgress(NonNegLong.unsafeFrom(epochData.epochProgress1Month))
    val nodeValidatorConfig = NodeValidatorConfig(
      Seq(
        LpRewardInfo(
          EpochProgress.MinValue,
          None,
          Amount(NonNegLong.unsafeFrom(1)),
          Seq(TokenPairStrings("*", lp1TokenB))
        )
      )
    )
    val config = rewardsConfig.copy(nodeValidatorConfig = nodeValidatorConfig)
    val result: EitherT[IO, RewardError, RewardDistribution] = EitherT {
      createCalculator(config).flatMap(
        _.calculateEpochRewards(
          currentProgress,
          validators,
          frozenVotingPowers(currentProgress),
          frozenGovernanceVotes,
          currentLiquidityPools,
          approvedValidators
        )
      )
    }

    whenSuccessF(result.value) { rewards =>
      expect(
        rewards.nodeValidatorRewards.keys.toSet == Set(lpParticipant1, lpParticipant2)
      )
    }
  }

  test("Node validator rewards should be distributed taking into account liquidity pools if config have ANY for tokenB literal") {
    val currentProgress = EpochProgress(NonNegLong.unsafeFrom(epochData.epochProgress1Month))
    val nodeValidatorConfig = NodeValidatorConfig(
      Seq(
        LpRewardInfo(
          EpochProgress.MinValue,
          None,
          Amount(NonNegLong.unsafeFrom(1)),
          Seq(TokenPairStrings(lp1TokenA, "*"))
        )
      )
    )
    val config = rewardsConfig.copy(nodeValidatorConfig = nodeValidatorConfig)
    val result: EitherT[IO, RewardError, RewardDistribution] = EitherT {
      createCalculator(config).flatMap(
        _.calculateEpochRewards(
          currentProgress,
          validators,
          frozenVotingPowers(currentProgress),
          frozenGovernanceVotes,
          currentLiquidityPools,
          approvedValidators
        )
      )
    }

    whenSuccessF(result.value) { rewards =>
      expect(
        rewards.nodeValidatorRewards.keys.toSet == Set(lpParticipant1, lpParticipant2)
      )
    }
  }

  test("Node validator rewards should be distributed taking into account liquidity pools min epoch configuration") {
    val currentProgress = EpochProgress(NonNegLong.unsafeFrom(epochData.epochProgress1Month))
    val nodeValidatorConfig = NodeValidatorConfig(
      Seq(
        LpRewardInfo(
          EpochProgress(NonNegLong.unsafeFrom(currentProgress.value.value + 1)),
          None,
          Amount(NonNegLong.unsafeFrom(1)),
          Seq(TokenPairStrings(lp1TokenA, lp1TokenB))
        )
      )
    )
    val config = rewardsConfig.copy(nodeValidatorConfig = nodeValidatorConfig)
    val result: EitherT[IO, RewardError, RewardDistribution] = EitherT {
      createCalculator(config).flatMap(
        _.calculateEpochRewards(
          currentProgress,
          validators,
          frozenVotingPowers(currentProgress),
          frozenGovernanceVotes,
          currentLiquidityPools,
          approvedValidators
        )
      )
    }

    whenSuccessF(result.value) { rewards =>
      expect(
        rewards.nodeValidatorRewards.keys.isEmpty
      )
    }
  }

  test("Node validator rewards should be distributed taking into account liquidity pools max epoch configuration") {
    val currentProgress = EpochProgress(NonNegLong.unsafeFrom(epochData.epochProgress1Month))
    val nodeValidatorConfig = NodeValidatorConfig(
      Seq(
        LpRewardInfo(
          EpochProgress.MinValue,
          EpochProgress(NonNegLong.unsafeFrom(currentProgress.value.value - 1)).some,
          Amount(NonNegLong.unsafeFrom(1)),
          Seq(TokenPairStrings(lp1TokenA, lp1TokenB))
        )
      )
    )
    val config = rewardsConfig.copy(nodeValidatorConfig = nodeValidatorConfig)
    val result: EitherT[IO, RewardError, RewardDistribution] = EitherT {
      createCalculator(config).flatMap(
        _.calculateEpochRewards(
          currentProgress,
          validators,
          frozenVotingPowers(currentProgress),
          frozenGovernanceVotes,
          currentLiquidityPools,
          approvedValidators
        )
      )
    }

    whenSuccessF(result.value) { rewards =>
      expect(
        rewards.nodeValidatorRewards.keys.isEmpty
      )
    }
  }

  test("Node validator rewards should be distributed taking into account liquidity pools max epoch as none configuration") {
    val currentProgress = EpochProgress(NonNegLong.unsafeFrom(epochData.epochProgress1Month))
    val nodeValidatorConfig = NodeValidatorConfig(
      Seq(
        LpRewardInfo(
          EpochProgress.MinValue,
          None,
          Amount(NonNegLong.unsafeFrom(1)),
          Seq(TokenPairStrings(lp1TokenA, lp1TokenB))
        )
      )
    )
    val config = rewardsConfig.copy(nodeValidatorConfig = nodeValidatorConfig)
    val result: EitherT[IO, RewardError, RewardDistribution] = EitherT {
      createCalculator(config).flatMap(
        _.calculateEpochRewards(
          currentProgress,
          validators,
          frozenVotingPowers(currentProgress),
          frozenGovernanceVotes,
          currentLiquidityPools,
          approvedValidators
        )
      )
    }

    whenSuccessF(result.value) { rewards =>
      expect(
        rewards.nodeValidatorRewards.keys.toSet == Set(lpParticipant1, lpParticipant2)
      )
    }
  }

  test("Node validator rewards should be distributed taking into account liquidity pools amount configuration") {
    val currentProgress = EpochProgress(NonNegLong.unsafeFrom(epochData.epochProgress1Month))
    val nodeValidatorConfig = NodeValidatorConfig(
      Seq(
        LpRewardInfo(
          EpochProgress.MinValue,
          None,
          Amount(NonNegLong.unsafeFrom(lpParticipant2Lp1Share)),
          Seq(TokenPairStrings(lp1TokenA, lp1TokenB))
        )
      )
    )
    val config = rewardsConfig.copy(nodeValidatorConfig = nodeValidatorConfig)
    val result: EitherT[IO, RewardError, RewardDistribution] = EitherT {
      createCalculator(config).flatMap(
        _.calculateEpochRewards(
          currentProgress,
          validators,
          frozenVotingPowers(currentProgress),
          frozenGovernanceVotes,
          currentLiquidityPools,
          approvedValidators
        )
      )
    }

    whenSuccessF(result.value) { rewards =>
      expect(
        rewards.nodeValidatorRewards.keys.toSet == Set(lpParticipant2)
      )
    }
  }

  test("Node validator rewards should be distributed taking into account many liquidity pools configuration") {
    val currentProgress = EpochProgress(NonNegLong.unsafeFrom(epochData.epochProgress1Month))
    val nodeValidatorConfig = NodeValidatorConfig(
      Seq(
        LpRewardInfo(
          EpochProgress.MaxValue,
          EpochProgress.MaxValue.some,
          Amount(NonNegLong.unsafeFrom(1)),
          Seq(TokenPairStrings(lp1TokenA, lp1TokenB))
        ),
        LpRewardInfo(
          EpochProgress.MinValue,
          EpochProgress.MinValue.some,
          Amount(NonNegLong.unsafeFrom(1)),
          Seq(TokenPairStrings(lp1TokenA, lp1TokenB))
        ),
        LpRewardInfo(
          EpochProgress.MinValue,
          EpochProgress.MaxValue.some,
          Amount(NonNegLong.unsafeFrom(1)),
          Seq(TokenPairStrings("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ", lp1TokenB), TokenPairStrings(lp1TokenB, lp1TokenA))
        )
      )
    )

    val config = rewardsConfig.copy(nodeValidatorConfig = nodeValidatorConfig)
    val result: EitherT[IO, RewardError, RewardDistribution] = EitherT {
      createCalculator(config).flatMap(
        _.calculateEpochRewards(
          currentProgress,
          validators,
          frozenVotingPowers(currentProgress),
          frozenGovernanceVotes,
          currentLiquidityPools,
          approvedValidators
        )
      )
    }

    whenSuccessF(result.value) { rewards =>
      expect(
        rewards.nodeValidatorRewards.keys.toSet == Set(lpParticipant1, lpParticipant2)
      )
    }
  }

  test("Voting rewards as expected") {
    val currentProgress = EpochProgress(NonNegLong.unsafeFrom(epochData.epochProgress1Month))

    val result: EitherT[IO, RewardError, RewardDistribution] = EitherT {
      createCalculator().flatMap(
        _.calculateEpochRewards(
          currentProgress,
          validators,
          frozenVotingPowers(currentProgress),
          frozenGovernanceVotes,
          currentLiquidityPools,
          approvedValidators
        )
      )
    }

    whenSuccessF(result.value) { rewards =>
      val totalVotingRewards = rewards.voteBasedRewards.values.map(_.toBigDecimal).sum
      expect(totalVotingRewards == BigDecimal(75000000000000L))
      val percentageMap =
        rewards.voteBasedRewards.map { case (address, reward) => address -> reward.toBigDecimal / totalVotingRewards }
      val expectedPercentage = Map(
        address("DAG7coCMRPJah33MMcfAEZVeB1vYn3vDRe6WqeGU") -> 0.01666666666666666666666666666666667,
        address("DAG0y4eLqhhXUafeE3mgBstezPTnr8L3tZjAtMWB") -> 0.3833333333333333333333333333333333,
        address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb") -> 0.3333333333333333333333333333333333,
        address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ") -> 0.15,
        address("DAG6kfTqFxLLPLopHqR43CeQrcvJ5k3eXgYSeELt") -> 0.1166666666666666666666666666666667
      )

      expect(percentageMap.forall { case (address, value) => approximatelyEqual(value.toDouble, expectedPercentage(address), 0.001) })
    }
  }

  test("when no voters, their share should go to DAO") {
    val currentProgress = EpochProgress(NonNegLong.unsafeFrom(epochData.epochProgress1Month))

    val result: EitherT[IO, RewardError, RewardDistribution] = EitherT {
      createCalculator().flatMap(
        _.calculateEpochRewards(
          currentProgress,
          validators,
          frozenVotingPowers(currentProgress),
          Map.empty,
          currentLiquidityPools,
          approvedValidators
        )
      )
    }

    whenSuccessF(result.value) { rewards =>
      val totalRewards =
        rewards.nodeValidatorRewards.values.map(_.toBigDecimal).sum +
          rewards.daoRewards._2.toBigDecimal +
          rewards.voteBasedRewards.values.map(_.toBigDecimal).sum

      val validators = rewards.nodeValidatorRewards.values.map(_.toBigDecimal).sum
      val dao = rewards.daoRewards._2.toBigDecimal
      val voting = rewards.voteBasedRewards.values.map(_.toBigDecimal).sum

      expect(approximatelyEqual((validators / totalRewards).doubleValue, validatorProportion.value.doubleValue / 100, 0.1)) &&
      expect(
        approximatelyEqual(
          (dao / totalRewards).doubleValue,
          (daoProportion.value.doubleValue + votingProportion.value.doubleValue) / 100,
          0.1
        )
      ) &&
      expect(approximatelyEqual((voting / totalRewards).doubleValue, 0, 0.1))
    }
  }

  test("whole year rewards should sum up to annual tokens") {
    val initialEpoch = EpochProgress.MinValue

    val result = (0L until epochData.epochProgress1Year).toList.traverse { offset =>
      val epochNumber = initialEpoch.value.value + offset
      val progress = EpochProgress(NonNegLong.unsafeFrom(epochNumber))
      EitherT[IO, RewardError, RewardDistribution] {
        createCalculator(rewardsConfig.copy(initialEpoch = initialEpoch)).flatMap(
          _.calculateEpochRewards(
            progress,
            validators,
            frozenVotingPowers(progress),
            frozenGovernanceVotes,
            currentLiquidityPools,
            approvedValidators
          )
        )
      }
    }

    whenSuccessF(result.value) { rewards =>
      val totalValidatorRewards = rewards.flatMap(_.nodeValidatorRewards.values).map(_.value.value).sum
      val totalDaoRewards = rewards.map(_.daoRewards._2.value.value).sum
      val totalVotingRewards = rewards.flatMap(_.voteBasedRewards.values).map(_.value.value).sum

      val totalDistributed = totalValidatorRewards + totalDaoRewards + totalVotingRewards
      val annualAmount = rewardsConfig.totalAnnualTokens.value.value

      expect(totalDistributed === annualAmount) &&
      // Base proportions should be maintained for distributable amount (excluding remainders in DAO)
      expect((totalValidatorRewards * 100L / annualAmount - rewardsConfig.nodeValidatorWeight.value).abs <= 1) &&
      expect((totalVotingRewards * 100L / annualAmount - rewardsConfig.voteBasedWeight.value).abs <= 1)
    }
  }
}
