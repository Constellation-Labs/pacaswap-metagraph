package org.amm_metagraph.l0.rewards

import cats.Functor
import cats.data.EitherT
import cats.syntax.all._
import io.constellationnetwork.schema.address.{Address, DAGAddressRefined}
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.tokenLock.{TokenLock, TokenLockAmount, TokenLockFee, TokenLockReference}
import eu.timepit.refined.refineV
import eu.timepit.refined.types.all.NonNegLong
import eu.timepit.refined.types.numeric.PosLong
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.epochProgress._
import org.amm_metagraph.shared_data.types.Governance.{VotingWeight, VotingWeightInfo}
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

  val config: ApplicationConfig.Rewards =
    ApplicationConfig.Rewards(
      totalAnnualTokens = toAmount(toFixedPoint(65000000)),
      governancePool = toAmount(toFixedPoint(20000000)),
      validatorWeight = NonNegLong(5L),
      daoWeight = NonNegLong(20L),
      votingWeight = NonNegLong(75L),
      initialEpoch = EpochProgress.MinValue,
      daoAddress = address("DAG7coCMRPJah33MMcfAEZVeB1vYn3vDRe6WqeGU")
    )

  private def address(str: String) = Address(refineV[DAGAddressRefined].unsafeFrom(str))

  val validatorA: Address = address("DAG7coCMRPJah33MMcfAEZVeB1vYn3vDRe6WqeGU")
  val validatorB: Address = address("DAG0y4eLqhhXUafeE3mgBstezPTnr8L3tZjAtMWB")
  val validators: List[Address] = List(validatorA, validatorB)

  val voterA: Address = address("DAG8Yy2enxizZdWoipKKZg6VXwk7rY2Z54mJqUdC")
  val voterB: Address = address("DAG07tqNLYW8jHU9emXcRTT3CfgCUoumwcLghopd")
  val voterC: Address = address("DAG0y4eLqhhXUafeE3mgBstezPTnr8L3tZjAtMWB")

  def createVotingWeightInfo(epochProgress: EpochProgress): VotingWeightInfo =
    VotingWeightInfo(
      NonNegLong(1000L),
      TokenLock(
        source = voterA,
        amount = TokenLockAmount(PosLong(1000L)),
        fee = TokenLockFee(NonNegLong.MinValue),
        parent = TokenLockReference.empty,
        currencyId = None,
        unlockEpoch = epochProgress
      ),
      epochProgress
    )

  def createVotingPowers(currentProgress: EpochProgress): List[VotingPower] = List(
    VotingPower(voterA, VotingWeight(NonNegLong(4000L), List(createVotingWeightInfo(currentProgress)))),
    VotingPower(voterB, VotingWeight(NonNegLong(1000L), List(createVotingWeightInfo(currentProgress))))
  )

  def createCalculator(config: ApplicationConfig.Rewards = config): RewardCalculator =
    RewardCalculator.make(config)

  test("total rewards per epoch should match configuration") {
    val currentProgress = EpochProgress(NonNegLong.unsafeFrom(epochProgress1Month))

    val result = EitherT {
      createCalculator().calculateEpochRewards(
        currentProgress,
        validators,
        createVotingPowers(currentProgress)
      )
    }

    whenSuccessF(result.value) { rewards =>
      val expectedPerEpoch = config.totalAnnualTokens.value.value / epochProgress1Year

      val totalRewards =
        rewards.validatorRewards.values.map(_.value.value).sum +
          rewards.daoRewards._2.value.value +
          rewards.votingRewards.values.map(_.value.value).sum

      expect(totalRewards == expectedPerEpoch)
    }
  }

  test("validator rewards should be distributed equally") {
    val currentProgress = EpochProgress(NonNegLong.unsafeFrom(epochProgressOneDay * 30))
    val result = EitherT {
      createCalculator().calculateEpochRewards(
        currentProgress,
        validators,
        createVotingPowers(currentProgress)
      )
    }

    whenSuccessF(result.value) { rewards =>
      val allValidatorsGetSameAmount = rewards.validatorRewards.values.toSet.size == 1
      val allValidatorsAreRewarded = rewards.validatorRewards.size == validators.length

      expect.all(
        allValidatorsGetSameAmount,
        allValidatorsAreRewarded
      )
    }
  }

  test("voting rewards should be proportional to voting power") {
    val currentProgress = EpochProgress(NonNegLong.unsafeFrom(epochProgressOneDay * 30))
    val result = EitherT {
      createCalculator().calculateEpochRewards(
        currentProgress,
        validators,
        createVotingPowers(currentProgress)
      )
    }

    whenSuccessF(result.value) { rewards =>
      val voterAReward = rewards.votingRewards(voterA).value.value
      val voterBReward = rewards.votingRewards(voterB).value.value

      // Allow for small rounding differences
      val expectedRatio = 4L // voterA has 4000 power, voterB has 1000 power (4:1 ratio)
      val actualRatioDiff = (voterAReward - voterBReward * expectedRatio).abs

      // Difference should be at most 1 token per power unit (4 in this case)
      expect(actualRatioDiff <= expectedRatio)
    }
  }

  test("only votes from current month should be rewarded") {
    val currentProgress = EpochProgress(NonNegLong.unsafeFrom(epochProgressOneDay * 60))
    val lastMonthProgress = EpochProgress(NonNegLong.unsafeFrom(epochProgressOneDay * 15))

    val mixedVotingPowers = List(
      VotingPower(voterA, VotingWeight(NonNegLong(4000L), List(createVotingWeightInfo(currentProgress)))),
      VotingPower(voterB, VotingWeight(NonNegLong(1000L), List(createVotingWeightInfo(lastMonthProgress))))
    )

    val result = EitherT {
      createCalculator().calculateEpochRewards(
        currentProgress,
        validators,
        mixedVotingPowers
      )
    }

    whenSuccessF(result.value) { rewards =>
      expect(rewards.votingRewards.keySet == Set(voterA))
    }
  }

  test("DAO should receive all remainders") {
    val currentProgress = EpochProgress(NonNegLong.unsafeFrom(epochProgressOneDay * 30))
    val result = EitherT {
      createCalculator().calculateEpochRewards(
        currentProgress,
        validators,
        createVotingPowers(currentProgress)
      )
    }

    whenSuccessF(result.value) { rewards =>
      val expectedPerEpoch = config.totalAnnualTokens.value.value / epochProgress1Year
      val baseDAOShare = expectedPerEpoch * config.daoWeight.value / 100L

      val actualDAOReward = rewards.daoRewards._2.value.value
      val remainderPortion = actualDAOReward - baseDAOShare

      expect(actualDAOReward > baseDAOShare) &&
      expect(remainderPortion > 0L) &&
      expect(actualDAOReward == baseDAOShare + remainderPortion)
    }
  }

  test("base reward proportions should match configured weights") {
    val currentProgress = EpochProgress(NonNegLong.unsafeFrom(epochProgressOneDay * 30))
    val result = EitherT {
      createCalculator().calculateEpochRewards(
        currentProgress,
        validators,
        createVotingPowers(currentProgress)
      )
    }

    whenSuccessF(result.value) { rewards =>
      val expectedPerEpoch = config.totalAnnualTokens.value.value / epochProgress1Year

      val validatorTotal = rewards.validatorRewards.values.map(_.value.value).sum
      val votingTotal = rewards.votingRewards.values.map(_.value.value).sum
      val baseDAOShare = expectedPerEpoch * config.daoWeight.value / 100L

      expect((validatorTotal * 100 - expectedPerEpoch * config.validatorWeight.value).abs <= expectedPerEpoch) &&
      expect((baseDAOShare * 100 - expectedPerEpoch * config.daoWeight.value).abs <= expectedPerEpoch) &&
      expect((votingTotal * 100 - expectedPerEpoch * config.votingWeight.value).abs <= expectedPerEpoch)
    }
  }

  test("when no voters, their share should go to DAO") {
    val currentProgress = EpochProgress(NonNegLong.unsafeFrom(epochProgressOneDay * 30))
    val result = EitherT {
      createCalculator().calculateEpochRewards(
        currentProgress,
        validators,
        List.empty
      )
    }

    whenSuccessF(result.value) { rewards =>
      expect(rewards.votingRewards.isEmpty) &&
      expect(rewards.daoRewards._2.value.value > 0L)
    }
  }

  test("whole year rewards should sum up to annual tokens") {
    val initialEpoch = EpochProgress.MinValue

    val result = (0L until epochProgress1Year).toList.traverse { offset =>
      val epochNumber = initialEpoch.value.value + offset
      val progress = EpochProgress(NonNegLong.unsafeFrom(epochNumber))
      EitherT {
        createCalculator(config.copy(initialEpoch = initialEpoch)).calculateEpochRewards(
          progress,
          validators,
          createVotingPowers(progress)
        )
      }
    }

    whenSuccessF(result.value) { rewards =>
      val totalValidatorRewards = rewards.flatMap(_.validatorRewards.values).map(_.value.value).sum
      val totalDaoRewards = rewards.map(_.daoRewards._2.value.value).sum
      val totalVotingRewards = rewards.flatMap(_.votingRewards.values).map(_.value.value).sum

      val totalDistributed = totalValidatorRewards + totalDaoRewards + totalVotingRewards
      val annualAmount = config.totalAnnualTokens.value.value

      expect(totalDistributed == annualAmount) &&
      // Base proportions should be maintained for distributable amount (excluding remainders in DAO)
      expect((totalValidatorRewards * 100L / annualAmount - config.validatorWeight.value).abs <= 1) &&
      expect((totalVotingRewards * 100L / annualAmount - config.votingWeight.value).abs <= 1)
    }
  }

  test("last epoch in year should get the remainder") {
    val initialEpoch = EpochProgress(NonNegLong.unsafeFrom(1000000L))
    val lastEpochInYear = EpochProgress(NonNegLong.unsafeFrom(1000000L + epochProgress1Year - 1))
    val regularEpoch = EpochProgress(NonNegLong.unsafeFrom(1000000L + 1000L))

    val result = for {
      lastEpochRewards <- EitherT {
        createCalculator(config.copy(initialEpoch = initialEpoch))
          .calculateEpochRewards(lastEpochInYear, validators, createVotingPowers(lastEpochInYear))
      }
      regularEpochRewards <- EitherT {
        createCalculator(config.copy(initialEpoch = initialEpoch))
          .calculateEpochRewards(regularEpoch, validators, createVotingPowers(regularEpoch))
      }
    } yield (lastEpochRewards, regularEpochRewards)

    whenSuccessF(result.value) {
      case (lastEpoch, regularEpoch) =>
        val lastEpochTotal = lastEpoch.validatorRewards.values.map(_.value.value).sum +
          lastEpoch.daoRewards._2.value.value +
          lastEpoch.votingRewards.values.map(_.value.value).sum

        val regularEpochTotal = regularEpoch.validatorRewards.values.map(_.value.value).sum +
          regularEpoch.daoRewards._2.value.value +
          regularEpoch.votingRewards.values.map(_.value.value).sum

        expect(lastEpochTotal > regularEpochTotal)
    }
  }

  test("governance rewards remainder should go the highest power voter") {
    val lastEpochInYear = EpochProgress(NonNegLong.unsafeFrom(1000000L + epochProgress1Year - 1))

    val votingPowers = List(
      VotingPower(voterA, VotingWeight(NonNegLong(4000L), List.empty)),
      VotingPower(voterB, VotingWeight(NonNegLong(2000L), List.empty)),
      VotingPower(voterC, VotingWeight(NonNegLong(2000L), List.empty))
    )

    val result = for {
      lastEpochRewards <- EitherT {
        createCalculator(config.copy(governancePool = toAmount(toFixedPoint(4) * epochProgress1Year))).calculateEpochRewards(
          lastEpochInYear,
          validators,
          votingPowers
        )
      }
    } yield lastEpochRewards

    whenSuccessF(result.value) { rewards =>
      expect.all(
        rewards.governanceRewards.get(voterA).exists(_ === toAmount(toFixedPoint(2))),
        rewards.governanceRewards.get(voterB).exists(_ === toAmount(toFixedPoint(1))),
        rewards.governanceRewards.get(voterC).exists(_ === toAmount(toFixedPoint(1)))
      )
    }
  }

  test("governance rewards remainder should go the random voter (seed based on epoch progress) in case of tie event") {

    val votingPowers = List(
      VotingPower(voterA, VotingWeight(NonNegLong(2000L), List.empty)),
      VotingPower(voterB, VotingWeight(NonNegLong(2000L), List.empty)),
      VotingPower(voterC, VotingWeight(NonNegLong(2000L), List.empty))
    )

    val result = for {
      calculator <- EitherT.pure(createCalculator(config.copy(governancePool = toAmount(toFixedPoint(4) * epochProgress1Year))))

      rewardsA <- EitherT {
        calculator.calculateEpochRewards(
          EpochProgress.MinValue,
          validators,
          votingPowers
        )
      }
      rewardsAA <- EitherT {
        calculator.calculateEpochRewards(
          EpochProgress.MinValue,
          validators,
          votingPowers
        )
      }
      rewardsB <- EitherT {
        calculator.calculateEpochRewards(
          EpochProgress.MaxValue,
          validators,
          votingPowers
        )
      }
      rewardsBB <- EitherT {
        calculator.calculateEpochRewards(
          EpochProgress.MaxValue,
          validators,
          votingPowers
        )
      }
    } yield (rewardsA, rewardsAA, rewardsB, rewardsBB)

    whenSuccessF(result.value) {
      case (a, aa, b, bb) =>
        expect.all(
          a.governanceRewards === aa.governanceRewards,
          b.governanceRewards === bb.governanceRewards,
          a.governanceRewards =!= b.governanceRewards
        )
    }
  }

  test("governance rewards should distribute entire annual pool over one year") {
    val initialEpoch = EpochProgress(NonNegLong.unsafeFrom(1000000L))

    val result = (0L until epochProgress1Year).toList.traverse { offset =>
      val epochNumber = initialEpoch.value.value + offset
      val progress = EpochProgress(NonNegLong.unsafeFrom(epochNumber))
      EitherT {
        createCalculator().calculateEpochRewards(
          progress,
          validators,
          createVotingPowers(progress)
        )
      }
    }

    whenSuccessF(result.value) { rewards =>
      val totalGovernanceRewards = rewards.flatMap(_.governanceRewards.values).map(_.value.value).sum
      expect(totalGovernanceRewards == config.governancePool.value.value)
    }
  }
}
