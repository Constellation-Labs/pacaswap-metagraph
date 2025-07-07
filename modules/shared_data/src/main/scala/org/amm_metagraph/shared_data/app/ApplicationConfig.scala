package org.amm_metagraph.shared_data.app

import scala.concurrent.duration.{DurationInt, FiniteDuration}

import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress

import eu.timepit.refined.types.numeric._
import org.amm_metagraph.shared_data.app.ApplicationConfig.TokenLockLimitsConfig

case class ApplicationConfig(
  expirationEpochProgresses: ApplicationConfig.ExpirationEpochProgresses,
  nodeValidatorsGovernanceAllocationId: String,
  environment: ApplicationConfig.Environment,
  governance: ApplicationConfig.Governance,
  rewards: ApplicationConfig.Rewards,
  tokenLimits: ApplicationConfig.TokenLimits,
  allowSpendEpochBufferDelay: EpochProgress,
  epochInfo: ApplicationConfig.EpochMetadata,
  tokenLockLimits: TokenLockLimitsConfig
)

object ApplicationConfig {

  sealed trait Environment
  case object Dev extends Environment
  case object Testnet extends Environment
  case object Integrationnet extends Environment
  case object Mainnet extends Environment

  case class ExpirationEpochProgresses(
    confirmedOperations: EpochProgress,
    failedOperations: EpochProgress
  )

  case class VotingWeightMultipliers(
    lockForSixMonthsMultiplier: PosDouble,
    lockForOneYearMultiplier: PosDouble,
    lockForTwoOrMoreYearsMultiplier: PosDouble
  )

  case class Governance(
    votingWeightMultipliers: VotingWeightMultipliers
  )

  case class Rewards(
    totalAnnualTokens: Amount,
    governancePool: Amount,
    nodeValidatorWeight: NonNegLong,
    daoWeight: NonNegLong,
    voteBasedWeight: NonNegLong,
    initialEpoch: EpochProgress,
    daoAddress: Address,
    rewardCalculationInterval: NonNegLong,
    rewardWithdrawDelay: EpochProgress,
    rewardTransactionsPerSnapshot: NonNegInt
  )

  case class TokenLimits(
    minTokens: NonNegLong,
    maxTokens: NonNegLong
  )

  case class EpochMetadata(oneEpochProgress: FiniteDuration, daysInMonth: Long) {
    val oneEpochProgressInSeconds: Long = oneEpochProgress.toSeconds
    val epochProgressOneDay: Long = (1.day / oneEpochProgress).toLong
    val epochProgress1Month: Long = epochProgressOneDay * daysInMonth
    val epochProgress6Months: Long = epochProgress1Month * 6L
    val epochProgress1Year: Long = epochProgress6Months * 2
    val epochProgress2Years: Long = epochProgress1Year * 2
  }

  case class TokenLockLimitsConfig(
    maxTokenLocksPerAddress: PosInt,
    minTokenLockAmount: PosLong
  )
}
