package org.amm_metagraph.shared_data.app

import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress

import eu.timepit.refined.types.numeric.{NonNegLong, PosDouble, PosLong}

case class ApplicationConfig(
  failedOperationsExpirationEpochProgresses: EpochProgress,
  nodeValidatorsGovernanceAllocationId: String,
  environment: ApplicationConfig.Environment,
  governance: ApplicationConfig.Governance,
  rewards: ApplicationConfig.Rewards,
  minTokensLiquidityPool: PosLong,
  allowSpendEpochBufferDelay: EpochProgress
)

object ApplicationConfig {

  sealed trait Environment
  case object Dev extends Environment
  case object Testnet extends Environment
  case object Integrationnet extends Environment
  case object Mainnet extends Environment

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
    validatorWeight: NonNegLong,
    daoWeight: NonNegLong,
    votingWeight: NonNegLong,
    initialEpoch: EpochProgress,
    daoAddress: Address
  )
}
