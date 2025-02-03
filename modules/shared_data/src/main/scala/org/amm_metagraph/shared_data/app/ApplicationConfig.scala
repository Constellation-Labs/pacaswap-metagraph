package org.amm_metagraph.shared_data.app

import io.constellationnetwork.schema.epoch.EpochProgress

import eu.timepit.refined.types.numeric.PosDouble

case class ApplicationConfig(
  failedOperationsExpirationEpochProgresses: EpochProgress,
  nodeValidatorsGovernanceAllocationId: String,
  environment: ApplicationConfig.Environment,
  governance: ApplicationConfig.Governance
)

object ApplicationConfig {

  sealed trait Environment
  case object Dev extends Environment
  case object Prod extends Environment

  case class VotingWeightMultipliers(
    lockForSixMonthsMultiplier: PosDouble,
    lockForOneYearMultiplier: PosDouble,
    lockForTwoOrMoreYearsMultiplier: PosDouble
  )

  case class Governance(
    votingWeightMultipliers: VotingWeightMultipliers
  )
}
