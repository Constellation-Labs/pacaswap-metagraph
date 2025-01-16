package org.amm_metagraph.shared_data.app

import eu.timepit.refined.types.numeric.PosDouble

case class ApplicationConfig(
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
