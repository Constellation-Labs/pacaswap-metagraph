package org.amm_metagraph.shared_data.app

import scala.concurrent.duration.{DurationInt, FiniteDuration}

import io.constellationnetwork.schema._
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress

import derevo.cats.show
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import eu.timepit.refined.cats._
import eu.timepit.refined.types.numeric._
import org.amm_metagraph.shared_data.app.ApplicationConfig.TokenLockLimitsConfig

@derive(show, encoder)
case class ApplicationConfig(
  expirationEpochProgresses: ApplicationConfig.ExpirationEpochProgresses,
  nodeValidatorsGovernanceAllocationId: String,
  environment: ApplicationConfig.Environment,
  governance: ApplicationConfig.Governance,
  rewards: ApplicationConfig.Rewards,
  tokenLimits: ApplicationConfig.TokenLimits,
  allowSpendEpochBufferDelay: EpochProgress,
  epochInfo: ApplicationConfig.EpochMetadata,
  tokenLockLimits: TokenLockLimitsConfig,
  activationEpochs: ApplicationConfig.ActivationEpochs = ApplicationConfig.ActivationEpochs()
)

object ApplicationConfig {

  @derive(show, encoder)
  sealed trait Environment
  case object Dev extends Environment
  case object Testnet extends Environment
  case object Integrationnet extends Environment
  case object Mainnet extends Environment

  @derive(show, encoder)
  case class ExpirationEpochProgresses(
    confirmedOperations: EpochProgress,
    failedOperations: EpochProgress
  )

  @derive(show, encoder)
  case class LockMultiplier(durationInMonths: NonNegLong, multiplier: PosDouble)

  @derive(show, encoder)
  case class VotingPowerMultipliers(locksConfig: Seq[LockMultiplier])

  @derive(show, encoder)
  case class Governance(
    votingPowerMultipliers: VotingPowerMultipliers
  )

  @derive(show, encoder)
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
    availableRewardsPerSnapshot: NonNegInt,
    nodeValidatorConfig: NodeValidatorConfig
  )

  @derive(show, decoder, encoder)
  case class NodeValidatorConfig(LiquidityPoolsConfig: Seq[LpRewardInfo])

  @derive(show, decoder, encoder)
  case class LpRewardInfo(
    startEpoch: EpochProgress,
    endEpoch: Option[EpochProgress],
    minimumShares: Amount,
    tokenPairs: Seq[TokenPairStrings]
  )

  @derive(show, decoder, encoder)
  case class TokenPairStrings(tokenA: String, tokenB: String)

  @derive(show, encoder)
  case class TokenLimits(
    minTokens: NonNegLong,
    maxTokens: NonNegLong
  )

  @derive(show, encoder)
  case class EpochMetadata(oneEpochProgress: FiniteDuration, daysInMonth: Long) {
    val oneEpochProgressInSeconds: Long = oneEpochProgress.toSeconds
    val epochProgressOneDay: Long = (1.day / oneEpochProgress).toLong
    val epochProgress1Month: Long = epochProgressOneDay * daysInMonth
    val epochProgress6Months: Long = epochProgress1Month * 6L
    val epochProgress1Year: Long = epochProgress6Months * 2
    val epochProgress2Years: Long = epochProgress1Year * 2
  }

  @derive(show, encoder)
  case class TokenLockLimitsConfig(
    maxTokenLocksPerAddress: PosInt,
    minTokenLockAmount: PosLong
  )

  /** Epochs at which consensus-visible behavior changes activate.
    *
    * CRITICAL: every field changes the bytes produced by `combine` or `hashCalculatedState`. On a live network all validators MUST flip at
    * the SAME epoch or they fork. Therefore:
    *   - the value MUST be identical in every node's config, and
    *   - it MUST be a future epoch agreed for a coordinated rollout (set once the new code is deployed to the whole cluster but before the
    *     epoch is reached).
    *
    * The default is `EpochProgress.MaxValue` ("never"), so an un-coordinated deploy keeps the legacy behavior on every node and cannot
    * accidentally fork. Ops sets the real epoch in the environment HOCON.
    */
  @derive(show, encoder)
  case class ActivationEpochs(
    // D2-01/D2-02: deterministic BigInt share math + rejection of dust stakes that would mint 0 shares or throw.
    // EPOCH SPACE: the GLOBAL synchronized snapshot epoch (lastSyncGlobalEpochProgress).
    stakingShareMintFix: EpochProgress = EpochProgress.MaxValue,
    // D1-01: fail-closed when the node-local cache is missing a global snapshot inside the consensus-agreed sync
    // range, instead of silently treating it as empty (which can fork via a divergent operations.confirmed hash).
    // EPOCH SPACE: the METAGRAPH snapshot epoch (currentSnapshotEpochProgress). Deploy cache persistence first, let
    // caches fill, THEN set this to a future epoch for a watched, coordinated rollout.
    globalSyncDataIntegrity: EpochProgress = EpochProgress.MaxValue,
    // D2-06: when the epoch advances by more than 1 between combines (catch-up / missed time triggers), distribute
    // rewards for EVERY scheduled distribution boundary crossed, not just the single observed epoch, so reward
    // emission is never silently skipped (under-emission). EPOCH SPACE: the METAGRAPH snapshot epoch.
    rewardEpochCatchUp: EpochProgress = EpochProgress.MaxValue
  )
}
