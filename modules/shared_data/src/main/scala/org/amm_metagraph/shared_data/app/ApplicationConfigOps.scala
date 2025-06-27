package org.amm_metagraph.shared_data.app

import cats.effect.kernel.Sync
import cats.syntax.all._

import io.constellationnetwork.node.shared.ext.pureconfig._
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress

import eu.timepit.refined.auto._
import eu.timepit.refined.pureconfig._
import eu.timepit.refined.types.numeric.NonNegLong
import org.amm_metagraph.shared_data.app.ApplicationConfig._
import pureconfig._
import pureconfig.error.CannotConvert
import pureconfig.generic.semiauto.deriveReader
import pureconfig.module.catseffect.syntax._

object ApplicationConfigOps {
  import ConfigReaders._
  implicit val applicationConfigReader: ConfigReader[ApplicationConfig] = deriveReader

  def readDefaultPure: ApplicationConfig =
    ConfigSource.default
      .load[ApplicationConfig](applicationConfigReader)
      .getOrElse(throw new RuntimeException("Failed to load ApplicationConfig"))

  def readDefault[F[_]: Sync]: F[ApplicationConfig] =
    ConfigSource.default
      .loadF[F, ApplicationConfig]()
}

object ConfigReaders {
  implicit val epochProgressReader: ConfigReader[EpochProgress] = ConfigReader[NonNegLong].map(EpochProgress(_))

  implicit val votingWeightMultipliersConfigReader: ConfigReader[ApplicationConfig.VotingWeightMultipliers] = deriveReader
  implicit val expirationEpochProgressesConfigReader: ConfigReader[ApplicationConfig.ExpirationEpochProgresses] = deriveReader
  implicit val tokenLimitsConfigReader: ConfigReader[ApplicationConfig.TokenLimits] = deriveReader
  implicit val governanceConfigReader: ConfigReader[ApplicationConfig.Governance] = deriveReader
  implicit val rewardsConfigReader: ConfigReader[ApplicationConfig.Rewards] = deriveReader[ApplicationConfig.Rewards].emap { _cfg =>
    val cfg = Rewards(
      totalAnnualTokens = Amount(65000000_00000000L),
      governancePool = Amount(20000000_00000000L),
      validatorWeight = NonNegLong(5L),
      daoWeight = NonNegLong(20L),
      votingWeight = NonNegLong(75L),
      initialEpoch = _cfg.initialEpoch,
      daoAddress = _cfg.daoAddress,
      rewardCalculationInterval = _cfg.rewardCalculationInterval,
      rewardWithdrawDelay = _cfg.rewardWithdrawDelay
    )

    Either.cond(
      cfg.validatorWeight.value + cfg.daoWeight.value + cfg.votingWeight.value === 100L,
      cfg,
      CannotConvert(cfg.toString, "Rewards", "Voting weights must sum up to 100")
    )
  }

  implicit val epochMetadataReader: ConfigReader[ApplicationConfig.EpochMetadata] = deriveReader

  implicit val configReader: ConfigReader[Environment] = ConfigReader.fromString[Environment] {
    case "dev"            => Right(Dev)
    case "testnet"        => Right(Testnet)
    case "integrationnet" => Right(Integrationnet)
    case "mainnet"        => Right(Mainnet)
    case other            => Left(CannotConvert(other, "Environment", "Must be 'dev', 'testnet', 'integrationnet', or 'mainnet'"))
  }

  implicit val tokenLockLimitsConfig: ConfigReader[ApplicationConfig.TokenLockLimitsConfig] = deriveReader
}
