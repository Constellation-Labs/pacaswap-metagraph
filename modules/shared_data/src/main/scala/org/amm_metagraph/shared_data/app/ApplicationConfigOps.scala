package org.amm_metagraph.shared_data.app

import cats.effect.kernel.Sync

import io.constellationnetwork.schema.epoch.EpochProgress

import eu.timepit.refined.pureconfig._
import eu.timepit.refined.types.numeric.NonNegLong
import org.amm_metagraph.shared_data.app.ApplicationConfig._
import pureconfig._
import pureconfig.error.CannotConvert
import pureconfig.generic.semiauto.deriveReader
import pureconfig.module.catseffect.syntax._

object ApplicationConfigOps {
  import ConfigReaders._

  def readDefault[F[_]: Sync]: F[ApplicationConfig] =
    ConfigSource.default
      .loadF[F, ApplicationConfig]()
}

object ConfigReaders {
  implicit val epochProgressReader: ConfigReader[EpochProgress] = ConfigReader[NonNegLong].map(EpochProgress(_))

  implicit val votingWeightMultipliersConfigReader: ConfigReader[ApplicationConfig.VotingWeightMultipliers] = deriveReader
  implicit val governanceConfigReader: ConfigReader[ApplicationConfig.Governance] = deriveReader
  implicit val configReader: ConfigReader[Environment] = ConfigReader.fromString[Environment] {
    case "dev"  => Right(Dev)
    case "prod" => Right(Prod)
    case other  => Left(CannotConvert(other, "Environment", "Must be 'dev' or 'prod'"))
  }

  implicit val applicationConfigReader: ConfigReader[ApplicationConfig] = deriveReader
}
