package org.amm_metagraph.shared_data.app

import cats.effect.kernel.Sync

import eu.timepit.refined.api.RefType
import eu.timepit.refined.types.numeric.PosDouble
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
  implicit val posDoubleReader: ConfigReader[PosDouble] = ConfigReader.fromString[PosDouble] { str =>
    RefType.applyRef[PosDouble](str.toDouble) match {
      case Right(value) => Right(value)
      case Left(error)  => Left(CannotConvert(str, "PosDouble", error))
    }
  }
  implicit val votingWeightMultipliersConfigReader: ConfigReader[ApplicationConfig.VotingWeightMultipliers] = deriveReader
  implicit val governanceConfigReader: ConfigReader[ApplicationConfig.Governance] = deriveReader
  implicit val configReader: ConfigReader[Environment] = ConfigReader.fromString[Environment] {
    case "dev"  => Right(Dev)
    case "prod" => Right(Prod)
    case other  => Left(CannotConvert(other, "Environment", "Must be 'dev' or 'prod'"))
  }

  implicit val applicationConfigReader: ConfigReader[ApplicationConfig] = deriveReader
}
