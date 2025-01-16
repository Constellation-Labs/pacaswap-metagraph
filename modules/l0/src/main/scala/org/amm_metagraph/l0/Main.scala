package org.amm_metagraph.l0

import java.util.UUID

import cats.effect.{IO, Resource}
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication._
import io.constellationnetwork.currency.l0.CurrencyL0App
import io.constellationnetwork.ext.cats.effect.ResourceIO
import io.constellationnetwork.json.JsonSerializer
import io.constellationnetwork.schema.cluster.ClusterId
import io.constellationnetwork.schema.semver.{MetagraphVersion, TessellationVersion}
import io.constellationnetwork.security.{Hasher, SecurityProvider}

import org.amm_metagraph.shared_data.app.ApplicationConfigOps
import org.amm_metagraph.shared_data.calculated_state.CalculatedStateService
import org.amm_metagraph.shared_data.combiners.CombinerService
import org.amm_metagraph.shared_data.types.codecs.JsonBinaryCodec
import org.amm_metagraph.shared_data.validations.ValidationService

object Main
    extends CurrencyL0App(
      "currency-l0",
      "currency L0 node",
      ClusterId(UUID.fromString("517c3a05-9219-471b-a54c-21b7d72f4ae5")),
      tessellationVersion = TessellationVersion.unsafeFrom(io.constellationnetwork.BuildInfo.version),
      metagraphVersion = MetagraphVersion.unsafeFrom(org.amm_metagraph.l0.BuildInfo.version)
    ) {

  override def dataApplication: Option[Resource[IO, BaseDataApplicationL0Service[IO]]] = (for {
    implicit0(sp: SecurityProvider[IO]) <- SecurityProvider.forAsync[IO]
    implicit0(json2bin: JsonSerializer[IO]) <- JsonBinaryCodec.forSync[IO].asResource
    implicit0(hasher: Hasher[IO]) = Hasher.forJson[IO]

    config <- ApplicationConfigOps.readDefault[IO].asResource
    calculatedStateService <- CalculatedStateService.make[IO].asResource
    validationService <- ValidationService.make[IO](config).asResource
    combinerService <- CombinerService.make[IO](config).asResource
    l1Service <- MetagraphL0Service.make[IO](calculatedStateService, validationService, combinerService).asResource
  } yield l1Service).some

}
