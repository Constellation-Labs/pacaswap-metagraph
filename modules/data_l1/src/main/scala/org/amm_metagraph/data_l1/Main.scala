package org.amm_metagraph.data_l1

import java.util.UUID

import cats.effect.{IO, Resource}
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication._
import io.constellationnetwork.currency.l1.CurrencyL1App
import io.constellationnetwork.ext.cats.effect.ResourceIO
import io.constellationnetwork.json.JsonSerializer
import io.constellationnetwork.schema.cluster.ClusterId
import io.constellationnetwork.schema.semver.{MetagraphVersion, TessellationVersion}
import io.constellationnetwork.security.{Hasher, SecurityProvider}

import org.amm_metagraph.shared_data.app.ApplicationConfigOps
import org.amm_metagraph.shared_data.types.DataUpdates.AmmUpdate
import org.amm_metagraph.shared_data.types.codecs.{JsonBinaryCodec, JsonWithBase64BinaryCodec}
import org.amm_metagraph.shared_data.validations.ValidationService

object Main
    extends CurrencyL1App(
      "currency-data_l1",
      "currency data L1 node",
      ClusterId(UUID.fromString("517c3a05-9219-471b-a54c-21b7d72f4ae5")),
      tessellationVersion = TessellationVersion.unsafeFrom(io.constellationnetwork.BuildInfo.version),
      metagraphVersion = MetagraphVersion.unsafeFrom(org.amm_metagraph.data_l1.BuildInfo.version)
    ) {

  override def dataApplication: Option[Resource[IO, BaseDataApplicationL1Service[IO]]] = (for {
    implicit0(sp: SecurityProvider[IO]) <- SecurityProvider.forAsync[IO]
    implicit0(json2bin: JsonSerializer[IO]) <- JsonBinaryCodec.forSync[IO].asResource
    implicit0(hasher: Hasher[IO]) = Hasher.forJson[IO]

    dataUpdateCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate].asResource
    config <- ApplicationConfigOps.readDefault[IO].asResource
    validationService <- ValidationService.make[IO](config).asResource
    l1Service <- DataL1Service.make[IO](validationService, dataUpdateCodec).asResource
  } yield l1Service).some
}
