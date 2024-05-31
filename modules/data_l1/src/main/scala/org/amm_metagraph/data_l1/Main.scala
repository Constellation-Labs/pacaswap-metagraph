package org.amm_metagraph.data_l1

import cats.syntax.all._
import cats.effect.{IO, Resource}
import org.amm_metagraph.shared_data.calculated_state.CalculatedStateService
import org.amm_metagraph.shared_data.types.codecs.JsonBinaryCodec
import org.amm_metagraph.shared_data.validations.ValidationService
import org.tessellation.currency.dataApplication._
import org.tessellation.currency.l1.CurrencyL1App
import org.tessellation.ext.cats.effect.ResourceIO
import org.tessellation.json.JsonSerializer
import org.tessellation.schema.cluster.ClusterId
import org.tessellation.schema.semver.{MetagraphVersion, TessellationVersion}
import org.tessellation.security.SecurityProvider


import java.util.UUID

object Main extends CurrencyL1App(
  "currency-data_l1",
  "currency data L1 node",
  ClusterId(UUID.fromString("517c3a05-9219-471b-a54c-21b7d72f4ae5")),
  tessellationVersion = TessellationVersion.unsafeFrom(org.tessellation.BuildInfo.version),
  metagraphVersion = MetagraphVersion.unsafeFrom(org.amm_metagraph.data_l1.BuildInfo.version)
) {

  override def dataApplication: Option[Resource[IO, BaseDataApplicationL1Service[IO]]] = (for {
    implicit0(sp: SecurityProvider[IO]) <- SecurityProvider.forAsync[IO]
    implicit0(json2bin: JsonSerializer[IO]) <- JsonBinaryCodec.forSync[IO].asResource
    calculatedStateService <- CalculatedStateService.make[IO].asResource
    validationService <- ValidationService.make[IO].asResource
    l1Service <- DataL1Service.make[IO](calculatedStateService, validationService).asResource
  } yield l1Service).some
}
