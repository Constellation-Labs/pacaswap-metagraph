package org.amm_metagraph.shared_data.types.codecs

import io.constellationnetwork.currency.dataApplication.DataUpdate

import io.circe._
import io.circe.syntax.EncoderOps
import org.amm_metagraph.shared_data.types.DataUpdates.AmmUpdate

object DataUpdateCodec {
  implicit val dataUpdateEncoder: Encoder[DataUpdate] = {
    case event: AmmUpdate => event.asJson
    case _                => Json.Null
  }

  implicit val dataUpdateDecoder: Decoder[DataUpdate] = (c: HCursor) => c.as[AmmUpdate]
}
