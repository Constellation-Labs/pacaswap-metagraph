package org.amm_metagraph.data_l1

import cats.effect.Async

import io.constellationnetwork.currency.dataApplication._
import io.constellationnetwork.currency.dataApplication.dataApplication.{DataApplicationBlock, DataApplicationValidationErrorOr}
import io.constellationnetwork.json.JsonSerializer
import io.constellationnetwork.security.signature.Signed

import io.circe.{Decoder, Encoder}
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.codecs.DataUpdateCodec._
import org.amm_metagraph.shared_data.types.codecs.JsonWithBase64BinaryCodec
import org.amm_metagraph.shared_data.validations.ValidationService
import org.http4s.circe.CirceEntityCodec.circeEntityDecoder
import org.http4s.{EntityDecoder, HttpRoutes}

object DataL1Service {

  def make[F[+_]: Async](
    validationService: ValidationService[F],
    dataUpdateCodec: JsonWithBase64BinaryCodec[F, AmmUpdate],
    jsonSerializer: JsonSerializer[F]
  ): BaseDataApplicationL1Service[F] =
    makeBaseDataApplicationL1Service(
      validationService,
      dataUpdateCodec,
      jsonSerializer
    )

  private def makeBaseDataApplicationL1Service[F[+_]: Async](
    validationService: ValidationService[F],
    dataUpdateCodec: JsonWithBase64BinaryCodec[F, AmmUpdate],
    jsonSerializer: JsonSerializer[F]
  ): BaseDataApplicationL1Service[F] = BaseDataApplicationL1Service(
    new DataApplicationL1Service[F, AmmUpdate, AmmOnChainState, AmmCalculatedState] {
      override def validateUpdate(
        update: AmmUpdate
      )(implicit context: L1NodeContext[F]): F[DataApplicationValidationErrorOr[Unit]] =
        validationService.validateUpdate(update)

      override def routes(implicit context: L1NodeContext[F]): HttpRoutes[F] =
        HttpRoutes.empty

      override def dataEncoder: Encoder[AmmUpdate] =
        implicitly[Encoder[AmmUpdate]]

      override def dataDecoder: Decoder[AmmUpdate] =
        implicitly[Decoder[AmmUpdate]]

      override def calculatedStateEncoder: Encoder[AmmCalculatedState] =
        implicitly[Encoder[AmmCalculatedState]]

      override def calculatedStateDecoder: Decoder[AmmCalculatedState] =
        implicitly[Decoder[AmmCalculatedState]]

      override def signedDataEntityDecoder: EntityDecoder[F, Signed[AmmUpdate]] =
        circeEntityDecoder

      override def serializeBlock(
        block: Signed[DataApplicationBlock]
      ): F[Array[Byte]] =
        jsonSerializer.serialize[Signed[DataApplicationBlock]](block)

      override def deserializeBlock(
        bytes: Array[Byte]
      ): F[Either[Throwable, Signed[DataApplicationBlock]]] =
        jsonSerializer.deserialize[Signed[DataApplicationBlock]](bytes)

      override def serializeState(
        state: AmmOnChainState
      ): F[Array[Byte]] =
        jsonSerializer.serialize[AmmOnChainState](state)

      override def deserializeState(
        bytes: Array[Byte]
      ): F[Either[Throwable, AmmOnChainState]] =
        jsonSerializer.deserialize[AmmOnChainState](bytes)

      override def serializeUpdate(
        update: AmmUpdate
      ): F[Array[Byte]] =
        dataUpdateCodec.serialize(update)

      override def deserializeUpdate(
        bytes: Array[Byte]
      ): F[Either[Throwable, AmmUpdate]] =
        dataUpdateCodec.deserialize(bytes)

      override def serializeCalculatedState(
        state: AmmCalculatedState
      ): F[Array[Byte]] =
        jsonSerializer.serialize[AmmCalculatedState](state)

      override def deserializeCalculatedState(
        bytes: Array[Byte]
      ): F[Either[Throwable, AmmCalculatedState]] =
        jsonSerializer.deserialize[AmmCalculatedState](bytes)
    }
  )
}
