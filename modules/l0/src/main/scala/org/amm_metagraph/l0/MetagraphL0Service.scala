package org.amm_metagraph.l0

import cats.data.NonEmptyList
import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication._
import io.constellationnetwork.currency.dataApplication.dataApplication.{DataApplicationBlock, DataApplicationValidationErrorOr}
import io.constellationnetwork.json.JsonSerializer
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed

import io.circe.{Decoder, Encoder}
import org.amm_metagraph.l0.custom_routes.CustomRoutes
import org.amm_metagraph.shared_data.calculated_state.CalculatedStateService
import org.amm_metagraph.shared_data.combiners.CombinerService
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.codecs.DataUpdateCodec._
import org.amm_metagraph.shared_data.validations.ValidationService
import org.http4s.circe.CirceEntityCodec.circeEntityDecoder
import org.http4s.{EntityDecoder, HttpRoutes}

object MetagraphL0Service {

  def make[F[+_]: Async: JsonSerializer](
    calculatedStateService: CalculatedStateService[F],
    validationService: ValidationService[F],
    combinerService: CombinerService[F]
  ): F[BaseDataApplicationL0Service[F]] = Async[F].delay {
    makeBaseDataApplicationL0Service(
      calculatedStateService,
      validationService,
      combinerService
    )
  }

  private def makeBaseDataApplicationL0Service[F[+_]: Async: JsonSerializer](
    calculatedStateService: CalculatedStateService[F],
    validationService: ValidationService[F],
    combinerService: CombinerService[F]
  ): BaseDataApplicationL0Service[F] =
    BaseDataApplicationL0Service(new DataApplicationL0Service[F, AmmUpdate, AmmOnChainState, AmmCalculatedState] {
      override def genesis: DataState[AmmOnChainState, AmmCalculatedState] =
        DataState(AmmOnChainState(List.empty), AmmCalculatedState(Map.empty))

      override def validateUpdate(
        update: AmmUpdate
      )(implicit context: L0NodeContext[F]): F[DataApplicationValidationErrorOr[Unit]] =
        validationService.validateUpdate(update)

      override def validateData(
        state: DataState[AmmOnChainState, AmmCalculatedState],
        updates: NonEmptyList[Signed[AmmUpdate]]
      )(implicit context: L0NodeContext[F]): F[DataApplicationValidationErrorOr[Unit]] =
        validationService.validateData(updates, state)

      override def combine(
        state: DataState[AmmOnChainState, AmmCalculatedState],
        updates: List[Signed[AmmUpdate]]
      )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] =
        combinerService.combine(state, updates)

      override def dataEncoder: Encoder[AmmUpdate] =
        implicitly[Encoder[AmmUpdate]]

      override def calculatedStateEncoder: Encoder[AmmCalculatedState] =
        implicitly[Encoder[AmmCalculatedState]]

      override def dataDecoder: Decoder[AmmUpdate] =
        implicitly[Decoder[AmmUpdate]]

      override def calculatedStateDecoder: Decoder[AmmCalculatedState] =
        implicitly[Decoder[AmmCalculatedState]]

      override def signedDataEntityDecoder: EntityDecoder[F, Signed[AmmUpdate]] =
        circeEntityDecoder

      override def serializeBlock(
        block: Signed[DataApplicationBlock]
      ): F[Array[Byte]] =
        JsonSerializer[F].serialize[Signed[DataApplicationBlock]](block)

      override def deserializeBlock(
        bytes: Array[Byte]
      ): F[Either[Throwable, Signed[DataApplicationBlock]]] =
        JsonSerializer[F].deserialize[Signed[DataApplicationBlock]](bytes)

      override def serializeState(
        state: AmmOnChainState
      ): F[Array[Byte]] =
        JsonSerializer[F].serialize[AmmOnChainState](state)

      override def deserializeState(
        bytes: Array[Byte]
      ): F[Either[Throwable, AmmOnChainState]] =
        JsonSerializer[F].deserialize[AmmOnChainState](bytes)

      override def serializeUpdate(
        update: AmmUpdate
      ): F[Array[Byte]] =
        JsonSerializer[F].serialize[AmmUpdate](update)

      override def deserializeUpdate(
        bytes: Array[Byte]
      ): F[Either[Throwable, AmmUpdate]] =
        JsonSerializer[F].deserialize[AmmUpdate](bytes)

      override def getCalculatedState(implicit context: L0NodeContext[F]): F[(SnapshotOrdinal, AmmCalculatedState)] =
        calculatedStateService.get.map(calculatedState => (calculatedState.ordinal, calculatedState.state))

      override def setCalculatedState(
        ordinal: SnapshotOrdinal,
        state: AmmCalculatedState
      )(implicit context: L0NodeContext[F]): F[Boolean] =
        calculatedStateService.update(ordinal, state)

      override def hashCalculatedState(
        state: AmmCalculatedState
      )(implicit context: L0NodeContext[F]): F[Hash] =
        calculatedStateService.hash(state)

      override def routes(implicit context: L0NodeContext[F]): HttpRoutes[F] =
        CustomRoutes[F](calculatedStateService).public

      override def serializeCalculatedState(
        state: AmmCalculatedState
      ): F[Array[Byte]] =
        JsonSerializer[F].serialize[AmmCalculatedState](state)

      override def deserializeCalculatedState(
        bytes: Array[Byte]
      ): F[Either[Throwable, AmmCalculatedState]] =
        JsonSerializer[F].deserialize[AmmCalculatedState](bytes)
    })
}
