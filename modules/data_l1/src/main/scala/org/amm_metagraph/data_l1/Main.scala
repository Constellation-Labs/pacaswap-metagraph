package org.amm_metagraph.data_l1

import cats.data.NonEmptyList
import cats.effect.{IO, Resource}
import cats.syntax.applicative.catsSyntaxApplicativeId
import cats.syntax.option.catsSyntaxOptionId
import cats.syntax.validated._
import io.circe.{Decoder, Encoder}
import org.amm_metagraph.shared_data.LifecycleSharedFunctions
import org.amm_metagraph.shared_data.calculated_state.CalculatedStateService
import org.amm_metagraph.shared_data.deserializers.Deserializers
import org.amm_metagraph.shared_data.serializers.Serializers
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.States._
import org.http4s.circe.CirceEntityCodec.circeEntityDecoder
import org.http4s.{EntityDecoder, HttpRoutes}
import org.tessellation.currency.dataApplication._
import org.tessellation.currency.dataApplication.dataApplication.{DataApplicationBlock, DataApplicationValidationErrorOr}
import org.tessellation.currency.l1.CurrencyL1App
import org.tessellation.ext.cats.effect.ResourceIO
import org.tessellation.schema.SnapshotOrdinal
import org.tessellation.schema.cluster.ClusterId
import org.tessellation.schema.semver.{MetagraphVersion, TessellationVersion}
import org.tessellation.security.hash.Hash
import org.tessellation.security.signature.Signed

import java.util.UUID

object Main extends CurrencyL1App(
  "currency-data_l1",
  "currency data L1 node",
  ClusterId(UUID.fromString("517c3a05-9219-471b-a54c-21b7d72f4ae5")),
  tessellationVersion = TessellationVersion.unsafeFrom(org.tessellation.BuildInfo.version),
  metagraphVersion = MetagraphVersion.unsafeFrom(org.amm_metagraph.data_l1.BuildInfo.version)
) {

  private def makeBaseDataApplicationL1Service(
    calculatedStateService: CalculatedStateService[IO]
  ): BaseDataApplicationL1Service[IO] = BaseDataApplicationL1Service(
    new DataApplicationL1Service[IO, AmmUpdate, AmmOnChainState, AmmCalculatedState] {
      override def validateData(
        state  : DataState[AmmOnChainState, AmmCalculatedState],
        updates: NonEmptyList[Signed[AmmUpdate]]
      )(implicit context: L1NodeContext[IO]): IO[DataApplicationValidationErrorOr[Unit]] =
        ().validNec.pure[IO]

      override def validateUpdate(
        update: AmmUpdate
      )(implicit context: L1NodeContext[IO]): IO[DataApplicationValidationErrorOr[Unit]] =
        LifecycleSharedFunctions.validateUpdate[IO](update)

      override def combine(
        state  : DataState[AmmOnChainState, AmmCalculatedState],
        updates: List[Signed[AmmUpdate]]
      )(implicit context: L1NodeContext[IO]): IO[DataState[AmmOnChainState, AmmCalculatedState]] =
        state.pure[IO]

      override def routes(implicit context: L1NodeContext[IO]): HttpRoutes[IO] =
        HttpRoutes.empty

      override def dataEncoder: Encoder[AmmUpdate] =
        implicitly[Encoder[AmmUpdate]]

      override def dataDecoder: Decoder[AmmUpdate] =
        implicitly[Decoder[AmmUpdate]]

      override def calculatedStateEncoder: Encoder[AmmCalculatedState] =
        implicitly[Encoder[AmmCalculatedState]]

      override def calculatedStateDecoder: Decoder[AmmCalculatedState] =
        implicitly[Decoder[AmmCalculatedState]]

      override def signedDataEntityDecoder: EntityDecoder[IO, Signed[AmmUpdate]] =
        circeEntityDecoder

      override def serializeBlock(
        block: Signed[DataApplicationBlock]
      ): IO[Array[Byte]] =
        IO(Serializers.serializeBlock(block)(dataEncoder.asInstanceOf[Encoder[DataUpdate]]))

      override def deserializeBlock(
        bytes: Array[Byte]
      ): IO[Either[Throwable, Signed[DataApplicationBlock]]] =
        IO(Deserializers.deserializeBlock(bytes)(dataDecoder.asInstanceOf[Decoder[DataUpdate]]))

      override def serializeState(
        state: AmmOnChainState
      ): IO[Array[Byte]] =
        IO(Serializers.serializeState(state))

      override def deserializeState(
        bytes: Array[Byte]
      ): IO[Either[Throwable, AmmOnChainState]] =
        IO(Deserializers.deserializeState(bytes))

      override def serializeUpdate(
        update: AmmUpdate
      ): IO[Array[Byte]] =
        IO(Serializers.serializeUpdate(update))

      override def deserializeUpdate(
        bytes: Array[Byte]
      ): IO[Either[Throwable, AmmUpdate]] =
        IO(Deserializers.deserializeUpdate(bytes))

      override def getCalculatedState(implicit context: L1NodeContext[IO]): IO[(SnapshotOrdinal, AmmCalculatedState)] =
        calculatedStateService.get.map(calculatedState => (calculatedState.ordinal, calculatedState.state))

      override def setCalculatedState(
        ordinal: SnapshotOrdinal,
        state  : AmmCalculatedState
      )(implicit context: L1NodeContext[IO]): IO[Boolean] =
        calculatedStateService.update(ordinal, state)

      override def hashCalculatedState(
        state: AmmCalculatedState
      )(implicit context: L1NodeContext[IO]): IO[Hash] =
        calculatedStateService.hash(state)

      override def serializeCalculatedState(
        state: AmmCalculatedState
      ): IO[Array[Byte]] =
        IO(Serializers.serializeCalculatedState(state))

      override def deserializeCalculatedState(
        bytes: Array[Byte]
      ): IO[Either[Throwable, AmmCalculatedState]] =
        IO(Deserializers.deserializeCalculatedState(bytes))
    }
  )

  override def dataApplication: Option[Resource[IO, BaseDataApplicationL1Service[IO]]] =
    CalculatedStateService.make[IO].map(makeBaseDataApplicationL1Service).asResource.some
}
