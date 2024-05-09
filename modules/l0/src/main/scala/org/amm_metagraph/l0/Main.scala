package org.amm_metagraph.l0

import cats.data.NonEmptyList
import cats.effect.{IO, Resource}
import cats.syntax.applicative._
import cats.syntax.option._
import cats.syntax.validated._
import io.circe.{Decoder, Encoder}
import org.amm_metagraph.l0.custom_routes.CustomRoutes
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
import org.tessellation.currency.l0.CurrencyL0App
import org.tessellation.ext.cats.effect.ResourceIO
import org.tessellation.schema.SnapshotOrdinal
import org.tessellation.schema.cluster.ClusterId
import org.tessellation.schema.semver.{MetagraphVersion, TessellationVersion}
import org.tessellation.security.hash.Hash
import org.tessellation.security.signature.Signed

import java.util.UUID

object Main extends CurrencyL0App(
  "currency-l0",
  "currency L0 node",
  ClusterId(UUID.fromString("517c3a05-9219-471b-a54c-21b7d72f4ae5")),
  tessellationVersion = TessellationVersion.unsafeFrom(org.tessellation.BuildInfo.version),
  metagraphVersion = MetagraphVersion.unsafeFrom(org.amm_metagraph.l0.BuildInfo.version)
) {
  private def makeBaseDataApplicationL0Service(
    calculatedStateService: CalculatedStateService[IO]
  ): BaseDataApplicationL0Service[IO] =
    BaseDataApplicationL0Service(
      new DataApplicationL0Service[IO, AmmUpdate, AmmOnChainState, AmmCalculatedState] {
        override def genesis: DataState[AmmOnChainState, AmmCalculatedState] =
          DataState(AmmOnChainState(List.empty), AmmCalculatedState(Map.empty))

        override def validateUpdate(
          update: AmmUpdate
        )(implicit context: L0NodeContext[IO]): IO[DataApplicationValidationErrorOr[Unit]] =
          ().validNec.pure[IO]

        override def validateData(
          state  : DataState[AmmOnChainState, AmmCalculatedState],
          updates: NonEmptyList[Signed[AmmUpdate]]
        )(implicit context: L0NodeContext[IO]): IO[DataApplicationValidationErrorOr[Unit]] =
          LifecycleSharedFunctions.validateData[IO](state, updates)

        override def combine(
          state  : DataState[AmmOnChainState, AmmCalculatedState],
          updates: List[Signed[AmmUpdate]]
        )(implicit context: L0NodeContext[IO]): IO[DataState[AmmOnChainState, AmmCalculatedState]] =
          LifecycleSharedFunctions.combine[IO](state, updates)

        override def dataEncoder: Encoder[AmmUpdate] =
          implicitly[Encoder[AmmUpdate]]

        override def calculatedStateEncoder: Encoder[AmmCalculatedState] =
          implicitly[Encoder[AmmCalculatedState]]

        override def dataDecoder: Decoder[AmmUpdate] =
          implicitly[Decoder[AmmUpdate]]

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

        override def getCalculatedState(implicit context: L0NodeContext[IO]): IO[(SnapshotOrdinal, AmmCalculatedState)] =
          calculatedStateService.get.map(calculatedState => (calculatedState.ordinal, calculatedState.state))

        override def setCalculatedState(
          ordinal: SnapshotOrdinal,
          state  : AmmCalculatedState
        )(implicit context: L0NodeContext[IO]): IO[Boolean] =
          calculatedStateService.update(ordinal, state)

        override def hashCalculatedState(
          state: AmmCalculatedState
        )(implicit context: L0NodeContext[IO]): IO[Hash] =
          calculatedStateService.hash(state)

        override def routes(implicit context: L0NodeContext[IO]): HttpRoutes[IO] =
          CustomRoutes[IO](calculatedStateService).public

        override def serializeCalculatedState(
          state: AmmCalculatedState
        ): IO[Array[Byte]] =
          IO(Serializers.serializeCalculatedState(state))

        override def deserializeCalculatedState(
          bytes: Array[Byte]
        ): IO[Either[Throwable, AmmCalculatedState]] =
          IO(Deserializers.deserializeCalculatedState(bytes))
      })

  override def dataApplication: Option[Resource[IO, BaseDataApplicationL0Service[IO]]] =
    CalculatedStateService.make[IO].map(makeBaseDataApplicationL0Service).asResource.some
}

