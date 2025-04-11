package org.amm_metagraph.l0

import cats.Applicative
import cats.data.NonEmptyList
import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication._
import io.constellationnetwork.currency.dataApplication.dataApplication.{DataApplicationBlock, DataApplicationValidationErrorOr}
import io.constellationnetwork.json.JsonSerializer
import io.constellationnetwork.routes.internal.ExternalUrlPrefix
import io.constellationnetwork.schema.{GlobalIncrementalSnapshot, GlobalSnapshotInfo, SnapshotOrdinal}
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hashed, SecurityProvider}

import eu.timepit.refined.auto._
import io.circe.{Decoder, Encoder}
import org.amm_metagraph.l0.custom_routes.CustomRoutes
import org.amm_metagraph.shared_data.calculated_state.CalculatedStateService
import org.amm_metagraph.shared_data.services.combiners.L0CombinerService
import org.amm_metagraph.shared_data.services.pricing.PricingService
import org.amm_metagraph.shared_data.storages.GlobalSnapshotsStorage
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.codecs.DataUpdateCodec._
import org.amm_metagraph.shared_data.types.codecs.{HasherSelector, JsonWithBase64BinaryCodec}
import org.amm_metagraph.shared_data.validations.ValidationService
import org.http4s.circe.CirceEntityCodec.circeEntityDecoder
import org.http4s.{EntityDecoder, HttpRoutes}

object MetagraphL0Service {

  def make[F[+_]: Async: HasherSelector](
    calculatedStateService: CalculatedStateService[F],
    validationService: ValidationService[F],
    combinerService: L0CombinerService[F],
    dataUpdateCodec: JsonWithBase64BinaryCodec[F, AmmUpdate],
    jsonSerializer: JsonSerializer[F],
    globalSnapshotsStorage: GlobalSnapshotsStorage[F],
    pricingService: PricingService[F]
  ): BaseDataApplicationL0Service[F] =
    makeBaseDataApplicationL0Service(
      calculatedStateService,
      validationService,
      combinerService,
      dataUpdateCodec,
      jsonSerializer,
      globalSnapshotsStorage,
      pricingService
    )

  private def makeBaseDataApplicationL0Service[F[+_]: Async: HasherSelector](
    calculatedStateService: CalculatedStateService[F],
    validationService: ValidationService[F],
    combinerService: L0CombinerService[F],
    dataUpdateCodec: JsonWithBase64BinaryCodec[F, AmmUpdate],
    jsonSerializer: JsonSerializer[F],
    globalSnapshotsStorage: GlobalSnapshotsStorage[F],
    pricingService: PricingService[F]
  ): BaseDataApplicationL0Service[F] =
    BaseDataApplicationL0Service(new DataApplicationL0Service[F, AmmUpdate, AmmOnChainState, AmmCalculatedState] {
      override def genesis: DataState[AmmOnChainState, AmmCalculatedState] =
        DataState(AmmOnChainState(List.empty), AmmCalculatedState(Map.empty))

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

      override def routes(implicit context: L0NodeContext[F]): HttpRoutes[F] = {
        implicit val sp: SecurityProvider[F] = context.securityProvider
        CustomRoutes[F](calculatedStateService, pricingService, dataUpdateCodec).public
      }

      override def serializeCalculatedState(
        state: AmmCalculatedState
      ): F[Array[Byte]] =
        jsonSerializer.serialize[AmmCalculatedState](state)

      override def deserializeCalculatedState(
        bytes: Array[Byte]
      ): F[Either[Throwable, AmmCalculatedState]] =
        jsonSerializer.deserialize[AmmCalculatedState](bytes)

      override def onGlobalSnapshotPull(
        snapshot: Hashed[GlobalIncrementalSnapshot],
        context: GlobalSnapshotInfo
      )(implicit A: Applicative[F]): F[Unit] =
        globalSnapshotsStorage.set(snapshot)

      override def routesPrefix: ExternalUrlPrefix = "v1"
    })
}
