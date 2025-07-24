package org.amm_metagraph.l0.custom_routes

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.ext.http4s.{AddressVar, HashVar}
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.swap.{CurrencyId, SwapAmount}
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import io.circe.generic.auto._
import org.amm_metagraph.l0.custom_routes.Responses.SingleResponse
import org.amm_metagraph.shared_data.calculated_state.CalculatedStateService
import org.amm_metagraph.shared_data.types.DataUpdates.WithdrawalUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool.ShareAmount
import org.amm_metagraph.shared_data.types.States.StateTransitionType._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.Withdrawal.{WithdrawalCalculatedStateAddress, WithdrawalReference, getWithdrawalCalculatedState}
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.Http4sDsl
import org.http4s.{HttpRoutes, Response}

case class HealthCheckRoutes[F[_]: Async](
  calculatedStateService: CalculatedStateService[F]
) extends Http4sDsl[F] {

  @derive(encoder, decoder)
  case class LastGlobalSnapshotSyncResponse(
    lastSyncGlobalSnapshotOrdinal: SnapshotOrdinal
  )

  private def getLastGlobalSnapshotSync: F[Response[F]] =
    for {
      calculatedState <- calculatedStateService.get
      lastSyncGlobalSnapshotOrdinal = calculatedState.state.lastSyncGlobalSnapshotOrdinal
      result <- Ok(SingleResponse(LastGlobalSnapshotSyncResponse(lastSyncGlobalSnapshotOrdinal)))
    } yield result

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "health-check" / "last-global-snapshot-sync" => getLastGlobalSnapshotSync
  }
}
