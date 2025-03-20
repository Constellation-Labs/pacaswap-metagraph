package org.amm_metagraph.l0.custom_routes

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.ext.http4s.AddressVar
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.security.hash.Hash

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import io.circe.generic.auto._
import org.amm_metagraph.l0.custom_routes.Responses.SingleResponse
import org.amm_metagraph.shared_data.calculated_state.CalculatedStateService
import org.amm_metagraph.shared_data.types.Governance._
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.Http4sDsl
import org.http4s.{HttpRoutes, Response}

case class GovernanceRoutes[F[_]: Async](
  calculatedStateService: CalculatedStateService[F]
) extends Http4sDsl[F] {
  private def getAllocationsRewards: F[Response[F]] =
    calculatedStateService.get.flatMap { calculatedState =>
      Ok(SingleResponse(calculatedState.state.allocations.allocationsRewards))
    }

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "governance" / "allocations" / "rewards" => getAllocationsRewards
  }
}
