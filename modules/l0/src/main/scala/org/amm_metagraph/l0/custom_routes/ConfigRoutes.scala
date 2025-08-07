package org.amm_metagraph.l0.custom_routes

import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.SortedSet

import io.constellationnetwork.schema.address.Address

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import io.circe.generic.auto._
import org.amm_metagraph.l0.custom_routes.Responses.SingleResponse
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.calculated_state.CalculatedStateService
import org.amm_metagraph.shared_data.types.Governance.{Allocation, UserAllocations, VotingPower}
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.Http4sDsl
import org.http4s.{HttpRoutes, Response}

case class ConfigRoutes[F[_]: Async](applicationConfig: ApplicationConfig) extends Http4sDsl[F] {
  private def getConfig: F[Response[F]] =
    Ok(applicationConfig)

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "config" => getConfig
  }
}
