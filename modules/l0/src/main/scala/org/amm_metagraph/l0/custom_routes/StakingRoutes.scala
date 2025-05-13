package org.amm_metagraph.l0.custom_routes

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.ext.http4s.AddressVar
import io.constellationnetwork.schema.address.Address

import io.circe.generic.auto._
import org.amm_metagraph.l0.custom_routes.Responses.SingleResponse
import org.amm_metagraph.shared_data.calculated_state.CalculatedStateService
import org.amm_metagraph.shared_data.types.Staking.StakingReference
import org.amm_metagraph.shared_data.types.States.{OperationType, StakingCalculatedState}
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.Http4sDsl
import org.http4s.{HttpRoutes, Response}

case class StakingRoutes[F[_]: Async](
  calculatedStateService: CalculatedStateService[F]
) extends Http4sDsl[F] {
  private def getLastStakingReference(address: Address): F[Response[F]] =
    calculatedStateService.get.flatMap { calculatedState =>
      val maybeRef = for {
        stakingState <- calculatedState.state.operations.get(OperationType.Staking).collect {
          case s: StakingCalculatedState => s
        }
        stakingData <- stakingState.confirmed.value.get(address)
      } yield stakingData.lastReference

      Ok(SingleResponse(maybeRef.getOrElse(StakingReference.empty)))
    }

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "addresses" / AddressVar(address) / "stakings" / "last-reference" => getLastStakingReference(address)
  }
}
