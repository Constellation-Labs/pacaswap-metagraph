package org.amm_metagraph.l0.custom_routes

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.ext.http4s.AddressVar
import io.constellationnetwork.schema.address.Address

import io.circe.generic.auto._
import org.amm_metagraph.l0.custom_routes.Responses.SingleResponse
import org.amm_metagraph.shared_data.calculated_state.CalculatedStateService
import org.amm_metagraph.shared_data.types.States.{OperationType, WithdrawalCalculatedState}
import org.amm_metagraph.shared_data.types.Withdrawal.WithdrawalReference
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.Http4sDsl
import org.http4s.{HttpRoutes, Response}

case class WithdrawalRoutes[F[_]: Async](
  calculatedStateService: CalculatedStateService[F]
) extends Http4sDsl[F] {
  private def getLastWithdrawalReference(address: Address): F[Response[F]] =
    calculatedStateService.get.flatMap { calculatedState =>
      val maybeRef = for {
        withdrawalState <- calculatedState.state.operations.get(OperationType.Withdrawal).collect {
          case s: WithdrawalCalculatedState => s
        }
        withdrawalData <- withdrawalState.confirmed.value.get(address)
      } yield withdrawalData.lastReference

      Ok(SingleResponse(maybeRef.getOrElse(WithdrawalReference.empty)))
    }

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "addresses" / AddressVar(address) / "withdrawals" / "last-reference" => getLastWithdrawalReference(address)
  }
}
