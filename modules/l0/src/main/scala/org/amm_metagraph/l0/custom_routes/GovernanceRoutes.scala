package org.amm_metagraph.l0.custom_routes

import cats.effect.Async
import cats.syntax.all._

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import io.circe.generic.auto._
import org.amm_metagraph.l0.custom_routes.Responses.SingleResponse
import org.amm_metagraph.shared_data.calculated_state.CalculatedStateService
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.Http4sDsl
import org.http4s.{HttpRoutes, Response}

case class GovernanceRoutes[F[_]: Async](
  calculatedStateService: CalculatedStateService[F]
) extends Http4sDsl[F] {
  private def getAllocationsRewards: F[Response[F]] =
    calculatedStateService.get.flatMap { calculatedState =>
      Ok(SingleResponse(calculatedState.state.allocations.frozenUsedUserVotes))
    }

  private def getAllocationStats: F[Response[F]] =
    calculatedStateService.get.flatMap { calculatedState =>
      val totalVpIssued = calculatedState.state.votingWeights.values.map(v => BigDecimal(v.total.value)).sum

      val allocatedAddresses = calculatedState.state.votingWeights.keySet -- calculatedState.state.allocations.usersAllocations.keySet
      val totalVpAllocated =
        calculatedState.state.votingWeights.filter(vw => allocatedAddresses.contains(vw._1)).values.map(v => BigDecimal(v.total.value)).sum

      val totalTokenLocked =
        calculatedState.state.votingWeights.flatMap(_._2.info).map(vw => BigDecimal(vw.tokenLock.amount.value.value)).sum

      Ok(AllocationStats(totalVpIssued, totalVpAllocated, totalTokenLocked))
    }

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "governance" / "allocations" / "rewards" => getAllocationsRewards
    case GET -> Root / "governance" / "stats"                   => getAllocationStats
  }
}

@derive(encoder, decoder)
case class AllocationStats(totalVpIssued: BigDecimal, totalVpAllocated: BigDecimal, totalTokenLocked: BigDecimal)
