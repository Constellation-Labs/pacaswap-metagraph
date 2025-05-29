package org.amm_metagraph.l0.custom_routes

import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.SortedSet

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

case class VoteRoutes[F[_]: Async](
  calculatedStateService: CalculatedStateService[F]
) extends Http4sDsl[F] {

  @derive(encoder, decoder)
  case class VoteInfoResponse(
    credits: Double,
    parent: RewardAllocationVoteReference,
    monthlyReference: MonthlyReference,
    allocations: SortedSet[Allocation]
  )

  private def getAddressVoteInfo(address: Address): F[Response[F]] =
    calculatedStateService.get.flatMap { calculatedState =>
      calculatedState.state.allocations.usersAllocations.get(address).fold(NotFound()) { addressAllocations =>
        Ok(
          SingleResponse(
            VoteInfoResponse(
              addressAllocations.credits,
              addressAllocations.reference,
              calculatedState.state.allocations.monthlyReference,
              addressAllocations.allocations
            )
          )
        )
      }
    }

  private def getAddressVoteWeight(address: Address): F[Response[F]] =
    calculatedStateService.get.flatMap { calculatedState =>
      calculatedState.state.votingWeights.get(address).fold(NotFound()) { addressVotingWeights =>
        Ok(SingleResponse(addressVotingWeights))
      }
    }

  private def getAddressVoteInfoLastReference(address: Address): F[Response[F]] =
    calculatedStateService.get.flatMap { calculatedState =>
      calculatedState.state.allocations.usersAllocations
        .get(address)
        .fold(
          Ok(
            SingleResponse(RewardAllocationVoteReference(RewardAllocationVoteOrdinal.first, Hash.empty))
          )
        ) { addressAllocations =>
          Ok(SingleResponse(addressAllocations.reference))
        }
    }

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "addresses" / AddressVar(address) / "vote-info"                    => getAddressVoteInfo(address)
    case GET -> Root / "addresses" / AddressVar(address) / "vote-weight"                  => getAddressVoteWeight(address)
    case GET -> Root / "addresses" / AddressVar(address) / "vote-info" / "last-reference" => getAddressVoteInfoLastReference(address)
  }
}
