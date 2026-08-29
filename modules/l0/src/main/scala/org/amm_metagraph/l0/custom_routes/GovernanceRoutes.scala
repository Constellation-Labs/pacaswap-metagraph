package org.amm_metagraph.l0.custom_routes

import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.SortedSet

import io.constellationnetwork.schema.address.Address

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import io.circe.syntax.EncoderOps
import io.circe.{Encoder, JsonObject}
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
      val sum = calculatedState.state.allocations.frozenUsedUserVotes.votingPowerForAddresses.values.map(v => BigDecimal(v.total.value)).sum
      val newVotingResult = GovernanceVotingResultEx(sum, calculatedState.state.allocations.frozenUsedUserVotes)
      Ok(SingleResponse(newVotingResult))
    }

  private def getAllocationStats: F[Response[F]] =
    calculatedStateService.get.flatMap { calculatedState =>
      val totalVpIssued = calculatedState.state.votingPowers.values.map(v => BigDecimal(v.total.value)).sum

      val allocatedAddresses = calculatedState.state.votingPowers.keySet -- calculatedState.state.allocations.usersAllocations.keySet
      val totalVpAllocated =
        calculatedState.state.votingPowers.filter(vw => allocatedAddresses.contains(vw._1)).values.map(v => BigDecimal(v.total.value)).sum

      val totalTokenLocked =
        calculatedState.state.votingPowers.toList.flatMap(_._2.info).map(vw => BigDecimal(vw.tokenLock.amount.value.value)).sum

      Ok(AllocationStats(totalVpIssued, totalVpAllocated, totalTokenLocked))
    }

  private def getLastDistributedRewards: F[Response[F]] =
    calculatedStateService.get.flatMap { calculatedState =>
      Ok(calculatedState.state.rewards.distributedRewards.last)
    }

  private def getPreLastDistributedRewards: F[Response[F]] =
    calculatedStateService.get.flatMap { calculatedState =>
      if (calculatedState.state.rewards.distributedRewards.size > 2) {
        Ok(calculatedState.state.rewards.distributedRewards.takeRight(2).head)
      } else Ok()
    }

  private def getCurrentAllocations: F[Response[F]] =
    calculatedStateService.get.flatMap { calculatedState =>
      val res = calculatedState.state.allocations.usersAllocations.map {
        case (address, UserAllocations(_, _, _, allocations: SortedSet[Allocation])) =>
          val votingPower = calculatedState.state.votingPowers.getOrElse(address, VotingPower.empty)
          CurrentAllocationsForAddress(address, CurrentAllocations(allocations.toList, votingPower))
      }

      Ok(res)
    }

  private def getCurrentMonthReference: F[Response[F]] =
    calculatedStateService.get.flatMap { calculatedState =>
      Ok(calculatedState.state.allocations.monthlyReference)
    }

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "governance" / "stats"                                  => getAllocationStats
    case GET -> Root / "governance-rounds" / "last" / "voting-result"          => getAllocationsRewards
    case GET -> Root / "governance-rounds" / "last" / "distributed-rewards"    => getPreLastDistributedRewards
    case GET -> Root / "governance-rounds" / "current" / "distributed-rewards" => getLastDistributedRewards
    case GET -> Root / "governance-rounds" / "current" / "allocations"         => getCurrentAllocations
    case GET -> Root / "governance-rounds" / "current" / "month-reference"     => getCurrentMonthReference
  }
}

@derive(encoder, decoder)
case class AllocationStats(totalVpIssued: BigDecimal, totalVpAllocated: BigDecimal, totalTokenLocked: BigDecimal)

@derive(encoder, decoder)
case class CurrentAllocationsForAddress(address: Address, allocation: CurrentAllocations)

@derive(encoder, decoder)
case class CurrentAllocations(allocations: List[Allocation], votingPower: VotingPower)

case class GovernanceVotingResultEx(totalVotes: BigDecimal, governanceVotingResult: GovernanceVotingResult)
object GovernanceVotingResultEx {
  val empty = GovernanceVotingResultEx(BigDecimal(0), GovernanceVotingResult.empty)
  implicit val enc: Encoder.AsObject[GovernanceVotingResultEx] =
    Encoder.AsObject.instance { o =>
      val baseObj: JsonObject =
        o.governanceVotingResult.asJson.asObject.getOrElse(JsonObject.empty)
      baseObj.add("total-votes", o.totalVotes.asJson)
    }
}
