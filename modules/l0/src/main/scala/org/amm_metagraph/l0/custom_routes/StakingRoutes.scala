package org.amm_metagraph.l0.custom_routes

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.ext.http4s.{AddressVar, HashVar}
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.swap.CurrencyId
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import eu.timepit.refined.types.numeric.PosLong
import io.circe.generic.auto._
import io.circe.refined._
import org.amm_metagraph.l0.custom_routes.Responses.SingleResponse
import org.amm_metagraph.shared_data.calculated_state.CalculatedStateService
import org.amm_metagraph.shared_data.types.DataUpdates.StakingUpdate
import org.amm_metagraph.shared_data.types.Staking.{StakingCalculatedStateAddress, StakingReference, getStakingCalculatedState}
import org.amm_metagraph.shared_data.types.States.StateTransitionType._
import org.amm_metagraph.shared_data.types.States._
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.Http4sDsl
import org.http4s.{HttpRoutes, Response}

case class StakingRoutes[F[_]: Async](
  calculatedStateService: CalculatedStateService[F]
) extends Http4sDsl[F] {

  @derive(encoder, decoder)
  case class StakingStateResponse(
    sourceAddress: Option[Address],
    tokenAId: Option[CurrencyId],
    tokenAAmount: Option[PosLong],
    tokenBId: Option[CurrencyId],
    state: StateTransitionType
  )

  object StakingStateResponse {
    def from(update: Signed[StakingUpdate], stateTransitionType: StateTransitionType): StakingStateResponse =
      StakingStateResponse(
        update.source.some,
        update.tokenAId,
        update.tokenAAmount.some,
        update.tokenBId,
        stateTransitionType
      )

    def from(staking: StakingCalculatedStateAddress, stateTransitionType: StateTransitionType): StakingStateResponse =
      StakingStateResponse(
        staking.sourceAddress.some,
        staking.tokenA.identifier,
        staking.tokenA.amount.some,
        staking.tokenB.identifier,
        stateTransitionType
      )

    def from(stateTransitionType: StateTransitionType): StakingStateResponse =
      StakingStateResponse(
        none,
        none,
        none,
        none,
        stateTransitionType
      )
  }

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

  private def getStakingByHash(stakingHash: Hash): F[Response[F]] =
    for {
      calculatedState <- calculatedStateService.get
      stakingState = getStakingCalculatedState(calculatedState.state)
      result <- findStakingByHash(stakingState, stakingHash)
    } yield result

  private def findStakingByHash(
    state: StakingCalculatedState,
    hash: Hash
  ): F[Response[F]] =
    state.pending.collectFirst {
      case pending: PendingAllowSpend[StakingUpdate] if pending.updateHash === hash =>
        SingleResponse(StakingStateResponse.from(pending.update, PendingAllowSpends))
    }.orElse {
      state.pending.collectFirst {
        case pending: PendingSpendAction[StakingUpdate] if pending.updateHash === hash =>
          SingleResponse(StakingStateResponse.from(pending.update, PendingSpendTransactions))
      }
    }.orElse {
      state.confirmed.value.values.flatMap(_.values).collectFirst {
        case info if info.value.updateHash === hash =>
          SingleResponse(StakingStateResponse.from(info.value, Confirmed))
      }
    }.orElse {
      state.failed.collectFirst {
        case signed if signed.updateHash === hash =>
          SingleResponse(StakingStateResponse.from(Failed))
      }
    }.map(Ok(_))
      .getOrElse(NotFound())

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "stakings" / HashVar(stakingHashString) / "state"                 => getStakingByHash(stakingHashString)
    case GET -> Root / "addresses" / AddressVar(address) / "stakings" / "last-reference" => getLastStakingReference(address)
  }
}
