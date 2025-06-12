package org.amm_metagraph.l0.custom_routes

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.ext.http4s.{AddressVar, HashVar}
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.epoch.EpochProgress
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

case class WithdrawalRoutes[F[_]: Async](
  calculatedStateService: CalculatedStateService[F]
) extends Http4sDsl[F] {
  @derive(encoder, decoder)
  case class WithdrawalStateResponse(
    sourceAddress: Option[Address],
    tokenAId: Option[CurrencyId],
    tokenBId: Option[CurrencyId],
    shareToWithdraw: Option[ShareAmount],
    minAmountAOut: Option[SwapAmount],
    minAmountBOut: Option[SwapAmount],
    maxAmountAOut: Option[SwapAmount],
    maxAmountBOut: Option[SwapAmount],
    state: StateTransitionType
  )

  object WithdrawalStateResponse {
    def from(update: Signed[WithdrawalUpdate], stateTransitionType: StateTransitionType): WithdrawalStateResponse =
      WithdrawalStateResponse(
        update.source.some,
        update.tokenAId,
        update.tokenBId,
        update.shareToWithdraw.some,
        update.minAmountAOut,
        update.minAmountBOut,
        update.maxAmountAOut,
        update.maxAmountBOut,
        stateTransitionType
      )

    def from(withdrawal: WithdrawalCalculatedStateAddress, stateTransitionType: StateTransitionType): WithdrawalStateResponse =
      WithdrawalStateResponse(
        withdrawal.sourceAddress.some,
        withdrawal.tokenAId,
        withdrawal.tokenBId,
        withdrawal.shareToWithdraw.some,
        withdrawal.minAmountAOut,
        withdrawal.minAmountBOut,
        withdrawal.maxAmountAOut,
        withdrawal.maxAmountBOut,
        stateTransitionType
      )

    def from(stateTransitionType: StateTransitionType): WithdrawalStateResponse =
      WithdrawalStateResponse(
        none,
        none,
        none,
        none,
        none,
        none,
        none,
        none,
        stateTransitionType
      )
  }

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

  private def getWithdrawalByHash(withdrawalHash: Hash): F[Response[F]] =
    for {
      calculatedState <- calculatedStateService.get
      withdrawalCalculatedState = getWithdrawalCalculatedState(calculatedState.state)
      result <- findWithdrawalByHash(withdrawalCalculatedState, withdrawalHash)
    } yield result

  private def findWithdrawalByHash(
    state: WithdrawalCalculatedState,
    hash: Hash
  ): F[Response[F]] =
    state.pending.collectFirst {
      case pending: PendingAllowSpend[WithdrawalUpdate] if pending.updateHash === hash =>
        SingleResponse(WithdrawalStateResponse.from(pending.update, PendingAllowSpends))
    }.orElse {
      state.pending.collectFirst {
        case pending: PendingSpendAction[WithdrawalUpdate] if pending.updateHash === hash =>
          SingleResponse(WithdrawalStateResponse.from(pending.update, PendingSpendTransactions))
      }
    }.orElse {
      state.confirmed.value.values.flatMap(_.values).collectFirst {
        case info if info.value.updateHash === hash =>
          SingleResponse(WithdrawalStateResponse.from(info.value, Confirmed))
      }
    }.orElse {
      state.failed.collectFirst {
        case signed if signed.updateHash === hash =>
          SingleResponse(WithdrawalStateResponse.from(Failed))
      }
    }.map(Ok(_))
      .getOrElse(NotFound())

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "withdrawals" / HashVar(withdrawalHashString) / "state"              => getWithdrawalByHash(withdrawalHashString)
    case GET -> Root / "addresses" / AddressVar(address) / "withdrawals" / "last-reference" => getLastWithdrawalReference(address)
  }
}
