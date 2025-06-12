package org.amm_metagraph.l0.custom_routes

import cats.data.{Validated, ValidatedNec}
import cats.effect.Async
import cats.syntax.all._

import scala.util.Try

import io.constellationnetwork.ext.http4s.{AddressVar, HashVar}
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.{CurrencyId, SwapAmount}
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed

import derevo.cats.{eqv, show}
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import enumeratum.{Enum, EnumEntry}
import io.circe.generic.auto._
import io.circe.{Decoder, Encoder}
import org.amm_metagraph.shared_data.calculated_state.CalculatedStateService
import org.amm_metagraph.shared_data.refined.Percentage
import org.amm_metagraph.shared_data.refined.Percentage._
import org.amm_metagraph.shared_data.services.pricing.PricingService
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, SwapUpdate, WithdrawalUpdate}
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States.StateTransitionType._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.Swap._
import org.amm_metagraph.shared_data.types.Withdrawal.getWithdrawalCalculatedState
import org.amm_metagraph.shared_data.types.codecs.{HasherSelector, JsonWithBase64BinaryCodec}
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.Http4sDsl
import org.http4s.{HttpRoutes, Response}

object SwapRoutes {
  @derive(encoder, decoder)
  case class SwapQuoteRequest(
    fromTokenId: Option[CurrencyId],
    toTokenId: Option[CurrencyId],
    amount: Amount,
    slippagePercent: Percentage
  )

  @derive(encoder, decoder)
  case class SwapStateResponse(
    sourceAddress: Option[Address],
    swapFromPair: Option[CurrencyId],
    swapToPair: Option[CurrencyId],
    amountIn: Option[SwapAmount],
    amountOutGross: Option[SwapAmount],
    amountOutNet: Option[SwapAmount],
    amountOutMinimum: Option[SwapAmount],
    amountOutMaximum: Option[SwapAmount],
    state: StateTransitionType
  )

  object SwapStateResponse {
    def from(update: PendingAction[SwapUpdate], stateTransitionType: StateTransitionType): SwapStateResponse =
      update match {
        case PendingAllowSpend(update, _, _) =>
          SwapStateResponse(
            update.source.some,
            update.swapFromPair,
            update.swapToPair,
            update.amountIn.some,
            none,
            none,
            update.amountOutMinimum.some,
            update.amountOutMaximum,
            stateTransitionType
          )

        case PendingSpendAction(update, _, _, Some(swapInfo: SwapTokenInfo)) =>
          SwapStateResponse(
            update.source.some,
            update.swapFromPair,
            update.swapToPair,
            update.amountIn.some,
            swapInfo.grossReceived.some,
            swapInfo.netReceived.some,
            update.amountOutMinimum.some,
            update.amountOutMaximum,
            stateTransitionType
          )

        case PendingSpendAction(update, _, _, _) =>
          SwapStateResponse(
            update.source.some,
            update.swapFromPair,
            update.swapToPair,
            update.amountIn.some,
            none,
            none,
            update.amountOutMinimum.some,
            update.amountOutMaximum,
            stateTransitionType
          )
      }

    def from(swap: SwapCalculatedStateAddress, stateTransitionType: StateTransitionType): SwapStateResponse =
      SwapStateResponse(
        swap.sourceAddress.some,
        swap.fromToken.identifier,
        swap.toToken.identifier,
        swap.amountIn.some,
        swap.grossReceived.some,
        swap.netReceived.some,
        swap.amountOutMinimum.some,
        swap.amountOutMaximum,
        stateTransitionType
      )

    def from(swapFailedCalculatedState: SwapFailedCalculatedState, stateTransitionType: StateTransitionType): SwapStateResponse =
      SwapStateResponse(
        none,
        none,
        none,
        none,
        swapFailedCalculatedState.swapInfo.map(_.grossReceived),
        swapFailedCalculatedState.swapInfo.map(_.netReceived),
        none,
        none,
        stateTransitionType
      )
  }

  object SwapQuoteRequestValidator {

    trait ValidatedSwapQuoteRequest {
      val message: String
    }

    case class ValidSwapQuoteRequest(
      fromTokenId: Option[CurrencyId],
      amount: Amount
    ) extends ValidatedSwapQuoteRequest {
      val message: String = "Valid request"
    }

    case class ValidSwapQuoteRequestWithState(
      liquidityPool: LiquidityPool
    ) extends ValidatedSwapQuoteRequest {
      val message: String = "Valid request"
    }

    def validate(request: SwapQuoteRequest): ValidatedNec[String, ValidSwapQuoteRequest] =
      (
        validateTokenIds(request.fromTokenId, request.toTokenId),
        validateAmount(request.amount)
      ).mapN(ValidSwapQuoteRequest)

    def validateWithState[F[_]: Async](
      request: SwapQuoteRequest,
      liquidityPoolCalculatedState: LiquidityPoolCalculatedState
    ): F[ValidatedNec[String, ValidSwapQuoteRequestWithState]] =
      validateIfPoolExists(request.fromTokenId, request.toTokenId, liquidityPoolCalculatedState).map { validated =>
        validated.map(liquidityPool => ValidSwapQuoteRequestWithState(liquidityPool))
      }

    private def validateTokenIds(fromTokenId: Option[CurrencyId], toTokenId: Option[CurrencyId]): ValidatedNec[String, Option[CurrencyId]] =
      if (fromTokenId =!= toTokenId) fromTokenId.validNec
      else "fromTokenId and toTokenId must be different".invalidNec

    private def validateAmount(amount: Amount): ValidatedNec[String, Amount] =
      if (amount.value.value > 0) amount.validNec
      else "Amount must be greater than zero".invalidNec

    private def validateIfPoolExists[F[_]: Async](
      fromTokenId: Option[CurrencyId],
      toTokenId: Option[CurrencyId],
      liquidityPoolCalculatedState: LiquidityPoolCalculatedState
    ): F[ValidatedNec[String, LiquidityPool]] =
      for {
        maybePoolId <- buildLiquidityPoolUniqueIdentifier(fromTokenId, toTokenId).attempt
        result <- maybePoolId match {
          case Left(_) => Async[F].pure("Invalid pair of tokens".invalidNec[LiquidityPool])
          case Right(poolId) =>
            getLiquidityPoolByPoolId(liquidityPoolCalculatedState.confirmed.value, poolId).attempt.flatMap {
              case Left(_)      => Async[F].pure("Liquidity pool does not exists".invalidNec[LiquidityPool])
              case Right(value) => Async[F].pure(value.validNec[String])
            }
        }
      } yield result
  }
}

case class SwapRoutes[F[_]: Async: HasherSelector](
  calculatedStateService: CalculatedStateService[F],
  pricingService: PricingService[F],
  dataUpdateCodec: JsonWithBase64BinaryCodec[F, AmmUpdate]
) extends Http4sDsl[F] {
  import Responses._
  import SwapRoutes._

  private def getSwapByHash(swapHash: Hash): F[Response[F]] =
    for {
      calculatedState <- calculatedStateService.get
      swapCalculatedState = getSwapCalculatedState(calculatedState.state)
      result <- findSwapByHash(swapCalculatedState, swapHash)
    } yield result

  private def findSwapByHash(
    state: SwapCalculatedState,
    hash: Hash
  ): F[Response[F]] =
    state.pending.collectFirst {
      case pending: PendingAllowSpend[SwapUpdate] if pending.updateHash === hash =>
        SingleResponse(SwapStateResponse.from(pending, PendingAllowSpends))
    }.orElse {
      state.pending.collectFirst {
        case pending: PendingSpendAction[SwapUpdate] if pending.updateHash === hash =>
          SingleResponse(SwapStateResponse.from(pending, PendingSpendTransactions))
      }
    }.orElse {
      state.confirmed.value.values.flatMap(_.values).collectFirst {
        case info if info.value.updateHash === hash =>
          SingleResponse(SwapStateResponse.from(info.value, Confirmed))
      }
    }.orElse {
      state.failed.collectFirst {
        case failed if failed.updateHash === hash =>
          SingleResponse(SwapStateResponse.from(failed, Failed))
      }
    }.map(Ok(_))
      .getOrElse(NotFound())

  private def handleSwapQuote(
    request: SwapQuoteRequest
  ): F[Response[F]] =
    SwapQuoteRequestValidator.validate(request) match {
      case Validated.Valid(_) =>
        for {
          calculatedState <- calculatedStateService.get
          liquidityPoolState = getLiquidityPoolCalculatedState(calculatedState.state)
          validatedWithState <- SwapQuoteRequestValidator.validateWithState(request, liquidityPoolState)
          result <- validatedWithState match {
            case Validated.Valid(_) =>
              for {
                maybeQuote <- pricingService.getSwapQuote(
                  request.fromTokenId,
                  request.toTokenId,
                  request.amount,
                  request.slippagePercent
                )
                response <- maybeQuote match {
                  case Right(quote) => Ok(SingleResponse(quote))
                  case Left(error)  => BadRequest(ErrorResponse(error))
                }
              } yield response

            case Validated.Invalid(errors) =>
              BadRequest(ErrorResponse(errors.toList.mkString(", ")))
          }
        } yield result

      case Validated.Invalid(errors) =>
        BadRequest(ErrorResponse(errors.toList.mkString(", ")))
    }

  private def getLastSwapReference(address: Address): F[Response[F]] =
    calculatedStateService.get.flatMap { calculatedState =>
      val maybeRef = for {
        swapState <- calculatedState.state.operations.get(OperationType.Swap).collect {
          case s: SwapCalculatedState => s
        }
        swapData <- swapState.confirmed.value.get(address)
      } yield swapData.lastReference

      Ok(SingleResponse(maybeRef.getOrElse(SwapReference.empty)))
    }

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "swaps" / HashVar(swapHashString) / "state" => getSwapByHash(swapHashString)
    case req @ POST -> Root / "swap" / "quote" =>
      for {
        swapQuoteRequest <- req.as[SwapQuoteRequest]
        response <- handleSwapQuote(swapQuoteRequest)
      } yield response
    case GET -> Root / "addresses" / AddressVar(address) / "swaps" / "last-reference" => getLastSwapReference(address)
  }
}
