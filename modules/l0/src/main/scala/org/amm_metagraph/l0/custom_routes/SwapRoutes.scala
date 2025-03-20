package org.amm_metagraph.l0.custom_routes

import cats.data.{Validated, ValidatedNec}
import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.swap.CurrencyId

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import io.circe.generic.auto._
import org.amm_metagraph.shared_data.calculated_state.CalculatedStateService
import org.amm_metagraph.shared_data.refined.Percentage
import org.amm_metagraph.shared_data.refined.Percentage._
import org.amm_metagraph.shared_data.services.pricing.PricingService
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States.LiquidityPoolCalculatedState
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

case class SwapRoutes[F[_]: Async](
  calculatedStateService: CalculatedStateService[F],
  pricingService: PricingService[F]
) extends Http4sDsl[F] {
  import Responses._
  import SwapRoutes._

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

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ POST -> Root / "swap" / "quote" =>
      for {
        swapQuoteRequest <- req.as[SwapQuoteRequest]
        response <- handleSwapQuote(swapQuoteRequest)
      } yield response
  }
}
