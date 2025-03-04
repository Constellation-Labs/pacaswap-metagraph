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
import io.constellationnetwork.security.SecurityProvider
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
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, SwapUpdate}
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States.{LiquidityPoolCalculatedState, OperationType, SwapCalculatedState}
import org.amm_metagraph.shared_data.types.Swap._
import org.amm_metagraph.shared_data.types.codecs.{HasherSelector, JsonWithBase64BinaryCodec}
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.Http4sDsl
import org.http4s.{HttpRoutes, Response}

object SwapRoutes {

  @derive(eqv, show)
  sealed trait SwapState extends EnumEntry

  object SwapState extends Enum[SwapState] with SwapStateCodecs {
    val values = findValues

    case object Confirmed extends SwapState

    case object Pending extends SwapState
  }

  trait SwapStateCodecs {
    implicit val encode: Encoder[SwapState] = Encoder.encodeString.contramap[SwapState](_.entryName)
    implicit val decode: Decoder[SwapState] =
      Decoder.decodeString.emapTry(s => Try(SwapState.withName(s)))
  }

  @derive(encoder, decoder)
  case class SwapQuoteRequest(
    fromTokenId: Option[CurrencyId],
    toTokenId: Option[CurrencyId],
    amount: Amount,
    slippagePercent: Percentage
  )

  @derive(encoder, decoder)
  case class SwapResponse(
    sourceAddress: Address,
    swapFromPair: Option[CurrencyId],
    swapToPair: Option[CurrencyId],
    allowSpendReference: Hash,
    amountIn: SwapAmount,
    amountOutMinimum: SwapAmount,
    maxValidGsEpochProgress: EpochProgress,
    state: SwapState
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

case class SwapRoutes[F[_]: Async: HasherSelector: SecurityProvider](
  calculatedStateService: CalculatedStateService[F],
  pricingService: PricingService[F],
  dataUpdateCodec: JsonWithBase64BinaryCodec[F, AmmUpdate]
) extends Http4sDsl[F] {
  import Responses._
  import SwapRoutes._

  private def getSwapByHash(
    swapHash: Hash
  ): F[Response[F]] = {

    def buildPendingSwapResponse(signedSwap: Signed[SwapUpdate]): F[Response[F]] = {
      val swap = signedSwap.value
      Ok(
        SingleResponse(
          SwapResponse(
            sourceAddress = swap.source,
            swapFromPair = swap.swapFromPair,
            swapToPair = swap.swapToPair,
            allowSpendReference = swap.allowSpendReference,
            amountIn = swap.amountIn,
            amountOutMinimum = swap.amountOutMinimum,
            maxValidGsEpochProgress = swap.maxValidGsEpochProgress,
            state = SwapState.Pending
          )
        )
      )
    }

    def buildConfirmedSwapResponse(swap: SwapCalculatedStateAddress): F[Response[F]] =
      Ok(
        SingleResponse(
          SwapResponse(
            sourceAddress = swap.sourceAddress,
            swapFromPair = swap.fromToken.identifier,
            swapToPair = swap.toToken.identifier,
            allowSpendReference = swap.allowSpendReference,
            amountIn = swap.amountIn,
            amountOutMinimum = swap.amountOutMinimum,
            maxValidGsEpochProgress = swap.maxValidGsEpochProgress,
            state = SwapState.Confirmed
          )
        )
      )

    for {
      calculatedState <- calculatedStateService.get
      swapCalculatedState = getSwapCalculatedState(calculatedState.state)
      pendingAllowSpendsSwap = getPendingAllowSpendsSwapUpdates(calculatedState.state)
      pendingSpendActionSwap = getPendingSpendActionSwapUpdates(calculatedState.state).map(_.update)
      hashedPendingSwaps <- HasherSelector[F].withCurrent { implicit hs =>
        (pendingAllowSpendsSwap ++ pendingSpendActionSwap).toList
          .traverse(_.toHashed(dataUpdateCodec.serialize))
      }
      result <- hashedPendingSwaps.find(_.hash === swapHash) match {
        case Some(found) => buildPendingSwapResponse(found.signed)
        case None =>
          swapCalculatedState.confirmed.value.values.flatten
            .find(_.swapHash === swapHash)
            .map(buildConfirmedSwapResponse)
            .getOrElse(NotFound())
      }

    } yield result
  }

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
      calculatedState.state.operations
        .get(OperationType.Swap)
        .collect {
          case swapCalculatedState: SwapCalculatedState =>
            swapCalculatedState.confirmed.value
              .get(address)
              .flatMap(_.maxByOption(_.parent.ordinal))
              .map(_.parent)
        }
        .fold(Ok(SingleResponse(SwapReference.empty)))(lastRef => Ok(SingleResponse(lastRef)))
    }

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "swaps" / HashVar(swapHashString) => getSwapByHash(swapHashString)
    case req @ POST -> Root / "swap" / "quote" =>
      for {
        swapQuoteRequest <- req.as[SwapQuoteRequest]
        response <- handleSwapQuote(swapQuoteRequest)
      } yield response
    case GET -> Root / "addresses" / AddressVar(address) / "swaps" / "last-reference" => getLastSwapReference(address)
  }
}
