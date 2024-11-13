package org.amm_metagraph.l0.custom_routes

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.routes.internal.{InternalUrlPrefix, PublicRoutes}
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.{CurrencyId, SwapAmount}
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import eu.timepit.refined.auto._
import org.amm_metagraph.shared_data.calculated_state.CalculatedStateService
import org.amm_metagraph.shared_data.types.DataUpdates.SwapUpdate
import org.amm_metagraph.shared_data.types.States.AmmCalculatedState
import org.amm_metagraph.shared_data.types.Swap.{SwapCalculatedStateAddress, getSwapCalculatedState}
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.dsl.Http4sDsl
import org.http4s.server.middleware.CORS
import org.http4s.{HttpRoutes, Response}

case class CustomRoutes[F[_]: Async](calculatedStateService: CalculatedStateService[F]) extends Http4sDsl[F] with PublicRoutes[F] {

  @derive(encoder, decoder)
  case class CalculatedStateResponse(
    ordinal: Long,
    calculatedState: AmmCalculatedState
  )

  @derive(encoder, decoder)
  case class SwapResponse(
    sourceAddress: Address,
    swapFromPair: Option[CurrencyId],
    swapToPair: Option[CurrencyId],
    fee: Long,
    allowSpendReference: Hash,
    minAmount: SwapAmount,
    maxAmount: SwapAmount,
    maxValidGsEpochProgress: EpochProgress,
    state: String
  )

  private def getLatestCalculatedState: F[Response[F]] =
    calculatedStateService.get
      .flatMap(state => Ok(CalculatedStateResponse(state.ordinal.value.value, state.state)))

  private def getSwapByAllowSpendHash(
    allowSpendHashString: String
  ): F[Response[F]] = {
    def buildPendingSwapResponse(swap: Signed[SwapUpdate]): F[Response[F]] = Ok(
      SwapResponse(
        sourceAddress = swap.value.sourceAddress,
        swapFromPair = swap.value.swapFromPair,
        swapToPair = swap.value.swapToPair,
        fee = swap.value.fee.value,
        allowSpendReference = swap.value.allowSpendReference,
        minAmount = swap.value.minAmount,
        maxAmount = swap.value.maxAmount,
        maxValidGsEpochProgress = swap.value.maxValidGsEpochProgress,
        state = "Pending"
      )
    )

    def buildConfirmedSwapResponse(swap: SwapCalculatedStateAddress): F[Response[F]] = Ok(
      SwapResponse(
        sourceAddress = swap.sourceAddress,
        swapFromPair = swap.fromToken.identifier,
        swapToPair = swap.toToken.identifier,
        fee = swap.fee.value,
        allowSpendReference = swap.allowSpendReference,
        minAmount = swap.minAmount,
        maxAmount = swap.maxAmount,
        maxValidGsEpochProgress = swap.maxValidGsEpochProgress,
        state = "Confirmed"
      )
    )

    for {
      calculatedState <- calculatedStateService.get
      allowSpendHash = Hash(allowSpendHashString)
      swapCalculatedState = getSwapCalculatedState(calculatedState.state)
      result <- swapCalculatedState.pending.values.flatten
        .find(_.value.allowSpendReference === allowSpendHash)
        .map(buildPendingSwapResponse)
        .orElse {
          swapCalculatedState.confirmed.values.flatten
            .find(_.allowSpendReference === allowSpendHash)
            .map(buildConfirmedSwapResponse)
        }
        .getOrElse(NotFound())
    } yield result
  }

  private val routes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "calculated-state" / "latest"  => getLatestCalculatedState
    case GET -> Root / "swaps" / allowSpendHashString => getSwapByAllowSpendHash(allowSpendHashString)
  }

  val public: HttpRoutes[F] =
    CORS.policy
      .withAllowCredentials(false)
      .httpRoutes(routes)

  override protected def prefixPath: InternalUrlPrefix = "/"
}
