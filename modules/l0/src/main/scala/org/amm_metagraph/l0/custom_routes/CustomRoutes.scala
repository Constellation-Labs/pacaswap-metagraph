package org.amm_metagraph.l0.custom_routes

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.routes.internal.{InternalUrlPrefix, PublicRoutes}
import io.constellationnetwork.security.SecurityProvider

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import eu.timepit.refined.auto._
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.calculated_state.CalculatedStateService
import org.amm_metagraph.shared_data.services.pricing.PricingService
import org.amm_metagraph.shared_data.types.DataUpdates.AmmUpdate
import org.amm_metagraph.shared_data.types.States.AmmCalculatedState
import org.amm_metagraph.shared_data.types.codecs.{HasherSelector, JsonWithBase64BinaryCodec}
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.dsl.Http4sDsl
import org.http4s.server.middleware.CORS
import org.http4s.{HttpRoutes, Response}

case class CustomRoutes[F[_]: Async: HasherSelector: SecurityProvider](
  calculatedStateService: CalculatedStateService[F],
  pricingService: PricingService[F],
  dataUpdateCodec: JsonWithBase64BinaryCodec[F, AmmUpdate],
  config: ApplicationConfig
) extends Http4sDsl[F]
    with PublicRoutes[F] {

  @derive(encoder, decoder)
  case class CalculatedStateResponse(
    ordinal: Long,
    calculatedState: AmmCalculatedState
  )

  private def getLatestCalculatedState: F[Response[F]] =
    calculatedStateService.get
      .flatMap(state => Ok(CalculatedStateResponse(state.ordinal.value.value, state.state)))

  private val routes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "calculated-state" / "latest" => getLatestCalculatedState
  }

  val public: HttpRoutes[F] = {
    val liquidityPoolRoutes = LiquidityPoolRoutes(calculatedStateService, pricingService)
    val swapRoutes = SwapRoutes(calculatedStateService, pricingService, dataUpdateCodec)
    val governanceRoutes = GovernanceRoutes(calculatedStateService)
    val voteRoutes = VoteRoutes(calculatedStateService)
    val stakingRoutes = StakingRoutes(calculatedStateService)
    val withdrawalRoutes = WithdrawalRoutes(calculatedStateService)
    val rewardWithdrawRoutes = RewardWithdrawRoutes(calculatedStateService, config)
    val healthCheckRoutes = HealthCheckRoutes(calculatedStateService)

    CORS.policy
      .withAllowCredentials(false)
      .httpRoutes(
        routes <+>
          liquidityPoolRoutes.routes <+>
          swapRoutes.routes <+>
          governanceRoutes.routes <+>
          voteRoutes.routes <+>
          stakingRoutes.routes <+>
          withdrawalRoutes.routes <+>
          rewardWithdrawRoutes.routes <+>
          healthCheckRoutes.routes
      )
  }

  override protected def prefixPath: InternalUrlPrefix = "/"
}
