package org.amm_metagraph.l0.custom_routes

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.ext.http4s.AddressVar
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.balance.Amount

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import org.amm_metagraph.l0.custom_routes.Responses.SingleResponse
import org.amm_metagraph.shared_data.calculated_state.{CalculatedState, CalculatedStateService}
import org.amm_metagraph.shared_data.types.RewardWithdraw.RewardWithdrawReference
import org.amm_metagraph.shared_data.types.Rewards.{AddressAndRewardType, RewardType}
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.Http4sDsl
import org.http4s.{HttpRoutes, Response}

case class RewardWithdrawRoutes[F[_]: Async](
  calculatedStateService: CalculatedStateService[F]
) extends Http4sDsl[F] {
  private def getLastRewardWithdrawalReference(address: Address): F[Response[F]] =
    calculatedStateService.get.flatMap { calculatedState =>
      val maybeRef = for {
        withdrawalData <- calculatedState.state.rewards.withdraws.confirmed.get(address)
      } yield withdrawalData

      Ok(SingleResponse(maybeRef.getOrElse(RewardWithdrawReference.empty)))
    }

  private def getRewardForAddress(address: Address) =
    calculatedStateService.get.flatMap { calculatedState: CalculatedState =>
      val addressesAndTypes = RewardType.values.map(rewardType => AddressAndRewardType(address, rewardType)).toList
      val rewardsInfo = calculatedState.state.rewards.availableRewards.info
      val rewardInfos: List[RewardInfoForAddress] = addressesAndTypes.map { addressAndRewardType =>
        val amount = rewardsInfo.getOrElse(addressAndRewardType, Amount.empty)
        RewardInfoForAddress(addressAndRewardType.rewardType, amount)
      }
      Ok(SingleResponse(rewardInfos))
    }

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "addresses" / AddressVar(address) / "withdrawals" / "last-reference" =>
      getLastRewardWithdrawalReference(address)

    case GET -> Root / "addresses" / AddressVar(address) / "rewards-balance" =>
      getRewardForAddress(address)
  }

}

@derive(encoder, decoder)
case class RewardInfoForAddress(rewardType: RewardType, amount: Amount)
