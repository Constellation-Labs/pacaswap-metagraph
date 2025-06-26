package org.amm_metagraph.l0.rewards

import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.currency.dataApplication.DataCalculatedState
import io.constellationnetwork.currency.schema.currency.{CurrencyIncrementalSnapshot, CurrencySnapshotStateProof}
import io.constellationnetwork.node.shared.domain.rewards.Rewards
import io.constellationnetwork.node.shared.infrastructure.consensus.trigger.ConsensusTrigger
import io.constellationnetwork.node.shared.snapshot.currency.CurrencySnapshotEvent
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.balance.{Amount, Balance}
import io.constellationnetwork.schema.transaction.{RewardTransaction, Transaction, TransactionAmount}
import io.constellationnetwork.security.SecurityProvider
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.syntax.sortedCollection._

import eu.timepit.refined.types.all.PosLong
import org.amm_metagraph.shared_data.types.Rewards.{AddressAndRewardType, RewardInfo}
import org.amm_metagraph.shared_data.types.States.AmmCalculatedState
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

/** Rewards created by RewardWithdrawUpdate by moving from calculatedState.rewards.availableRewards to
  * calculatedState.rewards.withdraws.pending RewardsService create rewards based on "pending" data for current epoch. Clearance of pending
  * state is done in RewardWithdrawService
  */
object RewardsService {
  def make[F[_]: Async: SecurityProvider]: Rewards[F, CurrencySnapshotStateProof, CurrencyIncrementalSnapshot, CurrencySnapshotEvent] = {
    val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

    (
      lastArtifact: Signed[CurrencyIncrementalSnapshot],
      _: SortedMap[Address, Balance],
      _: SortedSet[Signed[Transaction]],
      _: ConsensusTrigger,
      _: Set[CurrencySnapshotEvent],
      maybeCalculatedState: Option[DataCalculatedState]
    ) =>
      maybeCalculatedState.collect {
        case ammCalculatedState: AmmCalculatedState => ammCalculatedState
      } match {
        case Some(calculatedState: AmmCalculatedState) =>
          val currentEpoch = lastArtifact.epochProgress

          val addressesAndAmount: List[(Address, Amount)] =
            calculatedState.rewards.withdraws.pending.getOrElse(currentEpoch, RewardInfo.empty).info.toList.map {
              case (AddressAndRewardType(address, _), amount) => address -> amount
            }

          val rewardTransactions: List[RewardTransaction] = addressesAndAmount.flatMap {
            case (address, amount) =>
              PosLong.from(amount.value.value).map(pos => RewardTransaction(address, TransactionAmount(pos))).toList
          }

          logger.info(show"Distribute rewards for epoch $currentEpoch: $rewardTransactions") >>
            rewardTransactions.toSortedSet.pure[F]

        case None => SortedSet.empty[RewardTransaction].pure[F]
      }
  }
}
