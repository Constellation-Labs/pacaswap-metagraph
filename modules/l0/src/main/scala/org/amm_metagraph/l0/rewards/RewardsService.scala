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
import io.constellationnetwork.schema.balance.Balance
import io.constellationnetwork.schema.transaction.{RewardTransaction, Transaction, TransactionAmount}
import io.constellationnetwork.security.SecurityProvider
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.syntax.sortedCollection._

import eu.timepit.refined.types.all.PosLong
import org.amm_metagraph.shared_data.app.ApplicationConfigOps
import org.amm_metagraph.shared_data.types.States.AmmCalculatedState

object RewardsService {
  def make[F[_]: Async: SecurityProvider]: Rewards[F, CurrencySnapshotStateProof, CurrencyIncrementalSnapshot, CurrencySnapshotEvent] =
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
        case Some(calculatedState) =>
          for {
            config <- ApplicationConfigOps.readDefault[F]
            calculator = RewardCalculator.make(config.rewards)
            facilitators <- lastArtifact.proofs.toList.traverse(_.id.toAddress[F])
            votingPowers = calculatedState.votingWeights.toList.map { case (addr, weight) => VotingPower(addr, weight) }
            distribution <- calculator.calculateEpochRewards[F](
              lastArtifact.epochProgress,
              facilitators,
              votingPowers
            )

            rewardTransactions = distribution.toList.flatMap { dist =>
              (dist.governanceRewards.toList ++ dist.votingRewards.toList ++ dist.validatorRewards.toList :+ dist.daoRewards).flatMap {
                case (addr, amount) => PosLong.from(amount.value.value).map(pos => RewardTransaction(addr, TransactionAmount(pos))).toList
              }
            }

          } yield rewardTransactions.toSortedSet
        case None => SortedSet.empty[RewardTransaction].pure[F]
      }
}
