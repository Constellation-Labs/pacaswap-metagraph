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
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.syntax.sortedCollection._

import eu.timepit.refined.types.all.PosLong
import org.amm_metagraph.l0.CsvRewardGenesisReader
import org.amm_metagraph.l0.CsvRewardGenesisReader.RewardGenesisInfo
import org.amm_metagraph.shared_data.types.Rewards.{AddressAndRewardType, RewardInfo}
import org.amm_metagraph.shared_data.types.States.AmmCalculatedState
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

/** Rewards created by RewardWithdrawUpdate by moving from calculatedState.rewards.availableRewards to
  * calculatedState.rewards.withdraws.pending RewardsService create rewards based on "pending" data for current epoch. Clearance of pending
  * state is done in RewardWithdrawService
  */
object RewardsService {
  val limitToDistributeGenesis = 10

  def logger[F[_]: Async]: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

  def buildGenesisRewards[F[_]: Async](currentOrdinal: Long): F[SortedSet[RewardTransaction]] = {
    val firstDistributionOrdinal = 2L
    val lastDistributionOrdinal = firstDistributionOrdinal + limitToDistributeGenesis - 1
    if (currentOrdinal < firstDistributionOrdinal || currentOrdinal > lastDistributionOrdinal)
      SortedSet.empty[RewardTransaction].pure[F]
    else
      for {
        addressInfos <- CsvRewardGenesisReader.readFromResources("genesis.csv")
        totalRows = addressInfos.size
        rowsPerOrdinal = math.ceil(totalRows.toDouble / limitToDistributeGenesis).toInt
        slot = (currentOrdinal - firstDistributionOrdinal).toInt
        startIndex = slot * rowsPerOrdinal
        endIndex = math.min(startIndex + rowsPerOrdinal, totalRows)

        rewards = addressInfos
          .slice(startIndex, endIndex)
          .map(info => RewardTransaction(info.address, TransactionAmount(info.amount)))
          .toSortedSet
      } yield rewards
  }

  def mergeRewardsByAddress(
    rewardSets: SortedSet[RewardTransaction]*
  ): Either[String, SortedSet[RewardTransaction]] = {
    val combined = rewardSets.foldLeft(SortedSet.empty[RewardTransaction])(_ ++ _)
    val groupedByAddress = combined.groupBy(_.destination)

    groupedByAddress.toList.traverse {
      case (address, transactions) =>
        val totalAmount = transactions.toList.map(_.amount.value.value).sum
        PosLong
          .from(totalAmount)
          .bimap(
            err => s"Invalid total amount for address $address: $err",
            pos => RewardTransaction(address, TransactionAmount(pos))
          )
    }.map(_.toSortedSet)
  }

  def make[F[_]: Async]: Rewards[F, CurrencySnapshotStateProof, CurrencyIncrementalSnapshot, CurrencySnapshotEvent] = {
    (
      lastArtifact: Signed[CurrencyIncrementalSnapshot],
      _: SortedMap[Address, Balance],
      _: SortedSet[Signed[Transaction]],
      _: ConsensusTrigger,
      _: Set[CurrencySnapshotEvent],
      maybeCalculatedState: Option[DataCalculatedState]
    ) =>
      val currentOrdinal = lastArtifact.ordinal.value.value + 1
      val genesisRewardsF = buildGenesisRewards(currentOrdinal)

      val regularRewardsF = maybeCalculatedState.collect {
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

      for {
        genesisRewards <- genesisRewardsF
        regularRewards <- regularRewardsF
        mergedRewards <- mergeRewardsByAddress(genesisRewards, regularRewards) match {
          case Right(rewards) => rewards.pure[F]
          case Left(err) =>
            Async[F].raiseError[SortedSet[RewardTransaction]](
              new Exception(s"Failed to merge rewards: $err")
            )
        }
      } yield mergedRewards
  }
}
