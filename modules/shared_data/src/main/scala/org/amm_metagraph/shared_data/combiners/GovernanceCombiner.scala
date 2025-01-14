package org.amm_metagraph.shared_data.combiners

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.DataState
import io.constellationnetwork.currency.schema.currency.CurrencySnapshotInfo
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.tokenLock.TokenLock
import io.constellationnetwork.security.Hasher
import io.constellationnetwork.security.signature.Signed

import monocle.syntax.all._
import org.amm_metagraph.shared_data.credits.getUpdatedCredits
import org.amm_metagraph.shared_data.epochProgress.{epochProgress1Year, epochProgress2Years}
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.types.DataUpdates.RewardAllocationVoteUpdate
import org.amm_metagraph.shared_data.types.Governance.AllocationEpochProgressInfo.getAllocationEpochProgressInfo
import org.amm_metagraph.shared_data.types.Governance._
import org.amm_metagraph.shared_data.types.LiquidityPool.getLiquidityPoolCalculatedState
import org.amm_metagraph.shared_data.types.States.{AmmCalculatedState, AmmOnChainState}
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object GovernanceCombiner {
  private val sixMonthsMultiplier: Double = 0.25
  private val oneYearMultiplier: Double = 0.5
  private val twoYearsMultiplier: Double = 1

  def updateVotingWeights(
    currentCalculatedState: AmmCalculatedState,
    lastCurrencySnapshotInfo: CurrencySnapshotInfo,
    lastSyncGlobalSnapshotEpochProgress: EpochProgress
  ): Map[Address, VotingWeight] = {
    val currentVotingWeights = currentCalculatedState.votingWeights

    lastCurrencySnapshotInfo.activeTokenLocks.map { activeTokenLocks =>
      activeTokenLocks.foldLeft(currentVotingWeights) { (acc, tokenLocksInfo) =>
        val (address, tokenLocks) = tokenLocksInfo
        val currentAddressVotingWeight = acc.getOrElse(address, VotingWeight.empty)

        val currentTokenLocks = currentAddressVotingWeight.info.map(_.tokenLock).toSet
        val fetchedTokenLocks = tokenLocks.map(_.value)

        val removedTokenLocks = currentTokenLocks.diff(fetchedTokenLocks)
        val addedTokenLocks = fetchedTokenLocks.diff(currentTokenLocks)

        val updatedInfo =
          updateVotingWeightInfo(currentAddressVotingWeight.info, addedTokenLocks, removedTokenLocks, lastSyncGlobalSnapshotEpochProgress)
        val updatedWeight = updatedInfo.map(_.weight.value).sum

        acc.updated(address, VotingWeight(updatedWeight.toNonNegDoubleUnsafe, updatedInfo))
      }
    }
      .getOrElse(currentVotingWeights)
  }

  private def updateVotingWeightInfo(
    currentVotingWeightInfo: List[VotingWeightInfo],
    addedTokenLocks: Set[TokenLock],
    removedTokenLocks: Set[TokenLock],
    lastSyncGlobalSnapshotEpochProgress: EpochProgress
  ): List[VotingWeightInfo] = {
    val filteredInfo = currentVotingWeightInfo.filterNot(info => removedTokenLocks.contains(info.tokenLock))

    val newInfo = addedTokenLocks.toList.map { lock =>
      val weight = calculateWeight(lock, lastSyncGlobalSnapshotEpochProgress)
      VotingWeightInfo(weight.toNonNegDoubleUnsafe, lock)
    }

    filteredInfo ++ newInfo
  }

  private def calculateWeight(
    tokenLock: TokenLock,
    lastSyncGlobalSnapshotEpochProgress: EpochProgress
  ): Double = {
    val unlockEpochValue = tokenLock.unlockEpoch.value.value
    val syncEpochProgressValue = lastSyncGlobalSnapshotEpochProgress.value.value

    if (syncEpochProgressValue + epochProgress2Years <= unlockEpochValue) {
      tokenLock.amount.value.value * twoYearsMultiplier
    } else if (syncEpochProgressValue + epochProgress1Year <= unlockEpochValue) {
      tokenLock.amount.value.value * oneYearMultiplier
    } else {
      tokenLock.amount.value.value * sixMonthsMultiplier
    }
  }

  def combineRewardAllocationVoteUpdate[F[_]: Async: Hasher](
    acc: DataState[AmmOnChainState, AmmCalculatedState],
    signedRewardAllocationVoteUpdate: Signed[RewardAllocationVoteUpdate],
    globalEpochProgress: EpochProgress
  ): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
    def logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("GovernanceCombiner")

    val liquidityPools = getLiquidityPoolCalculatedState(acc.calculated).liquidityPools

    RewardAllocationVoteReference.of(signedRewardAllocationVoteUpdate).flatMap { reference =>
      val allocationsSum = signedRewardAllocationVoteUpdate.allocations.map { case (_, weight) => weight.value }.sum
      val userVotingWeight = acc.calculated.votingWeights.getOrElse(signedRewardAllocationVoteUpdate.address, VotingWeight.empty)

      val allocationsUpdate = signedRewardAllocationVoteUpdate.allocations.map {
        case (key, weight) =>
          val votingWeight = userVotingWeight.total.value * (weight.value / allocationsSum.toDouble)
          val category = if (liquidityPools.contains(key)) {
            AllocationCategory.LiquidityPool
          } else {
            AllocationCategory.NodeOperator
          }

          Allocation(AllocationId(key), category, votingWeight.toNonNegDoubleUnsafe)
      }.toList
      val allocationEpochProgressInfo = getAllocationEpochProgressInfo(globalEpochProgress)

      acc.calculated.allocations
        .get(signedRewardAllocationVoteUpdate.address)
        .fold {
          UserAllocations(
            maxCredits,
            reference,
            allocationEpochProgressInfo,
            allocationsUpdate
          ).pure
        } { existing =>
          getUpdatedCredits(
            existing.allocationEpochProgressInfo.allocationGlobalEpochProgress.value.value,
            existing.credits,
            globalEpochProgress.value.value,
            maxCredits
          ).fold(
            msg => logger.warn(s"Error when combining reward allocation: $msg").as(existing),
            updatedCredits =>
              UserAllocations(
                updatedCredits,
                reference,
                allocationEpochProgressInfo,
                allocationsUpdate
              ).pure
          )
        }
        .map { allocationsCalculatedState =>
          val updatedAllocations = acc.calculated
            .focus(_.allocations)
            .modify(_.updated(signedRewardAllocationVoteUpdate.address, allocationsCalculatedState))

          acc.focus(_.calculated).replace(updatedAllocations)
        }
    }
  }

  def cleanExpiredAllocations(
    acc: DataState[AmmOnChainState, AmmCalculatedState],
    globalEpochProgress: EpochProgress
  ): DataState[AmmOnChainState, AmmCalculatedState] = {
    val filteredAllocations = acc.calculated.allocations.map {
      case (address, allocations) =>
        val isExpired = globalEpochProgress > allocations.allocationEpochProgressInfo.expireGlobalEpochProgress

        val matchesVotingWeight = acc.calculated.votingWeights.get(address).exists { votingWeight =>
          votingWeight.total == allocations.allocations.map(_.weight.value).sum.toNonNegDoubleUnsafe
        }

        if (!isExpired && matchesVotingWeight) {
          address -> allocations
        } else {
          address -> allocations.copy(allocations = List.empty)
        }
    }

    acc.focus(_.calculated.allocations).replace(filteredAllocations)
  }
}
