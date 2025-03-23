package org.amm_metagraph.shared_data.services.combiners

import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}
import scala.tools.nsc.tasty.SafeEq

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.currency.schema.currency.CurrencySnapshotInfo
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.{AllowSpend, CurrencyId}
import io.constellationnetwork.schema.tokenLock.TokenLock
import io.constellationnetwork.security.SecurityProvider
import io.constellationnetwork.security.signature.Signed

import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.NonNegInt
import monocle.syntax.all._
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.credits.getUpdatedCredits
import org.amm_metagraph.shared_data.epochProgress.{epochProgress1Year, epochProgress2Years}
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.types.DataUpdates.RewardAllocationVoteUpdate
import org.amm_metagraph.shared_data.types.Governance.MonthlyReference.getMonthlyReference
import org.amm_metagraph.shared_data.types.Governance._
import org.amm_metagraph.shared_data.types.LiquidityPool.getLiquidityPoolCalculatedState
import org.amm_metagraph.shared_data.types.States.{AmmCalculatedState, AmmOnChainState}
import org.amm_metagraph.shared_data.types.codecs.HasherSelector
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

trait GovernanceCombinerService[F[_]] {
  def combineNew(
    signedUpdate: Signed[RewardAllocationVoteUpdate],
    oldState: DataState[AmmOnChainState, AmmCalculatedState],
    globalEpochProgress: EpochProgress,
    lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
    currencyId: CurrencyId
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]]

  def updateVotingWeights(
    currentCalculatedState: AmmCalculatedState,
    lastCurrencySnapshotInfo: CurrencySnapshotInfo,
    lastSyncGlobalSnapshotEpochProgress: EpochProgress
  ): Map[Address, VotingWeight]

  def handleMonthlyGovernanceRewards(
    state: DataState[AmmOnChainState, AmmCalculatedState],
    globalEpochProgress: EpochProgress,
    currentMetagraphEpochProgress: EpochProgress
  ): DataState[AmmOnChainState, AmmCalculatedState]
}

object GovernanceCombinerService {
  def make[F[_]: Async: HasherSelector: SecurityProvider](
    applicationConfig: ApplicationConfig
  ): GovernanceCombinerService[F] =
    new GovernanceCombinerService[F] {
      def logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("GovernanceCombinerService")

      private def calculateWeight(
        tokenLock: TokenLock,
        lastSyncGlobalSnapshotEpochProgress: EpochProgress
      ): Double = {
        val syncEpochProgressValue = lastSyncGlobalSnapshotEpochProgress.value.value
        val votingWeightMultipliers = applicationConfig.governance.votingWeightMultipliers
        tokenLock.unlockEpoch match {
          case Some(unlockEpoch) if (syncEpochProgressValue + epochProgress2Years) <= unlockEpoch.value =>
            tokenLock.amount.value * votingWeightMultipliers.lockForTwoOrMoreYearsMultiplier
          case Some(unlockEpoch) if (syncEpochProgressValue + epochProgress1Year) <= unlockEpoch.value =>
            tokenLock.amount.value * votingWeightMultipliers.lockForOneYearMultiplier
          case _ =>
            tokenLock.amount.value * votingWeightMultipliers.lockForSixMonthsMultiplier
        }
      }

      private def calculateAllocationRewards(
        acc: DataState[AmmOnChainState, AmmCalculatedState],
        monthlyReference: MonthlyReference,
        currentMetagraphEpochProgress: EpochProgress
      ): AllocationsRewards = {
        val votingWeights = acc.calculated.votingWeights

        acc.calculated.allocations.usersAllocations.foldLeft(
          AllocationsRewards(
            monthlyReference.monthReference,
            currentMetagraphEpochProgress,
            Map.empty
          )
        ) { (allocationsRewards, userAllocation) =>
          userAllocation match {
            case (address, allocationDetails) =>
              votingWeights.get(address).fold(allocationsRewards) { votingWeight =>
                val updatedRewardsInfo = allocationDetails.allocations.foldLeft(allocationsRewards.rewardsInfo) {
                  case (rewardsInfo, allocation) =>
                    val percentage = allocation.percentage.value
                    val allocationAmount = votingWeight.total.value * percentage
                    rewardsInfo.updated(allocation.id, rewardsInfo.getOrElse(allocation.id, 0d) + allocationAmount)
                }

                allocationsRewards.focus(_.rewardsInfo).replace(updatedRewardsInfo)
              }
          }
        }
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
          VotingWeightInfo(weight.toLong.toNonNegLongUnsafe, lock, lastSyncGlobalSnapshotEpochProgress)
        }

        filteredInfo ++ newInfo
      }

      def combineNew(
        signedUpdate: Signed[RewardAllocationVoteUpdate],
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        globalEpochProgress: EpochProgress,
        lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
        currencyId: CurrencyId
      )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {

        val liquidityPools = getLiquidityPoolCalculatedState(oldState.calculated).confirmed.value

        HasherSelector[F].withCurrent { implicit hs =>
          RewardAllocationVoteReference.of(signedUpdate)
        }.flatMap { reference =>
          val allocationsSum = signedUpdate.allocations.map { case (_, weight) => weight.value }.sum

          val allocationsUpdate = signedUpdate.allocations.map {
            case (key, weight) =>
              val votingWeight = weight.value / allocationsSum.toDouble
              val category = if (liquidityPools.contains(key)) {
                AllocationCategory.LiquidityPool
              } else {
                AllocationCategory.NodeOperator
              }

              Allocation(key, category, votingWeight.toNonNegDoubleUnsafe)
          }.toList

          oldState.calculated.allocations.usersAllocations
            .get(signedUpdate.source)
            .fold {
              UserAllocations(
                maxCredits,
                reference,
                globalEpochProgress,
                allocationsUpdate
              ).pure
            } { existing =>
              getUpdatedCredits(
                existing.allocationGlobalEpochProgress.value.value,
                existing.credits,
                globalEpochProgress.value.value,
                maxCredits
              ) match {
                case Left(msg) => logger.warn(s"Error when combining reward allocation: $msg").as(existing)
                case Right(updatedCredits) =>
                  UserAllocations(
                    updatedCredits,
                    reference,
                    globalEpochProgress,
                    allocationsUpdate
                  ).pure
              }
            }
            .map { allocationsCalculatedState =>
              val updatedUsersAllocation = oldState.calculated.allocations
                .focus(_.usersAllocations)
                .modify(_.updated(signedUpdate.source, allocationsCalculatedState))

              val updatedAllocations = oldState.calculated
                .focus(_.allocations)
                .replace(updatedUsersAllocation)

              oldState.focus(_.calculated).replace(updatedAllocations)
            }
        }
      }

      override def updateVotingWeights(
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
              updateVotingWeightInfo(
                currentAddressVotingWeight.info,
                addedTokenLocks,
                removedTokenLocks,
                lastSyncGlobalSnapshotEpochProgress
              )
            val updatedWeight = updatedInfo.map(_.weight.value).sum

            acc.updated(address, VotingWeight(updatedWeight.toNonNegLongUnsafe, updatedInfo))
          }
        }
          .getOrElse(currentVotingWeights)
      }

      def handleMonthlyGovernanceRewards(
        state: DataState[AmmOnChainState, AmmCalculatedState],
        globalEpochProgress: EpochProgress,
        currentMetagraphEpochProgress: EpochProgress
      ): DataState[AmmOnChainState, AmmCalculatedState] = {
        val monthlyReference = state.calculated.allocations.monthlyReference

        val (monthlyReferenceParsed, stateParsed) = if (monthlyReference.monthReference === NonNegInt.MinValue) {
          val monthlyRef = getMonthlyReference(applicationConfig.environment, globalEpochProgress)
          (monthlyRef, state.focus(_.calculated.allocations.monthlyReference).replace(monthlyRef))
        } else {
          (monthlyReference, state)
        }

        val isExpired = globalEpochProgress > monthlyReferenceParsed.expireGlobalEpochProgress
        if (!isExpired) {
          stateParsed
        } else {
          val filteredAllocations = stateParsed.calculated.allocations.usersAllocations.map {
            case (address, allocations) => address -> allocations.copy(allocations = List.empty)
          }

          val allocationsRewards = calculateAllocationRewards(
            stateParsed,
            monthlyReferenceParsed,
            currentMetagraphEpochProgress
          )

          val currentAllocationsRewards = (
            stateParsed.calculated.allocations.allocationsRewards :+ allocationsRewards
          )
            .sortBy(-_.epochProgressToReward.value.value)
            .take(3)

          val updatedMonthlyReference = getMonthlyReference(applicationConfig.environment, globalEpochProgress)

          stateParsed
            .focus(_.calculated.allocations.usersAllocations)
            .replace(filteredAllocations)
            .focus(_.calculated.allocations.allocationsRewards)
            .replace(currentAllocationsRewards)
            .focus(_.calculated.allocations.monthlyReference)
            .replace(updatedMonthlyReference)
        }
      }
    }
}
