package org.amm_metagraph.shared_data.services.combiners.operations

import cats.data.EitherT
import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet, TreeMap}

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.currency.schema.currency.CurrencySnapshotInfo
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.{AllowSpend, CurrencyId}
import io.constellationnetwork.schema.tokenLock.TokenLock
import io.constellationnetwork.security.SecurityProvider
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.syntax.sortedCollection.sortedSetSyntax

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.NonNegInt
import eu.timepit.refined.types.numeric.NonNegLong
import monocle.syntax.all._
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.credits.getUpdatedCredits
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.types.DataUpdates.RewardAllocationVoteUpdate
import org.amm_metagraph.shared_data.types.Governance.MonthlyReference.getMonthlyReference
import org.amm_metagraph.shared_data.types.Governance._
import org.amm_metagraph.shared_data.types.LiquidityPool.getLiquidityPoolCalculatedState
import org.amm_metagraph.shared_data.types.Rewards.DistributedRewards
import org.amm_metagraph.shared_data.types.States.{AmmCalculatedState, AmmOnChainState, FailedCalculatedState}
import org.amm_metagraph.shared_data.types.codecs.HasherSelector
import org.amm_metagraph.shared_data.validations.GovernanceValidations
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

trait GovernanceCombinerService[F[_]] {
  def combineNew(
    signedUpdate: Signed[RewardAllocationVoteUpdate],
    oldState: DataState[AmmOnChainState, AmmCalculatedState],
    globalEpochProgress: EpochProgress,
    currentMetagraphEpochProgress: EpochProgress,
    lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
    currencyId: CurrencyId
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]]

  def updateVotingPowers(
    currentCalculatedState: AmmCalculatedState,
    lastCurrencySnapshotInfo: CurrencySnapshotInfo,
    lastSyncGlobalSnapshotEpochProgress: EpochProgress
  ): SortedMap[Address, VotingPower]

  def handleMonthExpiration(
    state: DataState[AmmOnChainState, AmmCalculatedState],
    currentMetagraphEpochProgress: EpochProgress
  ): F[DataState[AmmOnChainState, AmmCalculatedState]]
}

object GovernanceCombinerService {
  def make[F[_]: Async: HasherSelector: SecurityProvider](
    applicationConfig: ApplicationConfig,
    governanceValidations: GovernanceValidations[F]
  ): GovernanceCombinerService[F] =
    new GovernanceCombinerService[F] {
      val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)
      val tokensMultipliers: TokenLocksMultiplier =
        TokenLocksMultiplier.make(applicationConfig.governance.votingPowerMultipliers)

      // We store information about total amount of distributed rewards per type for epochs
      // that parameter define for how many previous epochs we would like to save that information
      private val distributedRewardsCacheSize = NonNegInt(5)

      private def handleFailedUpdate(
        acc: DataState[AmmOnChainState, AmmCalculatedState],
        failedCalculatedState: FailedCalculatedState
      ): F[DataState[AmmOnChainState, AmmCalculatedState]] =
        logger.warn(s"Received incorrect Governance update $failedCalculatedState") >> acc.pure[F]

      private def calculatePower(
        tokenLock: TokenLock,
        lastSyncGlobalSnapshotEpochProgress: EpochProgress
      ): Double = {
        val syncEpochProgressValue = lastSyncGlobalSnapshotEpochProgress.value.value
        val lockPeriodInMonths =
          tokenLock.unlockEpoch.map { unlockEpoch =>
            val diff = unlockEpoch.value.value - syncEpochProgressValue
            diff / applicationConfig.epochInfo.epochProgress1Month
          }.getOrElse(Long.MaxValue)

        val multiplier = tokensMultipliers.getMultiplier(lockPeriodInMonths)
        tokenLock.amount.value * multiplier
      }

      private def buildGovernanceVotingResult(
        acc: DataState[AmmOnChainState, AmmCalculatedState],
        monthlyReference: MonthlyReference
      ): GovernanceVotingResult = {
        val votingPowers = acc.calculated.votingPowers

        val allocationsAndVotes: Map[Address, (SortedSet[Allocation], VotingPower)] = acc.calculated.allocations.usersAllocations
          .withFilter(_._2.allocationEpochProgress >= monthlyReference.firstEpochOfMonth)
          .withFilter(_._2.allocationEpochProgress <= monthlyReference.lastEpochOfMonth)
          .flatMap {
            case (address, allocations) =>
              votingPowers.get(address).map(voteWeights => address -> (allocations.allocations, voteWeights))
          }

        val totalVotes =
          allocationsAndVotes.values.map { case (_, votingPower) => BigDecimal(votingPower.total.value) }.sum

        val allocationsToGlobalPercent: Seq[(AllocationId, Percentage)] = allocationsAndVotes.toSeq.flatMap {
          case (_, (allocations, weight)) =>
            // We could use unsafe here because totalVotes always bigger than weights
            val addressVotingPercentage = Percentage.unsafeFrom(weight.total.value / totalVotes)
            allocations.toSeq.map {
              case Allocation(AllocationId(id, category), percentage) =>
                val allocationId = AllocationId(id, category)
                val globalPercentage = addressVotingPercentage * percentage.value
                allocationId -> Percentage.unsafeFrom(globalPercentage)
            }
        }
        val resAllocations: SortedMap[AllocationId, Percentage] =
          SortedMap.from(
            allocationsToGlobalPercent
              .groupMapReduce(_._1)(_._2.value)(_ + _)
              .view
              .mapValues(v => Percentage.unsafeFrom(v))
          )

        val frozenVotes: SortedMap[Address, VotingPower] =
          SortedMap.from(allocationsAndVotes.map { case (address, (_, votingPower)) => address -> votingPower })

        GovernanceVotingResult(monthlyReference, frozenVotes, resAllocations)
      }

      private def updateVotingPowerInfo(
        currentVotingPowerInfo: SortedSet[VotingPowerInfo],
        addedTokenLocks: SortedSet[TokenLock],
        removedTokenLocks: SortedSet[TokenLock],
        lastSyncGlobalSnapshotEpochProgress: EpochProgress
      ): SortedSet[VotingPowerInfo] = {
        val filteredInfo = currentVotingPowerInfo.filterNot(info => removedTokenLocks.contains(info.tokenLock))

        val newInfo = addedTokenLocks.toList.map { lock =>
          val weight = calculatePower(lock, lastSyncGlobalSnapshotEpochProgress)
          VotingPowerInfo(weight.toLong.toNonNegLongUnsafe, lock, lastSyncGlobalSnapshotEpochProgress)
        }

        filteredInfo ++ newInfo
      }

      def combineNew(
        signedUpdate: Signed[RewardAllocationVoteUpdate],
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        globalEpochProgress: EpochProgress,
        currentMetagraphEpochProgress: EpochProgress,
        lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
        currencyId: CurrencyId
      )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {

        val liquidityPools = getLiquidityPoolCalculatedState(oldState.calculated).confirmed.value

        val combinedState: EitherT[F, FailedCalculatedState, DataState[AmmOnChainState, AmmCalculatedState]] =
          for {
            _ <- EitherT(
              governanceValidations.l0Validations(
                signedUpdate,
                oldState.calculated,
                globalEpochProgress
              )
            )
            result <- EitherT.liftF {
              HasherSelector[F].withCurrent { implicit hs =>
                RewardAllocationVoteReference.of(signedUpdate)
              }.flatMap { reference =>
                val allocationsSum = signedUpdate.allocations.map { case (_, weight) => weight.value }.sum

                val allocationsUpdate = signedUpdate.allocations.map {
                  case (key, weight) =>
                    val votingPower = weight.value / allocationsSum.toDouble
                    val category = if (liquidityPools.contains(key)) {
                      AllocationCategory.LiquidityPool
                    } else {
                      AllocationCategory.NodeOperator
                    }

                    Allocation(AllocationId(key, category), Percentage.unsafeFrom(votingPower))
                }.toSortedSet

                oldState.calculated.allocations.usersAllocations
                  .get(signedUpdate.source)
                  .fold {
                    UserAllocations(
                      maxCredits,
                      reference,
                      currentMetagraphEpochProgress,
                      allocationsUpdate
                    ).pure[F]
                  } { existing =>
                    getUpdatedCredits(
                      existing.allocationEpochProgress.value.value,
                      existing.credits,
                      currentMetagraphEpochProgress.value.value,
                      maxCredits,
                      applicationConfig.epochInfo.epochProgressOneDay
                    ) match {
                      case Left(msg) => logger.warn(s"Error when combining reward allocation: $msg").as(existing)
                      case Right(updatedCredits) =>
                        UserAllocations(
                          updatedCredits,
                          reference,
                          currentMetagraphEpochProgress,
                          allocationsUpdate
                        ).pure[F]
                    }
                  }
                  .flatMap { allocationsCalculatedState =>
                    val updatedUsersAllocation = oldState.calculated.allocations
                      .focus(_.usersAllocations)
                      .modify(_.updated(signedUpdate.source, allocationsCalculatedState))

                    val updatedAllocations = oldState.calculated
                      .focus(_.allocations)
                      .replace(updatedUsersAllocation)

                    logger.debug(show"Update user allocation with: ${updatedAllocations.allocations.usersAllocations.toList}") >>
                      oldState.focus(_.calculated).replace(updatedAllocations).pure[F]
                  }
              }
            }
          } yield result

        combinedState.valueOrF { failedCalculatedState: FailedCalculatedState =>
          handleFailedUpdate(oldState, failedCalculatedState)
        }
      }

      override def updateVotingPowers(
        currentCalculatedState: AmmCalculatedState,
        lastCurrencySnapshotInfo: CurrencySnapshotInfo,
        lastSyncGlobalSnapshotEpochProgress: EpochProgress
      ): SortedMap[Address, VotingPower] = {
        val currentVotingPowers = currentCalculatedState.votingPowers

        lastCurrencySnapshotInfo.activeTokenLocks.map { activeTokenLocks =>
          activeTokenLocks.foldLeft(currentVotingPowers) { (acc, tokenLocksInfo) =>
            val (address, tokenLocks) = tokenLocksInfo
            val currentAddressVotingPower = acc.getOrElse(address, VotingPower.empty)

            val currentTokenLocks = currentAddressVotingPower.info.map(_.tokenLock)
            val fetchedTokenLocks = tokenLocks.map(_.value)

            val removedTokenLocks = currentTokenLocks.diff(fetchedTokenLocks)
            val addedTokenLocks = fetchedTokenLocks.diff(currentTokenLocks)

            val updatedInfo =
              updateVotingPowerInfo(
                currentAddressVotingPower.info,
                addedTokenLocks,
                removedTokenLocks,
                lastSyncGlobalSnapshotEpochProgress
              )
            if (updatedInfo.nonEmpty) {
              val updatedWeight = updatedInfo.map(_.votingPower.value).sum
              acc.updated(address, VotingPower(updatedWeight.toNonNegLongUnsafe, updatedInfo))
            } else {
              acc.removed(address)
            }
          }
        }
          .getOrElse(currentVotingPowers)
      }

      override def handleMonthExpiration(
        state: DataState[AmmOnChainState, AmmCalculatedState],
        currentMetagraphEpochProgress: EpochProgress
      ): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
        val monthlyReference = state.calculated.allocations.monthlyReference

        val (monthlyReferenceParsed, stateParsed) = if (isFirstProcessedMonth(monthlyReference)) {
          val monthlyRef = getMonthlyReference(currentMetagraphEpochProgress, applicationConfig.epochInfo.epochProgress1Month)
          (monthlyRef, state.focus(_.calculated.allocations.monthlyReference).replace(monthlyRef))
        } else {
          (monthlyReference, state)
        }

        // clear possible governanceVotingResult which is set if month had been expired in previous epoch
        val stateWithClearedOnChain = stateParsed.focus(_.onChain.governanceVotingResult).replace(None)

        val isExpired = currentMetagraphEpochProgress > monthlyReferenceParsed.lastEpochOfMonth
        if (!isExpired) {
          val epochsToEndOfMonth = monthlyReferenceParsed.lastEpochOfMonth.value.value - currentMetagraphEpochProgress.value.value
          val logString =
            show"Month is not expired yet, need to wait additional $epochsToEndOfMonth epoch(s). " +
              show"Epoch length ${applicationConfig.epochInfo.oneEpochProgress}, " +
              show"Month length in epochs: ${applicationConfig.epochInfo.epochProgress1Month}"

          logger.debug(logString) >> stateWithClearedOnChain.pure[F]
        } else {
          processMonthExpiration(stateWithClearedOnChain, monthlyReferenceParsed, currentMetagraphEpochProgress)
        }
      }

      private def processMonthExpiration(
        state: DataState[AmmOnChainState, AmmCalculatedState],
        monthlyReference: MonthlyReference,
        currentMetagraphEpochProgress: EpochProgress
      ): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
        val governanceVotingResult = buildGovernanceVotingResult(state, monthlyReference)

        val clearedAllocations = state.calculated.allocations.usersAllocations.map {
          case (address, allocations) => address -> allocations.copy(allocations = SortedSet.empty)
        }

        val updatedMonthlyReference =
          getMonthlyReference(currentMetagraphEpochProgress, applicationConfig.epochInfo.epochProgress1Month)

        val updatedDistributedInfo: SortedMap[MonthlyReference, DistributedRewards] =
          state.calculated.rewards.distributedRewards
            .takeRight(distributedRewardsCacheSize.value) + (updatedMonthlyReference -> DistributedRewards.empty)

        logger.info(
          show"Month is expired, freeze user allocations: $governanceVotingResult, new user allocations are ${clearedAllocations.toList}, new month reference is $updatedMonthlyReference"
        ) >>
          state
            .focus(_.calculated.allocations.usersAllocations)
            .replace(clearedAllocations)
            .focus(_.calculated.allocations.frozenUsedUserVotes)
            .replace(governanceVotingResult)
            .focus(_.calculated.allocations.monthlyReference)
            .replace(updatedMonthlyReference)
            .focus(_.calculated.rewards.distributedRewards)
            .replace(updatedDistributedInfo)
            .focus(_.onChain.governanceVotingResult)
            .replace(governanceVotingResult.some)
            .pure[F]
      }
    }

  private def isFirstProcessedMonth(monthlyReference: MonthlyReference): Boolean =
    monthlyReference.lastEpochOfMonth == EpochProgress(NonNegLong.MinValue)
}

class TokenLocksMultiplier(locks: TreeMap[Long, Double]) {
  def getMultiplier(durationInMonths: Long): Double =
    locks
      .get(durationInMonths)
      .orElse(locks.maxBefore(durationInMonths).map(_._2))
      .getOrElse(0.0)
}

object TokenLocksMultiplier {
  def make(locks: ApplicationConfig.VotingPowerMultipliers): TokenLocksMultiplier = {
    val locksData = locks.locksConfig.map(lm => lm.durationInMonths.value -> lm.multiplier.value)
    new TokenLocksMultiplier(TreeMap.from(locksData))
  }
}
