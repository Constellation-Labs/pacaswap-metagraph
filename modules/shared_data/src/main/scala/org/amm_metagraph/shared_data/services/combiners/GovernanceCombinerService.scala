package org.amm_metagraph.shared_data.services.combiners

import cats.data.EitherT
import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}

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
import eu.timepit.refined.types.numeric.NonNegLong
import monocle.syntax.all._
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.credits.getUpdatedCredits
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.types.DataUpdates.RewardAllocationVoteUpdate
import org.amm_metagraph.shared_data.types.Governance.MonthlyReference.getMonthlyReference
import org.amm_metagraph.shared_data.types.Governance._
import org.amm_metagraph.shared_data.types.LiquidityPool.getLiquidityPoolCalculatedState
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

  def updateVotingWeights(
    currentCalculatedState: AmmCalculatedState,
    lastCurrencySnapshotInfo: CurrencySnapshotInfo,
    lastSyncGlobalSnapshotEpochProgress: EpochProgress
  ): SortedMap[Address, VotingWeight]

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

      private def handleFailedUpdate(
        acc: DataState[AmmOnChainState, AmmCalculatedState],
        failedCalculatedState: FailedCalculatedState
      ): F[DataState[AmmOnChainState, AmmCalculatedState]] =
        logger.warn(s"Received incorrect Governance update $failedCalculatedState") >> acc.pure[F]

      private def calculateWeight(
        tokenLock: TokenLock,
        lastSyncGlobalSnapshotEpochProgress: EpochProgress
      ): Double = {
        val syncEpochProgressValue = lastSyncGlobalSnapshotEpochProgress.value.value
        val votingWeightMultipliers = applicationConfig.governance.votingWeightMultipliers
        tokenLock.unlockEpoch match {
          case Some(unlockEpoch) if (syncEpochProgressValue + applicationConfig.epochInfo.epochProgress2Years) <= unlockEpoch.value =>
            tokenLock.amount.value * votingWeightMultipliers.lockForTwoOrMoreYearsMultiplier
          case Some(unlockEpoch) if (syncEpochProgressValue + applicationConfig.epochInfo.epochProgress2Years) <= unlockEpoch.value =>
            tokenLock.amount.value * votingWeightMultipliers.lockForOneAndHalfYearMultiplier
          case Some(unlockEpoch) if (syncEpochProgressValue + applicationConfig.epochInfo.epochProgress1Year) <= unlockEpoch.value =>
            tokenLock.amount.value * votingWeightMultipliers.lockForOneYearMultiplier
          case _ =>
            tokenLock.amount.value * votingWeightMultipliers.lockForSixMonthsMultiplier
        }
      }

      private def freezeUserVotes(
        acc: DataState[AmmOnChainState, AmmCalculatedState],
        monthlyReference: MonthlyReference
      ): FrozenAddressesVotes = {
        val votingWeights = acc.calculated.votingWeights

        val allocationsAndVotes: Map[Address, (SortedSet[Allocation], VotingWeight)] = acc.calculated.allocations.usersAllocations
          .withFilter(_._2.allocationEpochProgress >= monthlyReference.firstEpochOfMonth)
          .withFilter(_._2.allocationEpochProgress <= monthlyReference.lastEpochOfMonth)
          .flatMap {
            case (address, allocations) =>
              votingWeights.get(address).map(voteWeights => address -> (allocations.allocations, voteWeights))
          }

        val totalVotes =
          allocationsAndVotes.values.map { case (_, votingWeight) => BigDecimal(votingWeight.total.value) }.sum

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

        val frozenVotes: SortedMap[Address, VotingWeight] =
          SortedMap.from(allocationsAndVotes.map { case (address, (_, votingWeight)) => address -> votingWeight })

        FrozenAddressesVotes(monthlyReference, frozenVotes, resAllocations)
      }

      private def updateVotingWeightInfo(
        currentVotingWeightInfo: SortedSet[VotingWeightInfo],
        addedTokenLocks: SortedSet[TokenLock],
        removedTokenLocks: SortedSet[TokenLock],
        lastSyncGlobalSnapshotEpochProgress: EpochProgress
      ): SortedSet[VotingWeightInfo] = {
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
        currentMetagraphEpochProgress: EpochProgress,
        lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
        currencyId: CurrencyId
      )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {

        val liquidityPools = getLiquidityPoolCalculatedState(oldState.calculated).confirmed.value

        val combinedState: EitherT[F, FailedCalculatedState, DataState[AmmOnChainState, AmmCalculatedState]] =
          for {
            approvedValidators <- EitherT.liftF(context.getMetagraphL0Seedlist.getOrElse(Set.empty).toList.traverse(_.peerId.toAddress))
            _ <- EitherT(
              governanceValidations.l0Validations(
                signedUpdate,
                oldState.calculated,
                globalEpochProgress,
                approvedValidators
              )
            )
            result <- EitherT.liftF {
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

                    Allocation(AllocationId(key, category), Percentage.unsafeFrom(votingWeight))
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

      override def updateVotingWeights(
        currentCalculatedState: AmmCalculatedState,
        lastCurrencySnapshotInfo: CurrencySnapshotInfo,
        lastSyncGlobalSnapshotEpochProgress: EpochProgress
      ): SortedMap[Address, VotingWeight] = {
        val currentVotingWeights = currentCalculatedState.votingWeights

        lastCurrencySnapshotInfo.activeTokenLocks.map { activeTokenLocks =>
          activeTokenLocks.foldLeft(currentVotingWeights) { (acc, tokenLocksInfo) =>
            val (address, tokenLocks) = tokenLocksInfo
            val currentAddressVotingWeight = acc.getOrElse(address, VotingWeight.empty)

            val currentTokenLocks = currentAddressVotingWeight.info.map(_.tokenLock)
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
            if (updatedInfo.nonEmpty) {
              val updatedWeight = updatedInfo.map(_.weight.value).sum
              acc.updated(address, VotingWeight(updatedWeight.toNonNegLongUnsafe, updatedInfo))
            } else {
              acc.removed(address)
            }
          }
        }
          .getOrElse(currentVotingWeights)
      }

      override def handleMonthExpiration(
        state: DataState[AmmOnChainState, AmmCalculatedState],
        currentMetagraphEpochProgress: EpochProgress
      ): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
        val monthlyReference = state.calculated.allocations.monthlyReference

        val (monthlyReferenceParsed, stateParsed) = if (isFirstProcessedMonth(monthlyReference)) {
          val monthlyRef =
            getMonthlyReference(currentMetagraphEpochProgress, applicationConfig.epochInfo.epochProgress1Month)
          (monthlyRef, state.focus(_.calculated.allocations.monthlyReference).replace(monthlyRef))
        } else {
          (monthlyReference, state)
        }

        val isExpired = currentMetagraphEpochProgress > monthlyReferenceParsed.lastEpochOfMonth
        if (!isExpired) {
          val epochsToEndOfMonth = monthlyReferenceParsed.lastEpochOfMonth.value.value - currentMetagraphEpochProgress.value.value
          logger.debug(
            show"Month is not expired yet, need to wait additional $epochsToEndOfMonth epoch(s). Epoch length ${applicationConfig.epochInfo.oneEpochProgress}, Month length in epochs: ${applicationConfig.epochInfo.epochProgress1Month}"
          ) >>
            stateParsed.pure[F]
        } else {
          val frozenVotes = freezeUserVotes(stateParsed, monthlyReferenceParsed)

          val clearedAllocations = stateParsed.calculated.allocations.usersAllocations.map {
            case (address, allocations) => address -> allocations.copy(allocations = SortedSet.empty)
          }

          val updatedMonthlyReference =
            getMonthlyReference(currentMetagraphEpochProgress, applicationConfig.epochInfo.epochProgress1Month)

          logger.info(
            show"Month is expired, freeze user allocations: $frozenVotes, new user allocations are ${clearedAllocations.toList}, new month reference is $updatedMonthlyReference"
          ) >>
            stateParsed
              .focus(_.calculated.allocations.usersAllocations)
              .replace(clearedAllocations)
              .focus(_.calculated.allocations.frozenUsedUserVotes)
              .replace(frozenVotes)
              .focus(_.calculated.allocations.monthlyReference)
              .replace(updatedMonthlyReference)
              .pure[F]
        }
      }
    }

  private def isFirstProcessedMonth(monthlyReference: MonthlyReference): Boolean =
    monthlyReference.lastEpochOfMonth == EpochProgress(NonNegLong.MinValue)
}
