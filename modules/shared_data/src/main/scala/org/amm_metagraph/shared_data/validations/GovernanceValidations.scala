package org.amm_metagraph.shared_data.validations

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.security.SecurityProvider
import io.constellationnetwork.security.signature.Signed

import org.amm_metagraph.shared_data.credits.getUpdatedCredits
import org.amm_metagraph.shared_data.types.DataUpdates.RewardAllocationVoteUpdate
import org.amm_metagraph.shared_data.types.Governance.{UserAllocations, VotingWeight, maxCredits}
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.validations.Errors._
import org.amm_metagraph.shared_data.validations.SharedValidations.isSignedExclusivelyByValidation

object GovernanceValidations {
  def rewardAllocationValidationsL1[F[_]: Async](
    rewardAllocationVoteUpdate: RewardAllocationVoteUpdate
  ): F[DataApplicationValidationErrorOr[Unit]] =
    exceedingAllocationPercentage(rewardAllocationVoteUpdate).pure

  def rewardAllocationValidationsL0[F[_]: Async](
    rewardAllocationVoteUpdate: Signed[RewardAllocationVoteUpdate],
    state: AmmCalculatedState,
    lastSyncGlobalSnapshotEpochProgress: EpochProgress
  )(implicit sp: SecurityProvider[F]): F[DataApplicationValidationErrorOr[Unit]] = {
    val lastAllocations = state.allocations
    val lastVotingWeights = state.votingWeights
    val lastUserAllocation = lastAllocations.get(rewardAllocationVoteUpdate.address)

    for {
      isSignedExclusivelyBy <- isSignedExclusivelyByValidation(rewardAllocationVoteUpdate, rewardAllocationVoteUpdate.address)
      lastTransactionRef = lastTransactionRefValidation(rewardAllocationVoteUpdate, lastUserAllocation)
      dailyLimitAllocation = dailyLimitAllocationValidation(
        lastUserAllocation,
        lastSyncGlobalSnapshotEpochProgress
      )
      walletHasVotingWeight = walletHasVotingWeightValidation(
        lastVotingWeights,
        rewardAllocationVoteUpdate.address
      )
    } yield
      isSignedExclusivelyBy
        .productR(lastTransactionRef)
        .productR(dailyLimitAllocation)
        .productR(walletHasVotingWeight)
  }

  private def exceedingAllocationPercentage(
    rewardAllocationVoteUpdate: RewardAllocationVoteUpdate
  ): DataApplicationValidationErrorOr[Unit] = {
    val allocationsSum = rewardAllocationVoteUpdate.allocations.map {
      case (_, allocationWeight) => allocationWeight.value
    }.sum

    val allocationsWeightsNormalized = rewardAllocationVoteUpdate.allocations.map {
      case (_, allocationWeight) => (allocationWeight.value / allocationsSum).toDouble
    }

    AllocationPercentageExceed.whenA(allocationsWeightsNormalized.sum > 1.0)
  }

  private def lastTransactionRefValidation(
    rewardAllocationVoteUpdate: Signed[RewardAllocationVoteUpdate],
    lastUserAllocation: Option[UserAllocations]
  ): DataApplicationValidationErrorOr[Unit] = lastUserAllocation match {
    case None => valid
    case Some(value) =>
      val reference = value.reference

      if (rewardAllocationVoteUpdate.parent.ordinal < reference.ordinal)
        ParentOrdinalLowerThenLastProcessedTxOrdinal.invalid
      else {
        HasNoMatchingParent.unlessA(
          rewardAllocationVoteUpdate.parent.ordinal === reference.ordinal && rewardAllocationVoteUpdate.parent.hash === reference.hash
        )
      }
  }

  private def dailyLimitAllocationValidation(
    lastUserAllocation: Option[UserAllocations],
    lastCurrencySnapshotEpochProgress: EpochProgress
  ): DataApplicationValidationErrorOr[Unit] =
    lastUserAllocation.fold(valid) { allocation =>
      getUpdatedCredits(
        allocation.allocationEpochProgressInfo.allocationGlobalEpochProgress.value.value,
        allocation.credits,
        lastCurrencySnapshotEpochProgress.value.value,
        maxCredits
      ).fold(_ => DailyAllocationExceed.invalid, _ => valid)
    }

  private def walletHasVotingWeightValidation(
    lastVotingWeights: Map[Address, VotingWeight],
    address: Address
  ): DataApplicationValidationErrorOr[Unit] =
    MissingVotingWeight.unlessA(lastVotingWeights.get(address).exists(_.total.value > 0.0d))
}
