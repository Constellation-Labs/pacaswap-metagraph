package org.amm_metagraph.shared_data.validations

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.security.SecurityProvider
import io.constellationnetwork.security.signature.Signed

import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.credits.getUpdatedCredits
import org.amm_metagraph.shared_data.types.DataUpdates.RewardAllocationVoteUpdate
import org.amm_metagraph.shared_data.types.Governance.{UserAllocations, VotingWeight, maxCredits}
import org.amm_metagraph.shared_data.types.LiquidityPool.getLiquidityPoolCalculatedState
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.validations.Errors._
import org.amm_metagraph.shared_data.validations.SharedValidations._

trait GovernanceValidations[F[_]] {
  def l1Validations(
    rewardAllocationVoteUpdate: RewardAllocationVoteUpdate
  ): F[DataApplicationValidationErrorOr[Unit]]

  def l0Validations(
    applicationConfig: ApplicationConfig,
    rewardAllocationVoteUpdate: Signed[RewardAllocationVoteUpdate],
    state: AmmCalculatedState,
    lastSyncGlobalSnapshotEpochProgress: EpochProgress
  )(implicit sp: SecurityProvider[F]): F[DataApplicationValidationErrorOr[Unit]]
}

object GovernanceValidations {
  def make[F[_]: Async]: GovernanceValidations[F] = new GovernanceValidations[F] {
    def l1Validations(
      rewardAllocationVoteUpdate: RewardAllocationVoteUpdate
    ): F[DataApplicationValidationErrorOr[Unit]] = Async[F].delay {
      exceedingAllocationPercentage(rewardAllocationVoteUpdate)
    }

    def l0Validations(
      applicationConfig: ApplicationConfig,
      rewardAllocationVoteUpdate: Signed[RewardAllocationVoteUpdate],
      state: AmmCalculatedState,
      lastSyncGlobalSnapshotEpochProgress: EpochProgress
    )(implicit sp: SecurityProvider[F]): F[DataApplicationValidationErrorOr[Unit]] = {
      val lastAllocations = state.allocations
      val lastVotingWeights = state.votingWeights
      val liquidityPools = getLiquidityPoolCalculatedState(state)

      for {
        l1Validations <- l1Validations(rewardAllocationVoteUpdate.value)
        signatures <- signatureValidations(rewardAllocationVoteUpdate, rewardAllocationVoteUpdate.source)
        sourceAddress = rewardAllocationVoteUpdate.source
        lastUserAllocation = lastAllocations.usersAllocations.get(sourceAddress)
        lastTransactionRef = lastTransactionRefValidation(rewardAllocationVoteUpdate, lastUserAllocation)
        dailyLimitAllocation = dailyLimitAllocationValidation(
          applicationConfig,
          lastUserAllocation,
          lastSyncGlobalSnapshotEpochProgress
        )
        walletHasVotingWeight = walletHasVotingWeightValidation(
          lastVotingWeights,
          sourceAddress
        )
        isValidId = allocationIdValidation(
          applicationConfig,
          rewardAllocationVoteUpdate,
          liquidityPools
        )
      } yield
        l1Validations
          .productR(signatures)
          .productR(lastTransactionRef)
          .productR(dailyLimitAllocation)
          .productR(walletHasVotingWeight)
          .productR(isValidId)
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
    applicationConfig: ApplicationConfig,
    lastUserAllocation: Option[UserAllocations],
    lastCurrencySnapshotEpochProgress: EpochProgress
  ): DataApplicationValidationErrorOr[Unit] =
    lastUserAllocation.fold(valid) { allocation =>
      getUpdatedCredits(
        allocation.allocationGlobalEpochProgress.value.value,
        allocation.credits,
        lastCurrencySnapshotEpochProgress.value.value,
        maxCredits,
        applicationConfig.epochInfo.epochProgressOneDay
      ).fold(_ => DailyAllocationExceed.invalid, _ => valid)
    }

    private def walletHasVotingWeightValidation(
      lastVotingWeights: Map[Address, VotingWeight],
      address: Address
    ): DataApplicationValidationErrorOr[Unit] =
      MissingVotingWeight.unlessA(lastVotingWeights.get(address).exists(_.total.value > 0.0d))

    private def allocationIdValidation(
      applicationConfig: ApplicationConfig,
      rewardAllocationVoteUpdate: Signed[RewardAllocationVoteUpdate],
      liquidityPools: LiquidityPoolCalculatedState
    ): DataApplicationValidationErrorOr[Unit] = {
      val allocationIds = rewardAllocationVoteUpdate.allocations.map { case (id, _) => id }
      val liquidityPoolIds = liquidityPools.confirmed.value.keySet

      InvalidAllocationId.whenA(
        allocationIds.exists(id => !liquidityPoolIds.contains(id) && id != applicationConfig.nodeValidatorsGovernanceAllocationId)
      )
    }
  }

}
