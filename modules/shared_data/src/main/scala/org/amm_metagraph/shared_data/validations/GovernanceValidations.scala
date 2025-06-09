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
import org.amm_metagraph.shared_data.epochProgress.getFailureExpireEpochProgress
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
    rewardAllocationVoteUpdate: Signed[RewardAllocationVoteUpdate],
    state: AmmCalculatedState,
    lastSyncGlobalSnapshotEpochProgress: EpochProgress
  )(implicit sp: SecurityProvider[F]): F[Either[FailedCalculatedState, Signed[RewardAllocationVoteUpdate]]]
}

object GovernanceValidations {
  def make[F[_]: Async](
    applicationConfig: ApplicationConfig
  ): GovernanceValidations[F] = new GovernanceValidations[F] {
    def l1Validations(
      rewardAllocationVoteUpdate: RewardAllocationVoteUpdate
    ): F[DataApplicationValidationErrorOr[Unit]] = Async[F].delay {
      exceedingAllocationPercentage(rewardAllocationVoteUpdate)
    }
    def l0Validations(
      rewardAllocationVoteUpdate: Signed[RewardAllocationVoteUpdate],
      state: AmmCalculatedState,
      lastSyncGlobalSnapshotEpochProgress: EpochProgress
    )(implicit sp: SecurityProvider[F]): F[Either[FailedCalculatedState, Signed[RewardAllocationVoteUpdate]]] = {
      val lastAllocations = state.allocations
      val lastVotingWeights = state.votingWeights
      val liquidityPools = getLiquidityPoolCalculatedState(state)

      for {
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
        expireEpochProgress = getFailureExpireEpochProgress(applicationConfig, lastSyncGlobalSnapshotEpochProgress)

        result =
          if (lastTransactionRef.isInvalid) {
            failWith(InvalidLastReference(), expireEpochProgress, rewardAllocationVoteUpdate)
          } else if (signatures.isInvalid) {
            failWith(InvalidSignatures(signatures.map(_.show).mkString_(",")), expireEpochProgress, rewardAllocationVoteUpdate)
          } else if (dailyLimitAllocation.isInvalid) {
            failWith(GovernanceDailyLimitAllocation(rewardAllocationVoteUpdate.value), expireEpochProgress, rewardAllocationVoteUpdate)
          } else if (walletHasVotingWeight.isInvalid) {
            failWith(GovernanceWalletWithNoVotingWeight(rewardAllocationVoteUpdate.value), expireEpochProgress, rewardAllocationVoteUpdate)
          } else if (isValidId.isInvalid) {
            failWith(GovernanceInvalidVoteId(rewardAllocationVoteUpdate.value), expireEpochProgress, rewardAllocationVoteUpdate)
          } else {
            rewardAllocationVoteUpdate.asRight
          }
      } yield result
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
          allocation.allocationEpochProgress.value.value,
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
