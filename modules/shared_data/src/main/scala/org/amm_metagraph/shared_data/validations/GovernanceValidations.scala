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
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, RewardAllocationVoteUpdate}
import org.amm_metagraph.shared_data.types.Governance.{UserAllocations, VotingPower, maxCredits}
import org.amm_metagraph.shared_data.types.LiquidityPool.getLiquidityPoolCalculatedState
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.codecs.{HasherSelector, JsonWithBase64BinaryCodec}
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
  def make[F[_]: Async: HasherSelector](
    applicationConfig: ApplicationConfig,
    dataUpdateCodec: JsonWithBase64BinaryCodec[F, AmmUpdate]
  ): GovernanceValidations[F] = new GovernanceValidations[F] {
    override def l1Validations(
      rewardAllocationVoteUpdate: RewardAllocationVoteUpdate
    ): F[DataApplicationValidationErrorOr[Unit]] = Async[F].delay {
      allocationWeightsValidation(rewardAllocationVoteUpdate)
    }

    override def l0Validations(
      rewardAllocationVoteUpdate: Signed[RewardAllocationVoteUpdate],
      state: AmmCalculatedState,
      lastSyncGlobalSnapshotEpochProgress: EpochProgress
    )(implicit sp: SecurityProvider[F]): F[Either[FailedCalculatedState, Signed[RewardAllocationVoteUpdate]]] = {
      val lastAllocations = state.allocations
      val lastVotingPowers = state.votingPowers
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
        walletHasVotingPower = walletHasVotingPowerValidation(
          lastVotingPowers,
          sourceAddress
        )
        isValidId = allocationIdValidation(
          applicationConfig,
          rewardAllocationVoteUpdate,
          liquidityPools
        )
        allocationWeights = allocationWeightsValidation(rewardAllocationVoteUpdate.value)
        expireEpochProgress = getFailureExpireEpochProgress(applicationConfig, lastSyncGlobalSnapshotEpochProgress)

        hashedUpdate <- HasherSelector[F].withCurrent(implicit hs => rewardAllocationVoteUpdate.toHashed(dataUpdateCodec.serialize))
        updateHash = hashedUpdate.hash

        result =
          if (lastTransactionRef.isInvalid) {
            failWith(InvalidLastReference(), expireEpochProgress, rewardAllocationVoteUpdate, updateHash)
          } else if (signatures.isInvalid) {
            failWith(InvalidSignatures(signatures.map(_.show).mkString_(",")), expireEpochProgress, rewardAllocationVoteUpdate, updateHash)
          } else if (dailyLimitAllocation.isInvalid) {
            failWith(
              GovernanceDailyLimitAllocation(rewardAllocationVoteUpdate.value),
              expireEpochProgress,
              rewardAllocationVoteUpdate,
              updateHash
            )
          } else if (walletHasVotingPower.isInvalid) {
            failWith(
              GovernanceWalletWithNoVotingPower(rewardAllocationVoteUpdate.value),
              expireEpochProgress,
              rewardAllocationVoteUpdate,
              updateHash
            )
          } else if (isValidId.isInvalid) {
            failWith(GovernanceInvalidVoteId(rewardAllocationVoteUpdate.value), expireEpochProgress, rewardAllocationVoteUpdate, updateHash)
          } else if (allocationWeights.isInvalid) {
            failWith(
              GovernanceAllocationPercentageExceed(rewardAllocationVoteUpdate.value),
              expireEpochProgress,
              rewardAllocationVoteUpdate,
              updateHash
            )
          } else {
            rewardAllocationVoteUpdate.asRight
          }
      } yield result
    }

    private def allocationWeightsValidation(
      rewardAllocationVoteUpdate: RewardAllocationVoteUpdate
    ): DataApplicationValidationErrorOr[Unit] = {
      // Allocation values are relative PosLong weights, not percentages. Their exact normalized sum is therefore 1
      // for every non-empty vote (and 0 for the historically accepted empty vote). Compare each numerator to the
      // exact BigInt denominator instead of redundantly dividing Longs: the old check could wrap a crafted total to
      // zero and raise at public L1 ingress.
      val allocationsSum = rewardAllocationVoteUpdate.allocations.foldLeft(BigInt(0)) {
        case (sum, (_, allocationWeight)) => sum + BigInt(allocationWeight.value)
      }

      AllocationPercentageExceed.whenA(
        allocationsSum > 0 && rewardAllocationVoteUpdate.allocations.exists {
          case (_, allocationWeight) => BigInt(allocationWeight.value) > allocationsSum
        }
      )
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

    private def walletHasVotingPowerValidation(
      lastVotingPowers: Map[Address, VotingPower],
      address: Address
    ): DataApplicationValidationErrorOr[Unit] =
      MissingVotingPower.unlessA(lastVotingPowers.get(address).exists(_.total.value > 0.0d))

    private def allocationIdValidation(
      applicationConfig: ApplicationConfig,
      rewardAllocationVoteUpdate: Signed[RewardAllocationVoteUpdate],
      liquidityPools: LiquidityPoolCalculatedState
    ): DataApplicationValidationErrorOr[Unit] = {
      val allocationIds = rewardAllocationVoteUpdate.allocations.map { case (id, _) => id }
      val liquidityPoolIds = liquidityPools.confirmed.value.keySet

      InvalidAllocationId.whenA(
        allocationIds.exists(id =>
          !liquidityPoolIds.contains(id) &&
            (id != applicationConfig.nodeValidatorsGovernanceAllocationId)
        )
      )
    }
  }

}
