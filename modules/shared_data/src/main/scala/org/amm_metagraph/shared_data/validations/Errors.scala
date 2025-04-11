package org.amm_metagraph.shared_data.validations

import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.DataApplicationValidationError
import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr

object Errors {
  private type DataApplicationValidationType = DataApplicationValidationErrorOr[Unit]

  val valid: DataApplicationValidationType = ().validNec[DataApplicationValidationError]

  implicit class DataApplicationValidationTypeOps[E <: DataApplicationValidationError](err: E) {
    def invalid: DataApplicationValidationType = err.invalidNec[Unit]

    def unlessA(cond: Boolean): DataApplicationValidationType = if (cond) valid else invalid

    def whenA(cond: Boolean): DataApplicationValidationType = if (cond) invalid else valid
  }

  case object AllocationPercentageExceed extends DataApplicationValidationError {
    val message = "Allocation percentage exceed"
  }

  case object MultipleSignatures extends DataApplicationValidationError {
    val message = "Multiple signatures"
  }

  case object NotSignedExclusivelyBySourceAddress extends DataApplicationValidationError {
    val message = "Not signed exclusively by address owner"
  }

  case object ParentOrdinalLowerThenLastProcessedTxOrdinal extends DataApplicationValidationError {
    val message = "Parent ordinal lower then last processed tx ordinal"
  }

  case object DailyAllocationExceed extends DataApplicationValidationError {
    val message = "Daily allocation exceed"
  }

  case object HasNoMatchingParent extends DataApplicationValidationError {
    val message = "Has no matching parent"
  }

  case object MissingVotingWeight extends DataApplicationValidationError {
    val message = "Missing voting weight"
  }

  case object InvalidAllocationId extends DataApplicationValidationError {
    val message = "Invalid allocation ID"
  }

  case object StakingAmountShouldBeGreaterThanZero extends DataApplicationValidationError {
    val message = "Staking amount should be greater than zero (0)"
  }

  case object StakingTransactionAlreadyExists extends DataApplicationValidationError {
    val message = "Staking transaction already exists"
  }

  case object StakingLiquidityPoolDoesNotExists extends DataApplicationValidationError {
    val message = "Staking liquidity pool does not exists"
  }

  case object StakingMissingAllowSpend extends DataApplicationValidationError {
    val message = "Missing one or both allow spends to stake"
  }

  case object StakingDifferentAllowSpendSource extends DataApplicationValidationError {
    val message = "Different allow spend sources"
  }

  case object StakingDifferentAllowSpendDestination extends DataApplicationValidationError {
    val message = "Different allow spend destination"
  }

  case object InvalidStakingParent extends DataApplicationValidationError {
    val message: String = "Staking parent does not match last confirmed"
  }

  case object WithdrawalInsufficientShares extends DataApplicationValidationError {
    val message: String = "Insufficient shares for withdrawal"
  }

  case object WithdrawalAllLPShares extends DataApplicationValidationError {
    val message: String = "Withdrawal of all LP shares is not allowed"
  }

  case object WithdrawalAlreadyPending extends DataApplicationValidationError {
    val message: String = "Withdrawal already pending for this pool"
  }

  case object InvalidWithdrawalParent extends DataApplicationValidationError {
    val message: String = "Withdrawal parent does not match last confirmed"
  }

  case object LiquidityPoolNotEnoughInformation extends DataApplicationValidationError {
    val message = "You should provide at 2 tokens, or at least 1 token to use DAG as the second one"
  }

  case object LiquidityPoolAlreadyExists extends DataApplicationValidationError {
    val message = "Liquidity pool already exists"
  }

  case object LiquidityPoolDoesNotExists extends DataApplicationValidationError {
    val message = "Liquidity pool does not exists"
  }

  case object SwapLiquidityPoolDoesNotExists extends DataApplicationValidationError {
    val message = "Swap liquidity pool does not exists"
  }

  case object SwapLiquidityPoolNotEnoughTokens extends DataApplicationValidationError {
    val message = "Swap liquidity pool not enough tokens"
  }

  case object SwapMissingAllowSpend extends DataApplicationValidationError {
    val message = "Missing swap allow spend"
  }

  case object SwapTransactionAlreadyExists extends DataApplicationValidationError {
    val message = "Swap transaction already exists"
  }

  case object InvalidSwapParent extends DataApplicationValidationError {
    val message: String = "Swap ordinal lower than last confirmed"
  }

  case object DuplicatedOperation extends DataApplicationValidationError {
    val message = "Duplicated operation"
  }

  case object TokenIdsAreTheSame extends DataApplicationValidationError {
    val message = "Token ids are the same but should be different"
  }

  case object FeePercentagesMustEqualTotal extends DataApplicationValidationError {
    val message = "Fee percentage components must sum to total"
  }

  case object FeePercentageTotalMustBeGreaterThanZero extends DataApplicationValidationError {
    val message = "Fee percentage total must be greater than zero"
  }
}
