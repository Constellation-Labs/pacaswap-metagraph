package org.amm_metagraph.shared_data.validations

import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.DataApplicationValidationError
import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.swap.{AllowSpend, CurrencyId, SwapAmount}

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import org.amm_metagraph.shared_data.types.DataUpdates.AmmUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool.ShareAmount
import org.amm_metagraph.shared_data.types.Rewards.RewardType

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

  case object MissingVotingPower extends DataApplicationValidationError {
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

  case object InvalidAMMMetagraphId extends DataApplicationValidationError {
    val message: String = "Invalid AMM Metagraph ID"
  }

  case object InvalidRewardWithdrawParent extends DataApplicationValidationError {
    val message: String = "Reward withdrawal parent does not match last confirmed"
  }

  case object InvalidRewardWithdrawAmount extends DataApplicationValidationError {
    val message: String = "Not enough available reward amount withdraw"
  }

  @derive(encoder, decoder)
  sealed trait FailureReason

  case class OperationExpired(update: AmmUpdate) extends FailureReason
  case class AllowSpendExpired(allowSpend: AllowSpend) extends FailureReason
  case class AmountGreaterThanAllowSpendLimit(allowSpend: AllowSpend) extends FailureReason
  case class SwapLessThanMinAmount() extends FailureReason
  case class SwapHigherThanMaxAmount() extends FailureReason
  case class WithdrawalAmountExceedsAvailableShares(requestedShares: ShareAmount) extends FailureReason
  case class CannotWithdrawAllShares() extends FailureReason
  case class TokenExceedsAvailableAmount(tokenId: Option[CurrencyId], availableAmount: Long, requestedAmount: Long)
      extends FailureReason
  case class ArithmeticError(message: String) extends FailureReason
  case class SwapWouldDrainPoolBalance() extends FailureReason
  case class SwapExceedsMaxTokensLimit(update: AmmUpdate, grossReceived: SwapAmount) extends FailureReason
  case class WithdrawalWouldDrainPoolBalance() extends FailureReason
  case class WithdrawalLessThanMinAmount() extends FailureReason
  case class WithdrawalHigherThanMaxAmount() extends FailureReason
  case class InvalidLiquidityPool() extends FailureReason
  case class InvalidSwapTokenInfo(message: String) extends FailureReason
  case class DuplicatedLiquidityPoolRequest(update: AmmUpdate) extends FailureReason
  case class DuplicatedStakingRequest(update: AmmUpdate) extends FailureReason
  case class DuplicatedSwapRequest(update: AmmUpdate) extends FailureReason
  case class DuplicatedAllowSpend(update: AmmUpdate) extends FailureReason
  case class SourceAddressBetweenUpdateAndAllowSpendDifferent(update: AmmUpdate) extends FailureReason
  case class AllowSpendsDestinationAddressInvalid() extends FailureReason
  case class MissingSwapTokenInfo() extends FailureReason
  case class MissingStakingTokenInfo() extends FailureReason
  case class MissingWithdrawalsAmount() extends FailureReason
  case class InvalidCurrencyIdsBetweenAllowSpendsAndDataUpdate(update: AmmUpdate) extends FailureReason
  case object WrongRewardWithdrawEpoch extends FailureReason
  case class RewardWithdrawAmountError(currentAmount: Amount, error: String) extends FailureReason

  case class InvalidSignatures(messages: String) extends FailureReason
  case class TransactionAlreadyExists(update: AmmUpdate) extends FailureReason
  case class InvalidLastReference() extends FailureReason
  case class NotEnoughShares() extends FailureReason
  case class NotEnoughTokens() extends FailureReason
  case class WithdrawalAllLPSharesError() extends FailureReason
  case class WithdrawalNotPendingError() extends FailureReason
  case class GovernanceDailyLimitAllocation(update: AmmUpdate) extends FailureReason
  case class GovernanceWalletWithNoVotingPower(update: AmmUpdate) extends FailureReason
  case class GovernanceInvalidVoteId(update: AmmUpdate) extends FailureReason
  case class InvalidWithdrawalAmount(update: AmmUpdate) extends FailureReason
  case class DuplicatedUpdate(update: AmmUpdate) extends FailureReason
}
