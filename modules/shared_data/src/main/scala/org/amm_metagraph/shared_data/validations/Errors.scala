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

  case object StakingAmountShouldBeGreaterThanZero extends DataApplicationValidationError {
    val message = "Staking amount should be greater than zero (0)"
  }

  case object WithdrawAmountShouldBeGreaterThanZero extends DataApplicationValidationError {
    val message = "Withdraw amount should be greater than zero (0)"
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

  case object SwapAllowSpendDifferentCurrency extends DataApplicationValidationError {
    val message = "Different currency between swap update and allow spend"
  }

  case object SwapAllowSpendDifferentSourceAddress extends DataApplicationValidationError {
    val message = "Different source address between swap update and allow spend"
  }
}
