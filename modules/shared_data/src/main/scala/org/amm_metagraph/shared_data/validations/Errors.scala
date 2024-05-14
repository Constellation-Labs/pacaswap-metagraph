package org.amm_metagraph.shared_data.validations

import cats.syntax.validated.catsSyntaxValidatedIdBinCompat0
import org.tessellation.currency.dataApplication.DataApplicationValidationError
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr

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

  case object InvalidAddress extends DataApplicationValidationError {
    val message = "Invalid address"
  }
}
