package org.amm_metagraph.shared_data.validations

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.security.SecurityProvider

import org.amm_metagraph.shared_data.types.DataUpdates.WithdrawUpdate
import org.amm_metagraph.shared_data.types.States.AmmCalculatedState
import org.amm_metagraph.shared_data.validations.Errors.valid

object WithdrawValidations {
  def withdrawValidationsL1[F[_]: Async](
    withdrawUpdate: WithdrawUpdate
  ): F[DataApplicationValidationErrorOr[Unit]] = valid.pure

  def withdrawValidationsL0[F[_]: Async: SecurityProvider](
    withdrawUpdate: WithdrawUpdate,
    state: AmmCalculatedState
  ): F[DataApplicationValidationErrorOr[Unit]] = valid.pure
}
