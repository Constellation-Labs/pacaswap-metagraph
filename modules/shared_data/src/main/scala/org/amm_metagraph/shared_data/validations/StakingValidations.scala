package org.amm_metagraph.shared_data.validations

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.swap.CurrencyId
import io.constellationnetwork.security.SecurityProvider
import io.constellationnetwork.security.signature.Signed

import org.amm_metagraph.shared_data.AllowSpends.getAllAllowSpendsInUseFromState
import org.amm_metagraph.shared_data.types.DataUpdates.StakingUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool.{LiquidityPool, buildLiquidityPoolUniqueIdentifier, getConfirmedLiquidityPools}
import org.amm_metagraph.shared_data.types.Staking._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.validations.Errors._
import org.amm_metagraph.shared_data.validations.SharedValidations._

object StakingValidations {
  def stakingValidationsL1[F[_]: Async](
    stakingUpdate: StakingUpdate
  ): F[DataApplicationValidationErrorOr[Unit]] = Async[F].delay {
    validateIfTokenIdsAreTheSame(stakingUpdate.tokenAId, stakingUpdate.tokenBId)
  }

  def stakingValidationsL0[F[_]: Async](
    signedStakingUpdate: Signed[StakingUpdate],
    state: AmmCalculatedState
  )(implicit sp: SecurityProvider[F]): F[DataApplicationValidationErrorOr[Unit]] = {
    val stakingUpdate = signedStakingUpdate.value

    val stakingCalculatedState = getStakingCalculatedState(state)
    val liquidityPoolsCalculatedState = getConfirmedLiquidityPools(state)

    for {
      l1Validations <- stakingValidationsL1(stakingUpdate)
      signatures <- signatureValidations(signedStakingUpdate, signedStakingUpdate.source)
      sourceAddress = signedStakingUpdate.source

      transactionAlreadyExists = validateIfTransactionAlreadyExists(
        stakingUpdate,
        stakingCalculatedState.confirmed.value.getOrElse(sourceAddress, Set.empty),
        stakingCalculatedState.getPendingUpdates
      )

      liquidityPoolExists <- validateIfLiquidityPoolExists(
        stakingUpdate,
        liquidityPoolsCalculatedState
      )

      allAllowSpendsInUse = getAllAllowSpendsInUseFromState(state)

      tokenAAllowSpendIsDuplicated = validateIfAllowSpendsAreDuplicated(
        stakingUpdate.tokenAAllowSpend,
        allAllowSpendsInUse
      )
      tokenBAllowSpendIsDuplicated = validateIfAllowSpendsAreDuplicated(
        stakingUpdate.tokenBAllowSpend,
        allAllowSpendsInUse
      )

      lastRef = lastRefValidation(stakingCalculatedState, signedStakingUpdate, sourceAddress)

    } yield
      l1Validations
        .productR(signatures)
        .productR(transactionAlreadyExists)
        .productR(liquidityPoolExists)
        .productR(tokenAAllowSpendIsDuplicated)
        .productR(tokenBAllowSpendIsDuplicated)
        .productR(lastRef)
  }

  private def validateIfTransactionAlreadyExists(
    stakingUpdate: StakingUpdate,
    confirmedStakings: Set[StakingCalculatedStateAddress],
    pendingStakings: Set[Signed[StakingUpdate]]
  ): DataApplicationValidationErrorOr[Unit] =
    StakingTransactionAlreadyExists.whenA(
      confirmedStakings.exists(staking =>
        staking.tokenAAllowSpend === stakingUpdate.tokenAAllowSpend || staking.tokenBAllowSpend === stakingUpdate.tokenBAllowSpend
      ) ||
        pendingStakings.exists(staking =>
          staking.value.tokenAAllowSpend === stakingUpdate.tokenAAllowSpend || staking.value.tokenBAllowSpend === stakingUpdate.tokenBAllowSpend
        )
    )

  private def validateIfLiquidityPoolExists[F[_]: Async](
    stakingUpdate: StakingUpdate,
    currentLiquidityPools: Map[String, LiquidityPool]
  ): F[DataApplicationValidationErrorOr[Unit]] = for {
    poolId <- buildLiquidityPoolUniqueIdentifier(stakingUpdate.tokenAId, stakingUpdate.tokenBId)
    result = StakingLiquidityPoolDoesNotExists.unlessA(currentLiquidityPools.contains(poolId.value))
  } yield result

  private def lastRefValidation(
    stakingCalculatedState: StakingCalculatedState,
    signedStaking: Signed[StakingUpdate],
    address: Address
  ): DataApplicationValidationErrorOr[Unit] = {
    val lastConfirmed: Option[StakingReference] = stakingCalculatedState.confirmed.value
      .get(address)
      .flatMap(_.maxByOption(_.parent.ordinal))
      .map(_.parent)

    lastConfirmed match {
      case Some(last) if signedStaking.ordinal =!= last.ordinal.next || signedStaking.parent =!= last =>
        InvalidStakingParent.invalid
      case _ => valid
    }
  }
}
