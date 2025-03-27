package org.amm_metagraph.shared_data.validations

import cats.data.Validated.Invalid
import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.security.SecurityProvider
import io.constellationnetwork.security.signature.Signed

import eu.timepit.refined.auto._
import org.amm_metagraph.shared_data.types.DataUpdates.StakingUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool.{LiquidityPool, buildLiquidityPoolUniqueIdentifier, getLiquidityPools}
import org.amm_metagraph.shared_data.types.Staking._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.validations.Errors._
import org.amm_metagraph.shared_data.validations.SharedValidations.{
  signatureValidations,
  validateIfAllowSpendsAreDuplicated,
  validateIfTokenIdsAreTheSame
}

object StakingValidations {
  def stakingValidationsL1[F[_]: Async](
    stakingUpdate: StakingUpdate
  ): F[DataApplicationValidationErrorOr[Unit]] = Async[F].delay {
    val tokenIdsAreTheSame = validateIfTokenIdsAreTheSame(stakingUpdate.tokenAId, stakingUpdate.tokenBId)

    tokenIdsAreTheSame
  }

  def stakingValidationsL0[F[_]: Async](
    signedStakingUpdate: Signed[StakingUpdate],
    state: AmmCalculatedState
  )(implicit sp: SecurityProvider[F]): F[DataApplicationValidationErrorOr[Unit]] = {
    val stakingUpdate = signedStakingUpdate.value

    val stakingCalculatedState = getStakingCalculatedState(state)
    val pendingStaking = getPendingAllowSpendsStakingUpdates(state)

    val liquidityPoolsCalculatedState = getLiquidityPools(state)

    for {
      signatures <- signatureValidations(signedStakingUpdate, signedStakingUpdate.source)
      sourceAddress = signedStakingUpdate.source

      pendingTransactionAlreadyExists = validateIfPendingTransactionAlreadyExists(
        stakingUpdate,
        pendingStaking
      )

      tokenIdsAreTheSame = validateIfTokenIdsAreTheSame(stakingUpdate.tokenAId, stakingUpdate.tokenBId)

      confirmedTransactionAlreadyExists = validateIfConfirmedTransactionAlreadyExists(
        stakingUpdate,
        stakingCalculatedState.confirmed.value.get(sourceAddress)
      )

      liquidityPoolExists <- validateIfLiquidityPoolExists(
        stakingUpdate,
        liquidityPoolsCalculatedState
      )
      tokenAAllowSpendIsDuplicated = validateIfAllowSpendsAreDuplicated(
        stakingUpdate.tokenAAllowSpend,
        stakingCalculatedState.getPendingUpdates
      )
      tokenBAllowSpendIsDuplicated = validateIfAllowSpendsAreDuplicated(
        stakingUpdate.tokenBAllowSpend,
        stakingCalculatedState.getPendingUpdates
      )

      lastRef = lastRefValidation(stakingCalculatedState, signedStakingUpdate, sourceAddress)

    } yield
      signatures
        .productR(tokenIdsAreTheSame)
        .productR(confirmedTransactionAlreadyExists)
        .productR(pendingTransactionAlreadyExists)
        .productR(liquidityPoolExists)
        .productR(tokenAAllowSpendIsDuplicated)
        .productR(tokenBAllowSpendIsDuplicated)
        .productR(lastRef)
  }

  private def validateIfConfirmedTransactionAlreadyExists(
    stakingUpdate: StakingUpdate,
    maybeConfirmedStaking: Option[Set[StakingCalculatedStateAddress]]
  ): DataApplicationValidationErrorOr[Unit] =
    StakingTransactionAlreadyExists.whenA(maybeConfirmedStaking.exists(_.exists(_.tokenAAllowSpend == stakingUpdate.tokenBAllowSpend)))

  private def validateIfPendingTransactionAlreadyExists(
    stakingUpdate: StakingUpdate,
    maybePendingStaking: Set[Signed[StakingUpdate]]
  ): DataApplicationValidationErrorOr[Unit] =
    StakingTransactionAlreadyExists.whenA(maybePendingStaking.exists(_.tokenAAllowSpend == stakingUpdate.tokenBAllowSpend))

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
