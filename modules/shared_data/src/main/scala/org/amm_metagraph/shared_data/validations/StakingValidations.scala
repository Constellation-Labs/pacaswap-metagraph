package org.amm_metagraph.shared_data.validations

import cats.data.NonEmptySet
import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.L0NodeContext
import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.signature.signature.SignatureProof
import io.constellationnetwork.security.{Hasher, SecurityProvider}

import org.amm_metagraph.shared_data.Utils.{buildLiquidityPoolUniqueIdentifier, getAllowSpendLastSyncGlobalSnapshotState, toAddress}
import org.amm_metagraph.shared_data.types.DataUpdates.StakingUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool.{LiquidityPool, getLiquidityPools}
import org.amm_metagraph.shared_data.types.Staking.{StakingCalculatedStateAddress, getPendingStakeUpdates, getStakingCalculatedState}
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.validations.Errors._

object StakingValidations {
  def stakingValidationsL1[F[_]: Async](
    stakingUpdate: StakingUpdate
  ): F[DataApplicationValidationErrorOr[Unit]] =
    valid.pure

  def stakingValidationsL0[F[_]: Async: Hasher](
    stakingUpdate: StakingUpdate,
    state: AmmCalculatedState,
    proofs: NonEmptySet[SignatureProof]
  )(implicit sp: SecurityProvider[F], context: L0NodeContext[F]): F[DataApplicationValidationErrorOr[Unit]] = for {
    address <- toAddress(proofs.head)

    stakingCalculatedState = getStakingCalculatedState(state)
    pendingStaking = getPendingStakeUpdates(state)

    liquidityPoolsCalculatedState = getLiquidityPools(state)

    validAllowSpends <- validateStakingAllowSpends(
      stakingUpdate
    )
    confirmedTransactionAlreadyExists = validateIfConfirmedTransactionAlreadyExists(
      stakingUpdate,
      stakingCalculatedState.confirmed.get(address)
    )
    pendingTransactionAlreadyExists = validateIfPendingTransactionAlreadyExists(
      stakingUpdate,
      pendingStaking
    )
    liquidityPoolExists <- validateIfLiquidityPoolExists(
      stakingUpdate,
      liquidityPoolsCalculatedState
    )

    result = validAllowSpends
      .productR(confirmedTransactionAlreadyExists)
      .productR(pendingTransactionAlreadyExists)
      .productR(liquidityPoolExists)
  } yield result

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

  private def validateStakingAllowSpends[F[_]: Async: Hasher](
    stakingUpdate: StakingUpdate
  )(implicit context: L0NodeContext[F]): F[DataApplicationValidationErrorOr[Unit]] = for {
    tokenAAllowSpend <- getAllowSpendLastSyncGlobalSnapshotState(
      stakingUpdate.tokenAAllowSpend
    )
    tokenBAllowSpend <- getAllowSpendLastSyncGlobalSnapshotState(
      stakingUpdate.tokenBAllowSpend
    )
  } yield
    if (tokenAAllowSpend.isEmpty || tokenBAllowSpend.isEmpty) {
      StakingMissingAllowSpend.invalid
    } else if (tokenAAllowSpend.get.source != tokenBAllowSpend.get.source) {
      StakingDifferentAllowSpendSource.invalid
    } else if (tokenAAllowSpend.get.destination != tokenBAllowSpend.get.destination) {
      StakingDifferentAllowSpendDestination.invalid
    } else {
      valid
    }
}
