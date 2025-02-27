package org.amm_metagraph.shared_data.validations

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.L0NodeContext
import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hasher, SecurityProvider}

import eu.timepit.refined.auto._
import org.amm_metagraph.shared_data.globalSnapshots.getAllowSpendsLastSyncGlobalSnapshotState
import org.amm_metagraph.shared_data.types.DataUpdates.StakingUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool.{LiquidityPool, buildLiquidityPoolUniqueIdentifier, getLiquidityPools}
import org.amm_metagraph.shared_data.types.Staking._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.validations.Errors._
import org.amm_metagraph.shared_data.validations.SharedValidations.validateIfAllowSpendsAndSpendTransactionsAreDuplicated

object StakingValidations {
  def stakingValidationsL1[F[_]: Async](
    stakingUpdate: StakingUpdate
  ): F[DataApplicationValidationErrorOr[Unit]] =
    valid.pure

  def stakingValidationsL0[F[_]: Async: Hasher](
    signedStakingUpdate: Signed[StakingUpdate],
    state: AmmCalculatedState
  )(implicit sp: SecurityProvider[F], context: L0NodeContext[F]): F[DataApplicationValidationErrorOr[Unit]] = for {
    address <- signedStakingUpdate.proofs.head.id.toAddress
    stakingUpdate = signedStakingUpdate.value

    stakingCalculatedState = getStakingCalculatedState(state)
    pendingStaking = getPendingStakeUpdates(state)

    liquidityPoolsCalculatedState = getLiquidityPools(state)

    validAllowSpends <- validateStakingAllowSpends(
      stakingUpdate
    )
    confirmedTransactionAlreadyExists = validateIfConfirmedTransactionAlreadyExists(
      stakingUpdate,
      stakingCalculatedState.confirmed.value.get(address)
    )
    pendingTransactionAlreadyExists = validateIfPendingTransactionAlreadyExists(
      stakingUpdate,
      pendingStaking
    )
    liquidityPoolExists <- validateIfLiquidityPoolExists(
      stakingUpdate,
      liquidityPoolsCalculatedState
    )
    tokenAAllowSpendIsDuplicated = validateIfAllowSpendsAndSpendTransactionsAreDuplicated(
      stakingUpdate.tokenAAllowSpend,
      stakingCalculatedState.pending,
      state.spendTransactions
    )
    tokenBAllowSpendIsDuplicated = validateIfAllowSpendsAndSpendTransactionsAreDuplicated(
      stakingUpdate.tokenBAllowSpend,
      stakingCalculatedState.pending,
      state.spendTransactions
    )

    lastRef = lastRefValidation(stakingCalculatedState, signedStakingUpdate, address)

  } yield
    validAllowSpends
      .productR(confirmedTransactionAlreadyExists)
      .productR(pendingTransactionAlreadyExists)
      .productR(liquidityPoolExists)
      .productR(tokenAAllowSpendIsDuplicated)
      .productR(tokenBAllowSpendIsDuplicated)
      .productR(lastRef)

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
  )(implicit context: L0NodeContext[F]): F[DataApplicationValidationErrorOr[Unit]] =
    getAllowSpendsLastSyncGlobalSnapshotState(
      stakingUpdate.tokenAAllowSpend,
      stakingUpdate.tokenAId,
      stakingUpdate.tokenBAllowSpend,
      stakingUpdate.tokenBId
    ).map {
      case (None, _) | (_, None) => StakingMissingAllowSpend.invalid
      case (Some(allowSpendA), Some(allowSpendB)) if allowSpendA.source != allowSpendB.source =>
        StakingDifferentAllowSpendSource.invalid
      case (Some(allowSpendA), Some(allowSpendB)) if allowSpendA.destination != allowSpendB.destination =>
        StakingDifferentAllowSpendDestination.invalid
      case _ => valid
    }

  private def lastRefValidation(
    stakingCalculatedState: StakingCalculatedState,
    signedStaking: Signed[StakingUpdate],
    address: Address
  ): DataApplicationValidationErrorOr[Unit] = {
    val lastConfirmedOrdinal: Option[StakingOrdinal] = stakingCalculatedState.confirmed.value
      .get(address)
      .flatMap(_.maxByOption(_.ordinal.value.value))
      .map(_.ordinal)

    lastConfirmedOrdinal match {
      case Some(last) if last.value >= signedStaking.ordinal.value => StakingOrdinalLowerThanLastConfirmed.invalid
      case _                                                       => valid
    }
  }
}
