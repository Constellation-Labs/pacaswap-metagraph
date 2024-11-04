package org.amm_metagraph.shared_data.validations

import cats.data.NonEmptySet
import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.L0NodeContext
import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.security.signature.signature.SignatureProof
import io.constellationnetwork.security.{Hasher, SecurityProvider}

import org.amm_metagraph.shared_data.Utils.{buildLiquidityPoolUniqueIdentifier, getAllowSpendsLastSyncGlobalSnapshotState, toAddress}
import org.amm_metagraph.shared_data.types.DataUpdates.StakingUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool.LiquidityPool
import org.amm_metagraph.shared_data.types.Staking.StakingCalculatedStateAddress
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

    stakingCalculatedState = state.ammState.get(OperationType.Staking).fold(Map.empty[Address, StakingCalculatedStateAddress]) {
      case stakingCalculatedState: StakingCalculatedState => stakingCalculatedState.addresses
      case _                                              => Map.empty[Address, StakingCalculatedStateAddress]
    }
    liquidityPoolsCalculatedState = state.ammState.get(OperationType.LiquidityPool).fold(Map.empty[String, LiquidityPool]) {
      case liquidityPoolCalculatedState: LiquidityPoolCalculatedState => liquidityPoolCalculatedState.liquidityPools
      case _                                                          => Map.empty[String, LiquidityPool]
    }

    validAllowSpends <- validateStakingAllowSpends(
      stakingUpdate
    )
    transactionAlreadyExists = validateIfTransactionAlreadyExists(
      stakingUpdate,
      stakingCalculatedState.get(address)
    )
    liquidityPoolExists <- validateIfLiquidityPoolExists(
      stakingUpdate,
      liquidityPoolsCalculatedState
    )

    result = validAllowSpends.productR(transactionAlreadyExists).productR(liquidityPoolExists)
  } yield result

  private def validateIfTransactionAlreadyExists(
    stakingUpdate: StakingUpdate,
    maybeStakingCalculatedStateAddress: Option[StakingCalculatedStateAddress]
  ): DataApplicationValidationErrorOr[Unit] =
    StakingTransactionAlreadyExists.whenA(maybeStakingCalculatedStateAddress.exists(_.tokenAAllowSpend == stakingUpdate.tokenBAllowSpend))

  private def validateIfLiquidityPoolExists[F[_]: Async](
    stakingUpdate: StakingUpdate,
    currentLiquidityPools: Map[String, LiquidityPool]
  ): F[DataApplicationValidationErrorOr[Unit]] = for {
    poolId <- buildLiquidityPoolUniqueIdentifier(stakingUpdate.tokenAId, stakingUpdate.tokenBId)
    result = StakingLiquidityPoolDoesNotExists.unlessA(currentLiquidityPools.contains(poolId))
  } yield result

  private def validateStakingAllowSpends[F[_]: Async: Hasher](
    stakingUpdate: StakingUpdate
  )(implicit context: L0NodeContext[F]): F[DataApplicationValidationErrorOr[Unit]] =
    getAllowSpendsLastSyncGlobalSnapshotState(
      stakingUpdate.tokenAAllowSpend,
      stakingUpdate.tokenBAllowSpend
    ).map { allowSpends =>
      val (tokenAAllowSpend, tokenBAllowSpend) = allowSpends
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
}
