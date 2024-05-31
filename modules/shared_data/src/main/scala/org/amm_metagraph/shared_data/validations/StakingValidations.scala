package org.amm_metagraph.shared_data.validations

import cats.data.NonEmptySet
import cats.effect.Async
import cats.syntax.all._
import org.amm_metagraph.shared_data.Utils.{buildLiquidityPoolUniqueIdentifier, toAddress}
import org.amm_metagraph.shared_data.types.LiquidityPool.LiquidityPool
import org.amm_metagraph.shared_data.types.DataUpdates.StakingUpdate
import org.amm_metagraph.shared_data.types.Staking.StakingCalculatedStateAddress
import org.amm_metagraph.shared_data.types.States.{AmmCalculatedState, LiquidityPoolCalculatedState, OperationType, StakingCalculatedState}
import org.amm_metagraph.shared_data.validations.Errors._
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import org.tessellation.schema.address.Address
import org.tessellation.security.SecurityProvider
import org.tessellation.security.signature.signature.SignatureProof

object StakingValidations {
  def stakingValidationsL1[F[_] : Async](
    stakingUpdate: StakingUpdate,
  ): F[DataApplicationValidationErrorOr[Unit]] = Async[F].delay {
    valid
  }

  def stakingValidationsL0[F[_] : Async](
    stakingUpdate: StakingUpdate,
    state        : AmmCalculatedState,
    proofs       : NonEmptySet[SignatureProof]
  )(implicit sp: SecurityProvider[F]): F[DataApplicationValidationErrorOr[Unit]] = {
    for {
      address <- toAddress(proofs.head)
      stakingCalculatedState = state.ammState.get(OperationType.Staking).fold(Map.empty[Address, StakingCalculatedStateAddress]) {
        case stakingCalculatedState: StakingCalculatedState => stakingCalculatedState.addresses
        case _ => Map.empty[Address, StakingCalculatedStateAddress]
      }
      liquidityPoolsCalculatedState = state.ammState.get(OperationType.LiquidityPool).fold(Map.empty[String, LiquidityPool]) {
        case liquidityPoolCalculatedState: LiquidityPoolCalculatedState => liquidityPoolCalculatedState.liquidityPools
        case _ => Map.empty[String, LiquidityPool]
      }

      transactionAlreadyExists = validateIfTransactionAlreadyExists(
        stakingUpdate,
        stakingCalculatedState.get(address)
      )
      liquidityPoolExists <- validateIfLiquidityPoolExists(
        stakingUpdate,
        liquidityPoolsCalculatedState
      )
      stakingValidationsL1 <- stakingValidationsL1(stakingUpdate)
      result = stakingValidationsL1.productR(transactionAlreadyExists).productR(liquidityPoolExists)
    } yield result
  }

  private def validateIfTransactionAlreadyExists(
    stakingUpdate                     : StakingUpdate,
    maybeStakingCalculatedStateAddress: Option[StakingCalculatedStateAddress]
  ): DataApplicationValidationErrorOr[Unit] =
    StakingTransactionAlreadyExists.whenA(maybeStakingCalculatedStateAddress.exists(_.primaryAllowSpendReferenceTxnId == stakingUpdate.primaryAllowSpendReferenceTxnId))

  private def validateIfLiquidityPoolExists[F[_] : Async](
    stakingUpdate        : StakingUpdate,
    currentLiquidityPools: Map[String, LiquidityPool]
  ): F[DataApplicationValidationErrorOr[Unit]] = for {
    poolId <- buildLiquidityPoolUniqueIdentifier(stakingUpdate.primaryTokenId, stakingUpdate.pairTokenId)
    result = StakingLiquidityPoolDoesNotExists.unlessA(currentLiquidityPools.contains(poolId))
  } yield result

}
