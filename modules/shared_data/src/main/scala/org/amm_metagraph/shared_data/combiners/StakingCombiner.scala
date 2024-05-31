package org.amm_metagraph.shared_data.combiners

import cats.syntax.all._
import cats.effect.Async
import cats.implicits.catsSyntaxOption
import org.amm_metagraph.shared_data.Utils.{PosLongOps, buildLiquidityPoolUniqueIdentifier, getUpdatedTokenInformation}
import org.amm_metagraph.shared_data.types.Staking.{StakingCalculatedStateAddress, StakingCalculatedStateLastReference}
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, StakingUpdate}
import org.amm_metagraph.shared_data.types.LiquidityPool.{LiquidityPool, LiquidityProviders, TokenInformation}
import org.amm_metagraph.shared_data.types.States._
import org.tessellation.currency.dataApplication.DataState
import org.tessellation.schema.SnapshotOrdinal
import org.tessellation.schema.address.Address

object StakingCombiner {
  private def updateLiquidityPool(
    liquidityPool  : LiquidityPool,
    signerAddress  : Address,
    primaryToken   : TokenInformation,
    pairToken      : TokenInformation,
    liquidityMinted: Long
  ): LiquidityPool = {
    val tokenA = if (liquidityPool.tokenA.identifier == primaryToken.identifier) primaryToken else pairToken
    val tokenB = if (liquidityPool.tokenB.identifier == pairToken.identifier) pairToken else primaryToken

    val updatedTokenAAmount = (liquidityPool.tokenA.amount.value + tokenA.amount.value).toPosLongUnsafe
    val updatedTokenBAmount = (liquidityPool.tokenB.amount.value + tokenB.amount.value).toPosLongUnsafe

    val addressLiquidity = liquidityPool.liquidityProviders.providers.getOrElse(signerAddress, 0L)
    val updatedTotalLiquidity = liquidityPool.totalLiquidity + liquidityMinted
    val updatedLiquidityProviders = liquidityPool.liquidityProviders.providers.updated(signerAddress, addressLiquidity + liquidityMinted)

    liquidityPool.copy(
      tokenA = tokenA.copy(amount = updatedTokenAAmount),
      tokenB = tokenB.copy(amount = updatedTokenBAmount),
      k = (updatedTokenAAmount.value * updatedTokenBAmount.value).toPosLongUnsafe,
      totalLiquidity = updatedTotalLiquidity,
      liquidityProviders = LiquidityProviders(updatedLiquidityProviders)
    )
  }

  def combineStaking[F[_] : Async](
    acc                   : DataState[AmmOnChainState, AmmCalculatedState],
    stakingUpdate         : StakingUpdate,
    signerAddress         : Address,
    currentSnapshotOrdinal: SnapshotOrdinal
  ): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
    val stakingCalculatedStateAddresses = acc.calculated.ammState.get(OperationType.Staking).fold(Map.empty[Address, StakingCalculatedStateAddress]) {
      case stakingCalculatedState: StakingCalculatedState => stakingCalculatedState.addresses
      case _ => Map.empty
    }

    val liquidityPoolsCalculatedState = acc.calculated.ammState.get(OperationType.LiquidityPool).fold(Map.empty[String, LiquidityPool]) {
      case liquidityPoolsCalculatedState: LiquidityPoolCalculatedState => liquidityPoolsCalculatedState.liquidityPools
      case _ => Map.empty
    }

    val maybeLastStakingInfo = stakingCalculatedStateAddresses.get(signerAddress) match {
      case Some(stakingState: StakingCalculatedStateAddress) =>
        StakingCalculatedStateLastReference(
          stakingState.primaryAllowSpendReferenceTxnId,
          stakingState.pairAllowSpendReferenceTxnId,
          stakingState.primaryToken,
          stakingState.pairToken,
          stakingState.ordinal
        ).some

      case _ => none
    }

    for {
      poolId <- buildLiquidityPoolUniqueIdentifier(stakingUpdate.primaryTokenId, stakingUpdate.pairTokenId)
      liquidityPool <- liquidityPoolsCalculatedState.get(poolId).toOptionT.getOrRaise(new IllegalStateException("Liquidity Pool does not exists"))
      (primaryToken, pairToken, liquidityMinted) = getUpdatedTokenInformation(stakingUpdate, liquidityPool)
      liquidityPoolUpdated = updateLiquidityPool(liquidityPool, signerAddress, primaryToken, pairToken, liquidityMinted)

      stakingCalculatedStateAddress = StakingCalculatedStateAddress(
        stakingUpdate.primaryAllowSpendReferenceTxnId,
        stakingUpdate.pairAllowSpendReferenceTxnId,
        primaryToken,
        pairToken,
        currentSnapshotOrdinal,
        maybeLastStakingInfo
      )
      updatedStakingCalculatedState = StakingCalculatedState(stakingCalculatedStateAddresses.updated(signerAddress, stakingCalculatedStateAddress))
      updatedLiquidityPool = LiquidityPoolCalculatedState(liquidityPoolsCalculatedState.updated(poolId, liquidityPoolUpdated))

      updates: List[AmmUpdate] = stakingUpdate :: acc.onChain.updates
      updatedCalculatedState = acc.calculated.ammState
        .updated(OperationType.Staking, updatedStakingCalculatedState)
        .updated(OperationType.LiquidityPool, updatedLiquidityPool)

    } yield DataState(
      AmmOnChainState(updates),
      AmmCalculatedState(updatedCalculatedState)
    )
  }
}