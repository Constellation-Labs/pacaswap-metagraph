package org.amm_metagraph.shared_data.combiners

import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.SortedSet

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact._
import io.constellationnetwork.schema.swap.AllowSpend
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hashed, Hasher}

import org.amm_metagraph.shared_data.Utils._
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, StakingUpdate}
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.Staking.{
  StakingCalculatedStateAddress,
  StakingCalculatedStateLastReference,
  getStakingCalculatedState
}
import org.amm_metagraph.shared_data.types.States._

object StakingCombiner {

  private def getUpdatedTokenInformation(
    stakingUpdate: StakingUpdate,
    liquidityPool: LiquidityPool
  ): (TokenInformation, TokenInformation, Long) = {
    val primaryToken = if (stakingUpdate.tokenAId == liquidityPool.tokenA.identifier) liquidityPool.tokenA else liquidityPool.tokenB
    val pairToken = if (stakingUpdate.tokenBId == liquidityPool.tokenA.identifier) liquidityPool.tokenA else liquidityPool.tokenB

    val currentPrimaryTokenAmount = primaryToken.amount.value.fromTokenAmountFormat
    val currentPairTokenAmount = pairToken.amount.value.fromTokenAmountFormat

    val incomingPrimaryAmount = stakingUpdate.tokenAAmount.value
    val incomingPairAmount =
      (incomingPrimaryAmount * currentPairTokenAmount) / currentPrimaryTokenAmount // Calculate equivalent pair needed to maintain the invariant

    val liquidityMinted = math.min(
      incomingPrimaryAmount * liquidityPool.totalLiquidity / primaryToken.amount.value.toDouble,
      incomingPairAmount * liquidityPool.totalLiquidity / pairToken.amount.value.toDouble
    )

    (
      primaryToken.copy(amount = incomingPrimaryAmount.toTokenAmountFormat.toPosLongUnsafe),
      pairToken.copy(amount = incomingPairAmount.toTokenAmountFormat.toPosLongUnsafe),
      liquidityMinted.toTokenAmountFormat
    )
  }

  private def updateLiquidityPool(
    liquidityPool: LiquidityPool,
    signerAddress: Address,
    primaryToken: TokenInformation,
    pairToken: TokenInformation,
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
      k = updatedTokenAAmount.value.fromTokenAmountFormat * updatedTokenBAmount.value.fromTokenAmountFormat,
      totalLiquidity = updatedTotalLiquidity,
      liquidityProviders = LiquidityProviders(updatedLiquidityProviders)
    )
  }

  private def generateSpendTransactions(
    allowSpendTokenA: Hashed[AllowSpend],
    allowSpendTokenB: Hashed[AllowSpend]
  ): SortedSet[SharedArtifact] = {
    val spendTransactionTokenA = generatePendingSpendTransaction(allowSpendTokenA)
    val spendTransactionTokenB = generatePendingSpendTransaction(allowSpendTokenB)
    SortedSet[SharedArtifact](
      spendTransactionTokenA,
      spendTransactionTokenB
    )
  }

  private def getConfirmedAndPendingStaking(
    acc: DataState[AmmOnChainState, AmmCalculatedState]
  ): (Map[Address, Set[StakingCalculatedStateAddress]], Map[Address, Set[Signed[StakingUpdate]]]) = {
    val stakingCalculatedState = getStakingCalculatedState(acc.calculated)
    (stakingCalculatedState.confirmed, stakingCalculatedState.pending)
  }

  def combineStaking[F[_]: Async: Hasher](
    acc: DataState[AmmOnChainState, AmmCalculatedState],
    signedStakingUpdate: Signed[StakingUpdate],
    signerAddress: Address,
    currentSnapshotOrdinal: SnapshotOrdinal
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
    val stakingUpdate = signedStakingUpdate.value

    val (confirmedStaking, pendingStaking) = getConfirmedAndPendingStaking(acc)
    val updates = stakingUpdate :: acc.onChain.updates
    (
      getAllowSpendLastSyncGlobalSnapshotState(stakingUpdate.tokenAAllowSpend),
      getAllowSpendLastSyncGlobalSnapshotState(stakingUpdate.tokenBAllowSpend)
    ).flatMapN {
      case (None, _) | (_, None) =>
        for {
          lastSyncHashedGIS <- getLastSyncGlobalIncrementalSnapshot
          gsEpochProgress = lastSyncHashedGIS.epochProgress
          updatedPendingStakings =
            if (signedStakingUpdate.maxValidGsEpochProgress < gsEpochProgress) {
              pendingStaking.removed(signerAddress)
            } else {
              pendingStaking.updatedWith(signerAddress) {
                case Some(existingUpdates) => Some(existingUpdates + signedStakingUpdate)
                case None                  => Some(Set(signedStakingUpdate))
              }
            }
          newStakingState = StakingCalculatedState(confirmedStaking, updatedPendingStakings)
          updatedCalculatedState = acc.calculated.operations.updated(OperationType.Staking, newStakingState)
        } yield
          DataState(
            AmmOnChainState(updates),
            AmmCalculatedState(updatedCalculatedState)
          )

      case (Some(_), Some(_)) =>
        val liquidityPoolsCalculatedState = getLiquidityPools(acc.calculated)

        val maybeLastStakingInfo = confirmedStaking.get(signerAddress) match {
          case Some(stakingState: StakingCalculatedStateAddress) =>
            StakingCalculatedStateLastReference(
              stakingState.tokenAAllowSpend,
              stakingState.tokenBAllowSpend,
              stakingState.tokenA,
              stakingState.tokenB,
              stakingState.ordinal
            ).some

          case _ => none
        }

        for {
          poolId <- buildLiquidityPoolUniqueIdentifier(stakingUpdate.tokenAId, stakingUpdate.tokenBId)
          liquidityPool <- getLiquidityPoolByPoolId(liquidityPoolsCalculatedState, poolId)
          (primaryToken, pairToken, liquidityMinted) = getUpdatedTokenInformation(stakingUpdate, liquidityPool)
          liquidityPoolUpdated = updateLiquidityPool(liquidityPool, signerAddress, primaryToken, pairToken, liquidityMinted)

          stakingCalculatedStateAddress = StakingCalculatedStateAddress(
            stakingUpdate.tokenAAllowSpend,
            stakingUpdate.tokenBAllowSpend,
            primaryToken,
            pairToken,
            currentSnapshotOrdinal,
            maybeLastStakingInfo
          )
          updatedStakingCalculatedState = StakingCalculatedState(
            confirmedStaking.updatedWith(signerAddress) {
              case Some(confirmedSwaps) => Some(confirmedSwaps + stakingCalculatedStateAddress)
              case None                 => Some(Set(stakingCalculatedStateAddress))
            },
            pendingStaking - signerAddress
          )

          updatedLiquidityPool = LiquidityPoolCalculatedState(liquidityPoolsCalculatedState.updated(poolId.value, liquidityPoolUpdated))

          updates: List[AmmUpdate] = stakingUpdate :: acc.onChain.updates
          updatedCalculatedState = acc.calculated.operations
            .updated(OperationType.Staking, updatedStakingCalculatedState)
            .updated(OperationType.LiquidityPool, updatedLiquidityPool)

          allowSpendTokenA <- getAllowSpendLastSyncGlobalSnapshotState(
            stakingUpdate.tokenAAllowSpend
          )

          allowSpendTokenB <- getAllowSpendLastSyncGlobalSnapshotState(
            stakingUpdate.tokenBAllowSpend
          )

          spendTransactions = generateSpendTransactions(allowSpendTokenA.get, allowSpendTokenB.get)

        } yield
          DataState(
            AmmOnChainState(updates),
            AmmCalculatedState(updatedCalculatedState),
            spendTransactions
          )
    }
  }
}
