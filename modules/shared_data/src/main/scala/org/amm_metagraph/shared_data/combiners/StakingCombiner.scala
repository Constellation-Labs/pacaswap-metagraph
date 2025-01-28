package org.amm_metagraph.shared_data.combiners

import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.SortedSet

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact._
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.swap.AllowSpend
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hashed, Hasher}

import eu.timepit.refined.types.numeric.NonNegLong
import monocle.syntax.all._
import org.amm_metagraph.shared_data.SpendTransactions.generateSpendAction
import org.amm_metagraph.shared_data.globalSnapshots.{getAllowSpendLastSyncGlobalSnapshotState, getLastSyncGlobalIncrementalSnapshot}
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.types.DataUpdates.StakingUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.Staking.{
  StakingCalculatedStateAddress,
  StakingCalculatedStateLastReference,
  getStakingCalculatedState
}
import org.amm_metagraph.shared_data.types.States._

object StakingCombiner {

  private case class UpdatedTokenInformation(
    primaryTokenInformation: TokenInformation,
    pairTokenInformation: TokenInformation,
    newlyIssuedShares: Long
  )

  private def getUpdatedTokenInformation(
    stakingUpdate: StakingUpdate,
    liquidityPool: LiquidityPool
  ): UpdatedTokenInformation = {
    val (primaryToken, pairToken) = if (stakingUpdate.tokenAId == liquidityPool.tokenA.identifier) {
      (liquidityPool.tokenA, liquidityPool.tokenB)
    } else {
      (liquidityPool.tokenB, liquidityPool.tokenA)
    }

    val currentPrimaryTokenAmount = primaryToken.amount.value.fromTokenAmountFormat
    val currentPairTokenAmount = pairToken.amount.value.fromTokenAmountFormat

    val incomingPrimaryAmount = stakingUpdate.tokenAAmount.value
    val incomingPairAmount = (incomingPrimaryAmount * currentPairTokenAmount) / currentPrimaryTokenAmount // Maintain invariant

    val relativeDepositIncrease = incomingPrimaryAmount.toDouble / (currentPrimaryTokenAmount + incomingPrimaryAmount)
    val newlyIssuedShares = relativeDepositIncrease * liquidityPool.poolShares.totalShares.value.fromTokenAmountFormat

    UpdatedTokenInformation(
      primaryToken.copy(amount = incomingPrimaryAmount.toTokenAmountFormat.toPosLongUnsafe),
      pairToken.copy(amount = incomingPairAmount.toTokenAmountFormat.toPosLongUnsafe),
      newlyIssuedShares.toTokenAmountFormat
    )
  }

  private def updateLiquidityPool(
    liquidityPool: LiquidityPool,
    signerAddress: Address,
    updatedTokenInformation: UpdatedTokenInformation
  ): LiquidityPool = {
    val primaryToken = updatedTokenInformation.primaryTokenInformation
    val pairToken = updatedTokenInformation.pairTokenInformation
    val newlyIssuedShares = updatedTokenInformation.newlyIssuedShares

    val tokenA = if (liquidityPool.tokenA.identifier == primaryToken.identifier) primaryToken else pairToken
    val tokenB = if (liquidityPool.tokenB.identifier == pairToken.identifier) pairToken else primaryToken

    val updatedTokenAAmount = (liquidityPool.tokenA.amount.value + tokenA.amount.value).toPosLongUnsafe
    val updatedTokenBAmount = (liquidityPool.tokenB.amount.value + tokenB.amount.value).toPosLongUnsafe

    val addressSharesAmount = liquidityPool.poolShares.addressShares.getOrElse(signerAddress, ShareAmount(Amount.empty))
    val updatedAddressSharesAmount = ShareAmount(Amount(NonNegLong.unsafeFrom(addressSharesAmount.value.value.value + newlyIssuedShares)))
    val updatedAddressShares = liquidityPool.poolShares.addressShares.updated(signerAddress, updatedAddressSharesAmount)

    liquidityPool.copy(
      tokenA = tokenA.copy(amount = updatedTokenAAmount),
      tokenB = tokenB.copy(amount = updatedTokenBAmount),
      k = BigInt(updatedTokenAAmount.value) * BigInt(updatedTokenBAmount.value),
      poolShares = PoolShares(
        (liquidityPool.poolShares.totalShares.value + newlyIssuedShares).toPosLongUnsafe,
        updatedAddressShares
      )
    )
  }

  private def generateSpendTransactions(
    allowSpendTokenA: Hashed[AllowSpend],
    allowSpendTokenB: Hashed[AllowSpend]
  ): SortedSet[SharedArtifact] = {
    val spendTransactionTokenA = generateSpendAction(allowSpendTokenA)
    val spendTransactionTokenB = generateSpendAction(allowSpendTokenB)
    SortedSet[SharedArtifact](
      spendTransactionTokenA,
      spendTransactionTokenB
    )
  }

  def combineStaking[F[_]: Async: Hasher](
    acc: DataState[AmmOnChainState, AmmCalculatedState],
    signedStakingUpdate: Signed[StakingUpdate],
    signerAddress: Address,
    currentSnapshotOrdinal: SnapshotOrdinal
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
    val stakingUpdate = signedStakingUpdate.value

    val confirmedStaking = getStakingCalculatedState(acc.calculated).confirmed

    val updates = stakingUpdate :: acc.onChain.updates

    (
      getAllowSpendLastSyncGlobalSnapshotState(stakingUpdate.tokenAAllowSpend),
      getAllowSpendLastSyncGlobalSnapshotState(stakingUpdate.tokenBAllowSpend)
    ).flatMapN {
      case (None, _) | (_, None) =>
        for {
          lastSyncHashedGIS <- getLastSyncGlobalIncrementalSnapshot
          gsEpochProgress = lastSyncHashedGIS.epochProgress
          updatedPendingCalculatedState =
            if (stakingUpdate.maxValidGsEpochProgress < gsEpochProgress) {
              acc.calculated.pendingUpdates - signedStakingUpdate
            } else if (!acc.calculated.pendingUpdates.contains(signedStakingUpdate)) {
              acc.calculated.pendingUpdates + signedStakingUpdate
            } else {
              acc.calculated.pendingUpdates
            }

          newStakingState = StakingCalculatedState(confirmedStaking)
          updatedCalculatedState = acc.calculated
            .focus(_.confirmedOperations)
            .modify(_.updated(OperationType.Staking, newStakingState))
            .focus(_.pendingUpdates)
            .replace(updatedPendingCalculatedState)

        } yield
          DataState(
            AmmOnChainState(updates),
            updatedCalculatedState
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
          updatedTokenInformation = getUpdatedTokenInformation(stakingUpdate, liquidityPool)
          liquidityPoolUpdated = updateLiquidityPool(liquidityPool, signerAddress, updatedTokenInformation)

          stakingCalculatedStateAddress = StakingCalculatedStateAddress(
            stakingUpdate.tokenAAllowSpend,
            stakingUpdate.tokenBAllowSpend,
            updatedTokenInformation.primaryTokenInformation,
            updatedTokenInformation.pairTokenInformation,
            currentSnapshotOrdinal,
            maybeLastStakingInfo
          )

          updatedPendingCalculatedState = acc.calculated.pendingUpdates - signedStakingUpdate
          updatedStakingCalculatedState = StakingCalculatedState(
            confirmedStaking.updatedWith(signerAddress) {
              case Some(confirmedSwaps) => Some(confirmedSwaps + stakingCalculatedStateAddress)
              case None                 => Some(Set(stakingCalculatedStateAddress))
            }
          )

          updatedLiquidityPool = LiquidityPoolCalculatedState(liquidityPoolsCalculatedState.updated(poolId.value, liquidityPoolUpdated))

          updatedCalculatedState = acc.calculated
            .focus(_.confirmedOperations)
            .modify(_.updated(OperationType.Staking, updatedStakingCalculatedState))
            .focus(_.confirmedOperations)
            .modify(_.updated(OperationType.LiquidityPool, updatedLiquidityPool))
            .focus(_.pendingUpdates)
            .replace(updatedPendingCalculatedState)

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
            updatedCalculatedState,
            spendTransactions
          )
    }
  }
}
