package org.amm_metagraph.shared_data.combiners

import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.SortedSet

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact.SharedArtifact
import io.constellationnetwork.schema.swap.AllowSpend
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hashed, Hasher}

import monocle.syntax.all._
import org.amm_metagraph.shared_data.SpendTransactions.generateSpendAction
import org.amm_metagraph.shared_data.globalSnapshots.{getAllowSpendLastSyncGlobalSnapshotState, getLastSyncGlobalIncrementalSnapshot}
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.types.DataUpdates.LiquidityPoolUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._

object LiquidityPoolCombiner {
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

  def combineLiquidityPool[F[_]: Async: Hasher](
    acc: DataState[AmmOnChainState, AmmCalculatedState],
    signedLiquidityPoolUpdate: Signed[LiquidityPoolUpdate],
    signerAddress: Address
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
    val liquidityPoolUpdate = signedLiquidityPoolUpdate.value
    val liquidityPools = getLiquidityPools(acc.calculated)
    val existentLiquidityPools = getLiquidityPoolCalculatedState(acc.calculated).liquidityPools

    val updates = liquidityPoolUpdate :: acc.onChain.updates
    (
      getAllowSpendLastSyncGlobalSnapshotState(liquidityPoolUpdate.tokenAAllowSpend),
      getAllowSpendLastSyncGlobalSnapshotState(liquidityPoolUpdate.tokenBAllowSpend)
    ).flatMapN {
      case (None, _) | (_, None) =>
        for {
          lastSyncHashedGIS <- getLastSyncGlobalIncrementalSnapshot
          gsEpochProgress = lastSyncHashedGIS.epochProgress
          updatedPendingCalculatedState =
            if (liquidityPoolUpdate.maxValidGsEpochProgress < gsEpochProgress) {
              acc.calculated.pendingUpdates - signedLiquidityPoolUpdate
            } else if (!acc.calculated.pendingUpdates.contains(signedLiquidityPoolUpdate)) {
              acc.calculated.pendingUpdates + signedLiquidityPoolUpdate
            } else {
              acc.calculated.pendingUpdates
            }

          newLiquidityPoolState = LiquidityPoolCalculatedState(existentLiquidityPools)
          updatedCalculatedState = acc.calculated
            .focus(_.confirmedOperations)
            .modify(_.updated(OperationType.LiquidityPool, newLiquidityPoolState))
            .focus(_.pendingUpdates)
            .replace(updatedPendingCalculatedState)

        } yield
          DataState(
            AmmOnChainState(updates),
            updatedCalculatedState
          )

      case (Some(_), Some(_)) =>
        for {
          poolId <- buildLiquidityPoolUniqueIdentifier(liquidityPoolUpdate.tokenAId, liquidityPoolUpdate.tokenBId)
          amountA = liquidityPoolUpdate.tokenAAmount.value
          amountB = liquidityPoolUpdate.tokenBAmount.value
          poolTotalLiquidity = math.sqrt(amountA.toDouble * amountB.toDouble).toTokenAmountFormat

          liquidityPool = LiquidityPool(
            poolId,
            TokenInformation(
              liquidityPoolUpdate.tokenAId,
              liquidityPoolUpdate.tokenAAmount.value.toTokenAmountFormat.toPosLongUnsafe
            ),
            TokenInformation(
              liquidityPoolUpdate.tokenBId,
              liquidityPoolUpdate.tokenBAmount.value.toTokenAmountFormat.toPosLongUnsafe
            ),
            signerAddress,
            (amountA * amountB).toDouble,
            poolTotalLiquidity,
            LiquidityProviders(Map(signerAddress -> poolTotalLiquidity))
          )

          updatedPendingCalculatedState = acc.calculated.pendingUpdates - signedLiquidityPoolUpdate
          updatedLiquidityPoolCalculatedState = LiquidityPoolCalculatedState(liquidityPools.updated(poolId.value, liquidityPool))
          updatedCalculatedState = acc.calculated
            .focus(_.confirmedOperations)
            .modify(_.updated(OperationType.LiquidityPool, updatedLiquidityPoolCalculatedState))
            .focus(_.pendingUpdates)
            .replace(updatedPendingCalculatedState)

          allowSpendTokenA <- getAllowSpendLastSyncGlobalSnapshotState(
            liquidityPoolUpdate.tokenAAllowSpend
          )

          allowSpendTokenB <- getAllowSpendLastSyncGlobalSnapshotState(
            liquidityPoolUpdate.tokenBAllowSpend
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
