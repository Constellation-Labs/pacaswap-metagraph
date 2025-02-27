package org.amm_metagraph.shared_data.combiners

import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.SortedSet

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact.SharedArtifact
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.AllowSpend
import io.constellationnetwork.security.Hashed
import io.constellationnetwork.security.signature.Signed

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.PosLong
import eu.timepit.refined.types.numeric.NonNegLong
import monocle.syntax.all._
import org.amm_metagraph.shared_data.SpendTransactions.generateSpendAction
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.globalSnapshots.{getAllowSpendsLastSyncGlobalSnapshotState, getLastSyncGlobalIncrementalSnapshot}
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.types.DataUpdates.LiquidityPoolUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.codecs.HasherSelector
import org.typelevel.log4cats.slf4j.Slf4jLogger

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

  def combineLiquidityPool[F[_]: Async: HasherSelector](
    applicationConfig: ApplicationConfig,
    acc: DataState[AmmOnChainState, AmmCalculatedState],
    signedLiquidityPoolUpdate: Signed[LiquidityPoolUpdate],
    signerAddress: Address,
    lastSyncGlobalEpochProgress: EpochProgress
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
    val liquidityPoolUpdate = signedLiquidityPoolUpdate.value
    val liquidityPoolCalculatedState = getLiquidityPoolCalculatedState(acc.calculated)

    val updates = liquidityPoolUpdate :: acc.onChain.updates
    HasherSelector[F].withBrotli { implicit hs =>
      getAllowSpendsLastSyncGlobalSnapshotState(
        liquidityPoolUpdate.tokenAAllowSpend,
        liquidityPoolUpdate.tokenAId,
        liquidityPoolUpdate.tokenBAllowSpend,
        liquidityPoolUpdate.tokenBId
      )
    }.flatMap {
      case (None, _) | (_, None) =>
        for {
          lastSyncHashedGIS <- getLastSyncGlobalIncrementalSnapshot
          gsEpochProgress = lastSyncHashedGIS.epochProgress

          updatedLiquidityPoolPending =
            if (liquidityPoolUpdate.maxValidGsEpochProgress < gsEpochProgress) {
              liquidityPoolCalculatedState.pending - signedLiquidityPoolUpdate
            } else if (!liquidityPoolCalculatedState.pending.contains(signedLiquidityPoolUpdate)) {
              liquidityPoolCalculatedState.pending + signedLiquidityPoolUpdate
            } else {
              liquidityPoolCalculatedState.pending
            }

          newLiquidityPoolState = liquidityPoolCalculatedState
            .focus(_.pending)
            .replace(updatedLiquidityPoolPending)

          updatedCalculatedState = acc.calculated
            .focus(_.operations)
            .modify(_.updated(OperationType.LiquidityPool, newLiquidityPoolState))

        } yield
          DataState(
            AmmOnChainState(updates),
            updatedCalculatedState
          )

      case (Some(allowSpendTokenA), Some(allowSpendTokenB)) =>
        for {
          poolId <- buildLiquidityPoolUniqueIdentifier(liquidityPoolUpdate.tokenAId, liquidityPoolUpdate.tokenBId)

          maybeFailedUpdate = validateUpdate(
            applicationConfig,
            signedLiquidityPoolUpdate,
            allowSpendTokenA,
            allowSpendTokenB,
            lastSyncGlobalEpochProgress
          )

          response = maybeFailedUpdate match {
            case Some(failedCalculatedState) =>
              val updatedLiquidityPoolCalculatedState = liquidityPoolCalculatedState
                .focus(_.failed)
                .modify(_ + failedCalculatedState)
              val updatedCalculatedState = acc.calculated
                .focus(_.operations)
                .modify(_.updated(OperationType.LiquidityPool, updatedLiquidityPoolCalculatedState))

              DataState(
                AmmOnChainState(updates),
                updatedCalculatedState
              )
            case None =>
              val amountA = liquidityPoolUpdate.tokenAAmount.value
              val amountB = liquidityPoolUpdate.tokenBAmount.value
              val poolTotalShares: PosLong = 1.toTokenAmountFormat.toPosLongUnsafe

              val liquidityPool = LiquidityPool(
                poolId,
                TokenInformation(
                  liquidityPoolUpdate.tokenAId,
                  liquidityPoolUpdate.tokenAAmount
                ),
                TokenInformation(
                  liquidityPoolUpdate.tokenBId,
                  liquidityPoolUpdate.tokenBAmount
                ),
                signerAddress,
                BigInt(amountA) * BigInt(amountB),
                PoolShares(
                  poolTotalShares,
                  Map(signerAddress -> ShareAmount(Amount(poolTotalShares)))
                )
              )

              val updatedPendingCalculatedState = liquidityPoolCalculatedState.pending - signedLiquidityPoolUpdate
              val updatedLiquidityPoolCalculatedState = liquidityPoolCalculatedState
                .focus(_.confirmed.value)
                .modify(liquidityPools => liquidityPools.updated(poolId.value, liquidityPool))
                .focus(_.pending)
                .replace(updatedPendingCalculatedState)

              val updatedCalculatedState = acc.calculated
                .focus(_.operations)
                .modify(_.updated(OperationType.LiquidityPool, updatedLiquidityPoolCalculatedState))
              val spendTransactions = generateSpendTransactions(allowSpendTokenA, allowSpendTokenB)

              DataState(
                AmmOnChainState(updates),
                updatedCalculatedState,
                spendTransactions
              )
          }
        } yield response
    }
  }

  def validateUpdate(
    applicationConfig: ApplicationConfig,
    signedUpdate: Signed[LiquidityPoolUpdate],
    allowSpendTokenA: Hashed[AllowSpend],
    allowSpendTokenB: Hashed[AllowSpend],
    lastSyncGlobalEpochProgress: EpochProgress
  ): Option[FailedCalculatedState] = {
    val update = signedUpdate.value
    if (update.tokenAAmount > allowSpendTokenA.amount.value.value) {
      FailedCalculatedState(
        AmountGreaterThanAllowSpendLimit(allowSpendTokenA.signed.value),
        EpochProgress(
          NonNegLong.unsafeFrom(
            lastSyncGlobalEpochProgress.value.value + applicationConfig.failedOperationsExpirationEpochProgresses.value.value
          )
        ),
        signedUpdate
      ).some
    } else if (update.tokenBAmount > allowSpendTokenB.amount.value.value) {
      FailedCalculatedState(
        AmountGreaterThanAllowSpendLimit(allowSpendTokenB.signed.value),
        EpochProgress(
          NonNegLong.unsafeFrom(
            lastSyncGlobalEpochProgress.value.value + applicationConfig.failedOperationsExpirationEpochProgresses.value.value
          )
        ),
        signedUpdate
      ).some
    } else if (allowSpendTokenA.lastValidEpochProgress.value.value < lastSyncGlobalEpochProgress.value.value) {
      FailedCalculatedState(
        AllowSpendExpired(allowSpendTokenA.signed.value),
        EpochProgress(NonNegLong.unsafeFrom(lastSyncGlobalEpochProgress.value.value + 30L)),
        signedUpdate
      ).some
    } else if (allowSpendTokenB.lastValidEpochProgress.value.value < lastSyncGlobalEpochProgress.value.value) {
      FailedCalculatedState(
        AllowSpendExpired(allowSpendTokenB.signed.value),
        EpochProgress(NonNegLong.unsafeFrom(lastSyncGlobalEpochProgress.value.value + 30L)),
        signedUpdate
      ).some
    } else {
      None
    }
  }
}
