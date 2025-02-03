package org.amm_metagraph.shared_data.combiners

import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.SortedSet

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact._
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.AllowSpend
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hashed, Hasher}

import eu.timepit.refined.types.numeric.NonNegLong
import monocle.syntax.all._
import org.amm_metagraph.shared_data.SpendTransactions.generateSpendAction
import org.amm_metagraph.shared_data.app.ApplicationConfig
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

    val currentPrimaryTokenAmount = primaryToken.amount.value
    val currentPairTokenAmount = pairToken.amount.value

    val incomingPrimaryAmount = stakingUpdate.tokenAAmount.value
    val incomingPairAmount =
      (BigDecimal(incomingPrimaryAmount) * BigDecimal(currentPairTokenAmount)) / BigDecimal(currentPrimaryTokenAmount) // Maintain invariant

    val relativeDepositIncrease = incomingPrimaryAmount.toDouble / (currentPrimaryTokenAmount + incomingPrimaryAmount)
    val newlyIssuedShares = relativeDepositIncrease * liquidityPool.poolShares.totalShares.value

    UpdatedTokenInformation(
      primaryToken.copy(amount = incomingPrimaryAmount.toPosLongUnsafe),
      pairToken.copy(amount = incomingPairAmount.toLong.toPosLongUnsafe),
      newlyIssuedShares.toLong
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
      k = updatedTokenAAmount.value.fromTokenAmountFormat * updatedTokenBAmount.value.fromTokenAmountFormat,
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
    applicationConfig: ApplicationConfig,
    acc: DataState[AmmOnChainState, AmmCalculatedState],
    signedStakingUpdate: Signed[StakingUpdate],
    signerAddress: Address,
    currentSnapshotOrdinal: SnapshotOrdinal,
    lastSyncGlobalEpochProgress: EpochProgress
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
    val stakingUpdate = signedStakingUpdate.value
    val stakingCalculatedState = getStakingCalculatedState(acc.calculated)
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
              stakingCalculatedState.pending - signedStakingUpdate
            } else if (!stakingCalculatedState.pending.contains(signedStakingUpdate)) {
              stakingCalculatedState.pending + signedStakingUpdate
            } else {
              stakingCalculatedState.pending
            }

          newStakingState = stakingCalculatedState
            .focus(_.pending)
            .replace(updatedPendingCalculatedState)

          updatedCalculatedState = acc.calculated
            .focus(_.operations)
            .modify(_.updated(OperationType.Staking, newStakingState))

        } yield
          DataState(
            AmmOnChainState(updates),
            updatedCalculatedState
          )

      case (Some(_), Some(_)) =>
        val liquidityPoolsCalculatedState = getLiquidityPoolCalculatedState(acc.calculated)

        val maybeLastStakingInfo = stakingCalculatedState.confirmed.value.get(signerAddress) match {
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
          allowSpendTokenA <- getAllowSpendLastSyncGlobalSnapshotState(
            stakingUpdate.tokenAAllowSpend
          )

          allowSpendTokenB <- getAllowSpendLastSyncGlobalSnapshotState(
            stakingUpdate.tokenBAllowSpend
          )

          poolId <- buildLiquidityPoolUniqueIdentifier(stakingUpdate.tokenAId, stakingUpdate.tokenBId)
          liquidityPool <- getLiquidityPoolByPoolId(liquidityPoolsCalculatedState.confirmed.value, poolId)
          updatedTokenInformation = getUpdatedTokenInformation(stakingUpdate, liquidityPool)
          maybeFailedUpdate = validateUpdate(
            applicationConfig,
            signedStakingUpdate,
            updatedTokenInformation,
            allowSpendTokenA.get,
            allowSpendTokenB.get,
            lastSyncGlobalEpochProgress
          )
          response = maybeFailedUpdate match {
            case Some(failedCalculatedState) =>
              val updatedStakingCalculatedState = stakingCalculatedState
                .focus(_.failed)
                .modify(_ + failedCalculatedState)
              val updatedCalculatedState = acc.calculated
                .focus(_.operations)
                .modify(_.updated(OperationType.Staking, updatedStakingCalculatedState))

              DataState(
                AmmOnChainState(updates),
                updatedCalculatedState
              )

            case None =>
              val liquidityPoolUpdated = updateLiquidityPool(liquidityPool, signerAddress, updatedTokenInformation)
              val stakingCalculatedStateAddress =
                StakingCalculatedStateAddress(
                  stakingUpdate.tokenAAllowSpend,
                  stakingUpdate.tokenBAllowSpend,
                  updatedTokenInformation.primaryTokenInformation,
                  updatedTokenInformation.pairTokenInformation,
                  currentSnapshotOrdinal,
                  maybeLastStakingInfo
                )

              val updatedPendingCalculatedState = stakingCalculatedState.pending - signedStakingUpdate

              val updatedStakingCalculatedState =
                stakingCalculatedState
                  .focus(_.confirmed.value)
                  .modify(current =>
                    current.updatedWith(signerAddress) {
                      case Some(confirmedSwaps) => Some(confirmedSwaps + stakingCalculatedStateAddress)
                      case None                 => Some(Set(stakingCalculatedStateAddress))
                    }
                  )
                  .focus(_.pending)
                  .replace(updatedPendingCalculatedState)

              val updatedLiquidityPool = liquidityPoolsCalculatedState
                .focus(_.confirmed.value)
                .modify(_.updated(poolId.value, liquidityPoolUpdated))

              val updatedCalculatedState = acc.calculated
                .focus(_.operations)
                .modify(_.updated(OperationType.Staking, updatedStakingCalculatedState))
                .focus(_.operations)
                .modify(_.updated(OperationType.LiquidityPool, updatedLiquidityPool))

              val spendTransactions = generateSpendTransactions(allowSpendTokenA.get, allowSpendTokenB.get)

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
    signedUpdate: Signed[StakingUpdate],
    tokenInformation: UpdatedTokenInformation,
    allowSpendTokenA: Hashed[AllowSpend],
    allowSpendTokenB: Hashed[AllowSpend],
    lastSyncGlobalEpochProgress: EpochProgress
  ): Option[FailedCalculatedState] = {
    val (tokenA, tokenB) = if (tokenInformation.primaryTokenInformation.identifier == allowSpendTokenA.currency) {
      (tokenInformation.primaryTokenInformation, tokenInformation.pairTokenInformation)
    } else {
      (tokenInformation.pairTokenInformation, tokenInformation.primaryTokenInformation)
    }

    if (tokenA.amount.value > allowSpendTokenA.amount.value.value) {
      FailedCalculatedState(
        AmountGreaterThanAllowSpendLimit(allowSpendTokenA.signed.value),
        EpochProgress(
          NonNegLong.unsafeFrom(
            lastSyncGlobalEpochProgress.value.value + applicationConfig.failedOperationsExpirationEpochProgresses.value.value
          )
        ),
        signedUpdate
      ).some
    } else if (tokenB.amount.value > allowSpendTokenB.amount.value.value) {
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
