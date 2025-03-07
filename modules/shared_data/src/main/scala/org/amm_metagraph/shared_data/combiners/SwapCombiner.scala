package org.amm_metagraph.shared_data.combiners

import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}
import scala.math.BigDecimal.RoundingMode

import io.constellationnetwork.currency.dataApplication.DataState
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact.{SharedArtifact, SpendAction}
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.{AllowSpend, SwapAmount}
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hashed, Hasher}

import eu.timepit.refined.types.numeric.NonNegLong
import monocle.syntax.all._
import org.amm_metagraph.shared_data.SpendTransactions.{checkIfSpendActionAcceptedInGl0, generateSpendAction}
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.globalSnapshots.getAllowSpendGlobalSnapshotsState
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.Swap._
import org.amm_metagraph.shared_data.types.codecs.HasherSelector

object SwapCombiner {
  private case class UpdatedTokenInformation(
    primaryTokenInformation: TokenInformation,
    pairTokenInformation: TokenInformation,
    receivedAmount: Amount,
    effectivePrice: Amount
  )

  private def getUpdatedTokenInformation(
    swapUpdate: SwapUpdate,
    liquidityPool: LiquidityPool
  ): UpdatedTokenInformation = {

    val fromTokenInfo = if (swapUpdate.swapFromPair == liquidityPool.tokenA.identifier) liquidityPool.tokenA else liquidityPool.tokenB
    val toTokenInfo = if (swapUpdate.swapToPair == liquidityPool.tokenA.identifier) liquidityPool.tokenA else liquidityPool.tokenB

    val swapAmount = swapUpdate.maxAmount.value.value
    val newFromTokenReserve = BigInt(fromTokenInfo.amount.value) + BigInt(swapAmount)
    val newToTokenReserve = liquidityPool.k / newFromTokenReserve
    val receivedAmount = toTokenInfo.amount.value - newToTokenReserve
    val effectivePrice = ((BigDecimal(receivedAmount) / BigDecimal(swapAmount)) * BigDecimal(1e8)).setScale(0, RoundingMode.HALF_UP).toLong

    UpdatedTokenInformation(
      fromTokenInfo.copy(amount = newFromTokenReserve.toLong.toPosLongUnsafe),
      toTokenInfo.copy(amount = newToTokenReserve.toLong.toPosLongUnsafe),
      Amount(NonNegLong.unsafeFrom(receivedAmount.toLong)),
      Amount(NonNegLong.unsafeFrom(effectivePrice))
    )
  }

  private def updateLiquidityPool(
    liquidityPool: LiquidityPool,
    fromTokenInfo: TokenInformation,
    toTokenInfo: TokenInformation
  ): LiquidityPool = {
    val tokenA = if (liquidityPool.tokenA.identifier == fromTokenInfo.identifier) fromTokenInfo else toTokenInfo
    val tokenB = if (liquidityPool.tokenB.identifier == toTokenInfo.identifier) toTokenInfo else fromTokenInfo

    liquidityPool.copy(
      tokenA = tokenA,
      tokenB = tokenB
    )
  }

  private def validateUpdate(
    applicationConfig: ApplicationConfig,
    signedUpdate: Signed[SwapUpdate],
    maybeTokenInformation: Option[UpdatedTokenInformation],
    maybeAllowSpendToken: Option[Hashed[AllowSpend]],
    lastSyncGlobalEpochProgress: EpochProgress
  ): Option[FailedCalculatedState] =
    if (signedUpdate.maxValidGsEpochProgress < lastSyncGlobalEpochProgress) {
      FailedCalculatedState(
        OperationExpired(signedUpdate),
        EpochProgress(NonNegLong.unsafeFrom(lastSyncGlobalEpochProgress.value.value + 30L)),
        signedUpdate
      ).some
    } else {
      (maybeTokenInformation, maybeAllowSpendToken).mapN { (updatedTokenInformation, allowSpend) =>
        if (updatedTokenInformation.receivedAmount.value.value < signedUpdate.minAmount.value.value) {
          FailedCalculatedState(
            SwapLessThanMinAmount(),
            EpochProgress(
              NonNegLong.unsafeFrom(
                lastSyncGlobalEpochProgress.value.value + applicationConfig.failedOperationsExpirationEpochProgresses.value.value
              )
            ),
            signedUpdate
          ).some
        } else if (signedUpdate.minPrice.exists(p => updatedTokenInformation.effectivePrice.value.value < p.value)) {
          FailedCalculatedState(
            SwapPriceBelowAcceptableMinPrice(),
            EpochProgress(
              NonNegLong.unsafeFrom(
                lastSyncGlobalEpochProgress.value.value + applicationConfig.failedOperationsExpirationEpochProgresses.value.value
              )
            ),
            signedUpdate
          ).some
        } else if (signedUpdate.maxPrice.exists(p => updatedTokenInformation.effectivePrice.value.value > p.value)) {
          FailedCalculatedState(
            SwapPriceExceedsAcceptableMaxPrice(),
            EpochProgress(
              NonNegLong.unsafeFrom(
                lastSyncGlobalEpochProgress.value.value + applicationConfig.failedOperationsExpirationEpochProgresses.value.value
              )
            ),
            signedUpdate
          ).some
        } else if (allowSpend.lastValidEpochProgress.value.value < lastSyncGlobalEpochProgress.value.value) {
          FailedCalculatedState(
            AllowSpendExpired(allowSpend.signed.value),
            EpochProgress(NonNegLong.unsafeFrom(lastSyncGlobalEpochProgress.value.value + 30L)),
            signedUpdate
          ).some
        } else {
          None
        }
      }.flatten
    }

  private def handleFailedUpdate(
    updates: List[AmmUpdate],
    acc: DataState[AmmOnChainState, AmmCalculatedState],
    failedCalculatedState: FailedCalculatedState,
    swapCalculatedState: SwapCalculatedState
  ) = {
    val updatedSwapCalculatedState = swapCalculatedState
      .focus(_.failed)
      .modify(_ + failedCalculatedState)

    val updatedCalculatedState = acc.calculated
      .focus(_.operations)
      .modify(_.updated(OperationType.Swap, updatedSwapCalculatedState))

    DataState(
      AmmOnChainState(updates),
      updatedCalculatedState
    )
  }

  private def getUpdateAllowSpend[F[_]: Async: HasherSelector](
    swapUpdate: SwapUpdate,
    lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]]
  ) =
    HasherSelector[F].withBrotli { implicit hs =>
      getAllowSpendGlobalSnapshotsState(
        swapUpdate.allowSpendReference,
        swapUpdate.swapFromPair,
        lastGlobalSnapshotsAllowSpends
      )
    }

  private def removePendingAllowSpend(
    swapCalculatedState: SwapCalculatedState,
    signedSwapUpdate: Signed[SwapUpdate]
  ) =
    swapCalculatedState.pending.filterNot {
      case PendingAllowSpend(update) if update == signedSwapUpdate => true
      case _                                                       => false
    }

  private def removePendingSpendAction(
    swapCalculatedState: SwapCalculatedState,
    signedSwapUpdate: Signed[SwapUpdate]
  ) =
    swapCalculatedState.pending.filterNot {
      case PendingSpendAction(update, _) if update == signedSwapUpdate => true
      case _                                                           => false
    }

  def combinePendingSpendActionSwap[F[_]: Async: Hasher](
    applicationConfig: ApplicationConfig,
    acc: DataState[AmmOnChainState, AmmCalculatedState],
    pendingSwapUpdate: PendingSpendAction[SwapUpdate],
    currentSnapshotOrdinal: SnapshotOrdinal,
    lastSyncGlobalEpochProgress: EpochProgress,
    spendActions: List[SpendAction]
  ): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
    val liquidityPoolsCalculatedState = getLiquidityPoolCalculatedState(acc.calculated)
    val swapCalculatedState = getSwapCalculatedState(acc.calculated)

    val signedSwapUpdate = pendingSwapUpdate.update
    val swapUpdate = pendingSwapUpdate.update.value
    val updates = swapUpdate :: acc.onChain.updates
    validateUpdate(applicationConfig, signedSwapUpdate, none, none, lastSyncGlobalEpochProgress) match {
      case Some(value) => handleFailedUpdate(updates, acc, value, swapCalculatedState).pure
      case None =>
        for {
          metagraphGeneratedSpendActionHash <- Hasher[F].hash(pendingSwapUpdate.generatedSpendAction)
          globalSnapshotsHashes <- spendActions.traverse(action => Hasher[F].hash(action))
          allSpendActionsAccepted = checkIfSpendActionAcceptedInGl0(metagraphGeneratedSpendActionHash, globalSnapshotsHashes)
          updatedState <-
            if (!allSpendActionsAccepted) {
              acc.pure
            } else {
              for {
                poolId <- buildLiquidityPoolUniqueIdentifier(swapUpdate.swapFromPair, swapUpdate.swapToPair)
                liquidityPool <- getLiquidityPoolByPoolId(liquidityPoolsCalculatedState.confirmed.value, poolId)
                updatedTokenInformation = getUpdatedTokenInformation(swapUpdate, liquidityPool)

                liquidityPoolUpdated = updateLiquidityPool(
                  liquidityPool,
                  updatedTokenInformation.primaryTokenInformation,
                  updatedTokenInformation.pairTokenInformation
                )

                swapCalculatedStateAddress = SwapCalculatedStateAddress(
                  swapUpdate.sourceAddress,
                  updatedTokenInformation.primaryTokenInformation,
                  updatedTokenInformation.pairTokenInformation,
                  swapUpdate.allowSpendReference,
                  swapUpdate.minAmount,
                  swapUpdate.maxAmount,
                  swapUpdate.maxValidGsEpochProgress,
                  poolId.some,
                  swapUpdate.minPrice,
                  swapUpdate.maxPrice,
                  currentSnapshotOrdinal
                )

                updatedPendingCalculatedState = removePendingSpendAction(swapCalculatedState, signedSwapUpdate)
                newSwapState = swapCalculatedState
                  .focus(_.confirmed.value)
                  .modify(current =>
                    current.updatedWith(swapUpdate.sourceAddress) {
                      case Some(confirmedSwaps) => Some(confirmedSwaps + swapCalculatedStateAddress)
                      case None                 => Some(Set(swapCalculatedStateAddress))
                    }
                  )
                  .focus(_.pending)
                  .replace(updatedPendingCalculatedState)

                newLiquidityPoolState =
                  liquidityPoolsCalculatedState
                    .focus(_.confirmed.value)
                    .modify(_.updated(poolId.value, liquidityPoolUpdated))

                updatedCalculatedState = acc.calculated
                  .focus(_.operations)
                  .modify(_.updated(OperationType.Swap, newSwapState))
                  .focus(_.operations)
                  .modify(_.updated(OperationType.LiquidityPool, newLiquidityPoolState))

                response = DataState(
                  AmmOnChainState(updates),
                  updatedCalculatedState
                )

              } yield response
            }
        } yield updatedState
    }
  }

  def combinePendingAllowSpendSwap[F[_]: Async: HasherSelector](
    applicationConfig: ApplicationConfig,
    acc: DataState[AmmOnChainState, AmmCalculatedState],
    signedSwapUpdate: Signed[SwapUpdate],
    lastSyncGlobalEpochProgress: EpochProgress,
    lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]]
  ): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
    val swapCalculatedState = getSwapCalculatedState(acc.calculated)
    val liquidityPoolsCalculatedState = getLiquidityPoolCalculatedState(acc.calculated)

    val swapUpdate = signedSwapUpdate.value
    val updates = swapUpdate :: acc.onChain.updates
    validateUpdate(applicationConfig, signedSwapUpdate, none, none, lastSyncGlobalEpochProgress) match {
      case Some(failedCalculatedState) => handleFailedUpdate(updates, acc, failedCalculatedState, swapCalculatedState).pure
      case None =>
        getUpdateAllowSpend(swapUpdate, lastGlobalSnapshotsAllowSpends).flatMap {
          case Some(allowSpendToken) =>
            for {
              poolId <- buildLiquidityPoolUniqueIdentifier(swapUpdate.swapFromPair, swapUpdate.swapToPair)
              liquidityPool <- getLiquidityPoolByPoolId(liquidityPoolsCalculatedState.confirmed.value, poolId)
              updatedTokenInformation = getUpdatedTokenInformation(swapUpdate, liquidityPool)

              maybeFailedUpdate = validateUpdate(
                applicationConfig,
                signedSwapUpdate,
                updatedTokenInformation.some,
                allowSpendToken.some,
                lastSyncGlobalEpochProgress
              )

              response = maybeFailedUpdate match {
                case Some(failedCalculatedState) => handleFailedUpdate(updates, acc, failedCalculatedState, swapCalculatedState)
                case None =>
                  val spendActionToken = generateSpendAction(
                    allowSpendToken,
                    updatedTokenInformation.pairTokenInformation.identifier,
                    updatedTokenInformation.receivedAmount
                  )

                  val updatedPendingAllowSpendCalculatedState =
                    removePendingAllowSpend(swapCalculatedState, signedSwapUpdate)
                  val updatedPendingSpendActionCalculatedState = updatedPendingAllowSpendCalculatedState + PendingSpendAction(
                    signedSwapUpdate,
                    spendActionToken
                  )

                  val updatedPendingStakingCalculatedState =
                    swapCalculatedState
                      .focus(_.pending)
                      .replace(updatedPendingSpendActionCalculatedState)

                  val updatedCalculatedState = acc.calculated
                    .focus(_.operations)
                    .modify(_.updated(OperationType.Staking, updatedPendingStakingCalculatedState))

                  val updatedSharedArtifacts = acc.sharedArtifacts ++ SortedSet[SharedArtifact](
                    spendActionToken
                  )

                  DataState(
                    AmmOnChainState(updates),
                    updatedCalculatedState,
                    updatedSharedArtifacts
                  )
              }
            } yield response
          case _ => acc.pure
        }
    }
  }

  def combineNewSwap[F[_]: Async: HasherSelector](
    applicationConfig: ApplicationConfig,
    acc: DataState[AmmOnChainState, AmmCalculatedState],
    signedSwapUpdate: Signed[SwapUpdate],
    lastSyncGlobalEpochProgress: EpochProgress,
    lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]]
  ): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
    val swapUpdate = signedSwapUpdate.value
    val swapCalculatedState = getSwapCalculatedState(acc.calculated)
    val swapCalculatedStatePendingAllowSpend = swapCalculatedState.pending

    val updates = swapUpdate :: acc.onChain.updates
    getUpdateAllowSpend(swapUpdate, lastGlobalSnapshotsAllowSpends).flatMap {
      case None =>
        val updatedPendingSwapsCalculatedState = if (swapUpdate.maxValidGsEpochProgress < lastSyncGlobalEpochProgress) {
          removePendingAllowSpend(swapCalculatedState, signedSwapUpdate)
        } else if (!swapCalculatedStatePendingAllowSpend.exists(_.update == signedSwapUpdate)) {
          swapCalculatedStatePendingAllowSpend + PendingAllowSpend(signedSwapUpdate)
        } else {
          swapCalculatedStatePendingAllowSpend
        }

        val newSwapState = swapCalculatedState
          .focus(_.pending)
          .replace(updatedPendingSwapsCalculatedState)

        val updatedCalculatedState = acc.calculated
          .focus(_.operations)
          .modify(_.updated(OperationType.Swap, newSwapState))

        DataState(
          AmmOnChainState(updates),
          updatedCalculatedState
        ).pure

      case Some(_) =>
        combinePendingAllowSpendSwap(
          applicationConfig,
          acc,
          signedSwapUpdate,
          lastSyncGlobalEpochProgress,
          lastGlobalSnapshotsAllowSpends
        )
    }
  }

}
