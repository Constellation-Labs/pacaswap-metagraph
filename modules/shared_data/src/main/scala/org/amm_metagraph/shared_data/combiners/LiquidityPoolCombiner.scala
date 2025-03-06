package org.amm_metagraph.shared_data.combiners

import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.currency.dataApplication.DataState
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact.{SharedArtifact, SpendAction}
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.AllowSpend
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hashed, Hasher, SecurityProvider}

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.PosLong
import eu.timepit.refined.types.numeric.NonNegLong
import monocle.syntax.all._
import org.amm_metagraph.shared_data.SpendTransactions.{checkIfSpendActionsAcceptedInGl0, generateSpendAction}
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.globalSnapshots.getAllowSpendsGlobalSnapshotsState
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, LiquidityPoolUpdate}
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.codecs.HasherSelector

object LiquidityPoolCombiner {

  def validateUpdate(
    applicationConfig: ApplicationConfig,
    signedUpdate: Signed[LiquidityPoolUpdate],
    maybeAllowSpendTokenA: Option[Hashed[AllowSpend]],
    maybeAllowSpendTokenB: Option[Hashed[AllowSpend]],
    lastSyncGlobalEpochProgress: EpochProgress
  ): Option[FailedCalculatedState] =
    if (signedUpdate.maxValidGsEpochProgress < lastSyncGlobalEpochProgress) {
      FailedCalculatedState(
        OperationExpired(signedUpdate),
        EpochProgress(NonNegLong.unsafeFrom(lastSyncGlobalEpochProgress.value.value + 30L)),
        signedUpdate
      ).some
    } else {
      (maybeAllowSpendTokenA, maybeAllowSpendTokenB).mapN { (allowSpendTokenA, allowSpendTokenB) =>
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
      }.flatten
    }

  private def handleFailedUpdate(
    updates: List[AmmUpdate],
    acc: DataState[AmmOnChainState, AmmCalculatedState],
    failedCalculatedState: FailedCalculatedState,
    liquidityPoolCalculatedState: LiquidityPoolCalculatedState
  ) = {
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
  }

  private def getUpdateAllowSpends[F[_]: Async: HasherSelector](
    liquidityPoolUpdate: LiquidityPoolUpdate,
    lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]]
  ) =
    HasherSelector[F].withBrotli { implicit hs =>
      getAllowSpendsGlobalSnapshotsState(
        liquidityPoolUpdate.tokenAAllowSpend,
        liquidityPoolUpdate.tokenAId,
        liquidityPoolUpdate.tokenBAllowSpend,
        liquidityPoolUpdate.tokenBId,
        lastGlobalSnapshotsAllowSpends
      )
    }

  private def removePendingAllowSpend(
    liquidityPoolsCalculatedState: LiquidityPoolCalculatedState,
    signedLiquidityPoolUpdate: Signed[LiquidityPoolUpdate]
  ) =
    liquidityPoolsCalculatedState.pending.filterNot {
      case PendingAllowSpend(update) if update == signedLiquidityPoolUpdate => true
      case _                                                                => false
    }

  private def removePendingSpendAction(
    liquidityPoolsCalculatedState: LiquidityPoolCalculatedState,
    signedLiquidityPoolUpdate: Signed[LiquidityPoolUpdate]
  ) =
    liquidityPoolsCalculatedState.pending.filterNot {
      case PendingSpendAction(update, _) if update == signedLiquidityPoolUpdate => true
      case _                                                                    => false
    }

  def combinePendingSpendActionLiquidityPool[F[_]: Async: Hasher: SecurityProvider](
    applicationConfig: ApplicationConfig,
    acc: DataState[AmmOnChainState, AmmCalculatedState],
    pendingLiquidityPoolUpdate: PendingSpendAction[LiquidityPoolUpdate],
    lastSyncGlobalEpochProgress: EpochProgress,
    spendActions: List[SpendAction]
  ): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
    val liquidityPoolsCalculatedState = getLiquidityPoolCalculatedState(acc.calculated)

    val signedLiquidityPoolUpdate = pendingLiquidityPoolUpdate.update
    val liquidityPoolUpdate = pendingLiquidityPoolUpdate.update.value
    val updates = liquidityPoolUpdate :: acc.onChain.updates
    validateUpdate(applicationConfig, signedLiquidityPoolUpdate, none, none, lastSyncGlobalEpochProgress) match {
      case Some(value) => handleFailedUpdate(updates, acc, value, liquidityPoolsCalculatedState).pure
      case None =>
        for {
          signerAddress <- signedLiquidityPoolUpdate.proofs.head.id.toAddress
          metagraphGeneratedHashes <- pendingLiquidityPoolUpdate.generatedSpendActions.traverse(action => Hasher[F].hash(action))
          globalSnapshotsHashes <- spendActions.traverse(action => Hasher[F].hash(action))
          allSpendActionsAccepted = checkIfSpendActionsAcceptedInGl0(metagraphGeneratedHashes, globalSnapshotsHashes)
          updatedState <-
            if (!allSpendActionsAccepted) {
              acc.pure
            } else {
              for {
                poolId <- buildLiquidityPoolUniqueIdentifier(liquidityPoolUpdate.tokenAId, liquidityPoolUpdate.tokenBId)
                amountA = liquidityPoolUpdate.tokenAAmount.value
                amountB = liquidityPoolUpdate.tokenBAmount.value
                poolTotalShares: PosLong = 1.toTokenAmountFormat.toPosLongUnsafe

                liquidityPool = LiquidityPool(
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

                updatedPendingCalculatedState = removePendingSpendAction(liquidityPoolsCalculatedState, signedLiquidityPoolUpdate)
                updatedLiquidityPoolCalculatedState = liquidityPoolsCalculatedState
                  .focus(_.confirmed.value)
                  .modify(liquidityPools => liquidityPools.updated(poolId.value, liquidityPool))
                  .focus(_.pending)
                  .replace(updatedPendingCalculatedState)

                updatedCalculatedState = acc.calculated
                  .focus(_.operations)
                  .modify(_.updated(OperationType.LiquidityPool, updatedLiquidityPoolCalculatedState))

                response = DataState(
                  AmmOnChainState(updates),
                  updatedCalculatedState
                )

              } yield response
            }
        } yield updatedState

    }
  }

  def combinePendingAllowSpendLiquidityPool[F[_]: Async: HasherSelector](
    applicationConfig: ApplicationConfig,
    acc: DataState[AmmOnChainState, AmmCalculatedState],
    signedLiquidityPoolUpdate: Signed[LiquidityPoolUpdate],
    lastSyncGlobalEpochProgress: EpochProgress,
    lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]]
  ): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
    val liquidityPoolsCalculatedState = getLiquidityPoolCalculatedState(acc.calculated)

    val liquidityPoolUpdate = signedLiquidityPoolUpdate.value
    val updates = liquidityPoolUpdate :: acc.onChain.updates
    validateUpdate(applicationConfig, signedLiquidityPoolUpdate, none, none, lastSyncGlobalEpochProgress) match {
      case Some(failedCalculatedState) => handleFailedUpdate(updates, acc, failedCalculatedState, liquidityPoolsCalculatedState).pure
      case None =>
        getUpdateAllowSpends(liquidityPoolUpdate, lastGlobalSnapshotsAllowSpends).flatMap {
          case (Some(allowSpendTokenA), Some(allowSpendTokenB)) =>
            val maybeFailedUpdate = validateUpdate(
              applicationConfig,
              signedLiquidityPoolUpdate,
              allowSpendTokenA.some,
              allowSpendTokenB.some,
              lastSyncGlobalEpochProgress
            )

            maybeFailedUpdate match {
              case Some(failedCalculatedState) =>
                handleFailedUpdate(updates, acc, failedCalculatedState, liquidityPoolsCalculatedState).pure
              case None =>
                val spendActionTokenA = generateSpendAction(allowSpendTokenA)
                val spendActionTokenB = generateSpendAction(allowSpendTokenB)

                val updatedPendingAllowSpendCalculatedState =
                  removePendingAllowSpend(liquidityPoolsCalculatedState, signedLiquidityPoolUpdate)
                val updatedPendingSpendActionCalculatedState = updatedPendingAllowSpendCalculatedState + PendingSpendAction(
                  signedLiquidityPoolUpdate,
                  List(spendActionTokenA, spendActionTokenB)
                )

                val updatedLiquidityPoolCalculatedState =
                  liquidityPoolsCalculatedState
                    .focus(_.pending)
                    .replace(updatedPendingSpendActionCalculatedState)

                val updatedCalculatedState = acc.calculated
                  .focus(_.operations)
                  .modify(_.updated(OperationType.LiquidityPool, updatedLiquidityPoolCalculatedState))

                DataState(
                  AmmOnChainState(updates),
                  updatedCalculatedState,
                  SortedSet[SharedArtifact](
                    spendActionTokenA,
                    spendActionTokenB
                  )
                ).pure
            }

          case _ => acc.pure
        }
    }
  }

  def combineNewLiquidityPool[F[_]: Async: HasherSelector](
    applicationConfig: ApplicationConfig,
    acc: DataState[AmmOnChainState, AmmCalculatedState],
    signedLiquidityPoolUpdate: Signed[LiquidityPoolUpdate],
    lastSyncGlobalEpochProgress: EpochProgress,
    lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]]
  ): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
    val liquidityPoolUpdate = signedLiquidityPoolUpdate.value
    val liquidityPoolCalculatedState = getLiquidityPoolCalculatedState(acc.calculated)
    val pendingAllowSpendsCalculatedState = liquidityPoolCalculatedState.pending

    val updates = liquidityPoolUpdate :: acc.onChain.updates
    getUpdateAllowSpends(liquidityPoolUpdate, lastGlobalSnapshotsAllowSpends).flatMap {
      case (None, _) | (_, None) =>
        val updatedLiquidityPoolPending =
          if (liquidityPoolUpdate.maxValidGsEpochProgress < lastSyncGlobalEpochProgress) {
            removePendingAllowSpend(liquidityPoolCalculatedState, signedLiquidityPoolUpdate)
          } else if (!pendingAllowSpendsCalculatedState.exists(_.update == signedLiquidityPoolUpdate)) {
            pendingAllowSpendsCalculatedState + PendingAllowSpend(signedLiquidityPoolUpdate)
          } else {
            pendingAllowSpendsCalculatedState
          }

        val newLiquidityPoolState = liquidityPoolCalculatedState
          .focus(_.pending)
          .replace(updatedLiquidityPoolPending)

        val updatedCalculatedState = acc.calculated
          .focus(_.operations)
          .modify(_.updated(OperationType.LiquidityPool, newLiquidityPoolState))

        DataState(
          AmmOnChainState(updates),
          updatedCalculatedState
        ).pure

      case (Some(_), Some(_)) =>
        combinePendingAllowSpendLiquidityPool(
          applicationConfig,
          acc,
          signedLiquidityPoolUpdate,
          lastSyncGlobalEpochProgress,
          lastGlobalSnapshotsAllowSpends
        )
    }
  }
}
