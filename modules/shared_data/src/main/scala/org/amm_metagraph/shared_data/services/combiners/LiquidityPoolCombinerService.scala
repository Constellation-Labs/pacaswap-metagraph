package org.amm_metagraph.shared_data.services.combiners

import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}
import scala.tools.nsc.tasty.SafeEq

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact.{SharedArtifact, SpendAction}
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.{AllowSpend, CurrencyId}
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hashed, Hasher, SecurityProvider}

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.PosLong
import eu.timepit.refined.types.numeric.NonNegLong
import monocle.syntax.all._
import org.amm_metagraph.shared_data.SpendTransactions.{checkIfSpendActionAcceptedInGl0, generateSpendAction}
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.globalSnapshots.getAllowSpendsGlobalSnapshotsState
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, LiquidityPoolUpdate, RewardAllocationVoteUpdate}
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.codecs.HasherSelector
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

trait LiquidityPoolCombinerService[F[_]] {
  def combineNew(
    signedUpdate: Signed[LiquidityPoolUpdate],
    oldState: DataState[AmmOnChainState, AmmCalculatedState],
    globalEpochProgress: EpochProgress,
    lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
    currencyId: CurrencyId
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]]

  def combinePendingAllowSpend(
    pendingSignedUpdate: Signed[LiquidityPoolUpdate],
    oldState: DataState[AmmOnChainState, AmmCalculatedState],
    globalEpochProgress: EpochProgress,
    lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
    currencyId: CurrencyId
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]]

  def combinePendingSpendAction(
    pendingSpendAction: PendingSpendAction[LiquidityPoolUpdate],
    oldState: DataState[AmmOnChainState, AmmCalculatedState],
    globalEpochProgress: EpochProgress,
    spendActions: List[SpendAction],
    currentSnapshotOrdinal: SnapshotOrdinal
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]]
}

object LiquidityPoolCombinerService {
  def make[F[_]: Async: HasherSelector: SecurityProvider](
    applicationConfig: ApplicationConfig
  ): F[LiquidityPoolCombinerService[F]] = Async[F].delay {
    new LiquidityPoolCombinerService[F] {
      def logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("LiquidityPoolCombinerService")
      private def validateUpdate(
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

      private def getUpdateAllowSpends(
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
          case PendingAllowSpend(update) if update === signedLiquidityPoolUpdate => true
          case _                                                                 => false
        }

      private def removePendingSpendAction(
        liquidityPoolsCalculatedState: LiquidityPoolCalculatedState,
        signedLiquidityPoolUpdate: Signed[LiquidityPoolUpdate]
      ) =
        liquidityPoolsCalculatedState.pending.filterNot {
          case PendingSpendAction(update, _) if update === signedLiquidityPoolUpdate => true
          case _                                                                     => false
        }

      def combineNew(
        signedUpdate: Signed[LiquidityPoolUpdate],
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        globalEpochProgress: EpochProgress,
        lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
        currencyId: CurrencyId
      )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
        val liquidityPoolUpdate = signedUpdate.value
        val liquidityPoolCalculatedState = getLiquidityPoolCalculatedState(oldState.calculated)
        val pendingAllowSpendsCalculatedState = liquidityPoolCalculatedState.pending

        val updates = liquidityPoolUpdate :: oldState.onChain.updates
        getUpdateAllowSpends(liquidityPoolUpdate, lastGlobalSnapshotsAllowSpends).flatMap {
          case (None, _) | (_, None) =>
            val updatedLiquidityPoolPending =
              if (liquidityPoolUpdate.maxValidGsEpochProgress < globalEpochProgress) {
                removePendingAllowSpend(liquidityPoolCalculatedState, signedUpdate)
              } else if (!pendingAllowSpendsCalculatedState.exists(_.update === signedUpdate)) {
                pendingAllowSpendsCalculatedState + PendingAllowSpend(signedUpdate)
              } else {
                pendingAllowSpendsCalculatedState
              }

            val newLiquidityPoolState = liquidityPoolCalculatedState
              .focus(_.pending)
              .replace(updatedLiquidityPoolPending)

            val updatedCalculatedState = oldState.calculated
              .focus(_.operations)
              .modify(_.updated(OperationType.LiquidityPool, newLiquidityPoolState))

            DataState(
              AmmOnChainState(updates),
              updatedCalculatedState
            ).pure

          case (Some(_), Some(_)) =>
            combinePendingAllowSpend(
              signedUpdate,
              oldState,
              globalEpochProgress,
              lastGlobalSnapshotsAllowSpends,
              currencyId
            )
        }
      }

      def combinePendingAllowSpend(
        signedUpdate: Signed[LiquidityPoolUpdate],
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        globalEpochProgress: EpochProgress,
        lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
        currencyId: CurrencyId
      )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
        val liquidityPoolsCalculatedState = getLiquidityPoolCalculatedState(oldState.calculated)

        val liquidityPoolUpdate = signedUpdate.value
        val updates = liquidityPoolUpdate :: oldState.onChain.updates
        validateUpdate(applicationConfig, signedUpdate, none, none, globalEpochProgress) match {
          case Some(failedCalculatedState) =>
            handleFailedUpdate(updates, oldState, failedCalculatedState, liquidityPoolsCalculatedState).pure
          case None =>
            getUpdateAllowSpends(liquidityPoolUpdate, lastGlobalSnapshotsAllowSpends).flatMap {
              case (Some(allowSpendTokenA), Some(allowSpendTokenB)) =>
                val maybeFailedUpdate = validateUpdate(
                  applicationConfig,
                  signedUpdate,
                  allowSpendTokenA.some,
                  allowSpendTokenB.some,
                  globalEpochProgress
                )

                maybeFailedUpdate match {
                  case Some(failedCalculatedState) =>
                    handleFailedUpdate(updates, oldState, failedCalculatedState, liquidityPoolsCalculatedState).pure
                  case None =>
                    val spendAction = generateSpendAction(allowSpendTokenA, allowSpendTokenB)

                    val updatedPendingAllowSpendCalculatedState =
                      removePendingAllowSpend(liquidityPoolsCalculatedState, signedUpdate)
                    val updatedPendingSpendActionCalculatedState = updatedPendingAllowSpendCalculatedState + PendingSpendAction(
                      signedUpdate,
                      spendAction
                    )

                    val updatedLiquidityPoolCalculatedState =
                      liquidityPoolsCalculatedState
                        .focus(_.pending)
                        .replace(updatedPendingSpendActionCalculatedState)

                    val updatedCalculatedState = oldState.calculated
                      .focus(_.operations)
                      .modify(_.updated(OperationType.LiquidityPool, updatedLiquidityPoolCalculatedState))

                    val updatedSharedArtifacts = oldState.sharedArtifacts ++ SortedSet[SharedArtifact](
                      spendAction
                    )

                    DataState(
                      AmmOnChainState(updates),
                      updatedCalculatedState,
                      updatedSharedArtifacts
                    ).pure
                }

              case _ => oldState.pure
            }
        }
      }

      def combinePendingSpendAction(
        pendingSpendAction: PendingSpendAction[LiquidityPoolUpdate],
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        globalEpochProgress: EpochProgress,
        spendActions: List[SpendAction],
        currentSnapshotOrdinal: SnapshotOrdinal
      )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
        val liquidityPoolsCalculatedState = getLiquidityPoolCalculatedState(oldState.calculated)

        val signedLiquidityPoolUpdate = pendingSpendAction.update
        val liquidityPoolUpdate = pendingSpendAction.update.value
        val updates = liquidityPoolUpdate :: oldState.onChain.updates
        validateUpdate(applicationConfig, signedLiquidityPoolUpdate, none, none, globalEpochProgress) match {
          case Some(value) => handleFailedUpdate(updates, oldState, value, liquidityPoolsCalculatedState).pure
          case None =>
            for {
              signerAddress <- signedLiquidityPoolUpdate.proofs.head.id.toAddress
              metagraphGeneratedSpendActionHash <- HasherSelector[F].withCurrent(implicit hs =>
                Hasher[F].hash(pendingSpendAction.generatedSpendAction)
              )
              globalSnapshotsHashes <- HasherSelector[F].withCurrent(implicit hs => spendActions.traverse(action => Hasher[F].hash(action)))
              allSpendActionsAccepted = checkIfSpendActionAcceptedInGl0(metagraphGeneratedSpendActionHash, globalSnapshotsHashes)
              updatedState <-
                if (!allSpendActionsAccepted) {
                  oldState.pure
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

                    updatedCalculatedState = oldState.calculated
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

    }
  }
}
