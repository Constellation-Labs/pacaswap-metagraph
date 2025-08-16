package org.amm_metagraph.shared_data.services.combiners

import cats.data.EitherT
import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact._
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.{AllowSpend, CurrencyId}
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hasher, SecurityProvider}

import monocle.syntax.all._
import org.amm_metagraph.shared_data.SpendTransactions.{checkIfSpendActionAcceptedInGl0, generateSpendActionWithoutAllowSpends}
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.epochProgress.{getConfirmedExpireEpochProgress, getFailureExpireEpochProgress}
import org.amm_metagraph.shared_data.globalSnapshots.logger
import org.amm_metagraph.shared_data.services.pricing.PricingService
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, WithdrawalUpdate}
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States.StateTransitionType._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.Withdrawal._
import org.amm_metagraph.shared_data.types.codecs.{HasherSelector, JsonWithBase64BinaryCodec}
import org.amm_metagraph.shared_data.validations.Errors._
import org.amm_metagraph.shared_data.validations.WithdrawalValidations

trait WithdrawalCombinerService[F[_]] {
  def combineNew(
    signedUpdate: Signed[WithdrawalUpdate],
    oldState: DataState[AmmOnChainState, AmmCalculatedState],
    globalEpochProgress: EpochProgress,
    lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
    currencyId: CurrencyId
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]]

  def combinePendingSpendAction(
    pendingSpendAction: PendingSpendAction[WithdrawalUpdate],
    oldState: DataState[AmmOnChainState, AmmCalculatedState],
    globalEpochProgress: EpochProgress,
    spendActions: List[SpendAction],
    currentSnapshotOrdinal: SnapshotOrdinal
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]]

  def cleanupExpiredOperations(
    oldState: DataState[AmmOnChainState, AmmCalculatedState],
    globalEpochProgress: EpochProgress
  ): DataState[AmmOnChainState, AmmCalculatedState]
}

object WithdrawalCombinerService {
  def make[F[_]: Async: HasherSelector: SecurityProvider](
    applicationConfig: ApplicationConfig,
    pricingService: PricingService[F],
    withdrawalValidations: WithdrawalValidations[F],
    dataUpdateCodec: JsonWithBase64BinaryCodec[F, AmmUpdate]
  ): WithdrawalCombinerService[F] =
    new WithdrawalCombinerService[F] {
      private def handleFailedUpdate(
        withdrawalUpdate: Signed[WithdrawalUpdate],
        acc: DataState[AmmOnChainState, AmmCalculatedState],
        failedCalculatedState: FailedCalculatedState,
        withdrawalCalculatedState: WithdrawalCalculatedState,
        updateHash: Hash,
        currentState: StateTransitionType
      ) =
        failedCalculatedState.reason match {
          case DuplicatedUpdate(_) => logger.warn("Duplicated data update, ignoring") >> acc.pure
          case _ =>
            val updatedWithdrawalCalculatedState = withdrawalCalculatedState
              .focus(_.failed)
              .modify(_ + failedCalculatedState)
              .focus(_.pending)
              .modify(_.filter(_.update =!= withdrawalUpdate))

            val updatedCalculatedState = acc.calculated
              .focus(_.operations)
              .modify(_.updated(OperationType.Withdrawal, updatedWithdrawalCalculatedState))

            Async[F].pure(
              acc
                .focus(_.onChain.updatedStateDataUpdate)
                .modify { current =>
                  current + UpdatedStateDataUpdate(
                    currentState,
                    Failed,
                    OperationType.Withdrawal,
                    withdrawalUpdate,
                    updateHash,
                    none,
                    failedCalculatedState.reason.some
                  )
                }
                .focus(_.calculated)
                .replace(updatedCalculatedState)
            )
        }

      private def removePendingSpendAction(
        pendingActions: SortedSet[PendingAction[WithdrawalUpdate]],
        signedWithdrawalUpdate: Signed[WithdrawalUpdate]
      ): SortedSet[PendingAction[WithdrawalUpdate]] = pendingActions.collect {
        case spendAction @ PendingSpendAction(update, _, _, _) if update =!= signedWithdrawalUpdate => spendAction
      }

      private def rollbackAmountInLPs(
        signedUpdate: Signed[WithdrawalUpdate],
        updateHash: Hash,
        lastSyncGlobalSnapshotEpoch: EpochProgress,
        maybePricingTokenInfo: Option[PricingTokenInfo],
        oldState: DataState[AmmOnChainState, AmmCalculatedState]
      ): F[DataState[AmmOnChainState, AmmCalculatedState]] = maybePricingTokenInfo match {
        case Some(WithdrawalTokenInfo(tokenAIdentifier, tokenAAmount, tokenBIdentifier, tokenBAmount)) =>
          (for {
            poolId <- EitherT.liftF(
              buildLiquidityPoolUniqueIdentifier(
                tokenAIdentifier,
                tokenBIdentifier
              )
            )

            liquidityPoolCalculatedState = getLiquidityPoolCalculatedState(oldState.calculated)

            liquidityPool <- EitherT.liftF(
              getLiquidityPoolByPoolId(liquidityPoolCalculatedState.confirmed.value, poolId)
            )

            updatedLiquidityPool <- EitherT.fromEither(
              pricingService.rollbackWithdrawalLiquidityPoolAmounts(
                signedUpdate,
                updateHash,
                lastSyncGlobalSnapshotEpoch,
                liquidityPool,
                tokenAAmount,
                tokenBAmount
              )
            )

            updatedState = {
              val newLiquidityPoolState = liquidityPoolCalculatedState
                .focus(_.confirmed.value)
                .modify(_.updated(poolId.value, updatedLiquidityPool))

              val updatedCalculatedState = oldState.calculated
                .focus(_.operations)
                .modify(_.updated(OperationType.LiquidityPool, newLiquidityPoolState))

              oldState.copy(calculated = updatedCalculatedState)
            }
          } yield updatedState).valueOrF(_ => oldState.pure)
        case _ => oldState.pure
      }

      override def combineNew(
        signedUpdate: Signed[WithdrawalUpdate],
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        globalEpochProgress: EpochProgress,
        lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
        currencyId: CurrencyId
      )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
        val withdrawalUpdate = signedUpdate.value
        val withdrawalCalculatedState = getWithdrawalCalculatedState(oldState.calculated)
        val liquidityPoolsCalculatedState = getLiquidityPoolCalculatedState(oldState.calculated)

        if (withdrawalCalculatedState.pending.exists(_.update === signedUpdate)) {
          return oldState.pure[F]
        }
        val updateHashedF = HasherSelector[F].withCurrent(implicit hs => signedUpdate.toHashed(dataUpdateCodec.serialize))

        val combinedState = for {
          _ <- EitherT(
            withdrawalValidations.l0Validations(
              signedUpdate,
              oldState.calculated,
              globalEpochProgress
            )
          )

          poolId <- EitherT.liftF(buildLiquidityPoolUniqueIdentifier(withdrawalUpdate.tokenAId, withdrawalUpdate.tokenBId))
          liquidityPool <- EitherT.liftF(getLiquidityPoolByPoolId(liquidityPoolsCalculatedState.confirmed.value, poolId))
          updateHashed <- EitherT.liftF(updateHashedF)
          withdrawalAmounts <- EitherT.fromEither[F](
            pricingService.getWithdrawalTokenInfo(
              signedUpdate,
              updateHashed.hash,
              liquidityPool,
              globalEpochProgress
            )
          )

          updatedPool <- EitherT.fromEither[F](
            pricingService.getUpdatedLiquidityPoolDueNewWithdrawal(
              signedUpdate,
              updateHashed.hash,
              liquidityPool,
              withdrawalAmounts,
              globalEpochProgress
            )
          )

          _ <- EitherT(
            withdrawalValidations.newUpdateValidations(
              signedUpdate,
              withdrawalAmounts,
              globalEpochProgress,
              updatedPool
            )
          )

          spendAction = generateSpendActionWithoutAllowSpends(
            signedUpdate.tokenAId,
            withdrawalAmounts.tokenAAmount,
            signedUpdate.tokenBId,
            withdrawalAmounts.tokenBAmount,
            signedUpdate.source,
            currencyId
          )

          newLiquidityPoolState = liquidityPoolsCalculatedState
            .focus(_.confirmed.value)
            .modify(_.updated(poolId.value, updatedPool))

          pendingAllowSpend = PendingSpendAction(signedUpdate, updateHashed.hash, spendAction, withdrawalAmounts.some)

          updatedPendingWithdrawalCalculatedState = withdrawalCalculatedState
            .focus(_.pending)
            .replace(
              withdrawalCalculatedState.pending + pendingAllowSpend
            )

          updatedCalculatedState = oldState.calculated
            .focus(_.operations)
            .modify(_.updated(OperationType.Withdrawal, updatedPendingWithdrawalCalculatedState))
            .focus(_.operations)
            .modify(_.updated(OperationType.LiquidityPool, newLiquidityPoolState))

        } yield
          oldState
            .focus(_.onChain.updatedStateDataUpdate)
            .modify { current =>
              current + UpdatedStateDataUpdate(
                NewUpdate,
                PendingAllowSpends,
                OperationType.Withdrawal,
                signedUpdate,
                updateHashed.hash,
                Some(pendingAllowSpend.asInstanceOf[PendingAction[AmmUpdate]])
              )
            }
            .focus(_.calculated)
            .replace(updatedCalculatedState)
            .focus(_.sharedArtifacts)
            .modify(current =>
              current ++ SortedSet[SharedArtifact](
                spendAction
              )
            )

        combinedState.foldF(
          failed =>
            updateHashedF.flatMap(hashed =>
              handleFailedUpdate(signedUpdate, oldState, failed, withdrawalCalculatedState, hashed.hash, NewUpdate)
            ),
          success => success.pure[F]
        )
      }

      override def combinePendingSpendAction(
        pendingSpendAction: PendingSpendAction[WithdrawalUpdate],
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        globalEpochProgress: EpochProgress,
        spendActions: List[SpendAction],
        currentSnapshotOrdinal: SnapshotOrdinal
      )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
        val withdrawalCalculatedState = getWithdrawalCalculatedState(oldState.calculated)
        val signedWithdrawalUpdate = pendingSpendAction.update
        val withdrawalUpdate = pendingSpendAction.update.value

        for {
          metagraphGeneratedSpendActionHash <- HasherSelector[F].withCurrent(implicit hs =>
            Hasher[F].hash(pendingSpendAction.generatedSpendAction)
          )
          globalSnapshotsHashes <- HasherSelector[F].withCurrent(implicit hs => spendActions.traverse(action => Hasher[F].hash(action)))
          allSpendActionsAccepted = checkIfSpendActionAcceptedInGl0(metagraphGeneratedSpendActionHash, globalSnapshotsHashes)

          combinedState <-
            if (!allSpendActionsAccepted) {
              oldState.pure[F]
            } else {
              val processingState = for {
                _ <- EitherT(
                  withdrawalValidations.pendingSpendActionsValidation(
                    signedWithdrawalUpdate,
                    globalEpochProgress
                  )
                )

                withdrawalReference <- EitherT.liftF(
                  HasherSelector[F].withCurrent(implicit hs => WithdrawalReference.of(signedWithdrawalUpdate))
                )

                maybeWithdrawalTokenAmounts = pendingSpendAction.pricingTokenInfo.collect {
                  case withdrawalTokenAmounts: WithdrawalTokenInfo => withdrawalTokenAmounts
                }

                withdrawalAmounts <- EitherT.fromOption[F](
                  maybeWithdrawalTokenAmounts,
                  FailedCalculatedState(
                    MissingWithdrawalsAmount(),
                    getFailureExpireEpochProgress(applicationConfig, globalEpochProgress),
                    pendingSpendAction.updateHash,
                    pendingSpendAction.update
                  )
                )

                withdrawalCalculatedStateAddress = WithdrawalCalculatedStateAddress(
                  withdrawalUpdate.source,
                  pendingSpendAction.updateHash,
                  withdrawalUpdate.tokenAId,
                  withdrawalAmounts.tokenAAmount,
                  withdrawalUpdate.tokenBId,
                  withdrawalAmounts.tokenBAmount,
                  withdrawalUpdate.shareToWithdraw,
                  withdrawalUpdate.minAmountAOut,
                  withdrawalUpdate.minAmountBOut,
                  withdrawalUpdate.maxAmountAOut,
                  withdrawalUpdate.maxAmountBOut,
                  withdrawalReference
                )

                expirationEpochProgress = getConfirmedExpireEpochProgress(applicationConfig, globalEpochProgress)
                withdrawalCalculatedStateValue = WithdrawalCalculatedStateValue(
                  expirationEpochProgress,
                  withdrawalCalculatedStateAddress
                )

                newWithdrawalState = withdrawalCalculatedState
                  .focus(_.confirmed.value)
                  .modify(current =>
                    current.updatedWith(signedWithdrawalUpdate.source) {
                      case Some(confirmedWithdrawals) =>
                        Some(
                          WithdrawalCalculatedStateInfo(
                            withdrawalReference,
                            confirmedWithdrawals.values + withdrawalCalculatedStateValue
                          )
                        )
                      case None =>
                        Some(
                          WithdrawalCalculatedStateInfo(
                            withdrawalReference,
                            SortedSet(withdrawalCalculatedStateValue)
                          )
                        )
                    }
                  )
                  .focus(_.pending)
                  .modify(removePendingSpendAction(_, signedWithdrawalUpdate))
                updatedCalculatedState = oldState.calculated
                  .focus(_.operations)
                  .modify(_.updated(OperationType.Withdrawal, newWithdrawalState))
              } yield
                oldState
                  .focus(_.onChain.updatedStateDataUpdate)
                  .modify { current =>
                    current + UpdatedStateDataUpdate(
                      PendingSpendTransactions,
                      Confirmed,
                      OperationType.Withdrawal,
                      pendingSpendAction.update,
                      pendingSpendAction.updateHash,
                      none
                    )
                  }
                  .focus(_.calculated)
                  .replace(updatedCalculatedState)

              processingState.foldF(
                failed =>
                  rollbackAmountInLPs(
                    pendingSpendAction.update,
                    pendingSpendAction.updateHash,
                    globalEpochProgress,
                    pendingSpendAction.pricingTokenInfo,
                    oldState
                  ).flatMap { rolledBackState =>
                    handleFailedUpdate(
                      pendingSpendAction.update,
                      rolledBackState,
                      failed,
                      withdrawalCalculatedState,
                      pendingSpendAction.updateHash,
                      PendingSpendTransactions
                    )
                  },
                success => success.pure[F]
              )
            }
        } yield combinedState
      }

      def cleanupExpiredOperations(
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        globalEpochProgress: EpochProgress
      ): DataState[AmmOnChainState, AmmCalculatedState] = {
        val withdrawalCalculatedState = getWithdrawalCalculatedState(oldState.calculated)
        val unexpiredFailed = withdrawalCalculatedState.failed.filter(_.expiringEpochProgress > globalEpochProgress)
        val unexpiredConfirmed = withdrawalCalculatedState.confirmed.value.collect {
          case (address, infos) =>
            address -> infos
              .focus(_.values)
              .modify(_.filter(_.expiringEpochProgress > globalEpochProgress))
        }

        val updatedWithdrawalCalculatedState = withdrawalCalculatedState
          .focus(_.failed)
          .replace(unexpiredFailed)
          .focus(_.confirmed.value)
          .replace(unexpiredConfirmed)

        oldState
          .focus(_.calculated.operations)
          .modify(_.updated(OperationType.Withdrawal, updatedWithdrawalCalculatedState))
      }
    }
}
