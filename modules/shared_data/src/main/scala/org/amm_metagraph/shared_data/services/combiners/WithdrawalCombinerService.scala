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
import io.constellationnetwork.security.Hasher
import io.constellationnetwork.security.signature.Signed

import eu.timepit.refined.types.numeric.NonNegLong
import monocle.syntax.all._
import org.amm_metagraph.shared_data.SpendTransactions.{checkIfSpendActionAcceptedInGl0, generateSpendActionWithoutAllowSpends}
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.services.pricing.PricingService
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, WithdrawalUpdate}
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.Withdrawal.{WithdrawalCalculatedStateAddress, WithdrawalReference, getWithdrawalCalculatedState}
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
}

object WithdrawalCombinerService {
  def make[F[_]: Async: HasherSelector](
    applicationConfig: ApplicationConfig,
    pricingService: PricingService[F],
    withdrawalValidations: WithdrawalValidations[F],
    dataUpdateCodec: JsonWithBase64BinaryCodec[F, AmmUpdate]
  ): WithdrawalCombinerService[F] =
    new WithdrawalCombinerService[F] {
      private def handleFailedUpdate(
        updates: List[AmmUpdate],
        withdrawalUpdate: Signed[WithdrawalUpdate],
        acc: DataState[AmmOnChainState, AmmCalculatedState],
        failedCalculatedState: FailedCalculatedState,
        withdrawalCalculatedState: WithdrawalCalculatedState
      ) = {
        val updatedWithdrawalCalculatedState = withdrawalCalculatedState
          .focus(_.failed)
          .modify(_ + failedCalculatedState)
          .focus(_.pending)
          .modify(_.filter(_.update =!= withdrawalUpdate))

        val updatedCalculatedState = acc.calculated
          .focus(_.operations)
          .modify(_.updated(OperationType.Withdrawal, updatedWithdrawalCalculatedState))

        DataState(
          AmmOnChainState(updates),
          updatedCalculatedState
        )
      }

      private def removePendingSpendAction(
        pendingActions: Set[PendingAction[WithdrawalUpdate]],
        signedWithdrawalUpdate: Signed[WithdrawalUpdate]
      ): Set[PendingAction[WithdrawalUpdate]] = pendingActions.collect {
        case spendAction @ PendingSpendAction(update, _, _, _) if update =!= signedWithdrawalUpdate => spendAction
      }

      private def rollbackAmountInLPs(
        signedUpdate: Signed[WithdrawalUpdate],
        lastSyncGlobalSnapshotEpoch: EpochProgress,
        maybePricingTokenInfo: Option[PricingTokenInfo],
        oldState: DataState[AmmOnChainState, AmmCalculatedState]
      ): F[DataState[AmmOnChainState, AmmCalculatedState]] = maybePricingTokenInfo match {
        case Some(WithdrawalTokenAmounts(tokenAIdentifier, tokenAAmount, tokenBIdentifier, tokenBAmount)) =>
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
        case Some(_: SwapTokenInfo) => oldState.pure
        case None                   => oldState.pure
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
        val updates = withdrawalUpdate :: oldState.onChain.updates

        if (withdrawalCalculatedState.pending.exists(_.update === signedUpdate)) {
          return oldState.pure[F]
        }

        val combinedState = for {
          poolId <- EitherT.liftF(buildLiquidityPoolUniqueIdentifier(withdrawalUpdate.tokenAId, withdrawalUpdate.tokenBId))
          liquidityPool <- EitherT.liftF(getLiquidityPoolByPoolId(liquidityPoolsCalculatedState.confirmed.value, poolId))
          updateHashed <- EitherT.liftF(
            HasherSelector[F].withCurrent(implicit hs => signedUpdate.toHashed(dataUpdateCodec.serialize))
          )
          withdrawalAmounts <- EitherT.fromEither[F](
            pricingService.calculateWithdrawalAmounts(
              signedUpdate,
              liquidityPool,
              globalEpochProgress
            )
          )

          updatedPool <- EitherT.fromEither[F](
            pricingService.getUpdatedLiquidityPoolDueNewWithdrawal(
              signedUpdate,
              liquidityPool,
              withdrawalAmounts,
              globalEpochProgress
            )
          )

          _ <- EitherT.fromEither(
            withdrawalValidations.newUpdateValidations(
              signedUpdate,
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

          updatedPendingWithdrawalCalculatedState = withdrawalCalculatedState
            .focus(_.pending)
            .replace(
              withdrawalCalculatedState.pending + PendingSpendAction(signedUpdate, updateHashed.hash, spendAction, withdrawalAmounts.some)
            )

          updatedCalculatedState = oldState.calculated
            .focus(_.operations)
            .modify(_.updated(OperationType.Withdrawal, updatedPendingWithdrawalCalculatedState))
            .focus(_.operations)
            .modify(_.updated(OperationType.LiquidityPool, newLiquidityPoolState))
          updatedSharedArtifacts = oldState.sharedArtifacts + spendAction

        } yield
          DataState(
            AmmOnChainState(updates),
            updatedCalculatedState,
            updatedSharedArtifacts
          )

        combinedState.valueOr(failedCalculatedState =>
          handleFailedUpdate(updates, signedUpdate, oldState, failedCalculatedState, withdrawalCalculatedState)
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
        val updates = withdrawalUpdate :: oldState.onChain.updates

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
                _ <- EitherT.fromEither(
                  withdrawalValidations.pendingSpendActionsValidation(
                    signedWithdrawalUpdate,
                    globalEpochProgress
                  )
                )

                withdrawalReference <- EitherT.liftF(
                  HasherSelector[F].withCurrent(implicit hs => WithdrawalReference.of(signedWithdrawalUpdate))
                )

                maybeWithdrawalTokenAmounts = pendingSpendAction.pricingTokenInfo.collect {
                  case withdrawalTokenAmounts: WithdrawalTokenAmounts => withdrawalTokenAmounts
                }

                withdrawalAmounts <- EitherT.fromOption[F](
                  maybeWithdrawalTokenAmounts,
                  FailedCalculatedState(
                    MissingWithdrawalsAmount(),
                    getFailureExpireEpochProgress(applicationConfig, globalEpochProgress),
                    pendingSpendAction.update
                  )
                )

                withdrawalCalculatedStateAddress = WithdrawalCalculatedStateAddress(
                  withdrawalUpdate.tokenAId,
                  withdrawalAmounts.tokenAAmount,
                  withdrawalUpdate.tokenBId,
                  withdrawalAmounts.tokenBAmount,
                  withdrawalUpdate.shareToWithdraw,
                  withdrawalReference
                )

                newWithdrawalState = withdrawalCalculatedState
                  .focus(_.confirmed.value)
                  .modify(current =>
                    current.updatedWith(signedWithdrawalUpdate.source) {
                      case Some(confirmedWithdrawals) => Some(confirmedWithdrawals + withdrawalCalculatedStateAddress)
                      case None                       => Some(Set(withdrawalCalculatedStateAddress))
                    }
                  )
                  .focus(_.pending)
                  .modify(removePendingSpendAction(_, signedWithdrawalUpdate))
                updatedCalculatedState = oldState.calculated
                  .focus(_.operations)
                  .modify(_.updated(OperationType.Withdrawal, newWithdrawalState))
              } yield
                DataState[AmmOnChainState, AmmCalculatedState](
                  AmmOnChainState(updates),
                  updatedCalculatedState,
                  oldState.sharedArtifacts
                )

              processingState.valueOrF { failedCalculatedState =>
                rollbackAmountInLPs(
                  pendingSpendAction.update,
                  globalEpochProgress,
                  pendingSpendAction.pricingTokenInfo,
                  oldState
                ).map { rolledBackState =>
                  handleFailedUpdate(
                    updates,
                    pendingSpendAction.update,
                    rolledBackState,
                    failedCalculatedState,
                    withdrawalCalculatedState
                  )
                }
              }
            }
        } yield combinedState
      }

      private def getFailureExpireEpochProgress(
        applicationConfig: ApplicationConfig,
        lastSyncGlobalEpochProgress: EpochProgress
      ) = {
        val expireEpochProgress = EpochProgress(
          NonNegLong
            .from(
              lastSyncGlobalEpochProgress.value.value + applicationConfig.failedOperationsExpirationEpochProgresses.value.value
            )
            .getOrElse(NonNegLong.MinValue)
        )
        expireEpochProgress
      }
    }
}
