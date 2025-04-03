package org.amm_metagraph.shared_data.services.combiners

import cats.data.EitherT
import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact._
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.{AllowSpend, CurrencyId, SwapAmount}
import io.constellationnetwork.security.Hasher
import io.constellationnetwork.security.signature.Signed

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.{NonNegLong, PosLong}
import monocle.syntax.all._
import org.amm_metagraph.shared_data.SpendTransactions.{checkIfSpendActionAcceptedInGl0, generateSpendActionWithoutAllowSpends}
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, WithdrawalUpdate}
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.Withdrawal.{WithdrawalCalculatedStateAddress, WithdrawalReference, getWithdrawalCalculatedState}
import org.amm_metagraph.shared_data.types.codecs.HasherSelector

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
  def make[F[_]: Async: HasherSelector](applicationConfig: ApplicationConfig): WithdrawalCombinerService[F] =
    new WithdrawalCombinerService[F] {

      private case class WithdrawalTokenAmounts(
        tokenAAmount: SwapAmount,
        tokenBAmount: SwapAmount
      )

      private def getExpireEpochProgress(lastSyncGlobalEpochProgress: EpochProgress): EpochProgress = EpochProgress(
        NonNegLong
          .from(
            lastSyncGlobalEpochProgress.value.value + applicationConfig.failedOperationsExpirationEpochProgresses.value.value
          )
          .getOrElse(NonNegLong.MinValue)
      )

      private def calculateWithdrawalAmounts(
        signedUpdate: Signed[WithdrawalUpdate],
        liquidityPool: LiquidityPool,
        lastSyncGlobalEpochProgress: EpochProgress
      ): Either[FailedCalculatedState, WithdrawalTokenAmounts] = {
        val expireEpochProgress = getExpireEpochProgress(lastSyncGlobalEpochProgress)

        val PRECISION = 8
        val SCALING_FACTOR = BigInt(10).pow(PRECISION)

        for {
          _ <- liquidityPool.poolShares.addressShares
            .get(signedUpdate.source)
            .filter(signedUpdate.shareToWithdraw.value <= _.value)
            .toRight(
              FailedCalculatedState(
                WithdrawalAmountExceedsAvailableShares(signedUpdate.shareToWithdraw),
                expireEpochProgress,
                signedUpdate
              )
            )

          _ <- Either.cond(
            signedUpdate.shareToWithdraw.value.value < liquidityPool.poolShares.totalShares.value,
            (),
            FailedCalculatedState(
              CannotWithdrawAllShares(),
              expireEpochProgress,
              signedUpdate
            )
          )
          totalShares = BigInt(liquidityPool.poolShares.totalShares.value)
          sharesBigInt = BigInt(signedUpdate.shareToWithdraw.value.value)
          tokenAOut = (BigInt(liquidityPool.tokenA.amount.value) * sharesBigInt * SCALING_FACTOR / totalShares) / SCALING_FACTOR
          tokenBOut = (BigInt(liquidityPool.tokenB.amount.value) * sharesBigInt * SCALING_FACTOR / totalShares) / SCALING_FACTOR

          tokenAAmount <- PosLong
            .from(tokenAOut.toLong)
            .leftMap(_ =>
              FailedCalculatedState(
                ArithmeticError(s"Token A amount doesn't match PosLong: $tokenAOut"),
                expireEpochProgress,
                signedUpdate
              )
            )
          tokenBAmount <- PosLong
            .from(tokenBOut.toLong)
            .leftMap(_ =>
              FailedCalculatedState(
                ArithmeticError(s"Token B amount doesn't match PosLong: $tokenBOut"),
                expireEpochProgress,
                signedUpdate
              )
            )

          _ <- Either.cond(
            tokenAAmount <= liquidityPool.tokenA.amount.value,
            (),
            FailedCalculatedState(
              TokenExceedsAvailableAmount(liquidityPool.tokenA.identifier, liquidityPool.tokenA.amount.value, tokenAAmount.value),
              expireEpochProgress,
              signedUpdate
            )
          )
          _ <- Either.cond(
            tokenBAmount <= liquidityPool.tokenB.amount.value,
            (),
            FailedCalculatedState(
              TokenExceedsAvailableAmount(liquidityPool.tokenB.identifier, liquidityPool.tokenB.amount.value, tokenBAmount.value),
              expireEpochProgress,
              signedUpdate
            )
          )
        } yield WithdrawalTokenAmounts(SwapAmount(tokenAAmount), SwapAmount(tokenBAmount))
      }

      private def handleFailedUpdate(
        updates: List[AmmUpdate],
        acc: DataState[AmmOnChainState, AmmCalculatedState],
        failedCalculatedState: FailedCalculatedState,
        withdrawalCalculatedState: WithdrawalCalculatedState
      ) = {
        val updatedWithdrawalCalculatedState = withdrawalCalculatedState
          .focus(_.failed)
          .modify(_ + failedCalculatedState)

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
        case spendAction @ PendingSpendAction(update, _) if update =!= signedWithdrawalUpdate => spendAction
      }

      private def updateLiquidityPool(
        signedUpdate: Signed[WithdrawalUpdate],
        liquidityPool: LiquidityPool,
        withdrawalAmounts: WithdrawalTokenAmounts,
        lastSyncGlobalEpochProgress: EpochProgress
      ): Either[FailedCalculatedState, LiquidityPool] = {
        val tokenAValue = liquidityPool.tokenA.amount.value - withdrawalAmounts.tokenAAmount.value
        val tokenBValue = liquidityPool.tokenB.amount.value - withdrawalAmounts.tokenBAmount.value
        val totalSharesValue = liquidityPool.poolShares.totalShares.value - signedUpdate.shareToWithdraw.value.value
        val expireEpochProgress = getExpireEpochProgress(lastSyncGlobalEpochProgress)

        for {
          tokenAAmount <- PosLong
            .from(tokenAValue)
            .leftMap(_ =>
              FailedCalculatedState(
                ArithmeticError(s"Updated token A amount $tokenAValue is not positive"),
                expireEpochProgress,
                signedUpdate
              )
            )
          tokenBAmount <- PosLong
            .from(tokenBValue)
            .leftMap(_ =>
              FailedCalculatedState(
                ArithmeticError(s"Updated token B amount $tokenBValue is not positive"),
                expireEpochProgress,
                signedUpdate
              )
            )
          totalSharesAmount <- PosLong
            .from(totalSharesValue)
            .leftMap(_ =>
              FailedCalculatedState(
                ArithmeticError(s"Updated total shares $totalSharesValue is not positive"),
                expireEpochProgress,
                signedUpdate
              )
            )
          addressSharesAmount = liquidityPool.poolShares.addressShares.getOrElse(signedUpdate.source, ShareAmount(Amount.empty))
          sharesDifference = addressSharesAmount.value.value - signedUpdate.shareToWithdraw.value.value
          updatedAddressSharesAmount <- NonNegLong
            .from(sharesDifference)
            .leftMap(_ =>
              FailedCalculatedState(
                ArithmeticError(s"Updated address shares amount $sharesDifference is negative"),
                expireEpochProgress,
                signedUpdate
              )
            )
            .map(nonNeg => ShareAmount(Amount(nonNeg)))
          updatedAddressShares =
            if (updatedAddressSharesAmount.value === Amount.empty) {
              liquidityPool.poolShares.addressShares - signedUpdate.source
            } else {
              liquidityPool.poolShares.addressShares.updated(signedUpdate.source, updatedAddressSharesAmount)
            }

          k = BigInt(tokenAAmount.value) * BigInt(tokenBAmount.value)
        } yield
          liquidityPool.copy(
            tokenA = liquidityPool.tokenA.copy(amount = tokenAAmount),
            tokenB = liquidityPool.tokenB.copy(amount = tokenBAmount),
            k = k,
            poolShares = PoolShares(
              totalSharesAmount,
              updatedAddressShares,
              liquidityPool.poolShares.feeShares
            )
          )
      }

      private def validateUpdate(
        signedUpdate: Signed[WithdrawalUpdate],
        lastSyncGlobalEpochProgress: EpochProgress
      ): Either[FailedCalculatedState, Signed[WithdrawalUpdate]] = {
        val expireEpochProgress = getExpireEpochProgress(lastSyncGlobalEpochProgress)

        Either.cond(
          signedUpdate.maxValidGsEpochProgress >= lastSyncGlobalEpochProgress,
          signedUpdate,
          FailedCalculatedState(
            OperationExpired(signedUpdate),
            expireEpochProgress,
            signedUpdate
          )
        )
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
          _ <- EitherT.fromEither(validateUpdate(signedUpdate, globalEpochProgress))
          poolId <- EitherT.liftF(buildLiquidityPoolUniqueIdentifier(withdrawalUpdate.tokenAId, withdrawalUpdate.tokenBId))
          liquidityPool <- EitherT.liftF(getLiquidityPoolByPoolId(liquidityPoolsCalculatedState.confirmed.value, poolId))
          withdrawalAmounts <- EitherT.fromEither[F](calculateWithdrawalAmounts(signedUpdate, liquidityPool, globalEpochProgress))
          spendAction = generateSpendActionWithoutAllowSpends(
            signedUpdate.tokenAId,
            withdrawalAmounts.tokenAAmount,
            signedUpdate.tokenBId,
            withdrawalAmounts.tokenBAmount,
            signedUpdate.source,
            currencyId
          )
          updatedPendingWithdrawalCalculatedState = withdrawalCalculatedState
            .focus(_.pending)
            .replace(withdrawalCalculatedState.pending + PendingSpendAction(signedUpdate, spendAction))
          updatedCalculatedState = oldState.calculated
            .focus(_.operations)
            .modify(_.updated(OperationType.Withdrawal, updatedPendingWithdrawalCalculatedState))
          updatedSharedArtifacts = oldState.sharedArtifacts + spendAction

        } yield
          DataState(
            AmmOnChainState(updates),
            updatedCalculatedState,
            updatedSharedArtifacts
          )

        combinedState.valueOr(failedCalculatedState =>
          handleFailedUpdate(updates, oldState, failedCalculatedState, withdrawalCalculatedState)
        )
      }

      override def combinePendingSpendAction(
        pendingSpendAction: PendingSpendAction[WithdrawalUpdate],
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        globalEpochProgress: EpochProgress,
        spendActions: List[SpendAction],
        currentSnapshotOrdinal: SnapshotOrdinal
      )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
        val liquidityPoolsCalculatedState = getLiquidityPoolCalculatedState(oldState.calculated)
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
                _ <- EitherT.fromEither(validateUpdate(signedWithdrawalUpdate, globalEpochProgress))
                poolId <- EitherT.liftF(buildLiquidityPoolUniqueIdentifier(withdrawalUpdate.tokenAId, withdrawalUpdate.tokenBId))
                liquidityPool <- EitherT.liftF(getLiquidityPoolByPoolId(liquidityPoolsCalculatedState.confirmed.value, poolId))
                withdrawalReference <- EitherT.liftF(
                  HasherSelector[F].withCurrent(implicit hs => WithdrawalReference.of(signedWithdrawalUpdate))
                )
                withdrawalAmounts <- EitherT.fromEither[F](
                  calculateWithdrawalAmounts(
                    signedWithdrawalUpdate,
                    liquidityPool,
                    globalEpochProgress
                  )
                )
                updatedPool <- EitherT.fromEither[F](
                  updateLiquidityPool(
                    signedWithdrawalUpdate,
                    liquidityPool,
                    withdrawalAmounts,
                    globalEpochProgress
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
                newLiquidityPoolState = liquidityPoolsCalculatedState
                  .focus(_.confirmed.value)
                  .modify(_.updated(poolId.value, updatedPool))
                updatedCalculatedState = oldState.calculated
                  .focus(_.operations)
                  .modify(_.updated(OperationType.Withdrawal, newWithdrawalState))
                  .focus(_.operations)
                  .modify(_.updated(OperationType.LiquidityPool, newLiquidityPoolState))
              } yield
                DataState[AmmOnChainState, AmmCalculatedState](
                  AmmOnChainState(updates),
                  updatedCalculatedState,
                  oldState.sharedArtifacts
                )

              processingState.valueOr(failedCalculatedState =>
                handleFailedUpdate(updates, oldState, failedCalculatedState, withdrawalCalculatedState)
              )
            }
        } yield combinedState
      }
    }
}
