package org.amm_metagraph.shared_data.services.combiners.operations

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
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

trait WithdrawalCombinerService[F[_]] {
  def combineNew(
    signedUpdate: Signed[WithdrawalUpdate],
    oldState: DataState[AmmOnChainState, AmmCalculatedState],
    globalEpochProgress: EpochProgress,
    lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
    currentSnapshotOrdinal: SnapshotOrdinal,
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
      def logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

      private def handleFailedUpdate(
        withdrawalUpdate: Signed[WithdrawalUpdate],
        acc: DataState[AmmOnChainState, AmmCalculatedState],
        failedCalculatedState: FailedCalculatedState,
        withdrawalCalculatedState: WithdrawalCalculatedState,
        updateHash: Hash,
        currentState: StateTransitionType
      ): F[DataState[AmmOnChainState, AmmCalculatedState]] =
        failedCalculatedState.reason match {
          case DuplicatedUpdate(_) =>
            logger.warn("Duplicated data update, ignoring") >> acc.pure[F]
          case _ =>
            val updatedWithdrawalCalculatedState = withdrawalCalculatedState
              .focus(_.failed)
              .modify(_ + failedCalculatedState)
              .focus(_.pending)
              .modify(_.filter(_.update =!= withdrawalUpdate))

            val updatedCalculatedState = acc.calculated
              .focus(_.operations)
              .modify(_.updated(OperationType.Withdrawal, updatedWithdrawalCalculatedState))

            val dataUpdate = UpdatedStateDataUpdate(
              currentState,
              Failed,
              OperationType.Withdrawal,
              withdrawalUpdate.asInstanceOf[Signed[AmmUpdate]],
              updateHash,
              None,
              Some(failedCalculatedState.reason)
            )

            val result = acc
              .focus(_.onChain.updatedStateDataUpdate)
              .modify(_ + dataUpdate)
              .focus(_.calculated)
              .replace(updatedCalculatedState)

            result.pure[F]
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
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        currentSnapshotOrdinal: SnapshotOrdinal
      ): F[DataState[AmmOnChainState, AmmCalculatedState]] = maybePricingTokenInfo match {
        case Some(WithdrawalTokenInfo(tokenAIdentifier, tokenAAmount, tokenBIdentifier, tokenBAmount)) =>
          for {
            _ <- logger.debug(s"Rolling back withdrawal liquidity pool amounts for ${signedUpdate.source}")
            poolId <- buildLiquidityPoolUniqueIdentifier(tokenAIdentifier, tokenBIdentifier)
            liquidityPoolCalculatedState = getLiquidityPoolCalculatedState(oldState.calculated)
            liquidityPool <- getLiquidityPoolByPoolId(liquidityPoolCalculatedState.confirmed.value, poolId)

            rollbackResult <- pricingService
              .rollbackWithdrawalLiquidityPoolAmounts(
                signedUpdate,
                updateHash,
                lastSyncGlobalSnapshotEpoch,
                liquidityPool,
                tokenAAmount,
                tokenBAmount,
                currentSnapshotOrdinal
              )
              .flatMap {
                case Right(updatedLiquidityPool) =>
                  val newLiquidityPoolState = liquidityPoolCalculatedState
                    .focus(_.confirmed.value)
                    .modify(_.updated(poolId.value, updatedLiquidityPool))

                  val updatedCalculatedState = oldState.calculated
                    .focus(_.operations)
                    .modify(_.updated(OperationType.LiquidityPool, newLiquidityPoolState))

                  val updatedState = oldState.copy(calculated = updatedCalculatedState)
                  logger.debug(s"Successfully rolled back withdrawal liquidity pool amounts") >> updatedState.pure[F]
                case Left(error) =>
                  logger.warn(s"Failed to rollback withdrawal amounts: $error") >> oldState.pure[F]
              }
          } yield rollbackResult
        case _ =>
          logger.debug("No withdrawal token info available for rollback") >> oldState.pure[F]
      }

      override def combineNew(
        signedUpdate: Signed[WithdrawalUpdate],
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        globalEpochProgress: EpochProgress,
        lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
        currentSnapshotOrdinal: SnapshotOrdinal,
        currencyId: CurrencyId
      )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
        val withdrawalUpdate = signedUpdate.value
        val withdrawalCalculatedState = getWithdrawalCalculatedState(oldState.calculated)
        val liquidityPoolsCalculatedState = getLiquidityPoolCalculatedState(oldState.calculated)

        for {
          _ <- logger.info(
            s"Processing new withdrawal update from ${signedUpdate.source}: shareToWithdraw=${withdrawalUpdate.shareToWithdraw}"
          )

          // Early return if already pending
          result <-
            if (withdrawalCalculatedState.pending.exists(_.update === signedUpdate)) {
              logger.warn(s"Withdrawal update ${signedUpdate.source} already pending, ignoring") >> oldState.pure[F]
            } else {
              val updateHashedF = HasherSelector[F].withCurrent(implicit hs => signedUpdate.toHashed(dataUpdateCodec.serialize))

              val combinedState = for {
                _ <- EitherT.liftF[F, FailedCalculatedState, Unit](logger.debug(s"Starting validation for withdrawal update"))

                _ <- EitherT(
                  withdrawalValidations.l0Validations(
                    signedUpdate,
                    oldState.calculated,
                    globalEpochProgress
                  )
                ).leftWiden[FailedCalculatedState]

                poolId <- EitherT.liftF[F, FailedCalculatedState, PoolId](
                  buildLiquidityPoolUniqueIdentifier(withdrawalUpdate.tokenAId, withdrawalUpdate.tokenBId)
                )
                liquidityPool <- EitherT.liftF[F, FailedCalculatedState, LiquidityPool](
                  getLiquidityPoolByPoolId(liquidityPoolsCalculatedState.confirmed.value, poolId)
                )
                updateHashed <- EitherT
                  .liftF[F, FailedCalculatedState, io.constellationnetwork.security.Hashed[WithdrawalUpdate]](updateHashedF)

                _ <- EitherT.liftF[F, FailedCalculatedState, Unit](logger.debug(s"Getting withdrawal token info for pool ${poolId}"))
                withdrawalAmounts <- EitherT.fromEither[F](
                  pricingService.getWithdrawalTokenInfo(
                    signedUpdate,
                    updateHashed.hash,
                    liquidityPool,
                    globalEpochProgress
                  )
                )

                _ <- EitherT.liftF[F, FailedCalculatedState, Unit](
                  logger.debug(
                    s"Withdrawal amounts calculated: tokenA=${withdrawalAmounts.tokenAAmount}, tokenB=${withdrawalAmounts.tokenBAmount}"
                  )
                )
                updatedPool <- EitherT(
                  pricingService.getUpdatedLiquidityPoolDueNewWithdrawal(
                    signedUpdate,
                    updateHashed.hash,
                    liquidityPool,
                    withdrawalAmounts,
                    globalEpochProgress,
                    currentSnapshotOrdinal
                  )
                )

                _ <- EitherT(
                  withdrawalValidations.newUpdateValidations(
                    signedUpdate,
                    withdrawalAmounts,
                    globalEpochProgress,
                    updatedPool
                  )
                ).leftWiden[FailedCalculatedState]

                spendAction = generateSpendActionWithoutAllowSpends(
                  signedUpdate.value.tokenAId,
                  withdrawalAmounts.tokenAAmount,
                  signedUpdate.value.tokenBId,
                  withdrawalAmounts.tokenBAmount,
                  signedUpdate.source,
                  currencyId
                )

                newLiquidityPoolState = liquidityPoolsCalculatedState
                  .focus(_.confirmed.value)
                  .modify(_.updated(poolId.value, updatedPool))

                pendingAllowSpend = PendingSpendAction(signedUpdate, updateHashed.hash, spendAction, Some(withdrawalAmounts))

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

                dataUpdate = UpdatedStateDataUpdate(
                  NewUpdate,
                  PendingAllowSpends,
                  OperationType.Withdrawal,
                  signedUpdate.asInstanceOf[Signed[AmmUpdate]],
                  updateHashed.hash,
                  Some(pendingAllowSpend.asInstanceOf[PendingAction[AmmUpdate]])
                )

                _ <- EitherT.liftF[F, FailedCalculatedState, Unit](logger.debug(s"Successfully created pending withdrawal spend action"))

              } yield
                oldState
                  .focus(_.onChain.updatedStateDataUpdate)
                  .modify(_ + dataUpdate)
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
                  for {
                    _ <- logger.error(s"Failed to process withdrawal update: ${failed.reason}")
                    updateHashed <- updateHashedF
                    result <- handleFailedUpdate(signedUpdate, oldState, failed, withdrawalCalculatedState, updateHashed.hash, NewUpdate)
                  } yield result,
                success => logger.info("Successfully processed withdrawal update") >> success.pure[F]
              )
            }
        } yield result
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
          _ <- logger.info(
            s"Processing withdrawal spend action from ${signedWithdrawalUpdate.source}: shareToWithdraw=${withdrawalUpdate.shareToWithdraw}"
          )

          metagraphGeneratedSpendActionHash <- HasherSelector[F].withCurrent(implicit hs =>
            Hasher[F].hash(pendingSpendAction.generatedSpendAction)
          )
          globalSnapshotsHashes <- HasherSelector[F].withCurrent(implicit hs => spendActions.traverse(action => Hasher[F].hash(action)))
          allSpendActionsAccepted = checkIfSpendActionAcceptedInGl0(metagraphGeneratedSpendActionHash, globalSnapshotsHashes)

          _ <- logger.debug(s"Spend action accepted: $allSpendActionsAccepted")

          combinedState <-
            if (!allSpendActionsAccepted) {
              if (pendingSpendAction.update.value.maxValidGsEpochProgress <= globalEpochProgress) {
                logger.warn(s"Withdrawal spend action expired for ${signedWithdrawalUpdate.source}") >> {
                  val failedState = FailedCalculatedState(
                    OperationExpired(pendingSpendAction.update),
                    getFailureExpireEpochProgress(applicationConfig, globalEpochProgress),
                    pendingSpendAction.updateHash,
                    pendingSpendAction.update
                  )
                  handleFailedUpdate(
                    pendingSpendAction.update,
                    oldState,
                    failedState,
                    withdrawalCalculatedState,
                    pendingSpendAction.updateHash,
                    PendingSpendTransactions
                  )
                }
              } else {
                logger.debug(s"Withdrawal spend action not yet expired, keeping pending") >> oldState.pure[F]
              }
            } else {
              logger.info(s"Processing confirmed withdrawal spend action for ${signedWithdrawalUpdate.source}")
              val processingState = for {
                _ <- EitherT(
                  withdrawalValidations.pendingSpendActionsValidation(
                    signedWithdrawalUpdate,
                    globalEpochProgress
                  )
                ).leftWiden[FailedCalculatedState]

                withdrawalReference <- EitherT.liftF[F, FailedCalculatedState, WithdrawalReference](
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
                  ): FailedCalculatedState
                )

                _ <- EitherT.liftF[F, FailedCalculatedState, Unit](
                  logger.debug(
                    s"Creating withdrawal calculated state for ${withdrawalAmounts.tokenAAmount}/${withdrawalAmounts.tokenBAmount}"
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

                dataUpdate = UpdatedStateDataUpdate(
                  PendingSpendTransactions,
                  Confirmed,
                  OperationType.Withdrawal,
                  pendingSpendAction.update.asInstanceOf[Signed[AmmUpdate]],
                  pendingSpendAction.updateHash,
                  None
                )

                _ <- EitherT.liftF[F, FailedCalculatedState, Unit](
                  logger.debug(s"Successfully confirmed withdrawal for ${signedWithdrawalUpdate.source}")
                )
              } yield
                oldState
                  .focus(_.onChain.updatedStateDataUpdate)
                  .modify(_ + dataUpdate)
                  .focus(_.calculated)
                  .replace(updatedCalculatedState)

              processingState.foldF(
                failed =>
                  for {
                    _ <- logger.error(s"Failed to process withdrawal spend action: ${failed.reason}")
                    rolledBackState <- rollbackAmountInLPs(
                      pendingSpendAction.update,
                      pendingSpendAction.updateHash,
                      globalEpochProgress,
                      pendingSpendAction.pricingTokenInfo,
                      oldState,
                      currentSnapshotOrdinal
                    )
                    result <- handleFailedUpdate(
                      pendingSpendAction.update,
                      rolledBackState,
                      failed,
                      withdrawalCalculatedState,
                      pendingSpendAction.updateHash,
                      PendingSpendTransactions
                    )
                  } yield result,
                success => logger.info("Successfully processed withdrawal spend action") >> success.pure[F]
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
