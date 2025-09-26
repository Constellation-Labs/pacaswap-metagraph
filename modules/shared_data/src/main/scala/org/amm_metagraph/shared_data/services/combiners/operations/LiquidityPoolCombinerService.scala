package org.amm_metagraph.shared_data.services.combiners.operations

import cats.data.EitherT
import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact.{SharedArtifact, SpendAction}
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.{AllowSpend, CurrencyId, SwapAmount}
import io.constellationnetwork.security.Hasher
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.PosLong
import monocle.syntax.all._
import org.amm_metagraph.shared_data.FeeDistributor
import org.amm_metagraph.shared_data.SpendTransactions.{checkIfSpendActionAcceptedInGl0, generateSpendAction}
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.epochProgress.getFailureExpireEpochProgress
import org.amm_metagraph.shared_data.globalSnapshots.{getAllowSpendsGlobalSnapshotsState, logger}
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, LiquidityPoolUpdate}
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States.StateTransitionType._
import org.amm_metagraph.shared_data.types.States.{OperationType, _}
import org.amm_metagraph.shared_data.types.codecs.{HasherSelector, JsonWithBase64BinaryCodec}
import org.amm_metagraph.shared_data.validations.Errors.{DuplicatedUpdate, OperationExpired}
import org.amm_metagraph.shared_data.validations.LiquidityPoolValidations
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
    pendingSignedUpdate: PendingAllowSpend[LiquidityPoolUpdate],
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
    currentSnapshotOrdinal: SnapshotOrdinal,
    currencyId: CurrencyId
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]]

  def cleanupExpiredOperations(
    oldState: DataState[AmmOnChainState, AmmCalculatedState],
    globalEpochProgress: EpochProgress
  ): DataState[AmmOnChainState, AmmCalculatedState]
}

object LiquidityPoolCombinerService {
  def make[F[_]: Async: HasherSelector](
    applicationConfig: ApplicationConfig,
    liquidityPoolValidations: LiquidityPoolValidations[F],
    dataUpdateCodec: JsonWithBase64BinaryCodec[F, AmmUpdate]
  ): LiquidityPoolCombinerService[F] =
    new LiquidityPoolCombinerService[F] {
      def logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

      private def handleFailedUpdate(
        liquidityPoolUpdate: Signed[LiquidityPoolUpdate],
        acc: DataState[AmmOnChainState, AmmCalculatedState],
        failedCalculatedState: FailedCalculatedState,
        liquidityPoolCalculatedState: LiquidityPoolCalculatedState,
        updateHash: Hash,
        currentState: StateTransitionType
      ): F[DataState[AmmOnChainState, AmmCalculatedState]] =
        failedCalculatedState.reason match {
          case DuplicatedUpdate(_) =>
            logger.warn("Duplicated data update, ignoring") >> acc.pure[F]
          case _ =>
            val updatedLiquidityPoolCalculatedState = liquidityPoolCalculatedState
              .focus(_.failed)
              .modify(_ + failedCalculatedState)
              .focus(_.pending)
              .modify(_.filter(_.update =!= liquidityPoolUpdate))

            val updatedCalculatedState = acc.calculated
              .focus(_.operations)
              .modify(_.updated(OperationType.LiquidityPool, updatedLiquidityPoolCalculatedState))

            val dataUpdate = UpdatedStateDataUpdate(
              currentState,
              Failed,
              OperationType.LiquidityPool,
              liquidityPoolUpdate.asInstanceOf[Signed[AmmUpdate]],
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

      private def getUpdateAllowSpends(
        liquidityPoolUpdate: LiquidityPoolUpdate,
        lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]]
      ): F[(Option[io.constellationnetwork.security.Hashed[AllowSpend]], Option[io.constellationnetwork.security.Hashed[AllowSpend]])] =
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
      ): SortedSet[PendingAction[LiquidityPoolUpdate]] =
        liquidityPoolsCalculatedState.pending.filterNot {
          case PendingAllowSpend(update, _, _) if update === signedLiquidityPoolUpdate => true
          case _                                                                       => false
        }

      private def removePendingSpendAction(
        liquidityPoolsCalculatedState: LiquidityPoolCalculatedState,
        signedLiquidityPoolUpdate: Signed[LiquidityPoolUpdate]
      ): SortedSet[PendingAction[LiquidityPoolUpdate]] =
        liquidityPoolsCalculatedState.pending.filterNot {
          case PendingSpendAction(update, _, _, _) if update === signedLiquidityPoolUpdate => true
          case _                                                                           => false
        }

      def combineNew(
        signedUpdate: Signed[LiquidityPoolUpdate],
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        globalEpochProgress: EpochProgress,
        lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
        currencyId: CurrencyId
      )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
        val liquidityPoolUpdate = signedUpdate.value
        val liquidityPoolsCalculatedState = getLiquidityPoolCalculatedState(oldState.calculated)
        val confirmedLps = getConfirmedLiquidityPools(oldState.calculated)

        val pendingAllowSpendsCalculatedState = liquidityPoolsCalculatedState.pending
        val pendingLps = liquidityPoolsCalculatedState.getPendingUpdates
        val updateHashedF = HasherSelector[F].withCurrent(implicit hs => signedUpdate.toHashed(dataUpdateCodec.serialize))

        val combinedState = for {
          _ <- EitherT.liftF[F, FailedCalculatedState, Unit](
            logger.info(
              s"Processing new liquidity pool update from ${signedUpdate.source}: tokenA=${liquidityPoolUpdate.tokenAAmount}, tokenB=${liquidityPoolUpdate.tokenBAmount}"
            )
          )

          _ <- EitherT(
            liquidityPoolValidations.l0Validations(
              signedUpdate,
              oldState.calculated,
              globalEpochProgress
            )
          ).leftWiden[FailedCalculatedState]

          poolId <- EitherT.liftF[F, FailedCalculatedState, PoolId](
            buildLiquidityPoolUniqueIdentifier(signedUpdate.value.tokenAId, signedUpdate.value.tokenBId)
          )
          pendingLpsPoolsIds <- EitherT.liftF[F, FailedCalculatedState, List[PoolId]](
            pendingLps.toList.traverse(pendingLp => buildLiquidityPoolUniqueIdentifier(pendingLp.tokenAId, pendingLp.tokenBId))
          )
          updateHashed <- EitherT
            .liftF[F, FailedCalculatedState, io.constellationnetwork.security.Hashed[LiquidityPoolUpdate]](updateHashedF)
          _ <- EitherT(
            liquidityPoolValidations.newUpdateValidations(
              oldState.calculated,
              poolId,
              signedUpdate,
              globalEpochProgress,
              confirmedLps,
              pendingLpsPoolsIds
            )
          ).leftWiden[FailedCalculatedState]

          updateAllowSpends <- EitherT.liftF[
            F,
            FailedCalculatedState,
            (Option[io.constellationnetwork.security.Hashed[AllowSpend]], Option[io.constellationnetwork.security.Hashed[AllowSpend]])
          ](getUpdateAllowSpends(liquidityPoolUpdate, lastGlobalSnapshotsAllowSpends))
          response <- updateAllowSpends match {
            case (None, _) | (_, None) =>
              val pendingAllowSpend = PendingAllowSpend(
                signedUpdate,
                updateHashed.hash,
                None
              )

              val updatedPendingCalculatedState = pendingAllowSpendsCalculatedState + pendingAllowSpend
              val newStakingState = liquidityPoolsCalculatedState
                .focus(_.pending)
                .replace(updatedPendingCalculatedState)

              val updatedCalculatedState = oldState.calculated
                .focus(_.operations)
                .modify(_.updated(OperationType.LiquidityPool, newStakingState))

              val dataUpdate = UpdatedStateDataUpdate(
                NewUpdate,
                PendingAllowSpends,
                OperationType.LiquidityPool,
                signedUpdate.asInstanceOf[Signed[AmmUpdate]],
                updateHashed.hash,
                Some(pendingAllowSpend.asInstanceOf[PendingAction[AmmUpdate]])
              )

              EitherT.liftF[F, FailedCalculatedState, DataState[AmmOnChainState, AmmCalculatedState]](
                logger.warn(s"Allow spend not confirmed, storing as pending") >>
                  oldState
                    .focus(_.onChain.updatedStateDataUpdate)
                    .modify(_ + dataUpdate)
                    .focus(_.calculated)
                    .replace(updatedCalculatedState)
                    .pure[F]
              )
            case (Some(_), Some(_)) =>
              EitherT.liftF[F, FailedCalculatedState, DataState[AmmOnChainState, AmmCalculatedState]](
                logger.info(s"Allow spend confirmed, processing immediately") >>
                  combinePendingAllowSpend(
                    PendingAllowSpend(signedUpdate, updateHashed.hash, None),
                    oldState,
                    globalEpochProgress,
                    lastGlobalSnapshotsAllowSpends,
                    currencyId
                  )
              )
          }
        } yield response

        combinedState.foldF(
          failed =>
            for {
              _ <- logger.error(s"Failed to process liquidity pool update: ${failed.reason}")
              updateHashed <- updateHashedF
              result <- handleFailedUpdate(signedUpdate, oldState, failed, liquidityPoolsCalculatedState, updateHashed.hash, NewUpdate)
            } yield result,
          success => logger.info("Successfully processed liquidity pool update") >> success.pure[F]
        )
      }

      def combinePendingAllowSpend(
        pendingAllowSpendUpdate: PendingAllowSpend[LiquidityPoolUpdate],
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        globalEpochProgress: EpochProgress,
        lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
        currencyId: CurrencyId
      )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
        val liquidityPoolsCalculatedState = getLiquidityPoolCalculatedState(oldState.calculated)
        val liquidityPoolUpdate = pendingAllowSpendUpdate.update.value

        val combinedState: EitherT[F, FailedCalculatedState, DataState[AmmOnChainState, AmmCalculatedState]] = for {
          _ <- EitherT.liftF[F, FailedCalculatedState, Unit](logger.info(s"Processing pending allow spend for liquidity pool"))
          updateAllowSpends <- EitherT.liftF[
            F,
            FailedCalculatedState,
            (Option[io.constellationnetwork.security.Hashed[AllowSpend]], Option[io.constellationnetwork.security.Hashed[AllowSpend]])
          ](getUpdateAllowSpends(liquidityPoolUpdate, lastGlobalSnapshotsAllowSpends))
          result <- updateAllowSpends match {
            case (Some(allowSpendTokenA), Some(allowSpendTokenB)) =>
              for {
                _ <- EitherT(
                  liquidityPoolValidations.pendingAllowSpendsValidations(
                    pendingAllowSpendUpdate.update,
                    globalEpochProgress,
                    currencyId,
                    allowSpendTokenA,
                    allowSpendTokenB
                  )
                ).leftWiden[FailedCalculatedState]
                amountToSpendA = SwapAmount(liquidityPoolUpdate.tokenAAmount)
                amountToSpendB = SwapAmount(liquidityPoolUpdate.tokenBAmount)

                spendAction = generateSpendAction(
                  allowSpendTokenA,
                  amountToSpendA,
                  allowSpendTokenB,
                  amountToSpendB
                )

                updatedPendingAllowSpendCalculatedState =
                  removePendingAllowSpend(liquidityPoolsCalculatedState, pendingAllowSpendUpdate.update)
                pendingSpendAction = PendingSpendAction(
                  pendingAllowSpendUpdate.update,
                  pendingAllowSpendUpdate.updateHash,
                  spendAction,
                  None
                )

                updatedPendingSpendActionCalculatedState = updatedPendingAllowSpendCalculatedState + pendingSpendAction

                updatedLiquidityPoolCalculatedState =
                  liquidityPoolsCalculatedState
                    .focus(_.pending)
                    .replace(updatedPendingSpendActionCalculatedState)

                updatedCalculatedState = oldState.calculated
                  .focus(_.operations)
                  .modify(_.updated(OperationType.LiquidityPool, updatedLiquidityPoolCalculatedState))

                dataUpdate = UpdatedStateDataUpdate(
                  PendingAllowSpends,
                  PendingSpendTransactions,
                  OperationType.LiquidityPool,
                  pendingAllowSpendUpdate.update.asInstanceOf[Signed[AmmUpdate]],
                  pendingAllowSpendUpdate.updateHash,
                  Some(pendingSpendAction.asInstanceOf[PendingAction[AmmUpdate]])
                )

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
            case _ =>
              if (pendingAllowSpendUpdate.update.value.maxValidGsEpochProgress <= globalEpochProgress) {
                EitherT.leftT[F, DataState[AmmOnChainState, AmmCalculatedState]](
                  FailedCalculatedState(
                    OperationExpired(pendingAllowSpendUpdate.update),
                    getFailureExpireEpochProgress(applicationConfig, globalEpochProgress),
                    pendingAllowSpendUpdate.updateHash,
                    pendingAllowSpendUpdate.update
                  )
                )
              } else {
                EitherT.rightT[F, FailedCalculatedState](oldState)
              }
          }
        } yield result

        combinedState.foldF(
          failed =>
            for {
              _ <- logger.error(s"Failed to process pending allow spend: ${failed.reason}")
              result <- handleFailedUpdate(
                pendingAllowSpendUpdate.update,
                oldState,
                failed,
                liquidityPoolsCalculatedState,
                pendingAllowSpendUpdate.updateHash,
                PendingAllowSpends
              )
            } yield result,
          success => logger.info("Successfully processed pending allow spend") >> success.pure[F]
        )
      }

      def combinePendingSpendAction(
        pendingSpendAction: PendingSpendAction[LiquidityPoolUpdate],
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        globalEpochProgress: EpochProgress,
        spendActions: List[SpendAction],
        currentSnapshotOrdinal: SnapshotOrdinal,
        currencyId: CurrencyId
      )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
        val liquidityPoolsCalculatedState = getLiquidityPoolCalculatedState(oldState.calculated)

        val signedLiquidityPoolUpdate = pendingSpendAction.update
        val liquidityPoolUpdate = pendingSpendAction.update.value
        val combinedState: EitherT[F, FailedCalculatedState, DataState[AmmOnChainState, AmmCalculatedState]] = for {
          _ <- EitherT.liftF[F, FailedCalculatedState, Unit](
            logger.info(
              s"Processing liquidity pool spend action from ${signedLiquidityPoolUpdate.source}: tokenA=${liquidityPoolUpdate.tokenAAmount}, tokenB=${liquidityPoolUpdate.tokenBAmount}"
            )
          )

          poolId <- EitherT.liftF[F, FailedCalculatedState, PoolId](
            buildLiquidityPoolUniqueIdentifier(signedLiquidityPoolUpdate.value.tokenAId, signedLiquidityPoolUpdate.value.tokenBId)
          )
          _ <- EitherT(
            liquidityPoolValidations.pendingSpendActionsValidation(
              signedLiquidityPoolUpdate,
              globalEpochProgress
            )
          ).leftWiden[FailedCalculatedState]
          sourceAddress = liquidityPoolUpdate.source
          metagraphGeneratedSpendActionHash <- EitherT.liftF[F, FailedCalculatedState, Hash](
            HasherSelector[F].withCurrent(implicit hs => Hasher[F].hash(pendingSpendAction.generatedSpendAction))
          )
          globalSnapshotsHashes <- EitherT.liftF[F, FailedCalculatedState, List[Hash]](
            HasherSelector[F].withCurrent(implicit hs => spendActions.traverse(action => Hasher[F].hash(action)))
          )
          allSpendActionsAccepted <- EitherT.liftF[F, FailedCalculatedState, Boolean] {
            checkIfSpendActionAcceptedInGl0(metagraphGeneratedSpendActionHash, globalSnapshotsHashes).pure[F]
          }

          fees = liquidityPoolUpdate.poolFees.getOrElse(FeeDistributor.standard)
          result <-
            if (!allSpendActionsAccepted) {
              if (pendingSpendAction.update.value.maxValidGsEpochProgress <= globalEpochProgress) {
                EitherT.leftT[F, DataState[AmmOnChainState, AmmCalculatedState]](
                  FailedCalculatedState(
                    OperationExpired(pendingSpendAction.update),
                    getFailureExpireEpochProgress(applicationConfig, globalEpochProgress),
                    pendingSpendAction.updateHash,
                    pendingSpendAction.update
                  )
                )
              } else {
                EitherT.rightT[F, FailedCalculatedState](oldState)
              }
            } else {
              for {
                _ <- EitherT.liftF[F, FailedCalculatedState, Unit](logger.debug(s"Creating new liquidity pool with poolId: ${poolId}"))

                amountA = liquidityPoolUpdate.tokenAAmount.value
                amountB = liquidityPoolUpdate.tokenBAmount.value
                poolTotalShares: PosLong = 1.toTokenAmountFormat.toPosLongUnsafe

                liquidityPool = LiquidityPool(
                  pendingSpendAction.updateHash,
                  poolId,
                  TokenInformation(
                    liquidityPoolUpdate.tokenAId,
                    liquidityPoolUpdate.tokenAAmount
                  ),
                  TokenInformation(
                    liquidityPoolUpdate.tokenBId,
                    liquidityPoolUpdate.tokenBAmount
                  ),
                  sourceAddress,
                  BigInt(amountA) * BigInt(amountB),
                  PoolShares(
                    poolTotalShares,
                    Map(sourceAddress -> ShareAmount(Amount(poolTotalShares)))
                  ),
                  fees
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

                dataUpdate = UpdatedStateDataUpdate(
                  PendingSpendTransactions,
                  Confirmed,
                  OperationType.LiquidityPool,
                  pendingSpendAction.update.asInstanceOf[Signed[AmmUpdate]],
                  pendingSpendAction.updateHash,
                  None
                )

                _ <- EitherT.liftF[F, FailedCalculatedState, Unit](logger.debug(s"Successfully created liquidity pool ${poolId}"))

                result = oldState
                  .focus(_.onChain.updatedStateDataUpdate)
                  .modify(_ + dataUpdate)
                  .focus(_.calculated)
                  .replace(updatedCalculatedState)
              } yield result
            }
        } yield result

        combinedState.foldF(
          failed =>
            for {
              _ <- logger.error(s"Failed to process liquidity pool spend action: ${failed.reason}")
              result <- handleFailedUpdate(
                pendingSpendAction.update,
                oldState,
                failed,
                liquidityPoolsCalculatedState,
                pendingSpendAction.updateHash,
                PendingSpendTransactions
              )
            } yield result,
          success => logger.info("Successfully processed liquidity pool spend action") >> success.pure[F]
        )
      }

      def cleanupExpiredOperations(
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        globalEpochProgress: EpochProgress
      ): DataState[AmmOnChainState, AmmCalculatedState] = {
        val liquidityPoolCalculatedState = getLiquidityPoolCalculatedState(oldState.calculated)
        val unexpiredFailed = liquidityPoolCalculatedState.failed.filter(_.expiringEpochProgress > globalEpochProgress)

        val updatedLiquidityPoolCalculatedState = liquidityPoolCalculatedState
          .focus(_.failed)
          .replace(unexpiredFailed)

        oldState
          .focus(_.calculated.operations)
          .modify(_.updated(OperationType.LiquidityPool, updatedLiquidityPoolCalculatedState))
      }
    }
}
