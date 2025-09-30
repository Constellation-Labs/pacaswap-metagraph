package org.amm_metagraph.shared_data.services.combiners.operations

import cats.data.EitherT
import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact.{SharedArtifact, SpendAction}
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.{AllowSpend, CurrencyId}
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hashed, Hasher}

import monocle.syntax.all._
import org.amm_metagraph.shared_data.SpendTransactions.{checkIfSpendActionAcceptedInGl0, generateSpendAction}
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.epochProgress.{getConfirmedExpireEpochProgress, getFailureExpireEpochProgress}
import org.amm_metagraph.shared_data.globalSnapshots.{getAllowSpendGlobalSnapshotsState, logger}
import org.amm_metagraph.shared_data.services.pricing.PricingService
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States.StateTransitionType._
import org.amm_metagraph.shared_data.types.States.SwapFailedCalculatedState.fromFailedCalculatedState
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.Swap._
import org.amm_metagraph.shared_data.types.codecs.{HasherSelector, JsonWithBase64BinaryCodec}
import org.amm_metagraph.shared_data.validations.Errors._
import org.amm_metagraph.shared_data.validations.SwapValidations
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

trait SwapCombinerService[F[_]] {
  def combineNew(
    signedUpdate: Signed[SwapUpdate],
    oldState: DataState[AmmOnChainState, AmmCalculatedState],
    globalEpochProgress: EpochProgress,
    lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
    currentSnapshotOrdinal: SnapshotOrdinal,
    currencyId: CurrencyId
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]]

  def combinePendingAllowSpend(
    pendingSignedUpdate: PendingAllowSpend[SwapUpdate],
    oldState: DataState[AmmOnChainState, AmmCalculatedState],
    globalEpochProgress: EpochProgress,
    lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
    currentSnapshotOrdinal: SnapshotOrdinal,
    currencyId: CurrencyId
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]]

  def combinePendingSpendAction(
    pendingSpendAction: PendingSpendAction[SwapUpdate],
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

object SwapCombinerService {
  def make[F[_]: Async: HasherSelector](
    applicationConfig: ApplicationConfig,
    pricingService: PricingService[F],
    swapValidations: SwapValidations[F],
    dataUpdateCodec: JsonWithBase64BinaryCodec[F, AmmUpdate]
  ): SwapCombinerService[F] =
    new SwapCombinerService[F] {
      def logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

      private def hashUpdate(update: Signed[SwapUpdate]): F[Hash] =
        HasherSelector[F].withCurrent(implicit hs => update.toHashed(dataUpdateCodec.serialize).map(_.hash))

      private def getUpdateAllowSpend(
        swapUpdate: SwapUpdate,
        lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]]
      ): F[Option[Hashed[AllowSpend]]] =
        HasherSelector[F].withBrotli { implicit hs =>
          getAllowSpendGlobalSnapshotsState(
            swapUpdate.allowSpendReference,
            swapUpdate.swapFromPair,
            lastGlobalSnapshotsAllowSpends
          )
        }

      private def updateStateWithDataUpdate(
        state: DataState[AmmOnChainState, AmmCalculatedState],
        update: UpdatedStateDataUpdate
      ): DataState[AmmOnChainState, AmmCalculatedState] =
        state.focus(_.onChain.updatedStateDataUpdate).modify(_ + update)

      private def updateSwapState(
        state: DataState[AmmOnChainState, AmmCalculatedState],
        newSwapState: SwapCalculatedState
      ): DataState[AmmOnChainState, AmmCalculatedState] =
        state.focus(_.calculated.operations).modify(_.updated(OperationType.Swap, newSwapState))

      private def updateLiquidityPoolState(
        state: DataState[AmmOnChainState, AmmCalculatedState],
        poolId: PoolId,
        liquidityPool: LiquidityPool
      ): DataState[AmmOnChainState, AmmCalculatedState] = {
        val liquidityPoolState = getLiquidityPoolCalculatedState(state.calculated)
          .focus(_.confirmed.value)
          .modify(_.updated(poolId.value, liquidityPool))

        state
          .focus(_.calculated.operations)
          .modify(_.updated(OperationType.LiquidityPool, liquidityPoolState))
      }

      private def createPendingAllowSpendUpdate(
        signedUpdate: Signed[SwapUpdate],
        updateHash: Hash,
        tokenInfo: Option[PricingTokenInfo]
      ): UpdatedStateDataUpdate =
        UpdatedStateDataUpdate(
          NewUpdate,
          PendingAllowSpends,
          OperationType.Swap,
          signedUpdate.asInstanceOf[Signed[AmmUpdate]],
          updateHash,
          Some(PendingAllowSpend(signedUpdate, updateHash, tokenInfo).asInstanceOf[PendingAction[AmmUpdate]])
        )

      private def rollbackLiquidityPool(
        pendingAction: PendingAction[SwapUpdate],
        globalEpochProgress: EpochProgress,
        tokenInfo: Option[PricingTokenInfo],
        state: DataState[AmmOnChainState, AmmCalculatedState],
        currentSnapshotOrdinal: SnapshotOrdinal,
        currencyId: CurrencyId
      ): F[DataState[AmmOnChainState, AmmCalculatedState]] =
        tokenInfo.collect { case swapInfo: SwapTokenInfo => swapInfo } match {
          case Some(SwapTokenInfo(primary, pair, amountIn, grossReceived, _)) =>
            for {
              poolId <- buildLiquidityPoolUniqueIdentifier(primary.identifier, pair.identifier)
              liquidityPoolState = getLiquidityPoolCalculatedState(state.calculated)
              liquidityPool <- getLiquidityPoolByPoolId(liquidityPoolState.confirmed.value, poolId)
              updatedPool <- pricingService
                .rollbackSwapLiquidityPoolAmounts(
                  pendingAction.update,
                  pendingAction.updateHash,
                  globalEpochProgress,
                  liquidityPool,
                  amountIn,
                  grossReceived,
                  currencyId,
                  currentSnapshotOrdinal
                )
                .map(_.fold(_ => state, pool => updateLiquidityPoolState(state, poolId, pool)))
            } yield updatedPool
          case None => state.pure[F]
        }

      private def handleFailure(
        signedUpdate: Signed[SwapUpdate],
        state: DataState[AmmOnChainState, AmmCalculatedState],
        failed: FailedCalculatedState,
        swapState: SwapCalculatedState,
        updateHash: Hash,
        tokenInfo: Option[SwapTokenInfo],
        transitionType: StateTransitionType
      ): F[DataState[AmmOnChainState, AmmCalculatedState]] =
        failed.reason match {
          case DuplicatedUpdate(_) =>
            logger.warn("Duplicated data update, ignoring") >> state.pure[F]
          case _ =>
            val updatedSwapState = swapState
              .focus(_.failed)
              .modify(_ + fromFailedCalculatedState(failed, tokenInfo))
              .focus(_.pending)
              .modify(_.filter(_.update =!= signedUpdate))

            val dataUpdate = UpdatedStateDataUpdate(
              transitionType,
              Failed,
              OperationType.Swap,
              signedUpdate.asInstanceOf[Signed[AmmUpdate]],
              updateHash,
              None,
              Some(failed.reason)
            )

            val result = updateStateWithDataUpdate(
              updateSwapState(state, updatedSwapState),
              dataUpdate
            )
            result.pure[F]
        }

      private def processConfirmedSwap(
        pendingAction: PendingSpendAction[SwapUpdate],
        state: DataState[AmmOnChainState, AmmCalculatedState],
        tokenInfo: SwapTokenInfo,
        currentSnapshotOrdinal: SnapshotOrdinal,
        globalEpochProgress: EpochProgress
      ): F[DataState[AmmOnChainState, AmmCalculatedState]] =
        for {
          swapReference <- HasherSelector[F].withCurrent(implicit hs => SwapReference.of(pendingAction.update))
          poolId <- buildLiquidityPoolUniqueIdentifier(
            pendingAction.update.value.swapFromPair,
            pendingAction.update.value.swapToPair
          )

          swapCalculatedState = getSwapCalculatedState(state.calculated)
          sourceAddress = pendingAction.update.source

          swapStateAddress = SwapCalculatedStateAddress(
            pendingAction.updateHash,
            sourceAddress,
            tokenInfo.primaryTokenInformationUpdated,
            tokenInfo.pairTokenInformationUpdated,
            pendingAction.update.value.allowSpendReference,
            pendingAction.update.value.amountIn,
            tokenInfo.grossReceived,
            tokenInfo.netReceived,
            pendingAction.update.value.amountOutMinimum,
            pendingAction.update.value.amountOutMaximum,
            pendingAction.update.value.maxValidGsEpochProgress,
            Some(poolId),
            currentSnapshotOrdinal,
            swapReference
          )

          expirationEpoch = getConfirmedExpireEpochProgress(applicationConfig, globalEpochProgress)
          swapStateValue = SwapCalculatedStateValue(expirationEpoch, swapStateAddress)

          updatedPending = swapCalculatedState.pending.filterNot {
            case PendingSpendAction(update, _, _, _) => update === pendingAction.update
            case _                                   => false
          }

          newSwapState = swapCalculatedState
            .focus(_.confirmed.value)
            .modify(_.updatedWith(sourceAddress) {
              case Some(info) => Some(info.focus(_.values).modify(_ + swapStateValue))
              case None       => Some(SwapCalculatedStateInfo(swapReference, SortedSet(swapStateValue)))
            })
            .focus(_.pending)
            .replace(updatedPending)

          dataUpdate = UpdatedStateDataUpdate(
            PendingSpendTransactions,
            Confirmed,
            OperationType.Swap,
            pendingAction.update.asInstanceOf[Signed[AmmUpdate]],
            pendingAction.updateHash,
            None
          )

          result = updateStateWithDataUpdate(
            updateSwapState(state, newSwapState),
            dataUpdate
          )
        } yield result

      def combineNew(
        signedUpdate: Signed[SwapUpdate],
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        globalEpochProgress: EpochProgress,
        lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
        currentSnapshotOrdinal: SnapshotOrdinal,
        currencyId: CurrencyId
      )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {

        val swapUpdate = signedUpdate.value
        val swapCalculatedState = getSwapCalculatedState(oldState.calculated)
        val confirmedSwaps = swapCalculatedState.confirmed.value
          .get(signedUpdate.source)
          .map(_.values)
          .getOrElse(SortedSet.empty[SwapCalculatedStateValue])
        val pendingSwaps = swapCalculatedState.getPendingUpdates

        val result = for {
          _ <- EitherT.liftF[F, FailedCalculatedState, Unit](logger.info(s"Processing new swap update from ${signedUpdate.source}"))

          _ <- EitherT(swapValidations.l0Validations(signedUpdate, oldState.calculated, globalEpochProgress))
            .leftWiden[FailedCalculatedState]
          _ <- EitherT(
            swapValidations.newUpdateValidations(
              oldState.calculated,
              signedUpdate,
              globalEpochProgress,
              confirmedSwaps,
              pendingSwaps
            )
          ).leftWiden[FailedCalculatedState]

          updateHash <- EitherT.liftF[F, FailedCalculatedState, Hash](hashUpdate(signedUpdate))
          updateAllowSpends <- EitherT.liftF[F, FailedCalculatedState, Option[Hashed[AllowSpend]]](
            getUpdateAllowSpend(swapUpdate, lastGlobalSnapshotsAllowSpends)
          )
          poolId <- EitherT.liftF[F, FailedCalculatedState, PoolId](
            buildLiquidityPoolUniqueIdentifier(swapUpdate.swapFromPair, swapUpdate.swapToPair)
          )
          tokenInfo <- EitherT(pricingService.getSwapTokenInfo(signedUpdate, updateHash, poolId, globalEpochProgress))

          finalState <- updateAllowSpends match {
            case None =>
              // Allow spend not confirmed - store as pending
              val pendingAllowSpend = PendingAllowSpend(signedUpdate, updateHash, Some(tokenInfo))
              val updatedSwapState = swapCalculatedState.focus(_.pending).modify(_ + pendingAllowSpend)
              val dataUpdate = createPendingAllowSpendUpdate(signedUpdate, updateHash, Some(tokenInfo))

              EitherT.liftF[F, FailedCalculatedState, DataState[AmmOnChainState, AmmCalculatedState]](
                logger.warn(s"Allow spend not confirmed, storing as pending") >>
                  updateStateWithDataUpdate(updateSwapState(oldState, updatedSwapState), dataUpdate).pure[F]
              )

            case Some(_) =>
              // Allow spend confirmed - delegate to combinePendingAllowSpend
              EitherT.liftF[F, FailedCalculatedState, DataState[AmmOnChainState, AmmCalculatedState]](
                logger.info(s"Allow spend confirmed, processing immediately") >>
                  combinePendingAllowSpend(
                    PendingAllowSpend(signedUpdate, updateHash, Some(tokenInfo)),
                    oldState,
                    globalEpochProgress,
                    lastGlobalSnapshotsAllowSpends,
                    currentSnapshotOrdinal,
                    currencyId
                  )
              )
          }
        } yield finalState

        result.foldF(
          failed =>
            for {
              _ <- logger.error(s"Failed to process swap update: ${failed.reason}")
              updateHash <- hashUpdate(signedUpdate)
              result <- handleFailure(signedUpdate, oldState, failed, swapCalculatedState, updateHash, None, NewUpdate)
            } yield result,
          success => logger.info("Successfully processed swap update") >> success.pure[F]
        )
      }

      def combinePendingAllowSpend(
        pendingAllowSpend: PendingAllowSpend[SwapUpdate],
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        globalEpochProgress: EpochProgress,
        lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
        currentSnapshotOrdinal: SnapshotOrdinal,
        currencyId: CurrencyId
      )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {

        val swapCalculatedState = getSwapCalculatedState(oldState.calculated)
        val swapUpdate = pendingAllowSpend.update.value
        val tokenInfo = pendingAllowSpend.pricingTokenInfo.collect { case info: SwapTokenInfo => info }

        val result = for {
          _ <- EitherT.liftF[F, FailedCalculatedState, Unit](logger.info(s"Processing pending allow spend"))
          updateAllowSpends <- EitherT.liftF[F, FailedCalculatedState, Option[Hashed[AllowSpend]]](
            getUpdateAllowSpend(swapUpdate, lastGlobalSnapshotsAllowSpends)
          )

          finalState <- updateAllowSpends match {
            case Some(allowSpendToken) =>
              for {
                swapTokenInfo <- EitherT.fromOption[F](
                  tokenInfo,
                  FailedCalculatedState(
                    MissingSwapTokenInfo(),
                    getFailureExpireEpochProgress(applicationConfig, globalEpochProgress),
                    pendingAllowSpend.updateHash,
                    pendingAllowSpend.update
                  ): FailedCalculatedState
                )

                _ <- EitherT(
                  swapValidations.pendingAllowSpendsValidations(
                    pendingAllowSpend.update,
                    globalEpochProgress,
                    currencyId,
                    swapTokenInfo,
                    allowSpendToken
                  )
                ).leftWiden[FailedCalculatedState]

                // Update liquidity pool
                poolId <- EitherT.liftF[F, FailedCalculatedState, PoolId](
                  buildLiquidityPoolUniqueIdentifier(swapUpdate.swapFromPair, swapUpdate.swapToPair)
                )
                liquidityPoolState = getLiquidityPoolCalculatedState(oldState.calculated)
                liquidityPool <- EitherT.liftF[F, FailedCalculatedState, LiquidityPool](
                  getLiquidityPoolByPoolId(liquidityPoolState.confirmed.value, poolId)
                )
                updateHashed <- EitherT.liftF[F, FailedCalculatedState, Hashed[SwapUpdate]](
                  HasherSelector[F].withCurrent(implicit hs => pendingAllowSpend.update.toHashed(dataUpdateCodec.serialize))
                )

                updatedPool <- EitherT(
                  pricingService.getUpdatedLiquidityPoolDueNewSwap(
                    updateHashed,
                    liquidityPool,
                    swapTokenInfo.primaryTokenInformationUpdated,
                    swapTokenInfo.pairTokenInformationUpdated,
                    swapTokenInfo.grossReceived,
                    currencyId,
                    currentSnapshotOrdinal
                  )
                )

                // Generate spend action and update state
                spendAction = generateSpendAction(
                  allowSpendToken,
                  swapUpdate.amountIn,
                  swapTokenInfo.pairTokenInformationUpdated.identifier,
                  swapTokenInfo.netReceived,
                  currencyId.value
                )

                pendingSpendAction = PendingSpendAction(
                  pendingAllowSpend.update,
                  pendingAllowSpend.updateHash,
                  spendAction,
                  pendingAllowSpend.pricingTokenInfo
                )

                updatedPending = swapCalculatedState.pending.filterNot {
                  case PendingAllowSpend(update, _, _) => update === pendingAllowSpend.update; case _ => false
                }
                  .+(pendingSpendAction)

                updatedSwapState = swapCalculatedState.focus(_.pending).replace(updatedPending)
                stateWithLP = updateLiquidityPoolState(oldState, poolId, updatedPool)
                stateWithSwap = updateSwapState(stateWithLP, updatedSwapState)

                dataUpdate = UpdatedStateDataUpdate(
                  PendingAllowSpends,
                  PendingSpendTransactions,
                  OperationType.Swap,
                  pendingAllowSpend.update.asInstanceOf[Signed[AmmUpdate]], // Fix type mismatch
                  pendingAllowSpend.updateHash,
                  Some(pendingSpendAction.asInstanceOf[PendingAction[AmmUpdate]])
                )

                finalState = updateStateWithDataUpdate(stateWithSwap, dataUpdate)
                  .focus(_.sharedArtifacts)
                  .modify(_ ++ SortedSet[SharedArtifact](spendAction))

              } yield finalState

            case None =>
              if (pendingAllowSpend.update.value.maxValidGsEpochProgress <= globalEpochProgress) { // Added .value
                EitherT.leftT[F, DataState[AmmOnChainState, AmmCalculatedState]](
                  FailedCalculatedState(
                    OperationExpired(pendingAllowSpend.update),
                    getFailureExpireEpochProgress(applicationConfig, globalEpochProgress),
                    pendingAllowSpend.updateHash,
                    pendingAllowSpend.update
                  )
                )
              } else {
                EitherT.rightT[F, FailedCalculatedState](oldState)
              }
          }
        } yield finalState

        result.foldF(
          failed =>
            for {
              _ <- logger.error(s"Failed to process pending allow spend: ${failed.reason}")
              rolledBackState <- rollbackLiquidityPool(
                pendingAllowSpend,
                globalEpochProgress,
                pendingAllowSpend.pricingTokenInfo,
                oldState,
                currentSnapshotOrdinal,
                currencyId
              )
              result <- handleFailure(
                pendingAllowSpend.update,
                rolledBackState,
                failed,
                swapCalculatedState,
                pendingAllowSpend.updateHash,
                tokenInfo,
                PendingAllowSpends
              )
            } yield result,
          success => logger.info("Successfully processed pending allow spend") >> success.pure[F]
        )
      }

      def combinePendingSpendAction(
        pendingSpendAction: PendingSpendAction[SwapUpdate],
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        globalEpochProgress: EpochProgress,
        spendActions: List[SpendAction],
        currentSnapshotOrdinal: SnapshotOrdinal,
        currencyId: CurrencyId
      )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {

        val swapCalculatedState = getSwapCalculatedState(oldState.calculated)
        val tokenInfo = pendingSpendAction.pricingTokenInfo.collect { case info: SwapTokenInfo => info }

        val result = for {
          _ <- EitherT(swapValidations.pendingSpendActionsValidation(pendingSpendAction.update, globalEpochProgress))
            .leftWiden[FailedCalculatedState]

          metagraphHash <- EitherT.liftF[F, FailedCalculatedState, Hash](
            HasherSelector[F].withCurrent(implicit hs => Hasher[F].hash(pendingSpendAction.generatedSpendAction))
          )

          globalHashes <- EitherT.liftF[F, FailedCalculatedState, List[Hash]](
            HasherSelector[F].withCurrent(implicit hs => spendActions.traverse(action => Hasher[F].hash(action)))
          )

          isAccepted = checkIfSpendActionAcceptedInGl0(metagraphHash, globalHashes)

          finalState <-
            if (!isAccepted) {
              if (pendingSpendAction.update.value.maxValidGsEpochProgress <= globalEpochProgress) { // Added .value
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
                swapTokenInfo <- EitherT.fromOption[F](
                  tokenInfo,
                  FailedCalculatedState(
                    MissingSwapTokenInfo(),
                    getFailureExpireEpochProgress(applicationConfig, globalEpochProgress),
                    pendingSpendAction.updateHash,
                    pendingSpendAction.update
                  ): FailedCalculatedState
                )
                result <- EitherT.liftF[F, FailedCalculatedState, DataState[AmmOnChainState, AmmCalculatedState]](
                  processConfirmedSwap(
                    pendingSpendAction,
                    oldState,
                    swapTokenInfo,
                    currentSnapshotOrdinal,
                    globalEpochProgress
                  )
                )
              } yield result
            }
        } yield finalState

        result.foldF(
          failed =>
            for {
              _ <- logger.error(s"Failed to process pending spend transaction: ${failed.reason}")
              rolledBackState <- rollbackLiquidityPool(
                pendingSpendAction,
                globalEpochProgress,
                pendingSpendAction.pricingTokenInfo,
                oldState,
                currentSnapshotOrdinal,
                currencyId
              )
              result <- handleFailure(
                pendingSpendAction.update,
                rolledBackState,
                failed,
                swapCalculatedState,
                pendingSpendAction.updateHash,
                tokenInfo,
                PendingSpendTransactions
              )
            } yield result,
          success => logger.info("Successfully processed pending spend transaction") >> success.pure[F]
        )
      }

      def cleanupExpiredOperations(
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        globalEpochProgress: EpochProgress
      ): DataState[AmmOnChainState, AmmCalculatedState] = {
        val swapCalculatedState = getSwapCalculatedState(oldState.calculated)

        val unexpiredFailed = swapCalculatedState.failed.filter(_.expiringEpochProgress > globalEpochProgress)
        val unexpiredConfirmed = swapCalculatedState.confirmed.value.view
          .mapValues(
            _.focus(_.values).modify(_.filter(_.expiringEpochProgress > globalEpochProgress))
          )
          .to(SortedMap)

        val updatedSwapState = swapCalculatedState
          .focus(_.failed)
          .replace(unexpiredFailed)
          .focus(_.confirmed.value)
          .replace(unexpiredConfirmed)

        updateSwapState(oldState, updatedSwapState)
      }
    }
}
