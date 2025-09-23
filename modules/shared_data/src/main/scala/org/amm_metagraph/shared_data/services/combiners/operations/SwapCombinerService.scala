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
import io.constellationnetwork.security.Hasher
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed

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

trait SwapCombinerService[F[_]] {
  def combineNew(
    signedUpdate: Signed[SwapUpdate],
    oldState: DataState[AmmOnChainState, AmmCalculatedState],
    globalEpochProgress: EpochProgress,
    lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
    currencyId: CurrencyId
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]]

  def combinePendingAllowSpend(
    pendingSignedUpdate: PendingAllowSpend[SwapUpdate],
    oldState: DataState[AmmOnChainState, AmmCalculatedState],
    globalEpochProgress: EpochProgress,
    lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
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
      private def rollbackAmountInLPs(
        pendingAllowSpend: PendingAction[SwapUpdate],
        lastSyncGlobalSnapshotEpoch: EpochProgress,
        maybePricingTokenInfo: Option[PricingTokenInfo],
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        currencyId: CurrencyId
      ): F[DataState[AmmOnChainState, AmmCalculatedState]] = maybePricingTokenInfo match {

        case Some(SwapTokenInfo(primaryTokenInformationUpdated, pairTokenInformationUpdated, amountIn, grossReceived, _)) =>
          (for {
            poolId <- EitherT.liftF(
              buildLiquidityPoolUniqueIdentifier(
                primaryTokenInformationUpdated.identifier,
                pairTokenInformationUpdated.identifier
              )
            )

            liquidityPoolCalculatedState = getLiquidityPoolCalculatedState(oldState.calculated)

            liquidityPool <- EitherT.liftF(
              getLiquidityPoolByPoolId(liquidityPoolCalculatedState.confirmed.value, poolId)
            )

            updatedLiquidityPool <- EitherT.fromEither(
              pricingService.rollbackSwapLiquidityPoolAmounts(
                pendingAllowSpend.update,
                pendingAllowSpend.updateHash,
                lastSyncGlobalSnapshotEpoch,
                liquidityPool,
                amountIn,
                grossReceived,
                currencyId
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

      private def handleFailedUpdate(
        swapUpdate: Signed[SwapUpdate],
        acc: DataState[AmmOnChainState, AmmCalculatedState],
        failedCalculatedState: FailedCalculatedState,
        swapCalculatedState: SwapCalculatedState,
        updateHash: Hash,
        swapTokenInfo: Option[SwapTokenInfo],
        currentState: StateTransitionType
      ) =
        failedCalculatedState.reason match {
          case DuplicatedUpdate(_) => logger.warn("Duplicated data update, ignoring") >> acc.pure
          case _ =>
            val updatedSwapCalculatedState = swapCalculatedState
              .focus(_.failed)
              .modify(_ + fromFailedCalculatedState(failedCalculatedState, swapTokenInfo))
              .focus(_.pending)
              .modify(_.filter(_.update =!= swapUpdate))

            val updatedCalculatedState = acc.calculated
              .focus(_.operations)
              .modify(_.updated(OperationType.Swap, updatedSwapCalculatedState))

            Async[F].pure(
              acc
                .focus(_.onChain.updatedStateDataUpdate)
                .modify { current =>
                  current + UpdatedStateDataUpdate(
                    currentState,
                    Failed,
                    OperationType.Swap,
                    swapUpdate,
                    updateHash,
                    none,
                    failedCalculatedState.reason.some
                  )
                }
                .focus(_.calculated)
                .replace(updatedCalculatedState)
            )
        }

      private def getUpdateAllowSpend(
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
          case PendingAllowSpend(update, _, _) if update === signedSwapUpdate => true
          case _                                                              => false
        }

      private def removePendingSpendAction(
        swapCalculatedState: SwapCalculatedState,
        signedSwapUpdate: Signed[SwapUpdate]
      ) =
        swapCalculatedState.pending.filterNot {
          case PendingSpendAction(update, _, _, _) if update === signedSwapUpdate => true
          case _                                                                  => false
        }

      def combineNew(
        signedUpdate: Signed[SwapUpdate],
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        globalEpochProgress: EpochProgress,
        lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
        currencyId: CurrencyId
      )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
        val swapUpdate = signedUpdate.value
        val swapCalculatedState = getSwapCalculatedState(oldState.calculated)
        val confirmedSwaps = swapCalculatedState.confirmed.value
          .get(signedUpdate.source)
          .map(_.values)
          .getOrElse(SortedSet.empty[SwapCalculatedStateValue])
        val pendingSwaps = swapCalculatedState.getPendingUpdates
        val swapCalculatedStatePendingAllowSpend = swapCalculatedState.pending
        val liquidityPoolsCalculatedState = getLiquidityPoolCalculatedState(oldState.calculated)

        val updateHashedF = HasherSelector[F].withCurrent(implicit hs => signedUpdate.toHashed(dataUpdateCodec.serialize))

        val combinedState = for {
          _ <- EitherT(
            swapValidations.l0Validations(
              signedUpdate,
              oldState.calculated,
              globalEpochProgress
            )
          )
          _ <- EitherT(
            swapValidations.newUpdateValidations(
              oldState.calculated,
              signedUpdate,
              globalEpochProgress,
              confirmedSwaps,
              pendingSwaps
            )
          )
          updateAllowSpends <- EitherT.liftF(getUpdateAllowSpend(swapUpdate, lastGlobalSnapshotsAllowSpends))
          poolId <- EitherT.liftF(buildLiquidityPoolUniqueIdentifier(swapUpdate.swapFromPair, swapUpdate.swapToPair))
          liquidityPool <- EitherT.liftF(getLiquidityPoolByPoolId(liquidityPoolsCalculatedState.confirmed.value, poolId))
          updateHashed <- EitherT.liftF(updateHashedF)
          updatedTokenInformation <- EitherT(pricingService.getSwapTokenInfo(signedUpdate, updateHashed.hash, poolId, globalEpochProgress))

          liquidityPoolUpdated <- EitherT.fromEither[F](
            pricingService.getUpdatedLiquidityPoolDueNewSwap(
              updateHashed,
              liquidityPool,
              updatedTokenInformation.primaryTokenInformationUpdated,
              updatedTokenInformation.pairTokenInformationUpdated,
              updatedTokenInformation.grossReceived,
              currencyId
            )
          )
          newLiquidityPoolState =
            liquidityPoolsCalculatedState
              .focus(_.confirmed.value)
              .modify(_.updated(poolId.value, liquidityPoolUpdated))

          updatedCalculatedState = oldState.calculated
            .focus(_.operations)
            .modify(_.updated(OperationType.LiquidityPool, newLiquidityPoolState))

          updatedLpState = oldState
            .focus(_.calculated)
            .replace(updatedCalculatedState)

          response <- updateAllowSpends match {
            case None =>
              val pendingAllowSpend = PendingAllowSpend(
                signedUpdate,
                updateHashed.hash,
                updatedTokenInformation.some
              )

              val updatedPendingSwapsCalculatedState =
                swapCalculatedStatePendingAllowSpend + pendingAllowSpend
              val newSwapState = swapCalculatedState
                .focus(_.pending)
                .replace(updatedPendingSwapsCalculatedState)

              val updatedCalculatedState = updatedLpState.calculated
                .focus(_.operations)
                .modify(_.updated(OperationType.Swap, newSwapState))

              EitherT.rightT[F, FailedCalculatedState](
                oldState
                  .focus(_.onChain.updatedStateDataUpdate)
                  .modify { current =>
                    current + UpdatedStateDataUpdate(
                      NewUpdate,
                      PendingAllowSpends,
                      OperationType.Swap,
                      signedUpdate,
                      updateHashed.hash,
                      Some(pendingAllowSpend.asInstanceOf[PendingAction[AmmUpdate]])
                    )
                  }
                  .focus(_.calculated)
                  .replace(updatedCalculatedState)
              )

            case Some(_) =>
              EitherT.liftF[F, FailedCalculatedState, DataState[AmmOnChainState, AmmCalculatedState]](
                combinePendingAllowSpend(
                  PendingAllowSpend(signedUpdate, updateHashed.hash, updatedTokenInformation.some),
                  updatedLpState,
                  globalEpochProgress,
                  lastGlobalSnapshotsAllowSpends,
                  currencyId
                )
              )
          }
        } yield response

        combinedState.foldF(
          failed =>
            updateHashedF.flatMap(hashed =>
              handleFailedUpdate(signedUpdate, oldState, failed, swapCalculatedState, hashed.hash, none, NewUpdate)
            ),
          success => success.pure[F]
        )
      }

      def combinePendingAllowSpend(
        pendingAllowSpend: PendingAllowSpend[SwapUpdate],
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        globalEpochProgress: EpochProgress,
        lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
        currencyId: CurrencyId
      )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
        val swapCalculatedState = getSwapCalculatedState(oldState.calculated)
        val swapUpdate = pendingAllowSpend.update.value
        val maybeSwapTokenInfo = pendingAllowSpend.pricingTokenInfo.collect {
          case swapTokenInfo: SwapTokenInfo => swapTokenInfo
        }

        val combinedState: EitherT[F, FailedCalculatedState, DataState[AmmOnChainState, AmmCalculatedState]] = for {
          updateAllowSpends <- EitherT.liftF(getUpdateAllowSpend(pendingAllowSpend.update, lastGlobalSnapshotsAllowSpends))

          result <- updateAllowSpends match {
            case Some(allowSpendToken) =>
              for {
                updatedTokenInformation <- EitherT.fromOption[F](
                  maybeSwapTokenInfo,
                  FailedCalculatedState(
                    MissingSwapTokenInfo(),
                    getFailureExpireEpochProgress(applicationConfig, globalEpochProgress),
                    pendingAllowSpend.updateHash,
                    pendingAllowSpend.update
                  )
                )

                _ <- EitherT(
                  swapValidations.pendingAllowSpendsValidations(
                    pendingAllowSpend.update,
                    globalEpochProgress,
                    currencyId,
                    updatedTokenInformation,
                    allowSpendToken
                  )
                )

                spendActionToken = generateSpendAction(
                  allowSpendToken,
                  swapUpdate.amountIn,
                  updatedTokenInformation.pairTokenInformationUpdated.identifier,
                  updatedTokenInformation.netReceived,
                  currencyId.value
                )

                updatedPendingAllowSpendCalculatedState =
                  removePendingAllowSpend(swapCalculatedState, pendingAllowSpend.update)
                pendingSpendAction = PendingSpendAction(
                  pendingAllowSpend.update,
                  pendingAllowSpend.updateHash,
                  spendActionToken,
                  pendingAllowSpend.pricingTokenInfo
                )
                updatedPendingSpendActionCalculatedState =
                  updatedPendingAllowSpendCalculatedState + pendingSpendAction

                updatedPendingSwapCalculatedState =
                  swapCalculatedState
                    .focus(_.pending)
                    .replace(updatedPendingSpendActionCalculatedState)

                updatedCalculatedState = oldState.calculated
                  .focus(_.operations)
                  .modify(_.updated(OperationType.Swap, updatedPendingSwapCalculatedState))

              } yield
                oldState
                  .focus(_.onChain.updatedStateDataUpdate)
                  .modify { current =>
                    current + UpdatedStateDataUpdate(
                      PendingAllowSpends,
                      PendingSpendTransactions,
                      OperationType.Swap,
                      pendingAllowSpend.update,
                      pendingAllowSpend.updateHash,
                      Some(pendingSpendAction.asInstanceOf[PendingAction[AmmUpdate]])
                    )
                  }
                  .focus(_.calculated)
                  .replace(updatedCalculatedState)
                  .focus(_.sharedArtifacts)
                  .modify(current =>
                    current ++ SortedSet[SharedArtifact](
                      spendActionToken
                    )
                  )

            case None =>
              if (pendingAllowSpend.update.maxValidGsEpochProgress <= globalEpochProgress) {
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
        } yield result

        combinedState.foldF(
          failed =>
            rollbackAmountInLPs(
              pendingAllowSpend,
              globalEpochProgress,
              pendingAllowSpend.pricingTokenInfo,
              oldState,
              currencyId
            ).flatMap { rolledBackState =>
              handleFailedUpdate(
                pendingAllowSpend.update,
                rolledBackState,
                failed,
                swapCalculatedState,
                pendingAllowSpend.updateHash,
                maybeSwapTokenInfo,
                PendingAllowSpends
              )
            },
          success => success.pure[F]
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
        val signedSwapUpdate = pendingSpendAction.update
        val swapCalculatedState = getSwapCalculatedState(oldState.calculated)
        val swapUpdate = pendingSpendAction.update.value
        val maybeSwapTokenInfo = pendingSpendAction.pricingTokenInfo.collect {
          case swapTokenInfo: SwapTokenInfo => swapTokenInfo
        }

        val combinedState: EitherT[F, FailedCalculatedState, DataState[AmmOnChainState, AmmCalculatedState]] =
          for {
            _ <- EitherT(
              swapValidations.pendingSpendActionsValidation(
                signedSwapUpdate,
                globalEpochProgress
              )
            )
            metagraphGeneratedSpendActionHash <- EitherT.liftF[F, FailedCalculatedState, Hash](
              HasherSelector[F].withCurrent(implicit hs => Hasher[F].hash(pendingSpendAction.generatedSpendAction))
            )
            globalSnapshotsHashes <- EitherT.liftF[F, FailedCalculatedState, List[Hash]](
              HasherSelector[F].withCurrent(implicit hs => spendActions.traverse(action => Hasher[F].hash(action)))
            )
            allSpendActionsAccepted <- EitherT.liftF[F, FailedCalculatedState, Boolean] {
              Async[F].pure(checkIfSpendActionAcceptedInGl0(metagraphGeneratedSpendActionHash, globalSnapshotsHashes))
            }
            result <-
              if (!allSpendActionsAccepted) {
                if (pendingSpendAction.update.maxValidGsEpochProgress <= globalEpochProgress) {
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
                (for {
                  swapReference <- EitherT.liftF[F, FailedCalculatedState, SwapReference](
                    HasherSelector[F].withCurrent(implicit hs => SwapReference.of(signedSwapUpdate))
                  )

                  updatedTokenInformation <- maybeSwapTokenInfo match {
                    case Some(tokenInfo) => EitherT.rightT[F, FailedCalculatedState](tokenInfo)
                    case None =>
                      EitherT.leftT[F, SwapTokenInfo](
                        FailedCalculatedState(
                          MissingSwapTokenInfo(),
                          getFailureExpireEpochProgress(applicationConfig, globalEpochProgress),
                          pendingSpendAction.updateHash,
                          pendingSpendAction.update
                        )
                      )
                  }

                  sourceAddress = signedSwapUpdate.source
                  poolId <- EitherT.liftF(
                    buildLiquidityPoolUniqueIdentifier(swapUpdate.swapFromPair, swapUpdate.swapToPair)
                  )

                  swapCalculatedStateAddress = SwapCalculatedStateAddress(
                    pendingSpendAction.updateHash,
                    sourceAddress,
                    updatedTokenInformation.primaryTokenInformationUpdated,
                    updatedTokenInformation.pairTokenInformationUpdated,
                    swapUpdate.allowSpendReference,
                    swapUpdate.amountIn,
                    updatedTokenInformation.grossReceived,
                    updatedTokenInformation.netReceived,
                    swapUpdate.amountOutMinimum,
                    swapUpdate.amountOutMaximum,
                    swapUpdate.maxValidGsEpochProgress,
                    poolId.some,
                    currentSnapshotOrdinal,
                    swapReference
                  )
                  expirationEpochProgress = getConfirmedExpireEpochProgress(applicationConfig, globalEpochProgress)
                  swapCalculatedStateValue = SwapCalculatedStateValue(
                    expirationEpochProgress,
                    swapCalculatedStateAddress
                  )

                  updatedPendingCalculatedState = removePendingSpendAction(swapCalculatedState, signedSwapUpdate)
                  newSwapState = swapCalculatedState
                    .focus(_.confirmed.value)
                    .modify(current =>
                      current.updatedWith(sourceAddress) {
                        case Some(confirmedSwaps) =>
                          Some(
                            SwapCalculatedStateInfo(
                              swapReference,
                              confirmedSwaps.values + swapCalculatedStateValue
                            )
                          )
                        case None =>
                          Some(
                            SwapCalculatedStateInfo(
                              swapReference,
                              SortedSet(swapCalculatedStateValue)
                            )
                          )
                      }
                    )
                    .focus(_.pending)
                    .replace(updatedPendingCalculatedState)

                  updatedCalculatedState = oldState.calculated
                    .focus(_.operations)
                    .modify(_.updated(OperationType.Swap, newSwapState))
                } yield
                  oldState
                    .focus(_.onChain.updatedStateDataUpdate)
                    .modify { current =>
                      current + UpdatedStateDataUpdate(
                        PendingSpendTransactions,
                        Confirmed,
                        OperationType.Swap,
                        pendingSpendAction.update,
                        pendingSpendAction.updateHash,
                        none
                      )
                    }
                    .focus(_.calculated)
                    .replace(updatedCalculatedState)): EitherT[F, FailedCalculatedState, DataState[AmmOnChainState, AmmCalculatedState]]
              }
          } yield result

        combinedState.foldF(
          failed =>
            rollbackAmountInLPs(
              pendingSpendAction,
              globalEpochProgress,
              pendingSpendAction.pricingTokenInfo,
              oldState,
              currencyId
            ).flatMap { rolledBackState =>
              handleFailedUpdate(
                pendingSpendAction.update,
                rolledBackState,
                failed,
                swapCalculatedState,
                pendingSpendAction.updateHash,
                maybeSwapTokenInfo,
                PendingSpendTransactions
              )
            },
          success => success.pure[F]
        )
      }

      def cleanupExpiredOperations(
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        globalEpochProgress: EpochProgress
      ): DataState[AmmOnChainState, AmmCalculatedState] = {
        val swapCalculatedState = getSwapCalculatedState(oldState.calculated)
        val unexpiredFailed = swapCalculatedState.failed.filter(_.expiringEpochProgress > globalEpochProgress)
        val unexpiredConfirmed = swapCalculatedState.confirmed.value.collect {
          case (address, infos) =>
            address -> infos
              .focus(_.values)
              .modify(_.filter(_.expiringEpochProgress > globalEpochProgress))
        }

        val updatedSwapCalculatedState = swapCalculatedState
          .focus(_.failed)
          .replace(unexpiredFailed)
          .focus(_.confirmed.value)
          .replace(unexpiredConfirmed)

        oldState
          .focus(_.calculated.operations)
          .modify(_.updated(OperationType.Swap, updatedSwapCalculatedState))
      }
    }
}
