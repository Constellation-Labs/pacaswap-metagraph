package org.amm_metagraph.shared_data.services.combiners

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
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hashed, Hasher}

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.PosLong
import eu.timepit.refined.types.numeric.NonNegLong
import monocle.syntax.all._
import org.amm_metagraph.shared_data.FeeDistributor
import org.amm_metagraph.shared_data.SpendTransactions.{checkIfSpendActionAcceptedInGl0, generateSpendAction}
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.globalSnapshots.getAllowSpendsGlobalSnapshotsState
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, LiquidityPoolUpdate}
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.codecs.HasherSelector

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
    currentSnapshotOrdinal: SnapshotOrdinal,
    currencyId: CurrencyId
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]]
}

object LiquidityPoolCombinerService {
  def make[F[_]: Async: HasherSelector](
    applicationConfig: ApplicationConfig
  ): LiquidityPoolCombinerService[F] =
    new LiquidityPoolCombinerService[F] {
      private def validateUpdate(
        poolId: PoolId,
        applicationConfig: ApplicationConfig,
        signedUpdate: Signed[LiquidityPoolUpdate],
        maybeAllowSpendTokenA: Option[Hashed[AllowSpend]],
        maybeAllowSpendTokenB: Option[Hashed[AllowSpend]],
        lastSyncGlobalEpochProgress: EpochProgress,
        confirmedLps: Map[String, LiquidityPool],
        pendingLps: List[PoolId],
        currencyId: CurrencyId
      ): Either[FailedCalculatedState, Signed[LiquidityPoolUpdate]] = {
        val expireEpochProgress = EpochProgress(
          NonNegLong
            .from(
              lastSyncGlobalEpochProgress.value.value + applicationConfig.failedOperationsExpirationEpochProgresses.value.value
            )
            .getOrElse(NonNegLong.MinValue)
        )

        def failWith(reason: FailedCalculatedStateReason): Left[FailedCalculatedState, Signed[LiquidityPoolUpdate]] =
          Left(FailedCalculatedState(reason, expireEpochProgress, signedUpdate))

        if (signedUpdate.maxValidGsEpochProgress < lastSyncGlobalEpochProgress) {
          failWith(OperationExpired(signedUpdate))
        } else if (confirmedLps.contains(poolId.value) || pendingLps.contains(poolId)) {
          failWith(DuplicatedLiquidityPoolRequest(signedUpdate))
        } else {
          (maybeAllowSpendTokenA, maybeAllowSpendTokenB) match {
            case (Some(allowSpendTokenA), Some(allowSpendTokenB)) =>
              val update = signedUpdate.value
              if (allowSpendTokenA.source =!= signedUpdate.source || allowSpendTokenB.source =!= signedUpdate.source) {
                failWith(SourceAddressBetweenUpdateAndAllowSpendDifferent(signedUpdate))
              } else if (allowSpendTokenA.destination =!= currencyId.value || allowSpendTokenB.destination =!= currencyId.value) {
                failWith(AllowSpendsDestinationAddressInvalid())
              } else if (allowSpendTokenA.currencyId =!= signedUpdate.tokenAId || allowSpendTokenB.currencyId =!= signedUpdate.tokenBId) {
                failWith(InvalidCurrencyIdsBetweenAllowSpendsAndDataUpdate(signedUpdate))
              } else if (update.tokenAAmount > allowSpendTokenA.amount.value.value) {
                failWith(AmountGreaterThanAllowSpendLimit(allowSpendTokenA.signed.value))
              } else if (update.tokenBAmount > allowSpendTokenB.amount.value.value) {
                failWith(AmountGreaterThanAllowSpendLimit(allowSpendTokenB.signed.value))
              } else if (
                allowSpendTokenA.lastValidEpochProgress.value.value + applicationConfig.allowSpendEpochBufferDelay.value.value < lastSyncGlobalEpochProgress.value.value
              ) {
                failWith(AllowSpendExpired(allowSpendTokenA.signed.value))
              } else if (
                allowSpendTokenB.lastValidEpochProgress.value.value + applicationConfig.allowSpendEpochBufferDelay.value.value < lastSyncGlobalEpochProgress.value.value
              ) {
                failWith(AllowSpendExpired(allowSpendTokenB.signed.value))
              } else {
                Right(signedUpdate)
              }
            case _ => Right(signedUpdate)
          }
        }
      }

      private def handleFailedUpdate(
        updates: List[AmmUpdate],
        liquidityPoolUpdate: Signed[LiquidityPoolUpdate],
        acc: DataState[AmmOnChainState, AmmCalculatedState],
        failedCalculatedState: FailedCalculatedState,
        liquidityPoolCalculatedState: LiquidityPoolCalculatedState
      ) = {
        val updatedLiquidityPoolCalculatedState = liquidityPoolCalculatedState
          .focus(_.failed)
          .modify(_ + failedCalculatedState)
          .focus(_.pending)
          .modify(_.filter(_.update =!= liquidityPoolUpdate))

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
        val liquidityPoolsCalculatedState = getLiquidityPoolCalculatedState(oldState.calculated)
        val confirmedLps = getConfirmedLiquidityPools(oldState.calculated)

        val pendingAllowSpendsCalculatedState = liquidityPoolsCalculatedState.pending
        val pendingLps = liquidityPoolsCalculatedState.getPendingUpdates

        val updates = liquidityPoolUpdate :: oldState.onChain.updates
        val combinedState = for {
          poolId <- EitherT.liftF(buildLiquidityPoolUniqueIdentifier(signedUpdate.tokenAId, signedUpdate.tokenBId))
          pendingLpsPoolsIds <- EitherT.liftF(
            pendingLps.toList.traverse(pendingLp => buildLiquidityPoolUniqueIdentifier(pendingLp.tokenAId, pendingLp.tokenBId))
          )
          _ <- EitherT.fromEither(
            validateUpdate(
              poolId,
              applicationConfig,
              signedUpdate,
              none,
              none,
              globalEpochProgress,
              confirmedLps,
              pendingLpsPoolsIds,
              currencyId
            )
          )
          updateAllowSpends <- EitherT.liftF(getUpdateAllowSpends(liquidityPoolUpdate, lastGlobalSnapshotsAllowSpends))
          response <- updateAllowSpends match {
            case (None, _) | (_, None) =>
              val updatedPendingCalculatedState = pendingAllowSpendsCalculatedState + PendingAllowSpend(signedUpdate)
              val newStakingState = liquidityPoolsCalculatedState
                .focus(_.pending)
                .replace(updatedPendingCalculatedState)

              val updatedCalculatedState = oldState.calculated
                .focus(_.operations)
                .modify(_.updated(OperationType.LiquidityPool, newStakingState))

              val result = DataState(
                AmmOnChainState(updates),
                updatedCalculatedState,
                oldState.sharedArtifacts
              )

              EitherT.rightT[F, FailedCalculatedState](result)
            case (Some(_), Some(_)) =>
              EitherT.liftF[F, FailedCalculatedState, DataState[AmmOnChainState, AmmCalculatedState]](
                combinePendingAllowSpend(
                  signedUpdate,
                  oldState,
                  globalEpochProgress,
                  lastGlobalSnapshotsAllowSpends,
                  currencyId
                )
              )
          }
        } yield response

        combinedState.valueOr { failedCalculatedState =>
          handleFailedUpdate(updates, signedUpdate, oldState, failedCalculatedState, liquidityPoolsCalculatedState)
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
        val combinedState: EitherT[F, FailedCalculatedState, DataState[AmmOnChainState, AmmCalculatedState]] = for {
          poolId <- EitherT.liftF(buildLiquidityPoolUniqueIdentifier(signedUpdate.tokenAId, signedUpdate.tokenBId))
          _ <- EitherT.fromEither[F](
            validateUpdate(
              poolId,
              applicationConfig,
              signedUpdate,
              none,
              none,
              globalEpochProgress,
              Map.empty,
              List.empty,
              currencyId
            )
          )
          updateAllowSpends <- EitherT.liftF(getUpdateAllowSpends(liquidityPoolUpdate, lastGlobalSnapshotsAllowSpends))
          result <- updateAllowSpends match {
            case (Some(allowSpendTokenA), Some(allowSpendTokenB)) =>
              for {
                _ <- EitherT.fromEither[F](
                  validateUpdate(
                    poolId,
                    applicationConfig,
                    signedUpdate,
                    allowSpendTokenA.some,
                    allowSpendTokenB.some,
                    globalEpochProgress,
                    Map.empty,
                    List.empty,
                    currencyId
                  )
                )
                amountToSpendA = SwapAmount(liquidityPoolUpdate.tokenAAmount)
                amountToSpendB = SwapAmount(liquidityPoolUpdate.tokenBAmount)

                spendAction = generateSpendAction(
                  allowSpendTokenA,
                  amountToSpendA,
                  allowSpendTokenB,
                  amountToSpendB
                )

                updatedPendingAllowSpendCalculatedState =
                  removePendingAllowSpend(liquidityPoolsCalculatedState, signedUpdate)
                updatedPendingSpendActionCalculatedState = updatedPendingAllowSpendCalculatedState + PendingSpendAction(
                  signedUpdate,
                  spendAction
                )

                updatedLiquidityPoolCalculatedState =
                  liquidityPoolsCalculatedState
                    .focus(_.pending)
                    .replace(updatedPendingSpendActionCalculatedState)

                updatedCalculatedState = oldState.calculated
                  .focus(_.operations)
                  .modify(_.updated(OperationType.LiquidityPool, updatedLiquidityPoolCalculatedState))

                updatedSharedArtifacts = oldState.sharedArtifacts ++ SortedSet[SharedArtifact](
                  spendAction
                )

              } yield
                DataState(
                  AmmOnChainState(updates),
                  updatedCalculatedState,
                  updatedSharedArtifacts
                )
            case _ =>
              EitherT.rightT[F, FailedCalculatedState](oldState)
          }
        } yield result

        combinedState.valueOr { failedCalculatedState =>
          handleFailedUpdate(updates, signedUpdate, oldState, failedCalculatedState, liquidityPoolsCalculatedState)
        }
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
        val updates = liquidityPoolUpdate :: oldState.onChain.updates
        val combinedState: EitherT[F, FailedCalculatedState, DataState[AmmOnChainState, AmmCalculatedState]] = for {
          poolId <- EitherT.liftF(
            buildLiquidityPoolUniqueIdentifier(signedLiquidityPoolUpdate.tokenAId, signedLiquidityPoolUpdate.tokenBId)
          )
          _ <- EitherT.fromEither[F](
            validateUpdate(
              poolId,
              applicationConfig,
              signedLiquidityPoolUpdate,
              none,
              none,
              globalEpochProgress,
              Map.empty,
              List.empty,
              currencyId
            )
          )
          sourceAddress = liquidityPoolUpdate.source
          metagraphGeneratedSpendActionHash <- EitherT.liftF(
            HasherSelector[F].withCurrent(implicit hs => Hasher[F].hash(pendingSpendAction.generatedSpendAction))
          )
          globalSnapshotsHashes <- EitherT.liftF(
            HasherSelector[F].withCurrent(implicit hs => spendActions.traverse(action => Hasher[F].hash(action)))
          )
          allSpendActionsAccepted <- EitherT.liftF {
            Async[F].pure(checkIfSpendActionAcceptedInGl0(metagraphGeneratedSpendActionHash, globalSnapshotsHashes))
          }

          fees = liquidityPoolUpdate.poolFees.getOrElse(FeeDistributor.standard)
          result <-
            if (!allSpendActionsAccepted) {
              EitherT.rightT[F, FailedCalculatedState](oldState)
            } else {
              val amountA = liquidityPoolUpdate.tokenAAmount.value
              val amountB = liquidityPoolUpdate.tokenBAmount.value
              val poolTotalShares: PosLong = 1.toTokenAmountFormat.toPosLongUnsafe
              val initialFeeShares: NonNegLong = 0L.toNonNegLongUnsafe

              val liquidityPool = LiquidityPool(
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
                  Map(sourceAddress -> ShareAmount(Amount(poolTotalShares))),
                  Map(sourceAddress -> initialFeeShares)
                ),
                fees
              )

              val updatedPendingCalculatedState = removePendingSpendAction(liquidityPoolsCalculatedState, signedLiquidityPoolUpdate)
              val updatedLiquidityPoolCalculatedState = liquidityPoolsCalculatedState
                .focus(_.confirmed.value)
                .modify(liquidityPools => liquidityPools.updated(poolId.value, liquidityPool))
                .focus(_.pending)
                .replace(updatedPendingCalculatedState)

              val updatedCalculatedState = oldState.calculated
                .focus(_.operations)
                .modify(_.updated(OperationType.LiquidityPool, updatedLiquidityPoolCalculatedState))
              EitherT.rightT[F, FailedCalculatedState](
                DataState(
                  AmmOnChainState(updates),
                  updatedCalculatedState,
                  oldState.sharedArtifacts
                )
              )
            }
        } yield result

        combinedState.valueOr { failedCalculatedState =>
          handleFailedUpdate(updates, signedLiquidityPoolUpdate, oldState, failedCalculatedState, liquidityPoolsCalculatedState)
        }
      }
    }
}
