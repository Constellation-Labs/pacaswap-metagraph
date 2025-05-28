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
import io.constellationnetwork.security.Hasher
import io.constellationnetwork.security.signature.Signed

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.PosLong
import eu.timepit.refined.types.numeric.NonNegLong
import monocle.syntax.all._
import org.amm_metagraph.shared_data.FeeDistributor
import org.amm_metagraph.shared_data.SpendTransactions.{checkIfSpendActionAcceptedInGl0, generateSpendAction}
import org.amm_metagraph.shared_data.globalSnapshots.getAllowSpendsGlobalSnapshotsState
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, LiquidityPoolUpdate}
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.codecs.{HasherSelector, JsonWithBase64BinaryCodec}
import org.amm_metagraph.shared_data.validations.LiquidityPoolValidations

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
    liquidityPoolValidations: LiquidityPoolValidations[F],
    dataUpdateCodec: JsonWithBase64BinaryCodec[F, AmmUpdate]
  ): LiquidityPoolCombinerService[F] =
    new LiquidityPoolCombinerService[F] {
      private def handleFailedUpdate(
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

        acc
          .focus(_.onChain.updates)
          .modify(current => current + liquidityPoolUpdate.value)
          .focus(_.calculated)
          .replace(updatedCalculatedState)
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
          case PendingAllowSpend(update, _, _) if update === signedLiquidityPoolUpdate => true
          case _                                                                       => false
        }

      private def removePendingSpendAction(
        liquidityPoolsCalculatedState: LiquidityPoolCalculatedState,
        signedLiquidityPoolUpdate: Signed[LiquidityPoolUpdate]
      ) =
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

        val combinedState = for {
          poolId <- EitherT.liftF(buildLiquidityPoolUniqueIdentifier(signedUpdate.tokenAId, signedUpdate.tokenBId))
          pendingLpsPoolsIds <- EitherT.liftF(
            pendingLps.toList.traverse(pendingLp => buildLiquidityPoolUniqueIdentifier(pendingLp.tokenAId, pendingLp.tokenBId))
          )
          updateHashed <- EitherT.liftF(
            HasherSelector[F].withCurrent(implicit hs => signedUpdate.toHashed(dataUpdateCodec.serialize))
          )
          _ <- EitherT.fromEither(
            liquidityPoolValidations.newUpdateValidations(
              oldState.calculated,
              poolId,
              signedUpdate,
              globalEpochProgress,
              confirmedLps,
              pendingLpsPoolsIds
            )
          )
          updateAllowSpends <- EitherT.liftF(getUpdateAllowSpends(liquidityPoolUpdate, lastGlobalSnapshotsAllowSpends))
          response <- updateAllowSpends match {
            case (None, _) | (_, None) =>
              val updatedPendingCalculatedState = pendingAllowSpendsCalculatedState +
                PendingAllowSpend(
                  signedUpdate,
                  updateHashed.hash
                )
              val newStakingState = liquidityPoolsCalculatedState
                .focus(_.pending)
                .replace(updatedPendingCalculatedState)

              val updatedCalculatedState = oldState.calculated
                .focus(_.operations)
                .modify(_.updated(OperationType.LiquidityPool, newStakingState))

              EitherT.rightT[F, FailedCalculatedState](
                oldState
                  .focus(_.onChain.updates)
                  .modify(current => current + liquidityPoolUpdate)
                  .focus(_.calculated)
                  .replace(updatedCalculatedState)
              )
            case (Some(_), Some(_)) =>
              EitherT.liftF[F, FailedCalculatedState, DataState[AmmOnChainState, AmmCalculatedState]](
                combinePendingAllowSpend(
                  PendingAllowSpend(signedUpdate, updateHashed.hash),
                  oldState,
                  globalEpochProgress,
                  lastGlobalSnapshotsAllowSpends,
                  currencyId
                )
              )
          }
        } yield response

        combinedState.valueOr { failedCalculatedState =>
          handleFailedUpdate(signedUpdate, oldState, failedCalculatedState, liquidityPoolsCalculatedState)
        }
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
          updateAllowSpends <- EitherT.liftF(getUpdateAllowSpends(liquidityPoolUpdate, lastGlobalSnapshotsAllowSpends))
          result <- updateAllowSpends match {
            case (Some(allowSpendTokenA), Some(allowSpendTokenB)) =>
              for {
                _ <- EitherT.fromEither[F](
                  liquidityPoolValidations.pendingAllowSpendsValidations(
                    pendingAllowSpendUpdate.update,
                    globalEpochProgress,
                    currencyId,
                    allowSpendTokenA,
                    allowSpendTokenB
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
                  removePendingAllowSpend(liquidityPoolsCalculatedState, pendingAllowSpendUpdate.update)
                updatedPendingSpendActionCalculatedState = updatedPendingAllowSpendCalculatedState +
                  PendingSpendAction(
                    pendingAllowSpendUpdate.update,
                    pendingAllowSpendUpdate.updateHash,
                    spendAction
                  )

                updatedLiquidityPoolCalculatedState =
                  liquidityPoolsCalculatedState
                    .focus(_.pending)
                    .replace(updatedPendingSpendActionCalculatedState)

                updatedCalculatedState = oldState.calculated
                  .focus(_.operations)
                  .modify(_.updated(OperationType.LiquidityPool, updatedLiquidityPoolCalculatedState))

              } yield
                oldState
                  .focus(_.onChain.updates)
                  .modify(current => current + liquidityPoolUpdate)
                  .focus(_.calculated)
                  .replace(updatedCalculatedState)
                  .focus(_.sharedArtifacts)
                  .modify(current =>
                    current ++ SortedSet[SharedArtifact](
                      spendAction
                    )
                  )
            case _ =>
              EitherT.rightT[F, FailedCalculatedState](oldState)
          }
        } yield result

        combinedState.valueOr { failedCalculatedState =>
          handleFailedUpdate(pendingAllowSpendUpdate.update, oldState, failedCalculatedState, liquidityPoolsCalculatedState)
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
        val combinedState: EitherT[F, FailedCalculatedState, DataState[AmmOnChainState, AmmCalculatedState]] = for {
          poolId <- EitherT.liftF(
            buildLiquidityPoolUniqueIdentifier(signedLiquidityPoolUpdate.tokenAId, signedLiquidityPoolUpdate.tokenBId)
          )
          _ <- EitherT.fromEither[F](
            liquidityPoolValidations.pendingSpendActionsValidation(
              signedLiquidityPoolUpdate,
              globalEpochProgress
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
                  Map.empty,
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
                oldState
                  .focus(_.onChain.updates)
                  .modify(current => current + liquidityPoolUpdate)
                  .focus(_.calculated)
                  .replace(updatedCalculatedState)
              )
            }
        } yield result

        combinedState.valueOr { failedCalculatedState =>
          handleFailedUpdate(signedLiquidityPoolUpdate, oldState, failedCalculatedState, liquidityPoolsCalculatedState)
        }
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
