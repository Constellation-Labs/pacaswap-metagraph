package org.amm_metagraph.shared_data.services.combiners

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
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hashed, Hasher, SecurityProvider}

import eu.timepit.refined.types.numeric.NonNegLong
import monocle.syntax.all._
import org.amm_metagraph.shared_data.SpendTransactions.{checkIfSpendActionAcceptedInGl0, generateSpendAction}
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.globalSnapshots.getAllowSpendsGlobalSnapshotsState
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.services.pricing.PricingService
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, StakingUpdate}
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.Staking._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.codecs.HasherSelector

trait StakingCombinerService[F[_]] {
  def combineNew(
    signedUpdate: Signed[StakingUpdate],
    oldState: DataState[AmmOnChainState, AmmCalculatedState],
    globalEpochProgress: EpochProgress,
    lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
    currencyId: CurrencyId
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]]

  def combinePendingAllowSpend(
    pendingSignedUpdate: Signed[StakingUpdate],
    oldState: DataState[AmmOnChainState, AmmCalculatedState],
    globalEpochProgress: EpochProgress,
    lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
    currencyId: CurrencyId
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]]

  def combinePendingSpendAction(
    pendingSpendAction: PendingSpendAction[StakingUpdate],
    oldState: DataState[AmmOnChainState, AmmCalculatedState],
    globalEpochProgress: EpochProgress,
    spendActions: List[SpendAction],
    currentSnapshotOrdinal: SnapshotOrdinal
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]]
}

object StakingCombinerService {
  def make[F[_]: Async: HasherSelector: SecurityProvider](
    applicationConfig: ApplicationConfig,
    pricingService: PricingService[F]
  ): StakingCombinerService[F] =
    new StakingCombinerService[F] {

      private def updateLiquidityPool(
        liquidityPool: LiquidityPool,
        signerAddress: Address,
        stakingTokenInformation: StakingTokenInformation
      ): LiquidityPool = {
        val primaryToken = stakingTokenInformation.primaryTokenInformation
        val pairToken = stakingTokenInformation.pairTokenInformation
        val newlyIssuedShares = stakingTokenInformation.newlyIssuedShares

        val tokenA = if (liquidityPool.tokenA.identifier == primaryToken.identifier) primaryToken else pairToken
        val tokenB = if (liquidityPool.tokenB.identifier == pairToken.identifier) pairToken else primaryToken

        val updatedTokenAAmount = (liquidityPool.tokenA.amount.value + tokenA.amount.value).toPosLongUnsafe
        val updatedTokenBAmount = (liquidityPool.tokenB.amount.value + tokenB.amount.value).toPosLongUnsafe

        val addressSharesAmount = liquidityPool.poolShares.addressShares.getOrElse(signerAddress, ShareAmount(Amount.empty))
        val updatedAddressSharesAmount =
          ShareAmount(Amount(NonNegLong.unsafeFrom(addressSharesAmount.value.value.value + newlyIssuedShares)))
        val updatedAddressShares = liquidityPool.poolShares.addressShares.updated(signerAddress, updatedAddressSharesAmount)

        liquidityPool.copy(
          tokenA = tokenA.copy(amount = updatedTokenAAmount),
          tokenB = tokenB.copy(amount = updatedTokenBAmount),
          k = BigInt(updatedTokenAAmount.value) * BigInt(updatedTokenBAmount.value),
          poolShares = PoolShares(
            (liquidityPool.poolShares.totalShares.value + newlyIssuedShares).toPosLongUnsafe,
            updatedAddressShares,
            liquidityPool.poolShares.feeShares
          )
        )
      }

      private def validateUpdate(
        applicationConfig: ApplicationConfig,
        signedUpdate: Signed[StakingUpdate],
        maybeTokenInformation: Option[StakingTokenInformation],
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
          (maybeTokenInformation, maybeAllowSpendTokenA, maybeAllowSpendTokenB).mapN {
            (tokenInformation, allowSpendTokenA, allowSpendTokenB) =>
              val expireEpochProgress = EpochProgress(
                NonNegLong
                  .from(
                    lastSyncGlobalEpochProgress.value.value + applicationConfig.failedOperationsExpirationEpochProgresses.value.value
                  )
                  .getOrElse(NonNegLong.MinValue)
              )
              val (tokenA, tokenB) = if (tokenInformation.primaryTokenInformation.identifier == allowSpendTokenA.currencyId) {
                (tokenInformation.primaryTokenInformation, tokenInformation.pairTokenInformation)
              } else {
                (tokenInformation.pairTokenInformation, tokenInformation.primaryTokenInformation)
              }
              if (tokenA.amount.value > allowSpendTokenA.amount.value.value) {
                FailedCalculatedState(
                  AmountGreaterThanAllowSpendLimit(allowSpendTokenA.signed.value),
                  expireEpochProgress,
                  signedUpdate
                ).some
              } else if (tokenB.amount.value > allowSpendTokenB.amount.value.value) {
                FailedCalculatedState(
                  AmountGreaterThanAllowSpendLimit(allowSpendTokenB.signed.value),
                  expireEpochProgress,
                  signedUpdate
                ).some
              } else if (allowSpendTokenA.lastValidEpochProgress.value.value < lastSyncGlobalEpochProgress.value.value) {
                FailedCalculatedState(
                  AllowSpendExpired(allowSpendTokenA.signed.value),
                  expireEpochProgress,
                  signedUpdate
                ).some
              } else if (allowSpendTokenB.lastValidEpochProgress.value.value < lastSyncGlobalEpochProgress.value.value) {
                FailedCalculatedState(
                  AllowSpendExpired(allowSpendTokenB.signed.value),
                  expireEpochProgress,
                  signedUpdate
                ).some
              } else {
                None
              }
          }.flatten
        }

      private def getUpdateAllowSpends(
        stakingUpdate: StakingUpdate,
        lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]]
      ) =
        HasherSelector[F].withBrotli { implicit hs =>
          getAllowSpendsGlobalSnapshotsState(
            stakingUpdate.tokenAAllowSpend,
            stakingUpdate.tokenAId,
            stakingUpdate.tokenBAllowSpend,
            stakingUpdate.tokenBId,
            lastGlobalSnapshotsAllowSpends
          )
        }

      private def handleFailedUpdate(
        updates: List[AmmUpdate],
        acc: DataState[AmmOnChainState, AmmCalculatedState],
        failedCalculatedState: FailedCalculatedState,
        stakingCalculatedState: StakingCalculatedState
      ) = {
        val updatedStakingCalculatedState = stakingCalculatedState
          .focus(_.failed)
          .modify(_ + failedCalculatedState)

        val updatedCalculatedState = acc.calculated
          .focus(_.operations)
          .modify(_.updated(OperationType.Staking, updatedStakingCalculatedState))

        DataState(
          AmmOnChainState(updates),
          updatedCalculatedState
        )
      }

      private def removePendingAllowSpend(
        stakingCalculatedState: StakingCalculatedState,
        signedStakingUpdate: Signed[StakingUpdate]
      ) =
        stakingCalculatedState.pending.filterNot {
          case PendingAllowSpend(update) if update === signedStakingUpdate => true
          case _                                                           => false
        }

      private def removePendingSpendAction(
        stakingCalculatedState: StakingCalculatedState,
        signedStakingUpdate: Signed[StakingUpdate]
      ) =
        stakingCalculatedState.pending.filterNot {
          case PendingSpendAction(update, _) if update === signedStakingUpdate => true
          case _                                                               => false
        }

      def combineNew(
        signedUpdate: Signed[StakingUpdate],
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        globalEpochProgress: EpochProgress,
        lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
        currencyId: CurrencyId
      )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
        val stakingUpdate = signedUpdate.value
        val stakingCalculatedState = getStakingCalculatedState(oldState.calculated)
        val pendingAllowSpendsCalculatedState = stakingCalculatedState.pending

        val updates = stakingUpdate :: oldState.onChain.updates
        getUpdateAllowSpends(stakingUpdate, lastGlobalSnapshotsAllowSpends).flatMap {
          case (None, _) | (_, None) =>
            val updatedPendingCalculatedState =
              if (stakingUpdate.maxValidGsEpochProgress < globalEpochProgress) {
                removePendingAllowSpend(stakingCalculatedState, signedUpdate)
              } else if (!pendingAllowSpendsCalculatedState.exists(_.update === signedUpdate)) {
                pendingAllowSpendsCalculatedState + PendingAllowSpend(signedUpdate)
              } else {
                pendingAllowSpendsCalculatedState
              }

            val newStakingState = stakingCalculatedState
              .focus(_.pending)
              .replace(updatedPendingCalculatedState)

            val updatedCalculatedState = oldState.calculated
              .focus(_.operations)
              .modify(_.updated(OperationType.Staking, newStakingState))

            DataState(
              AmmOnChainState(updates),
              updatedCalculatedState,
              oldState.sharedArtifacts
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
        pendingSignedUpdate: Signed[StakingUpdate],
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        globalEpochProgress: EpochProgress,
        lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
        currencyId: CurrencyId
      )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
        val stakingCalculatedState = getStakingCalculatedState(oldState.calculated)

        val stakingUpdate = pendingSignedUpdate.value
        val updates = stakingUpdate :: oldState.onChain.updates

        validateUpdate(applicationConfig, pendingSignedUpdate, none, none, none, globalEpochProgress) match {
          case Some(failedCalculatedState) => handleFailedUpdate(updates, oldState, failedCalculatedState, stakingCalculatedState).pure
          case None =>
            getUpdateAllowSpends(stakingUpdate, lastGlobalSnapshotsAllowSpends).flatMap {
              case (Some(allowSpendTokenA), Some(allowSpendTokenB)) =>
                for {
                  poolId <- buildLiquidityPoolUniqueIdentifier(stakingUpdate.tokenAId, stakingUpdate.tokenBId)

                  stakingTokenInfo <- pricingService.getStakingTokenInfo(stakingUpdate, poolId)
                  response = stakingTokenInfo match {
                    case Left(_) => oldState
                    case Right(updatedTokenInformation) =>
                      val maybeFailedUpdate = validateUpdate(
                        applicationConfig,
                        pendingSignedUpdate,
                        updatedTokenInformation.some,
                        allowSpendTokenA.some,
                        allowSpendTokenB.some,
                        globalEpochProgress
                      )

                      maybeFailedUpdate match {
                        case Some(failedCalculatedState) =>
                          handleFailedUpdate(updates, oldState, failedCalculatedState, stakingCalculatedState)
                        case None =>
                          val (amountToSpendA, amountToSpendB) =
                            if (stakingUpdate.tokenAId === updatedTokenInformation.primaryTokenInformation.identifier) {
                              (
                                SwapAmount(updatedTokenInformation.primaryTokenInformation.amount),
                                SwapAmount(updatedTokenInformation.pairTokenInformation.amount)
                              )
                            } else {
                              (
                                SwapAmount(updatedTokenInformation.pairTokenInformation.amount),
                                SwapAmount(updatedTokenInformation.primaryTokenInformation.amount)
                              )
                            }

                          val spendAction = generateSpendAction(
                            allowSpendTokenA,
                            amountToSpendA,
                            allowSpendTokenB,
                            amountToSpendB
                          )

                          val updatedPendingAllowSpendCalculatedState =
                            removePendingAllowSpend(stakingCalculatedState, pendingSignedUpdate)
                          val updatedPendingSpendActionCalculatedState = updatedPendingAllowSpendCalculatedState + PendingSpendAction(
                            pendingSignedUpdate,
                            spendAction
                          )

                          val updatedPendingStakingCalculatedState =
                            stakingCalculatedState
                              .focus(_.pending)
                              .replace(updatedPendingSpendActionCalculatedState)

                          val updatedCalculatedState = oldState.calculated
                            .focus(_.operations)
                            .modify(_.updated(OperationType.Staking, updatedPendingStakingCalculatedState))

                          val updatedSharedArtifacts = oldState.sharedArtifacts ++ SortedSet[SharedArtifact](
                            spendAction
                          )

                          DataState(
                            AmmOnChainState(updates),
                            updatedCalculatedState,
                            updatedSharedArtifacts
                          )
                      }
                  }
                } yield response
              case _ => oldState.pure
            }
        }
      }

      def combinePendingSpendAction(
        pendingSpendAction: PendingSpendAction[StakingUpdate],
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        globalEpochProgress: EpochProgress,
        spendActions: List[SpendAction],
        currentSnapshotOrdinal: SnapshotOrdinal
      )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
        val liquidityPoolsCalculatedState = getLiquidityPoolCalculatedState(oldState.calculated)
        val stakingCalculatedState = getStakingCalculatedState(oldState.calculated)

        val signedStakingUpdate = pendingSpendAction.update
        val stakingUpdate = pendingSpendAction.update.value
        val updates = stakingUpdate :: oldState.onChain.updates
        validateUpdate(applicationConfig, signedStakingUpdate, none, none, none, globalEpochProgress) match {
          case Some(value) => handleFailedUpdate(updates, oldState, value, stakingCalculatedState).pure
          case None =>
            for {
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
                    poolId <- buildLiquidityPoolUniqueIdentifier(stakingUpdate.tokenAId, stakingUpdate.tokenBId)
                    liquidityPool <- getLiquidityPoolByPoolId(liquidityPoolsCalculatedState.confirmed.value, poolId)
                    stakingReference <- HasherSelector[F].withCurrent(implicit hs => StakingReference.of(signedStakingUpdate))
                    sourceAddress = signedStakingUpdate.source

                    stakingTokenInfo <- pricingService.getStakingTokenInfo(stakingUpdate, poolId)
                    response = stakingTokenInfo match {
                      case Left(_) => oldState
                      case Right(updatedTokenInformation) =>
                        val liquidityPoolUpdated = updateLiquidityPool(liquidityPool, sourceAddress, updatedTokenInformation)
                        val stakingCalculatedStateAddress =
                          StakingCalculatedStateAddress(
                            stakingUpdate.tokenAAllowSpend,
                            stakingUpdate.tokenBAllowSpend,
                            updatedTokenInformation.primaryTokenInformation,
                            updatedTokenInformation.pairTokenInformation,
                            stakingReference
                          )

                        val updatedPendingCalculatedState = removePendingSpendAction(stakingCalculatedState, signedStakingUpdate)
                        val updatedStakingCalculatedState =
                          stakingCalculatedState
                            .focus(_.confirmed.value)
                            .modify(current =>
                              current.updatedWith(sourceAddress) {
                                case Some(confirmedSwaps) => Some(confirmedSwaps + stakingCalculatedStateAddress)
                                case None                 => Some(Set(stakingCalculatedStateAddress))
                              }
                            )
                            .focus(_.pending)
                            .replace(updatedPendingCalculatedState)

                        val updatedLiquidityPool = liquidityPoolsCalculatedState
                          .focus(_.confirmed.value)
                          .modify(_.updated(poolId.value, liquidityPoolUpdated))

                        val updatedCalculatedState = oldState.calculated
                          .focus(_.operations)
                          .modify(_.updated(OperationType.Staking, updatedStakingCalculatedState))
                          .focus(_.operations)
                          .modify(_.updated(OperationType.LiquidityPool, updatedLiquidityPool))

                        DataState(
                          AmmOnChainState(updates),
                          updatedCalculatedState,
                          oldState.sharedArtifacts
                        )
                    }
                  } yield response
                }
            } yield updatedState
        }
      }
    }
}
