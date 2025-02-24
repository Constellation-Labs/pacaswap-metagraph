package org.amm_metagraph.shared_data.combiners

import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.currency.dataApplication.DataState
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact._
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.AllowSpend
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hashed, Hasher, SecurityProvider}

import eu.timepit.refined.types.numeric.NonNegLong
import monocle.syntax.all._
import org.amm_metagraph.shared_data.SpendTransactions.{checkIfSpendActionAcceptedInGl0, generateSpendAction}
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.globalSnapshots.getAllowSpendsGlobalSnapshotsState
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, StakingUpdate}
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.Staking._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.codecs.HasherSelector

object StakingCombiner {

  private case class UpdatedTokenInformation(
    primaryTokenInformation: TokenInformation,
    pairTokenInformation: TokenInformation,
    newlyIssuedShares: Long
  )

  private def getUpdatedTokenInformation(
    stakingUpdate: StakingUpdate,
    liquidityPool: LiquidityPool
  ): UpdatedTokenInformation = {
    val (primaryToken, pairToken) = if (stakingUpdate.tokenAId == liquidityPool.tokenA.identifier) {
      (liquidityPool.tokenA, liquidityPool.tokenB)
    } else {
      (liquidityPool.tokenB, liquidityPool.tokenA)
    }

    val currentPrimaryTokenAmount = primaryToken.amount.value
    val currentPairTokenAmount = pairToken.amount.value

    val incomingPrimaryAmount = stakingUpdate.tokenAAmount.value
    val incomingPairAmount =
      (BigDecimal(incomingPrimaryAmount) * BigDecimal(currentPairTokenAmount)) / BigDecimal(currentPrimaryTokenAmount) // Maintain invariant

    val relativeDepositIncrease = incomingPrimaryAmount.toDouble / (currentPrimaryTokenAmount + incomingPrimaryAmount)
    val newlyIssuedShares = relativeDepositIncrease * liquidityPool.poolShares.totalShares.value

    UpdatedTokenInformation(
      primaryToken.copy(amount = incomingPrimaryAmount.toPosLongUnsafe),
      pairToken.copy(amount = incomingPairAmount.toLong.toPosLongUnsafe),
      newlyIssuedShares.toLong
    )
  }

  private def updateLiquidityPool(
    liquidityPool: LiquidityPool,
    signerAddress: Address,
    updatedTokenInformation: UpdatedTokenInformation
  ): LiquidityPool = {
    val primaryToken = updatedTokenInformation.primaryTokenInformation
    val pairToken = updatedTokenInformation.pairTokenInformation
    val newlyIssuedShares = updatedTokenInformation.newlyIssuedShares

    val tokenA = if (liquidityPool.tokenA.identifier == primaryToken.identifier) primaryToken else pairToken
    val tokenB = if (liquidityPool.tokenB.identifier == pairToken.identifier) pairToken else primaryToken

    val updatedTokenAAmount = (liquidityPool.tokenA.amount.value + tokenA.amount.value).toPosLongUnsafe
    val updatedTokenBAmount = (liquidityPool.tokenB.amount.value + tokenB.amount.value).toPosLongUnsafe

    val addressSharesAmount = liquidityPool.poolShares.addressShares.getOrElse(signerAddress, ShareAmount(Amount.empty))
    val updatedAddressSharesAmount = ShareAmount(Amount(NonNegLong.unsafeFrom(addressSharesAmount.value.value.value + newlyIssuedShares)))
    val updatedAddressShares = liquidityPool.poolShares.addressShares.updated(signerAddress, updatedAddressSharesAmount)

    liquidityPool.copy(
      tokenA = tokenA.copy(amount = updatedTokenAAmount),
      tokenB = tokenB.copy(amount = updatedTokenBAmount),
      k = BigInt(updatedTokenAAmount.value) * BigInt(updatedTokenBAmount.value),
      poolShares = PoolShares(
        (liquidityPool.poolShares.totalShares.value + newlyIssuedShares).toPosLongUnsafe,
        updatedAddressShares
      )
    )
  }

  private def validateUpdate(
    applicationConfig: ApplicationConfig,
    signedUpdate: Signed[StakingUpdate],
    maybeTokenInformation: Option[UpdatedTokenInformation],
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
      (maybeTokenInformation, maybeAllowSpendTokenA, maybeAllowSpendTokenB).mapN { (tokenInformation, allowSpendTokenA, allowSpendTokenB) =>
        val (tokenA, tokenB) = if (tokenInformation.primaryTokenInformation.identifier == allowSpendTokenA.currency) {
          (tokenInformation.primaryTokenInformation, tokenInformation.pairTokenInformation)
        } else {
          (tokenInformation.pairTokenInformation, tokenInformation.primaryTokenInformation)
        }
        if (tokenA.amount.value > allowSpendTokenA.amount.value.value) {
          FailedCalculatedState(
            AmountGreaterThanAllowSpendLimit(allowSpendTokenA.signed.value),
            EpochProgress(
              NonNegLong.unsafeFrom(
                lastSyncGlobalEpochProgress.value.value + applicationConfig.failedOperationsExpirationEpochProgresses.value.value
              )
            ),
            signedUpdate
          ).some
        } else if (tokenB.amount.value > allowSpendTokenB.amount.value.value) {
          FailedCalculatedState(
            AmountGreaterThanAllowSpendLimit(allowSpendTokenB.signed.value),
            EpochProgress(
              NonNegLong.unsafeFrom(
                lastSyncGlobalEpochProgress.value.value + applicationConfig.failedOperationsExpirationEpochProgresses.value.value
              )
            ),
            signedUpdate
          ).some
        } else if (allowSpendTokenA.lastValidEpochProgress.value.value < lastSyncGlobalEpochProgress.value.value) {
          FailedCalculatedState(
            AllowSpendExpired(allowSpendTokenA.signed.value),
            EpochProgress(NonNegLong.unsafeFrom(lastSyncGlobalEpochProgress.value.value + 30L)),
            signedUpdate
          ).some
        } else if (allowSpendTokenB.lastValidEpochProgress.value.value < lastSyncGlobalEpochProgress.value.value) {
          FailedCalculatedState(
            AllowSpendExpired(allowSpendTokenB.signed.value),
            EpochProgress(NonNegLong.unsafeFrom(lastSyncGlobalEpochProgress.value.value + 30L)),
            signedUpdate
          ).some
        } else {
          None
        }
      }.flatten
    }

  private def getUpdateAllowSpends[F[_]: Async: HasherSelector](
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
      case PendingAllowSpend(update) if update == signedStakingUpdate => true
      case _                                                          => false
    }

  private def removePendingSpendAction(
    stakingCalculatedState: StakingCalculatedState,
    signedStakingUpdate: Signed[StakingUpdate]
  ) =
    stakingCalculatedState.pending.filterNot {
      case PendingSpendAction(update, _) if update == signedStakingUpdate => true
      case _                                                              => false
    }

  def combinePendingSpendActionStaking[F[_]: Async: Hasher: SecurityProvider](
    applicationConfig: ApplicationConfig,
    acc: DataState[AmmOnChainState, AmmCalculatedState],
    pendingStakingUpdate: PendingSpendAction[StakingUpdate],
    currentSnapshotOrdinal: SnapshotOrdinal,
    lastSyncGlobalEpochProgress: EpochProgress,
    spendActions: List[SpendAction]
  ): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
    val liquidityPoolsCalculatedState = getLiquidityPoolCalculatedState(acc.calculated)
    val stakingCalculatedState = getStakingCalculatedState(acc.calculated)

    val signedStakingUpdate = pendingStakingUpdate.update
    val stakingUpdate = pendingStakingUpdate.update.value
    val updates = stakingUpdate :: acc.onChain.updates
    validateUpdate(applicationConfig, signedStakingUpdate, none, none, none, lastSyncGlobalEpochProgress) match {
      case Some(value) => handleFailedUpdate(updates, acc, value, stakingCalculatedState).pure
      case None =>
        for {
          metagraphGeneratedSpendActionHash <- Hasher[F].hash(pendingStakingUpdate.generatedSpendAction)
          globalSnapshotsHashes <- spendActions.traverse(action => Hasher[F].hash(action))
          allSpendActionsAccepted = checkIfSpendActionAcceptedInGl0(metagraphGeneratedSpendActionHash, globalSnapshotsHashes)
          updatedState <-
            if (!allSpendActionsAccepted) {
              acc.pure
            } else {
              for {
                signerAddress <- signedStakingUpdate.proofs.head.id.toAddress
                poolId <- buildLiquidityPoolUniqueIdentifier(stakingUpdate.tokenAId, stakingUpdate.tokenBId)
                liquidityPool <- getLiquidityPoolByPoolId(liquidityPoolsCalculatedState.confirmed.value, poolId)

                updatedTokenInformation = getUpdatedTokenInformation(stakingUpdate, liquidityPool)

                stakingReference <- StakingReference.of(signedStakingUpdate)

                liquidityPoolUpdated = updateLiquidityPool(liquidityPool, signerAddress, updatedTokenInformation)
                stakingCalculatedStateAddress =
                  StakingCalculatedStateAddress(
                    stakingUpdate.tokenAAllowSpend,
                    stakingUpdate.tokenBAllowSpend,
                    updatedTokenInformation.primaryTokenInformation,
                    updatedTokenInformation.pairTokenInformation,
                    stakingReference
                  )

                updatedPendingCalculatedState = removePendingSpendAction(stakingCalculatedState, signedStakingUpdate)
                updatedStakingCalculatedState =
                  stakingCalculatedState
                    .focus(_.confirmed.value)
                    .modify(current =>
                      current.updatedWith(signerAddress) {
                        case Some(confirmedSwaps) => Some(confirmedSwaps + stakingCalculatedStateAddress)
                        case None                 => Some(Set(stakingCalculatedStateAddress))
                      }
                    )
                    .focus(_.pending)
                    .replace(updatedPendingCalculatedState)

                updatedLiquidityPool = liquidityPoolsCalculatedState
                  .focus(_.confirmed.value)
                  .modify(_.updated(poolId.value, liquidityPoolUpdated))

                updatedCalculatedState = acc.calculated
                  .focus(_.operations)
                  .modify(_.updated(OperationType.Staking, updatedStakingCalculatedState))
                  .focus(_.operations)
                  .modify(_.updated(OperationType.LiquidityPool, updatedLiquidityPool))

                response = DataState(
                  AmmOnChainState(updates),
                  updatedCalculatedState
                )

              } yield response
            }
        } yield updatedState

    }
  }

  def combinePendingAllowSpendStaking[F[_]: Async: HasherSelector](
    applicationConfig: ApplicationConfig,
    acc: DataState[AmmOnChainState, AmmCalculatedState],
    signedStakingUpdate: Signed[StakingUpdate],
    lastSyncGlobalEpochProgress: EpochProgress,
    lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]]
  ): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
    val liquidityPoolsCalculatedState = getLiquidityPoolCalculatedState(acc.calculated)
    val stakingCalculatedState = getStakingCalculatedState(acc.calculated)

    val stakingUpdate = signedStakingUpdate.value
    val updates = stakingUpdate :: acc.onChain.updates

    validateUpdate(applicationConfig, signedStakingUpdate, none, none, none, lastSyncGlobalEpochProgress) match {
      case Some(failedCalculatedState) => handleFailedUpdate(updates, acc, failedCalculatedState, stakingCalculatedState).pure
      case None =>
        getUpdateAllowSpends(stakingUpdate, lastGlobalSnapshotsAllowSpends).flatMap {
          case (Some(allowSpendTokenA), Some(allowSpendTokenB)) =>
            for {
              poolId <- buildLiquidityPoolUniqueIdentifier(stakingUpdate.tokenAId, stakingUpdate.tokenBId)
              liquidityPool <- getLiquidityPoolByPoolId(liquidityPoolsCalculatedState.confirmed.value, poolId)

              updatedTokenInformation = getUpdatedTokenInformation(stakingUpdate, liquidityPool)

              maybeFailedUpdate = validateUpdate(
                applicationConfig,
                signedStakingUpdate,
                updatedTokenInformation.some,
                allowSpendTokenA.some,
                allowSpendTokenB.some,
                lastSyncGlobalEpochProgress
              )

              response = maybeFailedUpdate match {
                case Some(failedCalculatedState) => handleFailedUpdate(updates, acc, failedCalculatedState, stakingCalculatedState)
                case None =>
                  val spendAction = generateSpendAction(allowSpendTokenA, allowSpendTokenB)

                  val updatedPendingAllowSpendCalculatedState =
                    removePendingAllowSpend(stakingCalculatedState, signedStakingUpdate)
                  val updatedPendingSpendActionCalculatedState = updatedPendingAllowSpendCalculatedState + PendingSpendAction(
                    signedStakingUpdate,
                    spendAction
                  )

                  val updatedPendingStakingCalculatedState =
                    stakingCalculatedState
                      .focus(_.pending)
                      .replace(updatedPendingSpendActionCalculatedState)

                  val updatedCalculatedState = acc.calculated
                    .focus(_.operations)
                    .modify(_.updated(OperationType.Staking, updatedPendingStakingCalculatedState))

                  val updatedSharedArtifacts = acc.sharedArtifacts ++ SortedSet[SharedArtifact](
                    spendAction
                  )

                  DataState(
                    AmmOnChainState(updates),
                    updatedCalculatedState,
                    updatedSharedArtifacts
                  )
              }
            } yield response
          case _ => acc.pure
        }
    }
  }

  def combineNewStaking[F[_]: Async: HasherSelector](
    applicationConfig: ApplicationConfig,
    acc: DataState[AmmOnChainState, AmmCalculatedState],
    signedStakingUpdate: Signed[StakingUpdate],
    lastSyncGlobalEpochProgress: EpochProgress,
    lastGlobalSnapshotsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]]
  ): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
    val stakingUpdate = signedStakingUpdate.value
    val stakingCalculatedState = getStakingCalculatedState(acc.calculated)
    val pendingAllowSpendsCalculatedState = stakingCalculatedState.pending

    val updates = stakingUpdate :: acc.onChain.updates
    getUpdateAllowSpends(stakingUpdate, lastGlobalSnapshotsAllowSpends).flatMap {
      case (None, _) | (_, None) =>
        val updatedPendingCalculatedState =
          if (stakingUpdate.maxValidGsEpochProgress < lastSyncGlobalEpochProgress) {
            removePendingAllowSpend(stakingCalculatedState, signedStakingUpdate)
          } else if (!pendingAllowSpendsCalculatedState.exists(_.update == signedStakingUpdate)) {
            pendingAllowSpendsCalculatedState + PendingAllowSpend(signedStakingUpdate)
          } else {
            pendingAllowSpendsCalculatedState
          }

        val newStakingState = stakingCalculatedState
          .focus(_.pending)
          .replace(updatedPendingCalculatedState)

        val updatedCalculatedState = acc.calculated
          .focus(_.operations)
          .modify(_.updated(OperationType.Staking, newStakingState))

        DataState(
          AmmOnChainState(updates),
          updatedCalculatedState
        ).pure

      case (Some(_), Some(_)) =>
        combinePendingAllowSpendStaking(
          applicationConfig,
          acc,
          signedStakingUpdate,
          lastSyncGlobalEpochProgress,
          lastGlobalSnapshotsAllowSpends
        )
    }
  }
}
