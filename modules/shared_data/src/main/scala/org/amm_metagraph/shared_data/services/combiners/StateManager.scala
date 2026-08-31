package org.amm_metagraph.shared_data.services.combiners

import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.SortedSet

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.epoch.EpochProgress

import fs2.concurrent.SignallingRef
import monocle.syntax.all._
import org.amm_metagraph.shared_data.services.combiners.operations._
import org.amm_metagraph.shared_data.types.States.{AmmCalculatedState, AmmOnChainState}
import org.amm_metagraph.shared_data.{IncidentSwapRollbackCorrection, IncidentTokenLockRemediation, ProtocolActivation}
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

trait StateManager[F[_]] {
  def prepareStateForNewOrdinal(
    oldState: DataState[AmmOnChainState, AmmCalculatedState],
    context: ProcessingContext
  ): F[DataState[AmmOnChainState, AmmCalculatedState]]

  def cleanupAndFinalize(
    state: DataState[AmmOnChainState, AmmCalculatedState],
    context: ProcessingContext
  )(implicit l0Context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]]
}

object StateManager {

  /** How far the cursor may fall behind the synchronized head before it is released.
    *
    * Holding on incomplete evidence is correct in principle and self-sustaining in practice: the in-memory global snapshot store is
    * forward-only, so after a restart the first ordinal above the cursor is never resolvable, the hold never lifts, and the cursor never
    * advances. On 2026-08-31 that pinned the cursor at global 6855921 for three hours. The monitoring service watches exactly this value,
    * saw no progress, and force-restarted all three nodes thirteen times, each restart re-emptying the cache that caused the hold.
    *
    * So the hold is bounded. Past the bound the cursor advances and says so, which restores liveness. It does NOT make expiry safe again -
    * that is what `generatedAtGlobalOrdinal` is for, and the two must ship together: releasing the cursor without it is precisely how an
    * already-settled operation gets judged against a window that never contained it.
    */
  val maxCursorHoldOrdinals: Long = 500L

  private[shared_data] def selectNextGlobalSnapshotCursor(
    currentSnapshotOrdinal: SnapshotOrdinal,
    evidenceComplete: Boolean,
    lastSyncGlobalOrdinal: SnapshotOrdinal,
    lastContiguousGlobalSnapshotOrdinal: SnapshotOrdinal
  ): SnapshotOrdinal =
    if (
      ProtocolActivation.evidenceCompletenessFirstActive(currentSnapshotOrdinal) &&
      !evidenceComplete &&
      lastSyncGlobalOrdinal.value.value - lastContiguousGlobalSnapshotOrdinal.value.value <= maxCursorHoldOrdinals
    ) lastContiguousGlobalSnapshotOrdinal
    else lastSyncGlobalOrdinal

  def make[F[_]: Async](
    liquidityPoolCombinerService: LiquidityPoolCombinerService[F],
    stakingCombinerService: StakingCombinerService[F],
    swapCombinerService: SwapCombinerService[F],
    withdrawalCombinerService: WithdrawalCombinerService[F],
    governanceCombinerService: GovernanceCombinerService[F],
    rewardsCombinerService: RewardsDistributionService[F],
    rewardsWithdrawService: RewardsWithdrawService[F],
    currentSnapshotOrdinalR: SignallingRef[F, SnapshotOrdinal]
  ): StateManager[F] = new StateManager[F] {

    val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

    override def prepareStateForNewOrdinal(
      oldState: DataState[AmmOnChainState, AmmCalculatedState],
      context: ProcessingContext
    ): F[DataState[AmmOnChainState, AmmCalculatedState]] =
      for {
        lastSnapshotOrdinalStored <- currentSnapshotOrdinalR.get
        _ <- logger.info(s"lastSnapshotOrdinalStored=$lastSnapshotOrdinalStored")

        /*
        On each call to `combine`, if the snapshot ordinal has increased,
        we must clear the previous `onChain` and `sharedArtifacts` state to avoid duplication.
        If the ordinal remains the same, we preserve the state.
         */
        // The node-local ref cannot be trusted: it is not persisted, so a restarted node and a
        // node that already combined this ordinal disagree about whether to clear, which diverges
        // onChain (the reward-minting instructions) and can replay the previous snapshot's reward
        // chunks. From the activation ordinal we take the ordinal out of the state itself, which
        // every node sees identically. Below it the old ref is used, so history replays unchanged.
        lastOrdinalForClearing =
          oldState.calculated.lastProcessedCurrencyOrdinal
            .filter(_ => ProtocolActivation.reserveAccountingFixesActive(context.currentSnapshotOrdinal))
            .getOrElse(lastSnapshotOrdinalStored)

        newState =
          if (lastOrdinalForClearing < context.currentSnapshotOrdinal) {
            oldState
              .focus(_.onChain)
              .replace(AmmOnChainState.empty)
              .focus(_.sharedArtifacts)
              .replace(SortedSet.empty)
          } else {
            oldState
          }

        // Update voting powers
        recomputedVotingPowers = governanceCombinerService.updateVotingPowers(
          newState.calculated,
          context.lastCurrencySnapshotInfo,
          context.lastSyncGlobalEpochProgress
        )

        // Active TokenLocks are imported on every ordinal, so the seven incident locks must be
        // removed after every rebuild until they are retired on Tessellation. Unrelated locks owned
        // by the same addresses remain and continue to confer their legitimate voting power.
        updatedVotingPowers = IncidentTokenLockRemediation.removeFromVotingPowers(
          recomputedVotingPowers,
          context.currentSnapshotOrdinal
        )

        // One-shot: restores the two swaps the combine rolled back at 741789 after they had already
        // settled. Left aborts the combine rather than committing a partial correction.
        correctedOperations <- IncidentSwapRollbackCorrection
          .applyTo(newState.calculated, context.currentSnapshotOrdinal)
          .fold(
            err =>
              logger.error(s"INCIDENT_SWAP_ROLLBACK_CORRECTION refused: ${err.message}") >>
                new IllegalStateException(s"swap rollback correction failed: ${err.message}")
                  .raiseError[F, AmmCalculatedState],
            _.pure[F]
          )

        updatedVotingPowerState = correctedOperations
          .focus(_.votingPowers)
          .replace(updatedVotingPowers)

        stateCombinedByVotingPower = newState
          .focus(_.calculated)
          .replace(updatedVotingPowerState)

        // Historically month expiration ran during cleanup, after incoming updates. A vote accepted
        // in the first epoch of a new month was therefore stamped with that epoch, excluded from the
        // closing month's result, and then cleared. After activation, close the old month first so
        // subsequent updates remain live for the new month. The legacy order is retained below the
        // gate so historical calculated-state proofs replay unchanged.
        statePreparedForUpdates <-
          if (ProtocolActivation.governanceMonthBoundaryFixActive(context.currentSnapshotOrdinal))
            governanceCombinerService.handleMonthExpiration(
              stateCombinedByVotingPower,
              context.currentSnapshotEpochProgress
            )
          else stateCombinedByVotingPower.pure[F]

      } yield statePreparedForUpdates

    override def cleanupAndFinalize(
      state: DataState[AmmOnChainState, AmmCalculatedState],
      context: ProcessingContext
    )(implicit l0Context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
      val cleanedState = cleanupExpiredOperations(state, context.lastSyncGlobalEpochProgress)

      for {
        governanceRewardsState <-
          if (ProtocolActivation.governanceMonthBoundaryFixActive(context.currentSnapshotOrdinal))
            cleanedState.pure[F]
          else
            governanceCombinerService.handleMonthExpiration(
              cleanedState,
              context.currentSnapshotEpochProgress
            )

        rewardsCleanedState = rewardsWithdrawService.clearDistributedRewards(
          governanceRewardsState,
          context.currentSnapshotEpochProgress
        )

        nextGlobalSnapshotCursor = StateManager.selectNextGlobalSnapshotCursor(
          context.currentSnapshotOrdinal,
          context.spendActionsEvidenceComplete,
          context.lastSyncGlobalOrdinal,
          context.lastContiguousGlobalSnapshotOrdinal
        )

        stateUpdatedByLastGlobalSync = rewardsCleanedState
          .focus(_.calculated.lastSyncGlobalSnapshotOrdinal)
          .replace(nextGlobalSnapshotCursor)

        _ <- logger.info(s"spendTxnProduced=${stateUpdatedByLastGlobalSync.sharedArtifacts}")

        stateUpdatedRewardDistribution <- rewardsCombinerService.updateRewardsDistribution(
          context.lastCurrencySnapshot.signed,
          stateUpdatedByLastGlobalSync,
          context.currentSnapshotEpochProgress
        )

        _ <- currentSnapshotOrdinalR.set(context.currentSnapshotOrdinal)

        // Record it in the state too, so the next combine does not have to trust the ref.
        finalState =
          if (ProtocolActivation.reserveAccountingFixesActive(context.currentSnapshotOrdinal))
            stateUpdatedRewardDistribution
              .focus(_.calculated.lastProcessedCurrencyOrdinal)
              .replace(context.currentSnapshotOrdinal.some)
          else stateUpdatedRewardDistribution

      } yield finalState
    }

    private def cleanupExpiredOperations(
      state: DataState[AmmOnChainState, AmmCalculatedState],
      lastSyncGlobalEpochProgress: EpochProgress
    ) = {
      val liquidityPoolCleanupState = liquidityPoolCombinerService.cleanupExpiredOperations(
        state,
        lastSyncGlobalEpochProgress
      )

      val stakesCleanupState = stakingCombinerService.cleanupExpiredOperations(
        liquidityPoolCleanupState,
        lastSyncGlobalEpochProgress
      )

      val swapsCleanupState = swapCombinerService.cleanupExpiredOperations(
        stakesCleanupState,
        lastSyncGlobalEpochProgress
      )

      withdrawalCombinerService.cleanupExpiredOperations(
        swapsCleanupState,
        lastSyncGlobalEpochProgress
      )
    }
  }
}
