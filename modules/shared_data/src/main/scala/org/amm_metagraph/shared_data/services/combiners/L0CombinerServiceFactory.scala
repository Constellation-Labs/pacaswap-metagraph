package org.amm_metagraph.shared_data.services.combiners

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.epoch.EpochProgress

import fs2.concurrent.SignallingRef
import org.amm_metagraph.shared_data.services.combiners.operations._
import org.amm_metagraph.shared_data.storages.GlobalSnapshotsStorage
import org.amm_metagraph.shared_data.types.codecs.HasherSelector

object L0CombinerServiceFactory {
  def make[F[_]: Async: HasherSelector](
    globalSnapshotsStorage: GlobalSnapshotsStorage[F],
    governanceCombinerService: GovernanceCombinerService[F],
    liquidityPoolCombinerService: LiquidityPoolCombinerService[F],
    stakingCombinerService: StakingCombinerService[F],
    swapCombinerService: SwapCombinerService[F],
    withdrawalCombinerService: WithdrawalCombinerService[F],
    rewardsCombinerService: RewardsDistributionService[F],
    rewardsWithdrawService: RewardsWithdrawService[F],
    globalSyncDataIntegrityActivation: EpochProgress = EpochProgress.MaxValue
  ): F[L0CombinerService[F]] = for {
    currentSnapshotOrdinalR <- SignallingRef.of[F, SnapshotOrdinal](SnapshotOrdinal.MinValue)

    contextHelper = ContextHelper.make(globalSnapshotsStorage, globalSyncDataIntegrityActivation)

    stateManager = StateManager.make(
      liquidityPoolCombinerService,
      stakingCombinerService,
      swapCombinerService,
      withdrawalCombinerService,
      governanceCombinerService,
      rewardsCombinerService,
      rewardsWithdrawService,
      currentSnapshotOrdinalR
    )

    updateProcessor = NewUpdatesProcessor.make(
      liquidityPoolCombinerService,
      stakingCombinerService,
      swapCombinerService,
      withdrawalCombinerService,
      governanceCombinerService,
      rewardsWithdrawService
    )

    pendingOperationsProcessor = PendingOperationsProcessor.make(
      liquidityPoolCombinerService,
      stakingCombinerService,
      swapCombinerService,
      withdrawalCombinerService
    )

    oneTimeFixesHandler = OneTimeFixesHandler.make(currentSnapshotOrdinalR)
    collateralInvariant = CollateralInvariant.make[F]

    service = L0CombinerService.make(
      stateManager,
      updateProcessor,
      pendingOperationsProcessor,
      oneTimeFixesHandler,
      contextHelper,
      collateralInvariant
    )
  } yield service
}
