package org.amm_metagraph.shared_data.services.combiners

import cats.data.OptionT
import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.ext.cats.syntax.next.catsSyntaxNext
import io.constellationnetwork.schema._
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact.SpendAction
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.CurrencyId
import io.constellationnetwork.security.signature.Signed

import fs2.concurrent.SignallingRef
import monocle.syntax.all._
import org.amm_metagraph.shared_data.globalSnapshots._
import org.amm_metagraph.shared_data.storages.GlobalSnapshotsStorage
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.Staking._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.Swap._
import org.amm_metagraph.shared_data.types.Withdrawal._
import org.amm_metagraph.shared_data.types.codecs.HasherSelector
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

trait L0CombinerService[F[_]] {
  def combine(
    oldState: DataState[AmmOnChainState, AmmCalculatedState],
    updates: List[Signed[AmmUpdate]]
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]]
}

object L0CombinerService {
  def make[F[_]: Async](
    globalSnapshotsStorage: GlobalSnapshotsStorage[F],
    governanceCombinerService: GovernanceCombinerService[F],
    liquidityPoolCombinerService: LiquidityPoolCombinerService[F],
    stakingCombinerService: StakingCombinerService[F],
    swapCombinerService: SwapCombinerService[F],
    withdrawalCombinerService: WithdrawalCombinerService[F],
    rewardsCombinerService: RewardsDistributionService[F],
    rewardsWithdrawService: RewardsWithdrawService[F]
  ): F[L0CombinerService[F]] = for {
    currentSnapshotOrdinalR <- SignallingRef
      .of[F, SnapshotOrdinal](SnapshotOrdinal.MinValue)
    result = make(
      globalSnapshotsStorage,
      governanceCombinerService,
      liquidityPoolCombinerService,
      stakingCombinerService,
      swapCombinerService,
      withdrawalCombinerService,
      rewardsCombinerService,
      rewardsWithdrawService,
      currentSnapshotOrdinalR
    )
  } yield result

  def make[F[_]: Async](
    globalSnapshotsStorage: GlobalSnapshotsStorage[F],
    governanceCombinerService: GovernanceCombinerService[F],
    liquidityPoolCombinerService: LiquidityPoolCombinerService[F],
    stakingCombinerService: StakingCombinerService[F],
    swapCombinerService: SwapCombinerService[F],
    withdrawalCombinerService: WithdrawalCombinerService[F],
    rewardsCombinerService: RewardsDistributionService[F],
    rewardsWithdrawService: RewardsWithdrawService[F],
    currentSnapshotOrdinalR: SignallingRef[F, SnapshotOrdinal]
  ): L0CombinerService[F] = {
    new L0CombinerService[F] {
      val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

      def getPendingAllowSpendsUpdates(
        state: AmmCalculatedState
      ): SortedSet[PendingAllowSpend[AmmUpdate]] =
        getPendingAllowSpendsLiquidityPoolUpdates(state) ++
          getPendingAllowSpendsStakingUpdates(state) ++
          getPendingAllowSpendsSwapUpdates(state)

      def getPendingSpendActionsUpdates(
        state: AmmCalculatedState
      ): SortedSet[PendingSpendAction[AmmUpdate]] =
        getPendingSpendActionLiquidityPoolUpdates(state) ++
          getPendingSpendActionStakingUpdates(state) ++
          getPendingSpendActionSwapUpdates(state)
//          getPendingSpendActionWithdrawalUpdates(state)

      private def combineIncomingUpdates(
        incomingUpdates: List[Signed[AmmUpdate]],
        lastSyncGlobalEpochProgress: EpochProgress,
        currentSnapshotEpochProgress: EpochProgress,
        globalSnapshotSyncAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[swap.AllowSpend]]]],
        stateCombinedByVotingPower: DataState[AmmOnChainState, AmmCalculatedState],
        currencyId: CurrencyId
      )(implicit context: L0NodeContext[F]) =
        if (incomingUpdates.isEmpty) {
          logger.info("Snapshot without any updates, updating the state to empty updates").as(stateCombinedByVotingPower)
        } else {
          logger.info(s"Incoming updates: ${incomingUpdates.size}") >>
            incomingUpdates.foldLeftM(stateCombinedByVotingPower) { (acc, signedUpdate) =>
              for {
                combinedState <- signedUpdate.value match {
                  case liquidityPoolUpdate: LiquidityPoolUpdate =>
                    logger.info(s"Received liquidity pool update: $liquidityPoolUpdate") >>
                      liquidityPoolCombinerService.combineNew(
                        Signed(liquidityPoolUpdate, signedUpdate.proofs),
                        acc,
                        lastSyncGlobalEpochProgress,
                        globalSnapshotSyncAllowSpends,
                        currencyId
                      )

                  case stakingUpdate: StakingUpdate =>
                    logger.info(s"Received staking update: $stakingUpdate") >>
                      stakingCombinerService.combineNew(
                        Signed(stakingUpdate, signedUpdate.proofs),
                        acc,
                        lastSyncGlobalEpochProgress,
                        globalSnapshotSyncAllowSpends,
                        currencyId
                      )

                  case withdrawalUpdate: WithdrawalUpdate =>
                    logger.info(s"Received withdrawal update: $withdrawalUpdate").as(acc)
                  //                      withdrawalCombinerService.combineNew(
//                        Signed(withdrawalUpdate, signedUpdate.proofs),
//                        acc,
//                        lastSyncGlobalEpochProgress,
//                        globalSnapshotSyncAllowSpends,
//                        currencyId
//                      )

                  case swapUpdate: SwapUpdate =>
                    logger.info(s"Received swap update: $swapUpdate") >>
                      swapCombinerService.combineNew(
                        Signed(swapUpdate, signedUpdate.proofs),
                        acc,
                        lastSyncGlobalEpochProgress,
                        globalSnapshotSyncAllowSpends,
                        currencyId
                      )

                  case rewardAllocationVoteUpdate: RewardAllocationVoteUpdate =>
                    logger.info(s"Received reward allocation vote update: $rewardAllocationVoteUpdate") >>
                      governanceCombinerService.combineNew(
                        Signed(rewardAllocationVoteUpdate, signedUpdate.proofs),
                        acc,
                        lastSyncGlobalEpochProgress,
                        currentSnapshotEpochProgress,
                        globalSnapshotSyncAllowSpends,
                        currencyId
                      )
                  case rewardWithdrawUpdate: RewardWithdrawUpdate =>
                    logger.info(s"Received reward withdraw update: $rewardWithdrawUpdate") >>
                      rewardsWithdrawService.combineNew(
                        Signed(rewardWithdrawUpdate, signedUpdate.proofs),
                        acc,
                        currentSnapshotEpochProgress,
                        lastSyncGlobalEpochProgress
                      )
                }
              } yield combinedState
            }
        }

      private def combinePendingAllowSpendsUpdates(
        lastSyncGlobalEpochProgress: EpochProgress,
        globalSnapshotSyncAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[swap.AllowSpend]]]],
        pendingAllowSpends: SortedSet[PendingAllowSpend[AmmUpdate]],
        stateCombinedByNewUpdates: DataState[AmmOnChainState, AmmCalculatedState],
        currencyId: CurrencyId
      )(implicit context: L0NodeContext[F]) = {
        val pendingAllowSpendsToCombine = if (globalSnapshotSyncAllowSpends.nonEmpty) {
          pendingAllowSpends
        } else {
          pendingAllowSpends.filter { pending =>
            pending.update.value match {
              case lpUpdate: LiquidityPoolUpdate      => lpUpdate.maxValidGsEpochProgress < lastSyncGlobalEpochProgress
              case stakingUpdate: StakingUpdate       => stakingUpdate.maxValidGsEpochProgress < lastSyncGlobalEpochProgress
              case withdrawalUpdate: WithdrawalUpdate => withdrawalUpdate.maxValidGsEpochProgress < lastSyncGlobalEpochProgress
              case swapUpdate: SwapUpdate             => swapUpdate.maxValidGsEpochProgress < lastSyncGlobalEpochProgress
              case _                                  => false
            }
          }
        }

        pendingAllowSpendsToCombine.toList
          .foldLeftM(stateCombinedByNewUpdates) { (acc, pendingUpdate) =>
            pendingUpdate.update.value match {
              case lpUpdate: LiquidityPoolUpdate =>
                logger.info(s"Processing LP pending allow spend: ${lpUpdate}") >>
                  liquidityPoolCombinerService.combinePendingAllowSpend(
                    PendingAllowSpend(
                      Signed(lpUpdate, pendingUpdate.update.proofs),
                      pendingUpdate.updateHash,
                      pendingUpdate.pricingTokenInfo
                    ),
                    acc,
                    lastSyncGlobalEpochProgress,
                    globalSnapshotSyncAllowSpends,
                    currencyId
                  )

              case stakingUpdate: StakingUpdate =>
                logger.info(s"Processing staking pending allow spend: ${stakingUpdate}") >>
                  stakingCombinerService.combinePendingAllowSpend(
                    PendingAllowSpend(
                      Signed(stakingUpdate, pendingUpdate.update.proofs),
                      pendingUpdate.updateHash,
                      pendingUpdate.pricingTokenInfo
                    ),
                    acc,
                    lastSyncGlobalEpochProgress,
                    globalSnapshotSyncAllowSpends,
                    currencyId
                  )

              case swapUpdate: SwapUpdate =>
                logger.info(s"Processing swap pending allow spend: ${swapUpdate}") >>
                  swapCombinerService.combinePendingAllowSpend(
                    PendingAllowSpend(
                      Signed(swapUpdate, pendingUpdate.update.proofs),
                      pendingUpdate.updateHash,
                      pendingUpdate.pricingTokenInfo
                    ),
                    acc,
                    lastSyncGlobalEpochProgress,
                    globalSnapshotSyncAllowSpends,
                    currencyId
                  )
              case _ => acc.pure
            }
          }
      }

      private def combinePendingSpendTransactionsUpdates(
        lastSyncGlobalEpochProgress: EpochProgress,
        globalSnapshotSyncSpendActions: List[SpendAction],
        currencySnapshotOrdinal: SnapshotOrdinal,
        pendingSpendActions: SortedSet[PendingSpendAction[AmmUpdate]],
        stateCombinedByPendingAllowSpends: DataState[AmmOnChainState, AmmCalculatedState],
        currencyId: CurrencyId
      )(implicit context: L0NodeContext[F]) = {
        val pendingSpendActionsToCombine = if (globalSnapshotSyncSpendActions.nonEmpty) {
          pendingSpendActions
        } else {
          pendingSpendActions.filter { pending =>
            pending.update.value match {
              case lpUpdate: LiquidityPoolUpdate      => lpUpdate.maxValidGsEpochProgress < lastSyncGlobalEpochProgress
              case stakingUpdate: StakingUpdate       => stakingUpdate.maxValidGsEpochProgress < lastSyncGlobalEpochProgress
              case withdrawalUpdate: WithdrawalUpdate => withdrawalUpdate.maxValidGsEpochProgress < lastSyncGlobalEpochProgress
              case swapUpdate: SwapUpdate             => swapUpdate.maxValidGsEpochProgress < lastSyncGlobalEpochProgress
              case _                                  => false
            }
          }
        }

        pendingSpendActionsToCombine.toList
          .foldLeftM(stateCombinedByPendingAllowSpends) { (acc, pendingUpdate) =>
            pendingUpdate.update.value match {
              case lpUpdate: LiquidityPoolUpdate =>
                logger.info(s"Processing LP spend action: ${lpUpdate}") >>
                  liquidityPoolCombinerService.combinePendingSpendAction(
                    PendingSpendAction(
                      Signed(lpUpdate, pendingUpdate.update.proofs),
                      pendingUpdate.updateHash,
                      pendingUpdate.generatedSpendAction,
                      pendingUpdate.pricingTokenInfo
                    ),
                    acc,
                    lastSyncGlobalEpochProgress,
                    globalSnapshotSyncSpendActions,
                    currencySnapshotOrdinal,
                    currencyId
                  )
              case stakingUpdate: StakingUpdate =>
                logger.info(s"Processing Staking spend action: ${stakingUpdate}") >>
                  stakingCombinerService.combinePendingSpendAction(
                    PendingSpendAction(
                      Signed(stakingUpdate, pendingUpdate.update.proofs),
                      pendingUpdate.updateHash,
                      pendingUpdate.generatedSpendAction,
                      pendingUpdate.pricingTokenInfo
                    ),
                    acc,
                    lastSyncGlobalEpochProgress,
                    globalSnapshotSyncSpendActions,
                    currencySnapshotOrdinal,
                    currencyId
                  )
              case withdrawalUpdate: WithdrawalUpdate =>
                acc.pure
              case swapUpdate: SwapUpdate =>
                logger.info(s"Processing Swap spend action: ${swapUpdate}") >>
                  swapCombinerService.combinePendingSpendAction(
                    PendingSpendAction(
                      Signed(swapUpdate, pendingUpdate.update.proofs),
                      pendingUpdate.updateHash,
                      pendingUpdate.generatedSpendAction,
                      pendingUpdate.pricingTokenInfo
                    ),
                    acc,
                    lastSyncGlobalEpochProgress,
                    globalSnapshotSyncSpendActions,
                    currencySnapshotOrdinal,
                    currencyId
                  )
              case _ => acc.pure
            }
          }
      }

      private def cleanupExpiredOperations(
        stateCombinedByPendingSpendTransactions: DataState[AmmOnChainState, AmmCalculatedState],
        lastSyncGlobalEpochProgress: EpochProgress
      ) = {
        val liquidityPoolCleanupState = liquidityPoolCombinerService.cleanupExpiredOperations(
          stateCombinedByPendingSpendTransactions,
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

      override def combine(
        oldState: DataState[AmmOnChainState, AmmCalculatedState],
        incomingUpdates: List[Signed[AmmUpdate]]
      )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
        val combined = for {
          (lastCurrencySnapshot, lastCurrencySnapshotInfo) <- OptionT(context.getLastCurrencySnapshotCombined).getOrRaise(
            new IllegalStateException("lastCurrencySnapshot unavailable")
          )

          lastSyncGlobal = context.getLastSynchronizedGlobalSnapshotCombined
          (lastSyncGlobalEpochProgress, lastSyncGlobalOrdinal, lastSyncState) <- OptionT(lastSyncGlobal).map {
            case (snapshot, info) => (snapshot.epochProgress, snapshot.ordinal, info)
          }.getOrElseF {
            val message = "Could not get last synchronized global snapshot data"
            logger.error(message) >> new Exception(message).raiseError[F, (EpochProgress, SnapshotOrdinal, GlobalSnapshotInfo)]
          }

          currentSnapshotOrdinalFromContext = lastCurrencySnapshot.ordinal.next
          currentSnapshotEpochProgress = lastCurrencySnapshot.epochProgress.next
          lastSnapshotOrdinalStored <- currentSnapshotOrdinalR.get

          _ <- logger.info(s"LAST SYNC GLOBAL SNAPSHOT EPOCH PROGRESS: $lastSyncGlobalEpochProgress")
          _ <- logger.info(s"LAST SYNC GLOBAL SNAPSHOT ORDINAL: $lastSyncGlobalOrdinal")
          _ <- logger.info(s"CURRENT SNAPSHOT ORDINAL: $currentSnapshotOrdinalFromContext")
          _ <- logger.info(s"LAST SNAPSHOT ORDINAL STORED: $lastSnapshotOrdinalStored")

          globalSnapshotSyncAllowSpends = getAllowSpendsFromGlobalSnapshotState(lastSyncState)
          globalSnapshotsSyncSpendActions <- getSpendActionsFromGlobalSnapshots(
            oldState.calculated.lastSyncGlobalSnapshotOrdinal,
            lastSyncGlobalOrdinal,
            globalSnapshotsStorage
          )

          /*
            On each call to `combine`, if the snapshot ordinal has increased,
            we must clear the previous `onChain` and `sharedArtifacts` state to avoid duplication.
            If the ordinal remains the same, we preserve the state.
            In other words:
              - state is preserved across updates within the same ordinal,
              - state is reset whenever the ordinal advances.
           */
          newState =
            if (lastSnapshotOrdinalStored < currentSnapshotOrdinalFromContext) {
              oldState
                .focus(_.onChain)
                .replace(AmmOnChainState.empty)
                .focus(_.sharedArtifacts)
                .replace(SortedSet.empty)
            } else {
              oldState
            }

          pendingAllowSpends = getPendingAllowSpendsUpdates(newState.calculated)
          pendingSpendActions = getPendingSpendActionsUpdates(newState.calculated)

          updatedVotingPowers = governanceCombinerService.updateVotingPowers(
            newState.calculated,
            lastCurrencySnapshotInfo,
            lastSyncGlobalEpochProgress
          )

          updatedVotingPowerState = newState.calculated
            .focus(_.votingPowers)
            .replace(updatedVotingPowers)

          stateCombinedByVotingPower = newState
            .focus(_.calculated)
            .replace(updatedVotingPowerState)

          currencyId <- context.getCurrencyId

          stateCombinedByNewUpdates <-
            combineIncomingUpdates(
              incomingUpdates,
              lastSyncGlobalEpochProgress,
              currentSnapshotEpochProgress,
              globalSnapshotSyncAllowSpends,
              stateCombinedByVotingPower,
              currencyId
            )

          stateCombinedByPendingAllowSpendUpdates <-
            combinePendingAllowSpendsUpdates(
              lastSyncGlobalEpochProgress,
              globalSnapshotSyncAllowSpends,
              pendingAllowSpends,
              stateCombinedByNewUpdates,
              currencyId
            )

          stateCombinedByPendingSpendActions <-
            combinePendingSpendTransactionsUpdates(
              lastSyncGlobalEpochProgress,
              globalSnapshotsSyncSpendActions,
              currentSnapshotOrdinalFromContext,
              pendingSpendActions,
              stateCombinedByPendingAllowSpendUpdates,
              currencyId
            )

          stateCombinedByCleanupOperations = cleanupExpiredOperations(
            stateCombinedByPendingSpendActions,
            lastSyncGlobalEpochProgress
          )

          stateCombinedGovernanceRewards <- governanceCombinerService.handleMonthExpiration(
            stateCombinedByCleanupOperations,
            currentSnapshotEpochProgress
          )

          stateUpdatedByLastGlobalSync = stateCombinedGovernanceRewards
            .focus(_.calculated.lastSyncGlobalSnapshotOrdinal)
            .replace(lastSyncGlobalOrdinal)

          _ <- logger.info(s"SpendTxnProduced: ${stateUpdatedByLastGlobalSync.sharedArtifacts}")

          stateUpdatedRewardDistribution <- rewardsCombinerService.updateRewardsDistribution(
            lastCurrencySnapshot.signed,
            stateUpdatedByLastGlobalSync,
            currentSnapshotEpochProgress
          )

          _ <- currentSnapshotOrdinalR.set(currentSnapshotOrdinalFromContext)
        } yield stateUpdatedRewardDistribution

        combined.handleErrorWith { e =>
          logger.error(s"Error when combining: ${e.getMessage}").as(oldState)
        }
      }
    }
  }
}
