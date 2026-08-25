package org.amm_metagraph.shared_data.services.combiners

import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}
import scala.util.{Failure, Success}

import io.constellationnetwork.currency.dataApplication.DataState
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.swap.CurrencyId
import io.constellationnetwork.security.signature.Signed

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.{NonNegLong, PosLong}
import fs2.concurrent.SignallingRef
import monocle.syntax.all._
import org.amm_metagraph.shared_data.loaders.{LiquidityPoolLoader, PoolReservesLoader}
import org.amm_metagraph.shared_data.types.DataUpdates.AmmUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.Rewards.RewardInfo
import org.amm_metagraph.shared_data.types.States._
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

trait OneTimeFixesHandler[F[_]] {
  def handleOneTimeFixesOrdinals(
    oldState: DataState[AmmOnChainState, AmmCalculatedState],
    currentSnapshotOrdinal: SnapshotOrdinal
  ): F[Option[DataState[AmmOnChainState, AmmCalculatedState]]]
}

object OneTimeFixesHandler {
  def make[F[_]: Async](
    currentSnapshotOrdinalR: SignallingRef[F, SnapshotOrdinal]
  ): OneTimeFixesHandler[F] = new OneTimeFixesHandler[F] {

    val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

    val updatePoolsOrdinal: SnapshotOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(111700L))
    val flipTokensOrdinal: SnapshotOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(112222L))
    val updatePools2Ordinal: SnapshotOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(116018L))
    val updatePools3Ordinal: SnapshotOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(121013L))
    val updatePools4Ordinal: SnapshotOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(122569L))
    val updatePools5Ordinal: SnapshotOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(122869L))
    val updatePools6Ordinal: SnapshotOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(126824L))
    val updatePools7Ordinal: SnapshotOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(127786L))
    val updatePools8Ordinal: SnapshotOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(129586L))
    val updatePools9Ordinal: SnapshotOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(139337L))
    val updatePools10Ordinal: SnapshotOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(144253L))
    val updatePools11Ordinal: SnapshotOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(150973L))
    val updatePools12Ordinal: SnapshotOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(161148L))
    val updateUSDCPool: SnapshotOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(116115L))
    // Same ordinal as the fee-transaction balance deductions in Main.customArtifacts. The pool
    // reserves and the balances backing them have to move in one step or withdrawals in between
    // would price against reserves the metagraph can no longer pay out.
    val restorePoolReservesOrdinal: SnapshotOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(735000L))

    // The four wallets the fee-transaction mint credited, plus the address that signed the four
    // fee transactions. The BalanceAdjustment artifacts emitted at this same ordinal zero their
    // balances; calculated state is separate from the balance map, so anything they left behind
    // there -- a pending swap, an LP share, voting power, an unclaimed reward -- survives the
    // deduction and can still be settled after the restart. This removes all of it.
    //
    // Deliberately only the attacker-controlled addresses. The other twelve in the deduction set
    // are third parties who bought phantom PACA out of the pool; they keep their positions.
    val frozenAddresses: Set[Address] = Set(
      Address("DAG5Yno9tMKHLe1G6J5QSbiqRicWV2HRKunDtFuR"),
      Address("DAG8uqhyGtFABWSS5KeVB2ia1R4vXop5AeijXeoU"),
      Address("DAG4w5mUqNNxQNS4hgdpx3E8FGgiu2UCRsJxHwhX"),
      Address("DAG7ZjENTP4T36PPSp3skJdTHtQbcuLfpEaAFWdn"),
      Address("DAG1kEmLAgnCVBURHrL4AMsfn9TZdk4QCYQ8tUu3")
    )

    override def handleOneTimeFixesOrdinals(
      oldState: DataState[AmmOnChainState, AmmCalculatedState],
      currentSnapshotOrdinal: SnapshotOrdinal
    ): F[Option[DataState[AmmOnChainState, AmmCalculatedState]]] =
      if (currentSnapshotOrdinal === updatePoolsOrdinal) {
        updatePoolsAtOrdinal(oldState, "updated-pools.json").flatMap { updatedState =>
          currentSnapshotOrdinalR.set(currentSnapshotOrdinal).as(Some(updatedState))
        }
      } else if (currentSnapshotOrdinal === flipTokensOrdinal) {
        flipPoolTokens(oldState).flatMap { updatedState =>
          currentSnapshotOrdinalR.set(currentSnapshotOrdinal).as(Some(updatedState))
        }
      } else if (currentSnapshotOrdinal === updatePools2Ordinal) {
        updatePoolsAtOrdinal(oldState, "updated-pools-2.json").flatMap { updatedState =>
          currentSnapshotOrdinalR.set(currentSnapshotOrdinal).as(Some(updatedState))
        }
      } else if (currentSnapshotOrdinal === updatePools3Ordinal) {
        updatePoolsAtOrdinal(oldState, "updated-pools-3.json").flatMap { updatedState =>
          currentSnapshotOrdinalR.set(currentSnapshotOrdinal).as(Some(updatedState))
        }
      } else if (currentSnapshotOrdinal === updatePools4Ordinal) {
        updatePoolsAtOrdinal(oldState, "updated-pools-4.json").flatMap { updatedState =>
          currentSnapshotOrdinalR.set(currentSnapshotOrdinal).as(Some(updatedState))
        }
      } else if (currentSnapshotOrdinal === updatePools5Ordinal) {
        updatePoolsAtOrdinal(oldState, "updated-pools-5.json").flatMap { updatedState =>
          currentSnapshotOrdinalR.set(currentSnapshotOrdinal).as(Some(updatedState))
        }
      } else if (currentSnapshotOrdinal === updatePools6Ordinal) {
        updatePoolsAtOrdinal(oldState, "updated-pools-6.json").flatMap { updatedState =>
          currentSnapshotOrdinalR.set(currentSnapshotOrdinal).as(Some(updatedState))
        }
      } else if (currentSnapshotOrdinal === updatePools7Ordinal) {
        updatePoolsAtOrdinal(oldState, "updated-pools-7.json").flatMap { updatedState =>
          currentSnapshotOrdinalR.set(currentSnapshotOrdinal).as(Some(updatedState))
        }
      } else if (currentSnapshotOrdinal === updatePools8Ordinal) {
        updatePoolsAtOrdinal(oldState, "updated-pools-8.json").flatMap { updatedState =>
          currentSnapshotOrdinalR.set(currentSnapshotOrdinal).as(Some(updatedState))
        }
      } else if (currentSnapshotOrdinal === updatePools9Ordinal) {
        updatePoolsAtOrdinal(oldState, "updated-pools-9.json").flatMap { updatedState =>
          currentSnapshotOrdinalR.set(currentSnapshotOrdinal).as(Some(updatedState))
        }
      } else if (currentSnapshotOrdinal === updatePools10Ordinal) {
        updatePoolsAtOrdinal(oldState, "updated-pools-10.json").flatMap { updatedState =>
          currentSnapshotOrdinalR.set(currentSnapshotOrdinal).as(Some(updatedState))
        }
      } else if (currentSnapshotOrdinal === updatePools11Ordinal) {
        updatePoolsAtOrdinal(oldState, "updated-pools-11.json").flatMap { updatedState =>
          currentSnapshotOrdinalR.set(currentSnapshotOrdinal).as(Some(updatedState))
        }
      } else if (currentSnapshotOrdinal === updatePools12Ordinal) {
        updatePoolsAtOrdinal(oldState, "updated-pools-12.json").flatMap { updatedState =>
          currentSnapshotOrdinalR.set(currentSnapshotOrdinal).as(Some(updatedState))
        }
      } else if (currentSnapshotOrdinal === restorePoolReservesOrdinal) {
        // Reserves and the frozen-address purge land in the same snapshot as the balance
        // deductions. Splitting them across ordinals would leave a window where the attacker's
        // calculated state is still actionable against already-corrected reserves.
        updatePoolReservesAtOrdinal(oldState, "updated-pools-13.json")
          .map(st => st.copy(calculated = OneTimeFixesHandler.purgeFrozenAddresses(st.calculated, frozenAddresses)))
          .flatMap { updatedState =>
            currentSnapshotOrdinalR.set(currentSnapshotOrdinal).as(Some(updatedState))
          }
      } else if (currentSnapshotOrdinal === updateUSDCPool) {
        val usdcPool = CurrencyId(Address("DAG0S16WDgdAvh8VvroR6MWLdjmHYdzAF5S181xh")).some
        val newAmount = PosLong.unsafeFrom(1200116577579L)
        updatePoolAmount(
          oldState,
          usdcPool,
          newAmount
        ).flatMap { updatedState =>
          currentSnapshotOrdinalR.set(currentSnapshotOrdinal).as(Some(updatedState))
        }
      } else {
        none[DataState[AmmOnChainState, AmmCalculatedState]].pure[F]
      }

    private def updatePoolAmount(
      oldState: DataState[AmmOnChainState, AmmCalculatedState],
      poolToken: Option[CurrencyId],
      amount: PosLong
    ): F[DataState[AmmOnChainState, AmmCalculatedState]] =
      poolToken match {
        case None =>
          logger.warn("No pool token provided, returning unchanged state").as(oldState)

        case Some(token) =>
          for {
            _ <- logger.info(s"Starting to update pool token amount for: ${token.value.value.value}")

            currentCalculated = oldState.calculated
            liquidityPoolOps = currentCalculated
              .operations(OperationType.LiquidityPool)
              .asInstanceOf[LiquidityPoolCalculatedState]
            confirmedState = liquidityPoolOps.confirmed

            updatedPools = confirmedState.value.map {
              case (key, liquidityPool) =>
                if (key.contains(token.value.value.value)) {
                  val updatedPool =
                    if (liquidityPool.tokenA.identifier === poolToken) {
                      liquidityPool.copy(tokenA = liquidityPool.tokenA.copy(amount = amount))
                    } else if (liquidityPool.tokenB.identifier === poolToken) {
                      liquidityPool.copy(tokenB = liquidityPool.tokenB.copy(amount = amount))
                    } else {
                      liquidityPool
                    }
                  key -> updatedPool
                } else {
                  key -> liquidityPool
                }
            }

            updatedState = oldState.copy(
              calculated = currentCalculated.copy(
                operations = currentCalculated.operations.updated(
                  OperationType.LiquidityPool,
                  liquidityPoolOps.copy(confirmed = confirmedState.copy(value = updatedPools))
                )
              )
            )

            _ <- logger.debug("Successfully updated pool token amount")
          } yield updatedState
      }

    private def updatePoolsAtOrdinal(
      oldState: DataState[AmmOnChainState, AmmCalculatedState],
      resourcePath: String
    ): F[DataState[AmmOnChainState, AmmCalculatedState]] = for {
      _ <- logger.info("Starting to load the pools to update")
      result <- LiquidityPoolLoader.loadPools(resourcePath) match {
        case Failure(exception) =>
          logger.error(exception)("Error when updating the pools") >>
            oldState.pure[F]
        case Success(pools) =>
          pools.toList.traverse {
            case (_, pool) =>
              buildLiquidityPoolUniqueIdentifier(pool.tokenA.identifier, pool.tokenB.identifier)
                .map(uniquePoolId => (uniquePoolId, pool))
          }.flatMap { poolsWithIds =>
            poolsWithIds.foldM(oldState) {
              case (state, (uniquePoolId, pool)) =>
                val currentCalculated = state.calculated
                val liquidityPoolOps =
                  currentCalculated.operations(OperationType.LiquidityPool).asInstanceOf[LiquidityPoolCalculatedState]
                val confirmedState = liquidityPoolOps.confirmed

                confirmedState.value.get(uniquePoolId.value) match {
                  case Some(liquidityPool) =>
                    val updatedLiquidityPool = liquidityPool.copy(
                      poolShares = pool.poolShares,
                      k = pool.k,
                      tokenA = pool.tokenA,
                      tokenB = pool.tokenB
                    )

                    val updatedConfirmedState = confirmedState
                      .focus(_.value)
                      .modify(_.updated(uniquePoolId.value, updatedLiquidityPool))

                    val updatedLiquidityPoolOps = liquidityPoolOps.copy(confirmed = updatedConfirmedState)

                    val updatedOperations = currentCalculated.operations.updated(
                      OperationType.LiquidityPool,
                      updatedLiquidityPoolOps
                    )

                    val updatedCalculated = currentCalculated.copy(operations = updatedOperations)

                    // Reset onChain and sharedArtifacts as part of the pool update
                    val finalState = state
                      .copy(calculated = updatedCalculated)
                      .focus(_.onChain)
                      .replace(AmmOnChainState.empty)
                      .focus(_.sharedArtifacts)
                      .replace(SortedSet.empty)

                    finalState.pure[F]

                  case None =>
                    Async[F].raiseError(new RuntimeException(s"Pool ${uniquePoolId.value} not found in state"))
                }
            }
          }
      }
      _ <- logger.info("Pools successfully loaded")
    } yield result

    private def updatePoolReservesAtOrdinal(
      oldState: DataState[AmmOnChainState, AmmCalculatedState],
      resourcePath: String
    ): F[DataState[AmmOnChainState, AmmCalculatedState]] = for {
      _ <- logger.info("Starting to load the pool reserves to restore")
      result <- PoolReservesLoader.loadReserves(resourcePath) match {
        case Failure(exception) =>
          logger.error(exception)("Error when restoring the pool reserves") >>
            oldState.pure[F]
        case Success(pools) =>
          pools.toList.traverse {
            case (_, pool) =>
              buildLiquidityPoolUniqueIdentifier(pool.tokenA.identifier, pool.tokenB.identifier)
                .map(uniquePoolId => (uniquePoolId, pool))
          }.flatMap { poolsWithIds =>
            poolsWithIds.foldM(oldState) {
              case (state, (uniquePoolId, pool)) =>
                val currentCalculated = state.calculated
                val liquidityPoolOps =
                  currentCalculated.operations(OperationType.LiquidityPool).asInstanceOf[LiquidityPoolCalculatedState]
                val confirmedState = liquidityPoolOps.confirmed

                confirmedState.value.get(uniquePoolId.value) match {
                  case Some(liquidityPool) =>
                    // poolShares deliberately left alone: no add or withdraw settled during the
                    // incident, so the share ledger is the one part of the pool still correct.
                    val updatedLiquidityPool = liquidityPool.copy(
                      k = pool.k,
                      tokenA = pool.tokenA,
                      tokenB = pool.tokenB
                    )

                    val updatedConfirmedState = confirmedState
                      .focus(_.value)
                      .modify(_.updated(uniquePoolId.value, updatedLiquidityPool))

                    val updatedOperations = currentCalculated.operations.updated(
                      OperationType.LiquidityPool,
                      liquidityPoolOps.copy(confirmed = updatedConfirmedState)
                    )

                    // Pending spends carry prices quoted against the corrupted reserves, so they
                    // go out with the on-chain state instead of settling afterwards.
                    val finalState = state
                      .copy(calculated = currentCalculated.copy(operations = updatedOperations))
                      .focus(_.onChain)
                      .replace(AmmOnChainState.empty)
                      .focus(_.sharedArtifacts)
                      .replace(SortedSet.empty)

                    finalState.pure[F]

                  case None =>
                    Async[F].raiseError(new RuntimeException(s"Pool ${uniquePoolId.value} not found in state"))
                }
            }
          }
      }
      _ <- logger.info("Pool reserves successfully restored")
    } yield result

    private def flipPoolTokens(
      oldState: DataState[AmmOnChainState, AmmCalculatedState]
    ): F[DataState[AmmOnChainState, AmmCalculatedState]] =
      for {
        _ <- logger.info("Starting to flip the pool tokens")
        usdcMetagraphId = "DAG0S16WDgdAvh8VvroR6MWLdjmHYdzAF5S181xh"
        currentCalculated = oldState.calculated
        liquidityPoolOps =
          currentCalculated.operations(OperationType.LiquidityPool).asInstanceOf[LiquidityPoolCalculatedState]
        confirmedState = liquidityPoolOps.confirmed

        flippedState = confirmedState.copy(
          value = confirmedState.value.map {
            case (key, liquidityPool) =>
              if (key.contains(usdcMetagraphId)) {
                key -> liquidityPool
              } else {
                key -> liquidityPool.copy(
                  tokenA = liquidityPool.tokenB,
                  tokenB = liquidityPool.tokenA
                )
              }
          }
        )

        updatedState = oldState.copy(
          calculated = currentCalculated.copy(
            operations = currentCalculated.operations.updated(
              OperationType.LiquidityPool,
              liquidityPoolOps.copy(confirmed = flippedState)
            )
          )
        )

      } yield updatedState
  }

  /** Strips every reference to `frozenAddresses` out of calculated state.
    *
    * Balance deductions and calculated state are independent: zeroing a wallet's PACA does not cancel a pending swap it signed, release the
    * LP shares it holds, or clear voting power and unclaimed rewards attributed to it. Each of those is a separate claim on the pool that
    * would become actionable again the moment the metagraph restarts.
    */
  def purgeFrozenAddresses(state: AmmCalculatedState, frozen: Set[Address]): AmmCalculatedState = {
    def isFrozen(address: Address): Boolean = frozen.contains(address)
    // AmmUpdate carries the declared source, which is what every validator and combiner keys on;
    // no need to recover addresses from proofs here.
    def signedByFrozen(update: Signed[_ <: AmmUpdate]): Boolean = isFrozen(update.value.source)

    // Removing an address's shares without shrinking the denominator would silently reprice every
    // remaining provider's claim on the pool, so totalShares drops by the same amount.
    def purgePool(pool: LiquidityPool): LiquidityPool = {
      val (removed, kept) = pool.poolShares.addressShares.partition { case (address, _) => isFrozen(address) }
      if (removed.isEmpty) pool
      else {
        val removedShares = removed.values.map(_.value.value.value).sum
        val remaining = pool.poolShares.totalShares.value - removedShares
        pool.copy(poolShares =
          pool.poolShares.copy(
            totalShares = PosLong.from(remaining).getOrElse(pool.poolShares.totalShares),
            addressShares = kept
          )
        )
      }
    }

    def purgeOffChain(opType: OperationType, opState: AmmOffChainState): AmmOffChainState =
      (opType, opState) match {
        case (_, s: SwapCalculatedState) =>
          s.copy(
            confirmed = s.confirmed.copy(value = s.confirmed.value.filterNot { case (a, _) => isFrozen(a) }),
            pending = s.pending.filterNot(p => signedByFrozen(p.update)),
            failed = s.failed.filterNot(f => signedByFrozen(f.update))
          )
        case (_, s: StakingCalculatedState) =>
          s.copy(
            confirmed = s.confirmed.copy(value = s.confirmed.value.filterNot { case (a, _) => isFrozen(a) }),
            pending = s.pending.filterNot(p => signedByFrozen(p.update)),
            failed = s.failed.filterNot(f => signedByFrozen(f.update))
          )
        case (_, s: WithdrawalCalculatedState) =>
          s.copy(
            confirmed = s.confirmed.copy(value = s.confirmed.value.filterNot { case (a, _) => isFrozen(a) }),
            pending = s.pending.filterNot(p => signedByFrozen(p.update)),
            failed = s.failed.filterNot(f => signedByFrozen(f.update))
          )
        case (_, s: LiquidityPoolCalculatedState) =>
          s.copy(
            confirmed = s.confirmed.copy(value = s.confirmed.value.view.mapValues(purgePool).to(SortedMap)),
            pending = s.pending.filterNot(p => signedByFrozen(p.update)),
            failed = s.failed.filterNot(f => signedByFrozen(f.update))
          )
        case (_, other) => other
      }

    val purgedOperations = state.operations.map { case (k, v) => k -> purgeOffChain(k, v) }

    val purgedRewards = state.rewards.copy(
      availableRewards = RewardInfo(state.rewards.availableRewards.info.filterNot { case (k, _) => isFrozen(k.address) }),
      rewardsBuffer = state.rewards.rewardsBuffer.copy(
        data = state.rewards.rewardsBuffer.data.filterNot(chunk => isFrozen(chunk.receiver))
      ),
      withdraws = state.rewards.withdraws.copy(
        confirmed = state.rewards.withdraws.confirmed.filterNot { case (a, _) => isFrozen(a) },
        pending = state.rewards.withdraws.pending.view
          .mapValues(info => RewardInfo(info.info.filterNot { case (k, _) => isFrozen(k.address) }))
          .to(SortedMap)
      )
    )

    val purgedAllocations = state.allocations.copy(
      usersAllocations = state.allocations.usersAllocations.filterNot { case (a, _) => isFrozen(a) },
      frozenUsedUserVotes = state.allocations.frozenUsedUserVotes.copy(
        votingPowerForAddresses = state.allocations.frozenUsedUserVotes.votingPowerForAddresses.filterNot { case (a, _) => isFrozen(a) }
      )
    )

    val purged = state.copy(
      operations = purgedOperations,
      votingPowers = state.votingPowers.filterNot { case (a, _) => isFrozen(a) },
      allocations = purgedAllocations,
      rewards = purgedRewards
    )

    purged
  }
}
