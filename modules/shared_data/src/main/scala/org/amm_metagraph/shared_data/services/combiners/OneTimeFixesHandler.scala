package org.amm_metagraph.shared_data.services.combiners

import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.SortedSet
import scala.util.{Failure, Success}

import io.constellationnetwork.currency.dataApplication.DataState
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.swap.CurrencyId

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.{NonNegLong, PosLong}
import fs2.concurrent.SignallingRef
import monocle.syntax.all._
import org.amm_metagraph.shared_data.loaders.LiquidityPoolLoader
import org.amm_metagraph.shared_data.types.LiquidityPool._
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
    val updateUSDCPool: SnapshotOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(116115L))

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
}
