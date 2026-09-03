package org.amm_metagraph.shared_data

import cats.syntax.all._

import io.constellationnetwork.schema.SnapshotOrdinal

import eu.timepit.refined.types.numeric.PosLong
import monocle.syntax.all._
import org.amm_metagraph.shared_data.types.LiquidityPool.{LiquidityPool, getLiquidityPoolCalculatedState}
import org.amm_metagraph.shared_data.types.States.{AmmCalculatedState, OperationType}

/** One-shot reserve correction for the USDC.dag/DAG pool.
  *
  * At currency ordinal 747126 (2026-09-03) a 50 USDC.dag -> DAG swap was booked and its SpendAction emitted with the allow-spend deadline
  * already reached. The global layer refused it, the metagraph failed the swap at 747127, and the rollback was a no-op for that direction
  * (see ProtocolActivation.rollbackDirectionFix). The pool has since carried reserves the custody address does not back, and the deltas
  * below are the exact amounts the forward swap moved, equal to the divergence the collateral monitor measures on both ledgers with no
  * remainder:
  *
  * tokenA (DAG) +703636031393 tokenB (USDC.dag) -5000000000
  *
  * Applied as a delta rather than as absolute reserves, and armed until a finalized state proves it landed, for the reasons documented on
  * IncidentSwapRollbackCorrection. It refuses rather than guessing if the pool is not the shape it expects, including which side carries
  * which token.
  */
object IncidentUsdcRollbackCorrection {

  /** The confirmed map is keyed by the pool id as a plain String. */
  val poolId: String = "DAG0S16WDgdAvh8VvroR6MWLdjmHYdzAF5S181xh"

  /** tokenA of this pool is native DAG (no identifier), tokenB is USDC.dag. Asserted at apply time rather than assumed. */
  val dagDelta: Long = 703636031393L
  val usdcDelta: Long = -5000000000L

  case class CorrectionError(message: String)

  private def adjustLeg(label: String, current: Long, delta: Long): Either[CorrectionError, PosLong] = {
    val adjusted = BigInt(current) + BigInt(delta)

    for {
      value <- Either.cond(
        adjusted.isValidLong,
        adjusted.toLong,
        CorrectionError(s"$label leg is outside Long range after correction: $adjusted")
      )
      positive <- PosLong.from(value).leftMap(e => CorrectionError(s"$label leg would become invalid: $e"))
    } yield positive
  }

  /** Left on anything unexpected. The caller fails the snapshot rather than applying a partial correction. */
  def applyTo(
    state: AmmCalculatedState,
    currentSnapshotOrdinal: SnapshotOrdinal
  ): Either[CorrectionError, AmmCalculatedState] =
    if (
      !ProtocolActivation.usdcRollbackCorrectionActive(currentSnapshotOrdinal) ||
      state.lastProcessedCurrencyOrdinal.exists(_ >= ProtocolActivation.usdcRollbackCorrection)
    )
      Right(state)
    else
      for {
        lpState <- Right(getLiquidityPoolCalculatedState(state))
        pool <- lpState.confirmed.value
          .get(poolId)
          .toRight(CorrectionError(s"pool $poolId not present in calculated state"))
        _ <- Either.cond(
          pool.tokenA.identifier.isEmpty,
          (),
          CorrectionError(s"expected tokenA of $poolId to be DAG, found ${pool.tokenA.identifier}")
        )
        _ <- Either.cond(
          pool.tokenB.identifier.map(_.value.value.value).contains(poolId),
          (),
          CorrectionError(s"expected tokenB of $poolId to be USDC.dag, found ${pool.tokenB.identifier}")
        )
        newDag <- adjustLeg("DAG", pool.tokenA.amount.value, dagDelta)
        newUsdc <- adjustLeg("USDC.dag", pool.tokenB.amount.value, usdcDelta)
        corrected = pool
          .focus(_.tokenA.amount)
          .replace(newDag)
          .focus(_.tokenB.amount)
          .replace(newUsdc)
          .focus(_.k)
          .replace(BigInt(newDag.value) * BigInt(newUsdc.value))
        updatedLpState = lpState
          .focus(_.confirmed.value)
          .modify(_.updated(poolId, corrected: LiquidityPool))
      } yield
        state
          .focus(_.operations)
          .modify(_.updated(OperationType.LiquidityPool, updatedLpState))
}
