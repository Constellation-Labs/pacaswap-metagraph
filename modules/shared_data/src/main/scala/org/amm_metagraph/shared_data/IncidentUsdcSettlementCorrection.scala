package org.amm_metagraph.shared_data

import cats.syntax.all._

import io.constellationnetwork.schema.SnapshotOrdinal

import eu.timepit.refined.types.numeric.PosLong
import monocle.syntax.all._
import org.amm_metagraph.shared_data.types.LiquidityPool.{LiquidityPool, getLiquidityPoolCalculatedState}
import org.amm_metagraph.shared_data.types.States.{AmmCalculatedState, OperationType}

/** One-shot reserve correction for the USDC.dag/DAG pool after the swap reversed at 756063.
  *
  * At currency ordinal 756059 (2026-09-08) swap e7912a191a sold 53.06141748 USDC.dag for 21,774.19860646 DAG and its SpendAction was
  * emitted. At 756063 the metagraph expired it on the swap's own `maxValidGsEpochProgress` and reversed the book, while the global layer,
  * which enforces only the AllowSpend's deadline, settled it at global 6887529 (see ProtocolActivation.settlementDeadlineExpiry). The
  * wallet therefore holds the swap and the book does not. The deltas are the exact legs of that SpendAction and equal the divergence the
  * collateral monitor measured on both ledgers with no remainder:
  *
  * tokenA (DAG) -2177419860646 tokenB (USDC.dag) +5306141748
  *
  * Applied as a delta, armed until a finalized state proves it landed, and refused if the pool is not the shape it expects, for the reasons
  * documented on IncidentSwapRollbackCorrection.
  */
object IncidentUsdcSettlementCorrection {

  /** The confirmed map is keyed by the pool id as a plain String. */
  val poolId: String = "DAG0S16WDgdAvh8VvroR6MWLdjmHYdzAF5S181xh"

  /** tokenA of this pool is native DAG (no identifier), tokenB is USDC.dag. Asserted at apply time rather than assumed. */
  val dagDelta: Long = -2177419860646L
  val usdcDelta: Long = 5306141748L

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
      !ProtocolActivation.usdcSettlementCorrectionActive(currentSnapshotOrdinal) ||
      state.lastProcessedCurrencyOrdinal.exists(_ >= ProtocolActivation.usdcSettlementCorrection)
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
