package org.amm_metagraph.shared_data

import cats.syntax.all._

import io.constellationnetwork.schema.SnapshotOrdinal

import eu.timepit.refined.types.numeric.PosLong
import monocle.syntax.all._
import org.amm_metagraph.shared_data.types.LiquidityPool.{LiquidityPool, getLiquidityPoolCalculatedState}
import org.amm_metagraph.shared_data.types.States.{AmmCalculatedState, OperationType}

/** One-shot reserve correction for the SWAP/DAG pool.
  *
  * A pair of pending swap operations was reverted after their spend actions had already been accepted on the global layer, leaving the pool
  * book holding reserves the custody address does not back. These are the exact amounts the revert moved, taken from the pool balance log,
  * and they equal the difference the collateral check measures against the wallet on both ledgers with no remainder:
  *
  * tokenA (SWAP) +1955265547325 tokenB (DAG) -437171305445
  *
  * Applied as a delta rather than as absolute reserves: the chain keeps trading between the moment the amounts were measured and the
  * activation ordinal, and absolute values would discard everything in between.
  *
  * It stays armed at and after the activation ordinal until a finalized state proves it was applied, because a failed combine is turned
  * into the previous calculated state and the snapshot can still be built. It refuses rather than guessing if the pool is not the shape it
  * expects, including which side carries which token.
  */
object IncidentSwapRollbackCorrection {

  /** The confirmed map is keyed by the pool id as a plain String. */
  val poolId: String = "DAG7X5idd4aLfp4XC6WQdG1eDfR3LGPVEwtUUB2W"

  /** tokenA of this pool is SWAP, tokenB is DAG. Asserted at apply time rather than assumed: getting the sides the wrong way round is the
    * mistake that cost 147,940.10 DAG once already.
    */
  val swapDelta: Long = 1955265547325L
  val dagDelta: Long = -437171305445L

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
    // `combine` runs once per accepted data block and chains the resulting state, so the same
    // currency ordinal can be observed more than once. The state-carried ordinal prevents a second
    // data block in the activation snapshot from applying the delta again.
    //
    // The SDK converts a failed combine into the previous calculated state and may still build the
    // snapshot. Therefore this stays armed at and after the activation ordinal until a successfully
    // finalized state proves the activation ordinal was processed. A replay starts from the state
    // immediately before activation and applies the correction exactly once as well.
    if (
      !ProtocolActivation.swapRollbackCorrectionActive(currentSnapshotOrdinal) ||
      state.lastProcessedCurrencyOrdinal.exists(_ >= ProtocolActivation.swapRollbackCorrection)
    )
      Right(state)
    else
      for {
        lpState <- Right(getLiquidityPoolCalculatedState(state))
        pool <- lpState.confirmed.value
          .get(poolId)
          .toRight(CorrectionError(s"pool $poolId not present in calculated state"))
        _ <- Either.cond(
          pool.tokenA.identifier.map(_.value.value.value).contains(poolId),
          (),
          CorrectionError(s"expected tokenA of $poolId to be SWAP, found ${pool.tokenA.identifier}")
        )
        _ <- Either.cond(
          pool.tokenB.identifier.isEmpty,
          (),
          CorrectionError(s"expected tokenB of $poolId to be DAG, found ${pool.tokenB.identifier}")
        )
        newSwap <- adjustLeg("SWAP", pool.tokenA.amount.value, swapDelta)
        newDag <- adjustLeg("DAG", pool.tokenB.amount.value, dagDelta)
        corrected = pool
          .focus(_.tokenA.amount)
          .replace(newSwap)
          .focus(_.tokenB.amount)
          .replace(newDag)
          .focus(_.k)
          .replace(BigInt(newSwap.value) * BigInt(newDag.value))
        updatedLpState = lpState
          .focus(_.confirmed.value)
          .modify(_.updated(poolId, corrected: LiquidityPool))
      } yield
        state
          .focus(_.operations)
          .modify(_.updated(OperationType.LiquidityPool, updatedLpState))
}
