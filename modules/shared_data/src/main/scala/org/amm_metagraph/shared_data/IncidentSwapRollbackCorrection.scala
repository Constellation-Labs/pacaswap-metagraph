package org.amm_metagraph.shared_data

import cats.syntax.all._

import io.constellationnetwork.schema.SnapshotOrdinal

import eu.timepit.refined.types.numeric.PosLong
import monocle.syntax.all._
import org.amm_metagraph.shared_data.types.LiquidityPool.{LiquidityPool, getLiquidityPoolCalculatedState}
import org.amm_metagraph.shared_data.types.States.{AmmCalculatedState, OperationType}

/** Undoes the rollback the combine applied to two already-settled swaps at currency ordinal 741789.
  *
  * WHAT HAPPENED
  *
  * The three metagraph L0 nodes restarted at 20:16:39Z on 2026-08-31. At ordinal 741789 the combine judged three pending spend actions,
  * found no acceptance for them in the global range it scanned, and expired all three. Two of them had in fact settled hours earlier:
  *
  * 700ae9df… SWAP -> DAG, forward applied 17:43:59Z, rolled back 20:21:05Z 96e16834… SWAP -> DAG, forward applied 18:45:22Z, rolled back
  * 20:21:05Z
  *
  * The external collateral monitor read book == wallet at 17:02, 18:04, 19:02 and 20:03, which is the proof that both forward credits were
  * matched on the ledger: had they not settled, the book would already have disagreed from 17:43 onward. The rollbacks then moved the book
  * away from a wallet that had genuinely paid.
  *
  * The third expiry (c5113977…, DAG -> DOR) applied +0/+0 and is correctly excluded here. DOR reconciles to zero.
  *
  * WHY A DELTA AND NOT ABSOLUTE RESERVES
  *
  * updated-pools-14.json could carry absolute reserves because the chain was stopped. It is running now, so absolute values would silently
  * discard every trade between the moment they were measured and the activation ordinal. These are the exact amounts the rollback moved,
  * read from the SWAP_ROLLBACK entries in the node's own pool log, and they are applied to whatever the reserves are at activation:
  *
  * 700ae9df SWAP 5100830170115943 -> 5098896903620698 (-1933266495245) DAG 1143472155147592 -> 1143904409617034 (+432254469442) 96e16834
  * SWAP 5098896903620698 -> 5098874904568618 (-21999052080) DAG 1143904409617034 -> 1143909326453037 (+4916836003)
  *
  * Summed, and independently equal to the divergence the monitor measures against the live wallet:
  *
  * SWAP +1955265547325 (book was short by this) DAG -437171305445 (book was long by this)
  *
  * This restores the book. It does not compensate anyone: the two swappers received what the ledger paid them, and the pool holds what the
  * ledger gave it. Only the book was wrong.
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
