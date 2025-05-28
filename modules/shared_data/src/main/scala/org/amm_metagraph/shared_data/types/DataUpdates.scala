package org.amm_metagraph.shared_data.types

import io.constellationnetwork.currency.dataApplication.DataUpdate
import io.constellationnetwork.ext.cats.syntax.next._
import io.constellationnetwork.ext.derevo.ordering
import io.constellationnetwork.schema._
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.{CurrencyId, SwapAmount}
import io.constellationnetwork.security.hash.Hash

import derevo.cats.{eqv, order}
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import eu.timepit.refined.types.numeric.PosLong
import org.amm_metagraph.shared_data.FeeDistributor.FeePercentages
import org.amm_metagraph.shared_data.refined.Percentage._
import org.amm_metagraph.shared_data.types.Governance.{RewardAllocationVoteOrdinal, RewardAllocationVoteReference, VotingWeightInfo}
import org.amm_metagraph.shared_data.types.LiquidityPool.ShareAmount
import org.amm_metagraph.shared_data.types.Staking.{StakingOrdinal, StakingReference}
import org.amm_metagraph.shared_data.types.Swap.{SwapOrdinal, SwapReference}
import org.amm_metagraph.shared_data.types.Withdrawal.{WithdrawalOrdinal, WithdrawalReference}

object DataUpdates {
  @derive(encoder, decoder, order, ordering)
  sealed trait AmmUpdate extends DataUpdate {
    val metagraphId: CurrencyId
    val source: Address
  }

  @derive(eqv, decoder, encoder, order, ordering)
  case class LiquidityPoolUpdate(
    metagraphId: CurrencyId,
    source: Address,
    tokenAAllowSpend: Hash,
    tokenBAllowSpend: Hash,
    tokenAId: Option[CurrencyId],
    tokenBId: Option[CurrencyId],
    tokenAAmount: PosLong,
    tokenBAmount: PosLong,
    maxValidGsEpochProgress: EpochProgress,
    poolFees: Option[FeePercentages]
  ) extends AmmUpdate

  @derive(eqv, decoder, encoder, order, ordering)
  case class StakingUpdate(
    metagraphId: CurrencyId,
    source: Address,
    tokenAAllowSpend: Hash,
    tokenBAllowSpend: Hash,
    tokenAId: Option[CurrencyId],
    tokenAAmount: PosLong,
    tokenBId: Option[CurrencyId],
    parent: StakingReference,
    maxValidGsEpochProgress: EpochProgress
  ) extends AmmUpdate {
    val ordinal: StakingOrdinal = parent.ordinal.next
  }

  @derive(eqv, decoder, encoder, order, ordering)
  case class WithdrawalUpdate(
    metagraphId: CurrencyId,
    source: Address,
    tokenAId: Option[CurrencyId],
    tokenBId: Option[CurrencyId],
    shareToWithdraw: ShareAmount,
    minAmountAOut: Option[SwapAmount],
    minAmountBOut: Option[SwapAmount],
    maxAmountAOut: Option[SwapAmount],
    maxAmountBOut: Option[SwapAmount],
    parent: WithdrawalReference,
    maxValidGsEpochProgress: EpochProgress
  ) extends AmmUpdate {
    val ordinal: WithdrawalOrdinal = parent.ordinal.next
  }

  @derive(eqv, decoder, encoder, order, ordering)
  case class SwapUpdate(
    metagraphId: CurrencyId,
    source: Address,
    swapFromPair: Option[CurrencyId],
    swapToPair: Option[CurrencyId],
    allowSpendReference: Hash,
    amountIn: SwapAmount,
    amountOutMinimum: SwapAmount,
    amountOutMaximum: Option[SwapAmount],
    maxValidGsEpochProgress: EpochProgress,
    parent: SwapReference
  ) extends AmmUpdate {
    val ordinal: SwapOrdinal = parent.ordinal.next
  }

  @derive(decoder, encoder, order, ordering)
  case class RewardAllocationVoteUpdate(
    metagraphId: CurrencyId,
    source: Address,
    parent: RewardAllocationVoteReference,
    allocations: Seq[(String, PosLong)]
  ) extends AmmUpdate {
    val ordinal: RewardAllocationVoteOrdinal = parent.ordinal.next
  }
}
