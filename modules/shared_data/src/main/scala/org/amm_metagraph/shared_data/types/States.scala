package org.amm_metagraph.shared_data.types

import scala.collection.immutable.SortedSet

import io.constellationnetwork.currency.dataApplication.{DataCalculatedState, DataOnChainState}
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact.SpendTransaction
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.AllowSpend
import io.constellationnetwork.security.signature.Signed

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import enumeratum.values.{StringCirceEnum, StringEnum, StringEnumEntry}
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.Governance._
import org.amm_metagraph.shared_data.types.LiquidityPool.LiquidityPool
import org.amm_metagraph.shared_data.types.Staking.StakingCalculatedStateAddress
import org.amm_metagraph.shared_data.types.Swap.SwapCalculatedStateAddress
import org.amm_metagraph.shared_data.types.Withdrawal.WithdrawalCalculatedStateAddress

object States {
  @derive(encoder, decoder)
  case class AmmOnChainState(
    updates: List[AmmUpdate]
  ) extends DataOnChainState

  @derive(encoder, decoder)
  sealed trait ConfirmedCalculatedState

  @derive(encoder, decoder)
  case class FailedCalculatedState(
    reason: FailedCalculatedStateReason,
    expiringEpochProgress: EpochProgress,
    update: Signed[AmmUpdate]
  )

  @derive(encoder, decoder)
  case class ConfirmedLiquidityPoolCalculatedState(
    value: Map[String, LiquidityPool]
  ) extends ConfirmedCalculatedState

  object ConfirmedLiquidityPoolCalculatedState {
    def empty: ConfirmedLiquidityPoolCalculatedState = ConfirmedLiquidityPoolCalculatedState(Map.empty)
  }

  @derive(encoder, decoder)
  case class ConfirmedStakingCalculatedState(
    value: Map[Address, Set[StakingCalculatedStateAddress]]
  ) extends ConfirmedCalculatedState

  object ConfirmedStakingCalculatedState {
    def empty: ConfirmedStakingCalculatedState = ConfirmedStakingCalculatedState(Map.empty)
  }

  @derive(encoder, decoder)
  case class ConfirmedWithdrawalCalculatedState(
    value: Map[Address, Set[WithdrawalCalculatedStateAddress]]
  ) extends ConfirmedCalculatedState

  object ConfirmedWithdrawalCalculatedState {
    def empty: ConfirmedWithdrawalCalculatedState = ConfirmedWithdrawalCalculatedState(Map.empty)
  }

  @derive(encoder, decoder)
  case class ConfirmedSwapCalculatedState(
    value: Map[Address, Set[SwapCalculatedStateAddress]]
  ) extends ConfirmedCalculatedState

  object ConfirmedSwapCalculatedState {
    def empty: ConfirmedSwapCalculatedState = ConfirmedSwapCalculatedState(Map.empty)
  }

  @derive(encoder, decoder)
  sealed trait AmmOffChainState {
    val confirmed: ConfirmedCalculatedState
    val pending: Set[_ <: Signed[AmmUpdate]]
    val failed: Set[FailedCalculatedState]
  }

  @derive(encoder, decoder)
  case class LiquidityPoolCalculatedState(
    confirmed: ConfirmedLiquidityPoolCalculatedState,
    pending: Set[Signed[LiquidityPoolUpdate]],
    failed: Set[FailedCalculatedState]
  ) extends AmmOffChainState

  object LiquidityPoolCalculatedState {
    def empty: LiquidityPoolCalculatedState = LiquidityPoolCalculatedState(
      ConfirmedLiquidityPoolCalculatedState.empty,
      Set.empty,
      Set.empty
    )
  }

  @derive(encoder, decoder)
  case class StakingCalculatedState(
    confirmed: ConfirmedStakingCalculatedState,
    pending: Set[Signed[StakingUpdate]],
    failed: Set[FailedCalculatedState]
  ) extends AmmOffChainState

  object StakingCalculatedState {
    def empty: StakingCalculatedState = StakingCalculatedState(
      ConfirmedStakingCalculatedState.empty,
      Set.empty,
      Set.empty
    )
  }

  @derive(encoder, decoder)
  case class WithdrawalCalculatedState(
    confirmed: ConfirmedWithdrawalCalculatedState,
    pending: Set[Signed[WithdrawalUpdate]],
    failed: Set[FailedCalculatedState]
  ) extends AmmOffChainState

  object WithdrawalCalculatedState {
    def empty: WithdrawalCalculatedState = WithdrawalCalculatedState(
      ConfirmedWithdrawalCalculatedState.empty,
      Set.empty,
      Set.empty
    )
  }

  @derive(encoder, decoder)
  case class SwapCalculatedState(
    confirmed: ConfirmedSwapCalculatedState,
    pending: Set[Signed[SwapUpdate]],
    failed: Set[FailedCalculatedState]
  ) extends AmmOffChainState

  object SwapCalculatedState {
    def empty: SwapCalculatedState = SwapCalculatedState(
      ConfirmedSwapCalculatedState.empty,
      Set.empty,
      Set.empty
    )
  }

  @derive(encoder, decoder)
  sealed abstract class OperationType(val value: String) extends StringEnumEntry

  object OperationType extends StringEnum[OperationType] with StringCirceEnum[OperationType] {
    val values: IndexedSeq[OperationType] = findValues

    case object Staking extends OperationType("Staking")

    case object LiquidityPool extends OperationType("LiquidityPool")

    case object Swap extends OperationType("Swap")

    case object Withdrawal extends OperationType("Withdrawal")
  }

  @derive(encoder, decoder)
  case class AmmCalculatedState(
    operations: Map[OperationType, AmmOffChainState] = Map.empty,
    spendTransactions: SortedSet[SpendTransaction] = SortedSet.empty[SpendTransaction],
    votingWeights: Map[Address, VotingWeight] = Map.empty,
    allocations: Allocations = Allocations.empty
  ) extends DataCalculatedState

  @derive(encoder, decoder)
  sealed trait FailedCalculatedStateReason

  case class AllowSpendExpired(allowSpend: AllowSpend) extends FailedCalculatedStateReason
  case class AmountGreaterThanAllowSpendLimit(allowSpend: AllowSpend) extends FailedCalculatedStateReason
  case class SwapLessThanMinAmount() extends FailedCalculatedStateReason
  case class SwapPriceBelowAcceptableMinPrice() extends FailedCalculatedStateReason
  case class SwapPriceExceedsAcceptableMaxPrice() extends FailedCalculatedStateReason

}
