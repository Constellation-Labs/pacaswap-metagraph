package org.amm_metagraph.shared_data.types

import io.constellationnetwork.currency.dataApplication.{DataCalculatedState, DataOnChainState}
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact.SpendAction
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.{AllowSpend, CurrencyId, SwapAmount}
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import enumeratum.values.{StringCirceEnum, StringEnum, StringEnumEntry}
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.Governance._
import org.amm_metagraph.shared_data.types.LiquidityPool.{LiquidityPool, ShareAmount, TokenInformation}
import org.amm_metagraph.shared_data.types.Staking.StakingCalculatedStateInfo
import org.amm_metagraph.shared_data.types.Swap.SwapCalculatedStateInfo
import org.amm_metagraph.shared_data.types.Withdrawal.WithdrawalCalculatedStateInfo
import org.amm_metagraph.shared_data.validations.Errors.FailedCalculatedStateReason

object States {
  @derive(encoder, decoder)
  case class AmmOnChainState(
    updates: Set[AmmUpdate]
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
    value: Map[Address, StakingCalculatedStateInfo]
  ) extends ConfirmedCalculatedState

  object ConfirmedStakingCalculatedState {
    def empty: ConfirmedStakingCalculatedState = ConfirmedStakingCalculatedState(Map.empty)
  }

  @derive(encoder, decoder)
  case class ConfirmedWithdrawalCalculatedState(
    value: Map[Address, WithdrawalCalculatedStateInfo]
  ) extends ConfirmedCalculatedState

  private object ConfirmedWithdrawalCalculatedState {
    def empty: ConfirmedWithdrawalCalculatedState = ConfirmedWithdrawalCalculatedState(Map.empty)
  }

  @derive(encoder, decoder)
  case class ConfirmedSwapCalculatedState(
    value: Map[Address, SwapCalculatedStateInfo]
  ) extends ConfirmedCalculatedState

  private object ConfirmedSwapCalculatedState {
    def empty: ConfirmedSwapCalculatedState = ConfirmedSwapCalculatedState(Map.empty)
  }

  @derive(encoder, decoder)
  sealed trait AmmOffChainState {
    type UpdateType <: AmmUpdate
    val confirmed: ConfirmedCalculatedState
    val pending: Set[PendingAction[UpdateType]]
    val failed: Set[FailedCalculatedState]

    def getPendingUpdates: Set[Signed[UpdateType]] =
      pending.collect {
        case PendingAllowSpend(update, _, _)     => update
        case PendingSpendAction(update, _, _, _) => update
      }
  }

  @derive(encoder, decoder)
  case class LiquidityPoolCalculatedState(
    confirmed: ConfirmedLiquidityPoolCalculatedState,
    pending: Set[PendingAction[LiquidityPoolUpdate]],
    failed: Set[FailedCalculatedState]
  ) extends AmmOffChainState {
    type UpdateType = LiquidityPoolUpdate
  }

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
    pending: Set[PendingAction[StakingUpdate]],
    failed: Set[FailedCalculatedState]
  ) extends AmmOffChainState {
    type UpdateType = StakingUpdate
  }

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
    pending: Set[PendingAction[WithdrawalUpdate]],
    failed: Set[FailedCalculatedState]
  ) extends AmmOffChainState {
    type UpdateType = WithdrawalUpdate
  }

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
    pending: Set[PendingAction[SwapUpdate]],
    failed: Set[FailedCalculatedState]
  ) extends AmmOffChainState {
    type UpdateType = SwapUpdate
  }

  object SwapCalculatedState {
    def empty: SwapCalculatedState = SwapCalculatedState(
      ConfirmedSwapCalculatedState.empty,
      Set.empty,
      Set.empty
    )
  }

  @derive(encoder, decoder)
  sealed trait PricingTokenInfo

  @derive(encoder, decoder)
  case class WithdrawalTokenAmounts(
    tokenAIdentifier: Option[CurrencyId],
    tokenAAmount: SwapAmount,
    tokenBIdentifier: Option[CurrencyId],
    tokenBAmount: SwapAmount
  ) extends PricingTokenInfo

  @derive(encoder, decoder)
  case class SwapTokenInfo(
    primaryTokenInformationUpdated: TokenInformation,
    pairTokenInformationUpdated: TokenInformation,
    amount: SwapAmount,
    grossReceived: SwapAmount,
    netReceived: SwapAmount
  ) extends PricingTokenInfo

  @derive(encoder, decoder)
  sealed trait PendingAction[A <: AmmUpdate] {
    val update: Signed[A]
    val updateHash: Hash
    val pricingTokenInfo: Option[PricingTokenInfo] = None
  }

  @derive(encoder, decoder)
  case class PendingAllowSpend[A <: AmmUpdate](
    update: Signed[A],
    updateHash: Hash,
    override val pricingTokenInfo: Option[PricingTokenInfo] = None
  ) extends PendingAction[A]

  @derive(encoder, decoder)
  case class PendingSpendAction[A <: AmmUpdate](
    update: Signed[A],
    updateHash: Hash,
    generatedSpendAction: SpendAction,
    override val pricingTokenInfo: Option[PricingTokenInfo] = None
  ) extends PendingAction[A]

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
    votingWeights: Map[Address, VotingWeight] = Map.empty,
    allocations: Allocations = Allocations.empty,
    lastSyncGlobalSnapshotOrdinal: SnapshotOrdinal = SnapshotOrdinal.MinValue
  ) extends DataCalculatedState
}
