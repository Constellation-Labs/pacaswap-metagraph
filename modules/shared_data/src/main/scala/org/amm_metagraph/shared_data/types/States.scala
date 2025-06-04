package org.amm_metagraph.shared_data.types

import cats.Order

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.currency.dataApplication.{DataCalculatedState, DataOnChainState}
import io.constellationnetwork.ext.derevo.ordering
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact.SpendAction
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.{CurrencyId, SwapAmount}
import io.constellationnetwork.schema.{SnapshotOrdinal, nonNegLongKeyDecoder, nonNegLongKeyEncoder}
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed

import derevo.cats.{eqv, order, show}
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import enumeratum.values.{StringCirceEnum, StringEnum, StringEnumEntry}
import io.circe.{KeyDecoder, KeyEncoder}
import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.Governance._
import org.amm_metagraph.shared_data.types.LiquidityPool.{LiquidityPool, ShareAmount, TokenInformation}
import org.amm_metagraph.shared_data.types.RewardWithdraw.RewardWithdrawReference
import org.amm_metagraph.shared_data.types.Rewards.{RewardInfo, RewardType}
import org.amm_metagraph.shared_data.types.Staking.{StakingCalculatedStateAddress, StakingCalculatedStateInfo}
import org.amm_metagraph.shared_data.types.Swap.{SwapCalculatedStateAddress, SwapCalculatedStateInfo}
import org.amm_metagraph.shared_data.types.Withdrawal.{WithdrawalCalculatedStateAddress, WithdrawalCalculatedStateInfo}
import org.amm_metagraph.shared_data.validations.Errors.FailedCalculatedStateReason

object States {
  @derive(encoder, decoder)
  case class AmmOnChainState(
    updatedStateDataUpdate: SortedSet[UpdatedStateDataUpdate],
    rewardsUpdate: Option[RewardInfo]
  ) extends DataOnChainState

  @derive(encoder, decoder, order, ordering)
  case class UpdatedStateDataUpdate(
    previousState: StateTransitionType,
    newState: StateTransitionType,
    operationType: OperationType,
    update: Signed[AmmUpdate],
    updateHash: Hash,
    pendingInfo: Option[PendingAction[AmmUpdate]]
  )

  @derive(encoder, decoder)
  sealed abstract class StateTransitionType(val value: String) extends StringEnumEntry
  object StateTransitionType extends StringEnum[StateTransitionType] with StringCirceEnum[StateTransitionType] {
    val values: IndexedSeq[StateTransitionType] = findValues
    case object NewUpdate extends StateTransitionType("NewUpdate")
    case object PendingAllowSpends extends StateTransitionType("PendingAllowSpends")
    case object PendingSpendTransactions extends StateTransitionType("PendingSpendTransactions")
    case object Confirmed extends StateTransitionType("Confirmed")
    case object Failed extends StateTransitionType("Failed")

    implicit val order: Order[StateTransitionType] = Order.by(_.value)
    implicit val ordering: Ordering[StateTransitionType] = order.toOrdering
  }

  @derive(encoder, decoder)
  sealed trait ConfirmedCalculatedState

  @derive(encoder, decoder)
  case class FailedCalculatedState(
    reason: FailedCalculatedStateReason,
    expiringEpochProgress: EpochProgress,
    update: Signed[AmmUpdate]
  )

  object FailedCalculatedState {
    implicit val order: Order[FailedCalculatedState] = Order.by(_.expiringEpochProgress)
    implicit val ordering: Ordering[FailedCalculatedState] = order.toOrdering
  }

  @derive(encoder, decoder)
  case class ConfirmedLiquidityPoolCalculatedState(
    value: SortedMap[String, LiquidityPool]
  ) extends ConfirmedCalculatedState

  object ConfirmedLiquidityPoolCalculatedState {
    def empty: ConfirmedLiquidityPoolCalculatedState = ConfirmedLiquidityPoolCalculatedState(SortedMap.empty)
  }

  @derive(encoder, decoder)
  case class ConfirmedStakingCalculatedState(
    value: SortedMap[Address, StakingCalculatedStateInfo]
  ) extends ConfirmedCalculatedState

  object ConfirmedStakingCalculatedState {
    def empty: ConfirmedStakingCalculatedState = ConfirmedStakingCalculatedState(SortedMap.empty)
  }

  @derive(encoder, decoder)
  case class ConfirmedWithdrawalCalculatedState(
    value: SortedMap[Address, WithdrawalCalculatedStateInfo]
  ) extends ConfirmedCalculatedState

  private object ConfirmedWithdrawalCalculatedState {
    def empty: ConfirmedWithdrawalCalculatedState = ConfirmedWithdrawalCalculatedState(SortedMap.empty)
  }

  @derive(encoder, decoder)
  case class ConfirmedSwapCalculatedState(
    value: SortedMap[Address, SwapCalculatedStateInfo]
  ) extends ConfirmedCalculatedState

  private object ConfirmedSwapCalculatedState {
    def empty: ConfirmedSwapCalculatedState = ConfirmedSwapCalculatedState(SortedMap.empty)
  }

  @derive(encoder, decoder)
  sealed trait AmmOffChainState {
    type UpdateType <: AmmUpdate
    val confirmed: ConfirmedCalculatedState
    val pending: SortedSet[PendingAction[UpdateType]]
    val failed: SortedSet[FailedCalculatedState]

    def getPendingUpdates: SortedSet[Signed[UpdateType]] =
      pending.collect {
        case PendingAllowSpend(update, _, _)     => update
        case PendingSpendAction(update, _, _, _) => update
      }
  }

  object AmmOffChainState {
    implicit def signedOrdering[A <: AmmUpdate]: Ordering[Signed[A]] =
      Ordering.by(_.value.source)
  }

  @derive(encoder, decoder)
  case class LiquidityPoolCalculatedState(
    confirmed: ConfirmedLiquidityPoolCalculatedState,
    pending: SortedSet[PendingAction[LiquidityPoolUpdate]],
    failed: SortedSet[FailedCalculatedState]
  ) extends AmmOffChainState {
    type UpdateType = LiquidityPoolUpdate
  }

  object LiquidityPoolCalculatedState {
    def empty: LiquidityPoolCalculatedState = LiquidityPoolCalculatedState(
      ConfirmedLiquidityPoolCalculatedState.empty,
      SortedSet.empty[PendingAction[LiquidityPoolUpdate]],
      SortedSet.empty[FailedCalculatedState]
    )
  }

  @derive(encoder, decoder)
  case class StakingCalculatedState(
    confirmed: ConfirmedStakingCalculatedState,
    pending: SortedSet[PendingAction[StakingUpdate]],
    failed: SortedSet[FailedCalculatedState]
  ) extends AmmOffChainState {
    type UpdateType = StakingUpdate
  }

  object StakingCalculatedState {
    def empty: StakingCalculatedState = StakingCalculatedState(
      ConfirmedStakingCalculatedState.empty,
      SortedSet.empty,
      SortedSet.empty
    )
  }

  @derive(encoder, decoder)
  case class WithdrawalCalculatedState(
    confirmed: ConfirmedWithdrawalCalculatedState,
    pending: SortedSet[PendingAction[WithdrawalUpdate]],
    failed: SortedSet[FailedCalculatedState]
  ) extends AmmOffChainState {
    type UpdateType = WithdrawalUpdate
  }

  object WithdrawalCalculatedState {
    def empty: WithdrawalCalculatedState = WithdrawalCalculatedState(
      ConfirmedWithdrawalCalculatedState.empty,
      SortedSet.empty,
      SortedSet.empty
    )
  }

  @derive(encoder, decoder)
  case class SwapCalculatedState(
    confirmed: ConfirmedSwapCalculatedState,
    pending: SortedSet[PendingAction[SwapUpdate]],
    failed: SortedSet[FailedCalculatedState]
  ) extends AmmOffChainState {
    type UpdateType = SwapUpdate
  }

  object SwapCalculatedState {
    def empty: SwapCalculatedState = SwapCalculatedState(
      ConfirmedSwapCalculatedState.empty,
      SortedSet.empty,
      SortedSet.empty
    )
  }

  @derive(encoder, decoder)
  case class RewardWithdrawCalculatedState(
    confirmed: SortedMap[Address, RewardWithdrawReference],
    pending: SortedMap[EpochProgress, RewardInfo]
  )

  object RewardWithdrawCalculatedState {
    def empty: RewardWithdrawCalculatedState = RewardWithdrawCalculatedState(
      SortedMap.empty,
      SortedMap.empty
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

  object PendingAction {
    implicit def orderInstance[A <: AmmUpdate]: Order[PendingAction[A]] =
      Order.by(_.updateHash)

    implicit def orderingInstance[A <: AmmUpdate]: Ordering[PendingAction[A]] =
      orderInstance[A].toOrdering
  }

  @derive(encoder, decoder)
  case class PendingAllowSpend[A <: AmmUpdate](
    update: Signed[A],
    updateHash: Hash,
    override val pricingTokenInfo: Option[PricingTokenInfo] = None
  ) extends PendingAction[A]

  object PendingAllowSpend {
    implicit def orderInstance[A <: AmmUpdate]: Order[PendingAllowSpend[A]] =
      Order.by(_.updateHash)

    implicit def orderingInstance[A <: AmmUpdate]: Ordering[PendingAllowSpend[A]] =
      orderInstance[A].toOrdering
  }

  @derive(encoder, decoder)
  case class PendingSpendAction[A <: AmmUpdate](
    update: Signed[A],
    updateHash: Hash,
    generatedSpendAction: SpendAction,
    override val pricingTokenInfo: Option[PricingTokenInfo] = None
  ) extends PendingAction[A]

  object PendingSpendAction {
    implicit def orderInstance[A <: AmmUpdate]: Order[PendingSpendAction[A]] =
      Order.by(_.updateHash)

    implicit def orderingInstance[A <: AmmUpdate]: Ordering[PendingSpendAction[A]] =
      orderInstance[A].toOrdering
  }
  @derive(encoder, decoder)
  sealed abstract class OperationType(val value: String) extends StringEnumEntry

  object OperationType extends StringEnum[OperationType] with StringCirceEnum[OperationType] {
    val values: IndexedSeq[OperationType] = findValues

    case object Staking extends OperationType("Staking")

    case object LiquidityPool extends OperationType("LiquidityPool")

    case object Swap extends OperationType("Swap")

    case object Withdrawal extends OperationType("Withdrawal")

    implicit val order: Order[OperationType] = Order.by(_.value)
    implicit val ordering: Ordering[OperationType] = order.toOrdering
  }

  @derive(encoder, decoder)
  case class RewardsState(
    withdraws: RewardWithdrawCalculatedState = RewardWithdrawCalculatedState.empty,
    availableRewards: RewardInfo = RewardInfo.empty,
    lastProcessedEpoch: EpochProgress = EpochProgress.MinValue
  )

  @derive(encoder, decoder)
  case class AmmCalculatedState(
    operations: SortedMap[OperationType, AmmOffChainState] = SortedMap.empty[OperationType, AmmOffChainState],
    votingWeights: SortedMap[Address, VotingWeight] = SortedMap.empty[Address, VotingWeight],
    allocations: Allocations = Allocations.empty,
    lastSyncGlobalSnapshotOrdinal: SnapshotOrdinal = SnapshotOrdinal.MinValue,
    rewards: RewardsState = RewardsState()
  ) extends DataCalculatedState

  implicit val keyEncode: KeyEncoder[EpochProgress] = nonNegLongKeyEncoder.contramap(_.value)
  implicit val keyDecode: KeyDecoder[EpochProgress] = nonNegLongKeyDecoder.map(EpochProgress(_))
}
