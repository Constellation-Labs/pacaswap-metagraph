package org.amm_metagraph.shared_data.types

import scala.collection.immutable.SortedSet

import io.constellationnetwork.currency.dataApplication.{DataCalculatedState, DataOnChainState}
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact.SpendTransaction
import io.constellationnetwork.security.signature.Signed

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import enumeratum.values.{StringCirceEnum, StringEnum, StringEnumEntry}
import org.amm_metagraph.shared_data.types.DataUpdates.AmmUpdate
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
  sealed trait AmmOffChainState

  @derive(encoder, decoder)
  case class LiquidityPoolCalculatedState(
    liquidityPools: Map[String, LiquidityPool]
  ) extends AmmOffChainState

  object LiquidityPoolCalculatedState {
    def empty: LiquidityPoolCalculatedState = LiquidityPoolCalculatedState(Map.empty)
  }

  @derive(encoder, decoder)
  case class StakingCalculatedState(
    confirmed: Map[Address, Set[StakingCalculatedStateAddress]]
  ) extends AmmOffChainState

  object StakingCalculatedState {
    def empty: StakingCalculatedState = StakingCalculatedState(Map.empty)
  }

  @derive(encoder, decoder)
  case class WithdrawalCalculatedState(
    confirmed: Map[Address, Set[WithdrawalCalculatedStateAddress]]
  ) extends AmmOffChainState

  object WithdrawalCalculatedState {
    def empty: WithdrawalCalculatedState = WithdrawalCalculatedState(Map.empty)
  }

  @derive(encoder, decoder)
  case class SwapCalculatedState(
    confirmed: Map[Address, Set[SwapCalculatedStateAddress]]
  ) extends AmmOffChainState

  object SwapCalculatedState {
    def empty: SwapCalculatedState = SwapCalculatedState(Map.empty)
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
    confirmedOperations: Map[OperationType, AmmOffChainState],
    pendingUpdates: Set[Signed[AmmUpdate]] = Set.empty[Signed[AmmUpdate]],
    spendTransactions: SortedSet[SpendTransaction] = SortedSet.empty[SpendTransaction],
    votingWeights: Map[Address, VotingWeight] = Map.empty,
    allocations: Map[Address, UserAllocations] = Map.empty
  ) extends DataCalculatedState

}
