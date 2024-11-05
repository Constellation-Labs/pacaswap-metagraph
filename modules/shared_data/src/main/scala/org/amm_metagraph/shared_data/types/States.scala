package org.amm_metagraph.shared_data.types

import scala.collection.immutable.SortedSet

import io.constellationnetwork.currency.dataApplication.{DataCalculatedState, DataOnChainState}
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact.SpendTransaction

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import enumeratum.values.{StringCirceEnum, StringEnum, StringEnumEntry}
import org.amm_metagraph.shared_data.types.DataUpdates.AmmUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool.LiquidityPool
import org.amm_metagraph.shared_data.types.Staking.StakingCalculatedStateAddress
import org.amm_metagraph.shared_data.types.Swap.SwapCalculatedStateAddress
import org.amm_metagraph.shared_data.types.Withdraw.WithdrawCalculatedStateAddress

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

  @derive(encoder, decoder)
  case class StakingCalculatedState(
    addresses: Map[Address, StakingCalculatedStateAddress]
  ) extends AmmOffChainState

  @derive(encoder, decoder)
  case class WithdrawCalculatedState(
    addresses: Map[Address, WithdrawCalculatedStateAddress]
  ) extends AmmOffChainState

  @derive(encoder, decoder)
  case class SwapCalculatedState(
    addresses: Map[Address, SwapCalculatedStateAddress]
  ) extends AmmOffChainState

  @derive(encoder, decoder)
  sealed abstract class OperationType(val value: String) extends StringEnumEntry

  object OperationType extends StringEnum[OperationType] with StringCirceEnum[OperationType] {
    val values = findValues

    case object Staking extends OperationType("Staking")

    case object Withdraw extends OperationType("Withdraw")

    case object LiquidityPool extends OperationType("LiquidityPool")

    case object Swap extends OperationType("Swap")
  }

  @derive(encoder, decoder)
  case class AmmCalculatedState(
    operations: Map[OperationType, AmmOffChainState],
    spendTransactions: SortedSet[SpendTransaction] = SortedSet.empty[SpendTransaction]
  ) extends DataCalculatedState

}
