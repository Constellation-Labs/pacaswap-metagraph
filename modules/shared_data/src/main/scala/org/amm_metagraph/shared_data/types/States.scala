package org.amm_metagraph.shared_data.types

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import io.circe.{KeyDecoder, KeyEncoder}
import org.amm_metagraph.shared_data.types.DataUpdates.AmmUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool.LiquidityPool
import org.amm_metagraph.shared_data.types.Staking.StakingCalculatedStateAddress
import org.amm_metagraph.shared_data.types.States.OperationType.OperationType
import org.amm_metagraph.shared_data.types.Withdraw.WithdrawCalculatedStateAddresses
import org.tessellation.currency.dataApplication.{DataCalculatedState, DataOnChainState}
import org.tessellation.schema.address.Address

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
    addresses: Map[Address, WithdrawCalculatedStateAddresses]
  ) extends AmmOffChainState

  @derive(encoder, decoder)
  object OperationType extends Enumeration {
    type OperationType = Value
    val Staking, Withdraw, LiquidityPool = Value
    implicit val dataSourceTypeKeyEncoder: KeyEncoder[OperationType] = KeyEncoder.encodeKeyString.contramap(_.toString)
    implicit val dataSourceTypeKeyDecoder: KeyDecoder[OperationType] = KeyDecoder.decodeKeyString.map {
      case "Staking" => Staking
      case "Withdraw" => Withdraw
      case "LiquidityPool" => LiquidityPool
    }
  }

  @derive(encoder, decoder)
  case class AmmCalculatedState(
    ammState: Map[OperationType, AmmOffChainState]
  ) extends DataCalculatedState

}
