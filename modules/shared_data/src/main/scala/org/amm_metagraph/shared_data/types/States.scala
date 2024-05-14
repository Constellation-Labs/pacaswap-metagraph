package org.amm_metagraph.shared_data.types

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import eu.timepit.refined.types.numeric.PosLong
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, AmmUpdateProof}
import org.tessellation.currency.dataApplication.{DataCalculatedState, DataOnChainState}
import org.tessellation.schema.SnapshotOrdinal
import org.tessellation.schema.address.Address
import io.circe.refined._

object States {
  @derive(encoder, decoder)
  sealed trait AmmOffChainState

  @derive(encoder, decoder)
  case class StakingCalculatedStateLastReference(
    txnId        : String,
    originAddress: Address,
    proof        : AmmUpdateProof,
    amount       : PosLong,
    ordinal      : SnapshotOrdinal
  )

  @derive(encoder, decoder)
  case class StakingCalculatedState(
    txnId            : String,
    originAddress    : Address,
    proof            : AmmUpdateProof,
    amount           : PosLong,
    ordinal          : SnapshotOrdinal,
    lastStakingUpdate: Option[StakingCalculatedStateLastReference]
  ) extends AmmOffChainState

  @derive(encoder, decoder)
  case class WithdrawCalculatedState(
    currentProof : AmmUpdateProof,
    currentAmount: PosLong,
  ) extends AmmOffChainState

  @derive(encoder, decoder)
  case class AmmOnChainState(
    updates: List[AmmUpdate]
  ) extends DataOnChainState

  val Staking: String = "Staking"
  val Withdraw: String = "Withdraw"

  @derive(encoder, decoder)
  case class AmmCalculatedState(
    addresses: Map[Address, Map[String, AmmOffChainState]]
  ) extends DataCalculatedState

}
