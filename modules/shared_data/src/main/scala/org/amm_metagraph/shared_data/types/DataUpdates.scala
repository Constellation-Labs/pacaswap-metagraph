package org.amm_metagraph.shared_data.types

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import eu.timepit.refined.types.numeric.PosLong
import org.tessellation.currency.dataApplication.DataUpdate
import org.tessellation.schema.address.Address
import io.circe.refined._

object DataUpdates {
  @derive(encoder, decoder)
  case class AmmUpdateProof(
    id       : String,
    signature: String
  )

  @derive(encoder, decoder)
  sealed trait AmmUpdate extends DataUpdate

  @derive(decoder, encoder)
  case class StakingUpdate(
    txnId        : String,
    originAddress: Address,
    amount       : PosLong
  ) extends AmmUpdate

  @derive(decoder, encoder)
  case class WithdrawUpdate(
    amount: PosLong
  ) extends AmmUpdate
}
