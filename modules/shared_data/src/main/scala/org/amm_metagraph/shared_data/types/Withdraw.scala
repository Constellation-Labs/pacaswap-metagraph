package org.amm_metagraph.shared_data.types

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import eu.timepit.refined.types.numeric.PosLong
import io.circe.refined._


object Withdraw {
  @derive(encoder, decoder)
  case class WithdrawCalculatedStateAddresses(
    currentAmount: PosLong,
  )

}
