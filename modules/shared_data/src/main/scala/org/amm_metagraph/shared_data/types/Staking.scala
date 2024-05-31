package org.amm_metagraph.shared_data.types

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import org.amm_metagraph.shared_data.types.LiquidityPool.TokenInformation
import org.tessellation.schema.SnapshotOrdinal


object Staking {
  @derive(encoder, decoder)
  case class StakingCalculatedStateLastReference(
    primaryAllowSpendReferenceTxnId: String,
    pairAllowSpendReferenceTxnId   : String,
    primaryToken                   : TokenInformation,
    pairToken                      : TokenInformation,
    ordinal                        : SnapshotOrdinal
  )

  @derive(encoder, decoder)
  case class StakingCalculatedStateAddress(
    primaryAllowSpendReferenceTxnId: String,
    pairAllowSpendReferenceTxnId   : String,
    primaryToken                   : TokenInformation,
    pairToken                      : TokenInformation,
    ordinal                        : SnapshotOrdinal,
    lastStakingUpdate              : Option[StakingCalculatedStateLastReference]
  )

}
