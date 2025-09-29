package org.amm_metagraph.shared_data.services.pricing.models

import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.CurrencyId
import io.constellationnetwork.security.hash.Hash

case class PoolBalanceChange(
  operation: String,
  timestamp: String,
  epochProgress: Option[EpochProgress],
  updateHash: Option[Hash],
  beforeTokenA: (Option[CurrencyId], Long),
  beforeTokenB: (Option[CurrencyId], Long),
  afterTokenA: (Option[CurrencyId], Long),
  afterTokenB: (Option[CurrencyId], Long),
  tokenAChange: Long,
  tokenBChange: Long,
  beforeK: BigInt,
  afterK: BigInt,
  address: Option[Address] = None,
  additionalInfo: Map[String, String] = Map.empty
)
