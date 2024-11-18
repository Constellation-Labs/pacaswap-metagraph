package org.amm_metagraph.shared_data.types

import io.constellationnetwork.currency.dataApplication.DataUpdate
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.{CurrencyId, SwapAmount}
import io.constellationnetwork.security.hash.Hash

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import eu.timepit.refined.types.numeric.PosLong
import io.circe.refined._
import org.amm_metagraph.shared_data.types.LiquidityPool._

object DataUpdates {
  @derive(encoder, decoder)
  sealed trait AmmUpdate extends DataUpdate

  @derive(decoder, encoder)
  case class LiquidityPoolUpdate(
    tokenAAllowSpend: Hash,
    tokenBAllowSpend: Hash,
    tokenAId: Option[CurrencyId],
    tokenBId: Option[CurrencyId],
    tokenAAmount: PosLong,
    tokenBAmount: PosLong,
    maxValidGsEpochProgress: EpochProgress
  ) extends AmmUpdate

  @derive(decoder, encoder)
  case class StakingUpdate(
    tokenAAllowSpend: Hash,
    tokenBAllowSpend: Hash,
    tokenAId: Option[CurrencyId],
    tokenAAmount: PosLong,
    tokenBId: Option[CurrencyId],
    maxValidGsEpochProgress: EpochProgress
  ) extends AmmUpdate

  @derive(decoder, encoder)
  case class SwapUpdate(
    sourceAddress: Address,
    swapFromPair: Option[CurrencyId],
    swapToPair: Option[CurrencyId],
    allowSpendReference: Hash,
    minAmount: SwapAmount,
    maxAmount: SwapAmount,
    maxValidGsEpochProgress: EpochProgress,
    poolId: Option[PoolId],
    minPrice: Option[PosLong],
    maxPrice: Option[PosLong]
  ) extends AmmUpdate

  @derive(decoder, encoder)
  case class WithdrawUpdate(
    amount: PosLong
  ) extends AmmUpdate
}
