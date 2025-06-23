package org.amm_metagraph.l0

import scala.collection.immutable.SortedMap
import scala.concurrent.duration.DurationInt

import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.CurrencyId
import io.constellationnetwork.security.hash.Hash

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.{NonNegLong, PosDouble, PosLong}
import eu.timepit.refined.types.numeric.PosInt
import org.amm_metagraph.shared_data.FeeDistributor
import org.amm_metagraph.shared_data.FeeDistributor.FeePercentages
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.app.ApplicationConfig._
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States.{ConfirmedLiquidityPoolCalculatedState, LiquidityPoolCalculatedState}

object Shared {
  val ammMetagraphId: Address = Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9z5")
  val ammMetagraphIdAsCurrencyId: CurrencyId = CurrencyId(ammMetagraphId)
  val sourceAddress: Address = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")

  val config = ApplicationConfig(
    ExpirationEpochProgresses(
      EpochProgress(NonNegLong.unsafeFrom(30L)),
      EpochProgress(NonNegLong.unsafeFrom(30L))
    ),
    "NodeValidators",
    Dev,
    Governance(
      VotingWeightMultipliers(
        PosDouble.MinValue,
        PosDouble.MinValue,
        PosDouble.MinValue
      )
    ),
    Rewards(
      Amount.empty,
      Amount.empty,
      NonNegLong.MinValue,
      NonNegLong.MinValue,
      NonNegLong.MinValue,
      EpochProgress.MinValue,
      Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb"),
      NonNegLong(100L),
      EpochProgress(NonNegLong(10L))
    ),
    TokenLimits(
      NonNegLong.unsafeFrom((100 * 1e8).toLong),
      NonNegLong.unsafeFrom((9223372036854775000L * 1e8).toLong)
    ),
    EpochProgress(NonNegLong.unsafeFrom(0L)),
    EpochMetadata(43.seconds, 30L),
    TokenLockLimitsConfig(
      PosInt.unsafeFrom(10),
      PosLong.unsafeFrom(toFixedPoint(1000))
    )
  )

  def buildLiquidityPoolCalculatedState(
    tokenA: TokenInformation,
    tokenB: TokenInformation,
    owner: Address,
    additionalProvider: Option[(Address, ShareAmount)] = None,
    fees: FeePercentages = FeeDistributor.empty
  ): (String, LiquidityPoolCalculatedState) = {
    val primaryAddressAsString = tokenA.identifier.fold("")(address => address.value.value)
    val pairAddressAsString = tokenB.identifier.fold("")(address => address.value.value)
    val poolId = PoolId(s"$primaryAddressAsString-$pairAddressAsString")

    val baseShares = Map(owner -> ShareAmount(Amount(PosLong.unsafeFrom(toFixedPoint(1.0)))))
    val shares = additionalProvider.fold(baseShares)(provider => baseShares + (provider._1 -> provider._2))
    val feeShares = additionalProvider.foldLeft(Map(owner -> 0L.toNonNegLongUnsafe)) {
      case (acc, (addr, _)) => acc + (addr -> 0L.toNonNegLongUnsafe)
    }

    val totalShares = shares.values.map(_.value.value.value).sum.toPosLongUnsafe

    val liquidityPool = LiquidityPool(
      Hash.empty,
      poolId,
      tokenA,
      tokenB,
      owner,
      BigInt(tokenA.amount.value) * BigInt(tokenB.amount.value),
      PoolShares(totalShares, shares, Map.empty, feeShares),
      fees
    )
    (
      poolId.value,
      LiquidityPoolCalculatedState.empty.copy(confirmed =
        ConfirmedLiquidityPoolCalculatedState.empty.copy(value = SortedMap(poolId.value -> liquidityPool))
      )
    )
  }

  def toFixedPoint(decimal: Double): Long = (decimal * 1e8).toLong
}
