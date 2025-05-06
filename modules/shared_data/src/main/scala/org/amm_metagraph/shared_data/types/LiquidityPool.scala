package org.amm_metagraph.shared_data.types

import cats.MonadThrow
import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.SortedSet

import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.swap.CurrencyId
import io.constellationnetwork.security.hash.Hash

import derevo.cats.eqv
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import eu.timepit.refined.types.numeric.{NonNegLong, PosLong}
import io.circe.refined._
import io.circe.{KeyDecoder, KeyEncoder}
import io.estatico.newtype.macros.newtype
import org.amm_metagraph.shared_data.FeeDistributor.FeePercentages
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, LiquidityPoolUpdate}
import org.amm_metagraph.shared_data.types.States._

object LiquidityPool {
  @derive(encoder, decoder, eqv)
  @newtype
  case class PoolId(value: String)

  @derive(encoder, decoder)
  case class TokenInformation(
    identifier: Option[CurrencyId],
    amount: PosLong
  )

  @derive(eqv, encoder, decoder)
  @newtype
  case class ShareAmount(value: Amount)

  object ShareAmount {
    val empty: ShareAmount = ShareAmount(Amount.empty)
  }

  @derive(encoder, decoder)
  case class PoolShares(
    totalShares: PosLong,
    addressShares: Map[Address, ShareAmount],
    pendingFeeShares: Map[Hash, Map[Address, NonNegLong]],
    feeShares: Map[Address, NonNegLong]
  )

  object PoolShares {
    implicit val keyEncoderHash: KeyEncoder[Hash] = KeyEncoder.encodeKeyString.contramap(_.toString)
    implicit val keyDecoderHash: KeyDecoder[Hash] = KeyDecoder.decodeKeyString.map(Hash(_))

    def toPendingFeeShares(
      feeShares: Map[Address, NonNegLong],
      swapUpdateHash: Hash
    ): Map[Hash, Map[Address, NonNegLong]] =
      Map(swapUpdateHash -> feeShares)
  }

  @derive(encoder, decoder)
  case class LiquidityPool(
    poolId: PoolId,
    tokenA: TokenInformation,
    tokenB: TokenInformation,
    owner: Address,
    k: BigInt,
    poolShares: PoolShares,
    poolFees: FeePercentages
  )

  def getConfirmedLiquidityPools(state: AmmCalculatedState): Map[String, LiquidityPool] =
    state.operations.get(OperationType.LiquidityPool).fold(Map.empty[String, LiquidityPool]) {
      case liquidityPoolsCalculatedState: LiquidityPoolCalculatedState => liquidityPoolsCalculatedState.confirmed.value
      case _                                                           => Map.empty
    }

  def getLiquidityPoolCalculatedState(
    calculatedState: AmmCalculatedState
  ): LiquidityPoolCalculatedState =
    calculatedState.operations
      .get(OperationType.LiquidityPool)
      .collect { case t: LiquidityPoolCalculatedState => t }
      .getOrElse(LiquidityPoolCalculatedState.empty)

  def buildLiquidityPoolUniqueIdentifier[F[_]: MonadThrow](
    maybeTokenAId: Option[CurrencyId],
    maybeTokenBId: Option[CurrencyId]
  ): F[PoolId] =
    SortedSet(maybeTokenAId, maybeTokenBId).flatten
      .mkString("-")
      .pure[F]
      .ensure(new IllegalArgumentException("You should provide at least one currency token identifier"))(_.nonEmpty)
      .map(PoolId(_))

  def getLiquidityPoolByPoolId[F[_]: Async](
    liquidityPoolsCalculatedState: Map[String, LiquidityPool],
    poolId: PoolId
  ): F[LiquidityPool] =
    liquidityPoolsCalculatedState
      .get(poolId.value)
      .fold(
        Async[F].raiseError[LiquidityPool](new IllegalStateException("Liquidity Pool does not exist"))
      )(Async[F].pure)

  def getPendingAllowSpendsLiquidityPoolUpdates(
    state: AmmCalculatedState
  ): Set[PendingAllowSpend[AmmUpdate]] = {
    val onlyPendingLiquidity = getLiquidityPoolCalculatedState(state).pending.collect {
      case pending: PendingAllowSpend[LiquidityPoolUpdate] => pending
    }

    onlyPendingLiquidity.toList.map { pendingAllowSpend =>
      PendingAllowSpend[AmmUpdate](
        pendingAllowSpend.update,
        pendingAllowSpend.updateHash,
        pendingAllowSpend.pricingTokenInfo
      )
    }.toSet
  }

  def getPendingSpendActionLiquidityPoolUpdates(
    state: AmmCalculatedState
  ): Set[PendingSpendAction[AmmUpdate]] = {
    val onlyPendingLiquidity = getLiquidityPoolCalculatedState(state).pending.collect {
      case pending: PendingSpendAction[LiquidityPoolUpdate] => pending
    }

    onlyPendingLiquidity.toList.map { pendingSpend =>
      PendingSpendAction[AmmUpdate](
        pendingSpend.update,
        pendingSpend.updateHash,
        pendingSpend.generatedSpendAction,
        pendingSpend.pricingTokenInfo
      )
    }.toSet
  }
}
