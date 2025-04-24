package org.amm_metagraph.shared_data.types

import cats.Order._
import cats.effect.Async
import cats.kernel.Next
import cats.syntax.all._
import cats.{Order, PartialOrder}

import io.constellationnetwork.ext.crypto.RefinedHasher
import io.constellationnetwork.ext.derevo.ordering
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.{CurrencyId, SwapAmount}
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hashed, Hasher}

import derevo.cats.order
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.numeric.NonNegLong
import io.circe.refined._
import io.estatico.newtype.macros.newtype
import org.amm_metagraph.shared_data.refined.Percentage
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, SwapUpdate}
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.States._

object Swap {
  @derive(encoder, decoder)
  case class SwapCalculatedStateAddress(
    swapHash: Hash,
    sourceAddress: Address,
    fromToken: TokenInformation,
    toToken: TokenInformation,
    allowSpendReference: Hash,
    amountIn: SwapAmount,
    grossReceived: SwapAmount,
    netReceived: SwapAmount,
    amountOutMinimum: SwapAmount,
    maxValidGsEpochProgress: EpochProgress,
    poolId: Option[PoolId],
    ordinal: SnapshotOrdinal,
    parent: SwapReference
  )

  @derive(encoder, decoder)
  case class SwapQuote(
    fromTokenId: Option[CurrencyId],
    toTokenId: Option[CurrencyId],
    amount: Amount,
    slippagePercent: Percentage,
    rate: BigDecimal,
    priceImpactPercent: BigDecimal,
    estimatedReceived: BigInt,
    minimumReceived: BigInt
  )

  @derive(decoder, encoder, order, ordering)
  case class SwapReference(ordinal: SwapOrdinal, hash: Hash)

  object SwapReference {
    def of(hashedUpdate: Hashed[SwapUpdate]): SwapReference =
      SwapReference(hashedUpdate.ordinal, hashedUpdate.hash)

    def of[F[_]: Async: Hasher](signedTransaction: Signed[SwapUpdate]): F[SwapReference] =
      signedTransaction.value.hash.map(SwapReference(signedTransaction.ordinal, _))

    val empty: SwapReference = SwapReference(SwapOrdinal(0L), Hash.empty)
  }

  @derive(decoder, encoder, order, ordering)
  @newtype
  case class SwapOrdinal(value: NonNegLong)

  object SwapOrdinal {
    val first: SwapOrdinal = SwapOrdinal(1L)
    implicit val next: Next[SwapOrdinal] = new Next[SwapOrdinal] {
      def next(a: SwapOrdinal): SwapOrdinal = SwapOrdinal(a.value |+| 1L)
      def partialOrder: PartialOrder[SwapOrdinal] = Order[SwapOrdinal]
    }
  }

  def getSwapCalculatedState(
    state: AmmCalculatedState
  ): SwapCalculatedState =
    state.operations
      .get(OperationType.Swap)
      .collect { case t: SwapCalculatedState => t }
      .getOrElse(SwapCalculatedState.empty)

  def getPendingAllowSpendsSwapUpdates(
    state: AmmCalculatedState
  ): Set[PendingAllowSpend[AmmUpdate]] = {
    val onlyPendingSwap = getSwapCalculatedState(state).pending.collect {
      case pending: PendingAllowSpend[SwapUpdate] => pending
    }

    onlyPendingSwap.toList.map { pendingAllow =>
      PendingAllowSpend[AmmUpdate](
        pendingAllow.update,
        pendingAllow.updateHash,
        pendingAllow.pricingTokenInfo
      )
    }.toSet
  }

  def getPendingSpendActionSwapUpdates(
    state: AmmCalculatedState
  ): Set[PendingSpendAction[AmmUpdate]] = {
    val onlyPendingSwap = getSwapCalculatedState(state).pending.collect {
      case pending: PendingSpendAction[SwapUpdate] => pending
    }
    onlyPendingSwap.toList.map { pendingSpend =>
      PendingSpendAction[AmmUpdate](
        pendingSpend.update,
        pendingSpend.updateHash,
        pendingSpend.generatedSpendAction,
        pendingSpend.pricingTokenInfo
      )
    }.toSet
  }
}
