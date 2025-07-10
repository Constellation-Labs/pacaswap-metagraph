package org.amm_metagraph.shared_data.types

import cats.Order._
import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.SortedSet

import io.constellationnetwork.ext.crypto.RefinedHasher
import io.constellationnetwork.ext.derevo.ordering
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.{CurrencyId, SwapAmount}
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hashed, Hasher}
import io.constellationnetwork.syntax.sortedCollection.sortedSetSyntax

import derevo.cats.order
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.all.NonNegLong
import io.circe.refined._
import io.estatico.newtype.macros.newtype
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, WithdrawalUpdate}
import org.amm_metagraph.shared_data.types.LiquidityPool.ShareAmount
import org.amm_metagraph.shared_data.types.States._

object Withdrawal {

  @derive(encoder, decoder, order, ordering)
  case class WithdrawalCalculatedStateAddress(
    sourceAddress: Address,
    updateHash: Hash,
    tokenAId: Option[CurrencyId],
    tokenAAmount: SwapAmount,
    tokenBId: Option[CurrencyId],
    tokenBAmount: SwapAmount,
    shareToWithdraw: ShareAmount,
    minAmountAOut: Option[SwapAmount],
    minAmountBOut: Option[SwapAmount],
    maxAmountAOut: Option[SwapAmount],
    maxAmountBOut: Option[SwapAmount],
    parent: WithdrawalReference
  )

  @derive(encoder, decoder, order, ordering)
  case class WithdrawalCalculatedStateValue(
    expiringEpochProgress: EpochProgress,
    value: WithdrawalCalculatedStateAddress
  )

  @derive(encoder, decoder)
  case class WithdrawalCalculatedStateInfo(
    lastReference: WithdrawalReference,
    values: SortedSet[WithdrawalCalculatedStateValue]
  )

  @derive(decoder, encoder, order, ordering)
  case class WithdrawalReference(ordinal: WithdrawalOrdinal, hash: Hash)

  object WithdrawalReference {
    def of(hashedUpdate: Hashed[WithdrawalUpdate]): WithdrawalReference =
      WithdrawalReference(hashedUpdate.ordinal, hashedUpdate.hash)

    def of[F[_]: Async: Hasher](signedTransaction: Signed[WithdrawalUpdate]): F[WithdrawalReference] =
      signedTransaction.value.hash.map(WithdrawalReference(signedTransaction.ordinal, _))

    val empty: WithdrawalReference = WithdrawalReference(WithdrawalOrdinal(0L), Hash.empty)
  }

  @derive(decoder, encoder, order, ordering)
  @newtype
  case class WithdrawalOrdinal(value: NonNegLong) {
    def next: WithdrawalOrdinal = WithdrawalOrdinal(value |+| 1L)
  }

  object WithdrawalOrdinal {
    val first: WithdrawalOrdinal = WithdrawalOrdinal(1L)
  }

  def getWithdrawalCalculatedState(
    calculatedState: AmmCalculatedState
  ): WithdrawalCalculatedState =
    calculatedState.operations
      .get(OperationType.Withdrawal)
      .collect { case t: WithdrawalCalculatedState => t }
      .getOrElse(WithdrawalCalculatedState.empty)

  def getPendingSpendActionWithdrawalUpdates(
    state: AmmCalculatedState
  ): SortedSet[PendingSpendAction[AmmUpdate]] = {
    val onlyPendingWithdrawal = getWithdrawalCalculatedState(state).pending.collect {
      case pending: PendingSpendAction[WithdrawalUpdate] => pending
    }
    onlyPendingWithdrawal.toList.map { pendingSpend =>
      PendingSpendAction[AmmUpdate](
        pendingSpend.update,
        pendingSpend.updateHash,
        pendingSpend.generatedSpendAction,
        pendingSpend.expiringEpochProgress,
        pendingSpend.pricingTokenInfo
      )
    }.toSortedSet
  }
}
