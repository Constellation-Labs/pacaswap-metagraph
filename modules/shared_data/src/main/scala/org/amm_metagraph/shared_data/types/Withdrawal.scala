package org.amm_metagraph.shared_data.types

import cats.Order._
import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.ext.crypto.RefinedHasher
import io.constellationnetwork.ext.derevo.ordering
import io.constellationnetwork.schema.swap.{CurrencyId, SwapAmount}
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hashed, Hasher}

import derevo.cats.order
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.all.NonNegLong
import io.circe.refined._
import io.estatico.newtype.macros.newtype
import org.amm_metagraph.shared_data.types.DataUpdates.WithdrawalUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool.ShareAmount
import org.amm_metagraph.shared_data.types.States._

object Withdrawal {
  @derive(encoder, decoder)
  case class WithdrawalCalculatedStateAddress(
    tokenAId: Option[CurrencyId],
    tokenAAmount: SwapAmount,
    tokenBId: Option[CurrencyId],
    tokenBAmount: SwapAmount,
    shareToWithdraw: ShareAmount,
    parent: WithdrawalReference
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
  ): Set[PendingSpendAction[WithdrawalUpdate]] =
    getWithdrawalCalculatedState(state).pending.collect {
      case pendingSpend: PendingSpendAction[WithdrawalUpdate] => pendingSpend
    }
}
