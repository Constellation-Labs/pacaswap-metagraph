package org.amm_metagraph.shared_data.types

import cats.Order._
import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.ext.crypto.RefinedHasher
import io.constellationnetwork.ext.derevo.ordering
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
import org.amm_metagraph.shared_data.types.DataUpdates.RewardWithdrawUpdate

object RewardWithdraw {
  @derive(decoder, encoder, order, ordering)
  case class RewardWithdrawReference(ordinal: RewardWithdrawOrdinal, hash: Hash)

  object RewardWithdrawReference {
    def of(hashedUpdate: Hashed[RewardWithdrawUpdate]): RewardWithdrawReference =
      RewardWithdrawReference(hashedUpdate.ordinal, hashedUpdate.hash)

    def of[F[_]: Async: Hasher](signedTransaction: Signed[RewardWithdrawUpdate]): F[RewardWithdrawReference] =
      signedTransaction.value.hash.map(RewardWithdrawReference(signedTransaction.ordinal, _))

    val empty: RewardWithdrawReference = RewardWithdrawReference(RewardWithdrawOrdinal(0L), Hash.empty)
  }

  @derive(decoder, encoder, order, ordering)
  @newtype
  case class RewardWithdrawOrdinal(value: NonNegLong) {
    def next: RewardWithdrawOrdinal = RewardWithdrawOrdinal(value |+| 1L)
  }

  object RewardWithdrawOrdinal {
    val first: RewardWithdrawOrdinal = RewardWithdrawOrdinal(1L)
  }

}
