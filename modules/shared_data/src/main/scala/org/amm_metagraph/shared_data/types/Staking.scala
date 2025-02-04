package org.amm_metagraph.shared_data.types

import cats.Order
import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.ext.crypto.RefinedHasher
import io.constellationnetwork.ext.derevo.ordering
import io.constellationnetwork.schema._
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hashed, Hasher}

import derevo.cats.order
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import eu.timepit.refined.auto.autoRefineV
import eu.timepit.refined.cats._
import eu.timepit.refined.types.all.NonNegLong
import io.circe.{Decoder, Encoder}
import io.estatico.newtype.macros.newtype
import org.amm_metagraph.shared_data.types.DataUpdates.StakingUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool.TokenInformation
import org.amm_metagraph.shared_data.types.States.{AmmCalculatedState, OperationType, StakingCalculatedState}

object Staking {
  @derive(encoder, decoder)
  case class StakingCalculatedStateAddress(
    tokenAAllowSpend: Hash,
    tokenBAllowSpend: Hash,
    tokenA: TokenInformation,
    tokenB: TokenInformation,
    parent: StakingReference,
    ordinal: StakingOrdinal
  )

  @derive(decoder, encoder, order, ordering)
  case class StakingReference(ordinal: StakingOrdinal, hash: Hash)

  object StakingReference {
    def of(hashedUpdate: Hashed[StakingUpdate]): StakingReference =
      StakingReference(hashedUpdate.ordinal, hashedUpdate.hash)

    def of[F[_]: Async: Hasher](signedTransaction: Signed[StakingUpdate]): F[StakingReference] =
      signedTransaction.value.hash.map(StakingReference(signedTransaction.ordinal, _))

    val empty: StakingReference = StakingReference(StakingOrdinal(0L), Hash.empty)
  }

  @newtype
  @derive(order, encoder, decoder)
  case class StakingOrdinal(value: NonNegLong) {
    def next: StakingOrdinal = StakingOrdinal(value |+| 1L)
  }

  object StakingOrdinal {
    val first: StakingOrdinal = StakingOrdinal(1L)

    implicit val decoder: Decoder[StakingOrdinal] = deriving
    implicit val encoder: Encoder[StakingOrdinal] = deriving
    implicit val orderInstance: Order[StakingOrdinal] = Order.by(_.value)
  }

  def getStakingCalculatedState(
    calculatedState: AmmCalculatedState
  ): StakingCalculatedState =
    calculatedState.operations
      .get(OperationType.Staking)
      .collect { case t: StakingCalculatedState => t }
      .getOrElse(StakingCalculatedState.empty)

  def getPendingStakeUpdates(
    state: AmmCalculatedState
  ): Set[Signed[StakingUpdate]] =
    getStakingCalculatedState(state).pending.collect {
      case pendingUpdate @ Signed(stakingUpdate: StakingUpdate, _) =>
        Signed(stakingUpdate, pendingUpdate.proofs)
    }
}
