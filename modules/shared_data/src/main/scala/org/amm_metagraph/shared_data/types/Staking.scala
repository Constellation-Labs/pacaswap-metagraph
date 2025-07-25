package org.amm_metagraph.shared_data.types

import cats.Order._
import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.SortedSet

import io.constellationnetwork.ext.crypto.RefinedHasher
import io.constellationnetwork.ext.derevo.ordering
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hashed, Hasher}
import io.constellationnetwork.syntax.sortedCollection.sortedSetSyntax

import derevo.cats.order
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.numeric.NonNegLong
import io.circe.refined._
import io.estatico.newtype.macros.newtype
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, StakingUpdate}
import org.amm_metagraph.shared_data.types.LiquidityPool.TokenInformation
import org.amm_metagraph.shared_data.types.States._

object Staking {

  @derive(encoder, decoder)
  case class StakingCalculatedStateAddress(
    sourceAddress: Address,
    updateHash: Hash,
    tokenAAllowSpend: Hash,
    tokenBAllowSpend: Hash,
    tokenA: TokenInformation,
    tokenB: TokenInformation,
    parent: StakingReference
  )

  @derive(encoder, decoder, order, ordering)
  case class StakingCalculatedStateValue(
    expiringEpochProgress: EpochProgress,
    value: StakingCalculatedStateAddress
  )

  @derive(encoder, decoder)
  case class StakingCalculatedStateInfo(
    lastReference: StakingReference,
    values: SortedSet[StakingCalculatedStateValue]
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

  @derive(decoder, encoder, order, ordering)
  @newtype
  case class StakingOrdinal(value: NonNegLong) {
    def next: StakingOrdinal = StakingOrdinal(value |+| 1L)
  }

  object StakingOrdinal {
    val first: StakingOrdinal = StakingOrdinal(1L)
  }

  def getConfirmedStakings(state: AmmCalculatedState): Map[Address, StakingCalculatedStateInfo] =
    state.operations.get(OperationType.Staking).fold(Map.empty[Address, StakingCalculatedStateInfo]) {
      case stakingCalculatedState: StakingCalculatedState => stakingCalculatedState.confirmed.value
      case _                                              => Map.empty
    }

  def getStakingCalculatedState(
    calculatedState: AmmCalculatedState
  ): StakingCalculatedState =
    calculatedState.operations
      .get(OperationType.Staking)
      .collect { case t: StakingCalculatedState => t }
      .getOrElse(StakingCalculatedState.empty)

  def getPendingAllowSpendsStakingUpdates(
    state: AmmCalculatedState
  ): SortedSet[PendingAllowSpend[AmmUpdate]] = {
    val onlyPendingStaking = getStakingCalculatedState(state).pending.collect {
      case pending: PendingAllowSpend[StakingUpdate] => pending
    }

    onlyPendingStaking.toList.map { pendingAllow =>
      PendingAllowSpend[AmmUpdate](
        pendingAllow.update,
        pendingAllow.updateHash,
        pendingAllow.pricingTokenInfo
      )
    }.toSortedSet
  }

  def getPendingSpendActionStakingUpdates(
    state: AmmCalculatedState
  ): SortedSet[PendingSpendAction[AmmUpdate]] = {
    val onlyPendingStaking = getStakingCalculatedState(state).pending.collect {
      case pending: PendingSpendAction[StakingUpdate] => pending
    }

    onlyPendingStaking.toList.map { pendingSpend =>
      PendingSpendAction[AmmUpdate](
        pendingSpend.update,
        pendingSpend.updateHash,
        pendingSpend.generatedSpendAction,
        pendingSpend.pricingTokenInfo
      )
    }.toSortedSet
  }
}
