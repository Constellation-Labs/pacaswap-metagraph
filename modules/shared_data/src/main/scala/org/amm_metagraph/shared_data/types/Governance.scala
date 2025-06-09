package org.amm_metagraph.shared_data.types

import java.time._

import cats.Order._
import cats.effect.kernel.Async
import cats.syntax.all._
import cats.{Order, Show}

import scala.collection.immutable.{SortedMap, SortedSet}
import scala.concurrent.duration.FiniteDuration

import io.constellationnetwork.ext.crypto._
import io.constellationnetwork.ext.derevo.ordering
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.tokenLock.TokenLock
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hashed, Hasher}

import derevo.cats.{order, show}
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import enumeratum.values.{StringCirceEnum, StringEnum, StringEnumEntry}
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.numeric.{NonNegDouble, NonNegInt, NonNegLong}
import io.circe.refined._
import io.circe.{Decoder, Encoder}
import io.estatico.newtype.macros.newtype
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.app.ApplicationConfig.Environment
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.types.DataUpdates.RewardAllocationVoteUpdate
import org.amm_metagraph.shared_data.types.States.OperationType

object Governance {
  val maxCredits = 50.0

  implicit def showSortedMapAsList[K: Show, V: Show]: Show[SortedMap[K, V]] =
    Show.show(_.toList.show)

  implicit def showSortedSetAsList[T: Show]: Show[SortedSet[T]] =
    Show.show(_.toList.show)

  @derive(decoder, encoder, order, ordering, show)
  @newtype
  case class RewardAllocationVoteOrdinal(value: NonNegLong) {
    def next: RewardAllocationVoteOrdinal = RewardAllocationVoteOrdinal(value |+| 1L)
  }

  object RewardAllocationVoteOrdinal {
    val first: RewardAllocationVoteOrdinal = RewardAllocationVoteOrdinal(0L)
  }

  @derive(decoder, encoder, order, ordering, show)
  case class RewardAllocationVoteReference(ordinal: RewardAllocationVoteOrdinal, hash: Hash)

  object RewardAllocationVoteReference {
    def of(hashedTransaction: Hashed[RewardAllocationVoteUpdate]): RewardAllocationVoteReference =
      RewardAllocationVoteReference(hashedTransaction.ordinal, hashedTransaction.hash)

    def of[F[_]: Async: Hasher](signedTransaction: Signed[RewardAllocationVoteUpdate]): F[RewardAllocationVoteReference] =
      signedTransaction.value.hash.map(RewardAllocationVoteReference(signedTransaction.ordinal, _))

    val empty: RewardAllocationVoteReference = RewardAllocationVoteReference(RewardAllocationVoteOrdinal(0L), Hash.empty)
  }

  @derive(encoder, decoder, show)
  case class VotingWeightInfo(
    weight: NonNegLong,
    tokenLock: TokenLock,
    votedAtEpochProgress: EpochProgress
  )

  object VotingWeightInfo {
    implicit val order: Order[VotingWeightInfo] = Order.by(_.votedAtEpochProgress)
    implicit val ordering: Ordering[VotingWeightInfo] = order.toOrdering
  }

  @derive(encoder, decoder, show)
  case class VotingWeight(
    total: NonNegLong,
    info: SortedSet[VotingWeightInfo]
  )

  object VotingWeight {
    def empty: VotingWeight = VotingWeight(NonNegLong.MinValue, SortedSet.empty)

    implicit val order: Order[VotingWeight] = Order.by(_.total)
    implicit val ordering: Ordering[VotingWeight] = order.toOrdering
  }

  @derive(show)
  sealed abstract class AllocationCategory(val value: String) extends StringEnumEntry
  object AllocationCategory extends StringEnum[AllocationCategory] with StringCirceEnum[AllocationCategory] {
    val values: IndexedSeq[AllocationCategory] = findValues

    case object NodeOperator extends AllocationCategory("NodeOperator")
    case object LiquidityPool extends AllocationCategory("LiquidityPool")

    implicit val encoder: Encoder[AllocationCategory] = Encoder.encodeString.contramap(_.value)
    implicit val decoder: Decoder[AllocationCategory] = Decoder.decodeString.emap { str =>
      values.find(_.value === str).toRight(s"Invalid AllocationCategory value: $str")
    }
  }

  @derive(encoder, decoder, order, ordering, show)
  case class Allocation(
    id: String,
    category: AllocationCategory,
    percentage: NonNegDouble
  )

  @derive(encoder, decoder, order, ordering, show)
  case class MonthlyReference(
    firstEpochOfMonth: EpochProgress,
    lastEpochOfMonth: EpochProgress,
    monthReference: NonNegLong
  )

  object MonthlyReference {
    def empty: MonthlyReference =
      MonthlyReference(
        EpochProgress.MinValue,
        EpochProgress.MinValue,
        NonNegLong.MinValue
      )

    def getMonthlyReference(epochProgress: EpochProgress, epochProgress1Month: Long): MonthlyReference = {
      val monthNumber = epochProgress.value / epochProgress1Month
      val firstEpochOfMonth = monthNumber * epochProgress1Month
      val lastEpochOfMonth = firstEpochOfMonth + epochProgress1Month - 1
      MonthlyReference(
        EpochProgress(NonNegLong.unsafeFrom(firstEpochOfMonth)),
        EpochProgress(NonNegLong.unsafeFrom(lastEpochOfMonth)),
        NonNegLong.unsafeFrom(monthNumber)
      )
    }
  }

  @derive(encoder, decoder, order, ordering, show)
  case class FrozenAddressesVotes(monthlyReference: MonthlyReference, votes: SortedMap[Address, VotingWeight])
  object FrozenAddressesVotes {
    val empty: FrozenAddressesVotes = FrozenAddressesVotes(MonthlyReference.empty, SortedMap.empty)
  }

  @derive(encoder, decoder, order, ordering)
  case class Allocations(
    monthlyReference: MonthlyReference,
    usersAllocations: SortedMap[Address, UserAllocations],
    frozenUsedUserVotes: FrozenAddressesVotes
  )

  object Allocations {
    def empty: Allocations = Allocations(MonthlyReference.empty, SortedMap.empty, FrozenAddressesVotes.empty)
  }

  @derive(encoder, decoder, order, ordering, show)
  case class UserAllocations(
    credits: Double,
    reference: RewardAllocationVoteReference,
    allocationEpochProgress: EpochProgress,
    allocations: SortedSet[Allocation]
  )

  object UserAllocations {
    def empty: UserAllocations =
      UserAllocations(maxCredits, RewardAllocationVoteReference.empty, EpochProgress.MinValue, SortedSet.empty)
  }
}
