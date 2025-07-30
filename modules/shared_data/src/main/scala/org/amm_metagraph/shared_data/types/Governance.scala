package org.amm_metagraph.shared_data.types

import cats.Order._
import cats.effect.kernel.Async
import cats.syntax.all._
import cats.{Order, Show}

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.ext.crypto._
import io.constellationnetwork.ext.derevo.ordering
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.tokenLock.TokenLock
import io.constellationnetwork.schema.{tupleKeyDecoder, tupleKeyEncoder}
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hashed, Hasher}

import derevo.cats.{order, show}
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import enumeratum.values.{StringCirceEnum, StringEnum, StringEnumEntry}
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.numeric.NonNegLong
import io.circe._
import io.circe.refined._
import io.estatico.newtype.macros.newtype
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.types.DataUpdates.RewardAllocationVoteUpdate
import org.amm_metagraph.shared_data.types.States.{epochProgressKeyDecode, epochProgressKeyEncode}

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
    weight: NonNegLong, // Voting power for that locked token
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

    implicit val keyEncoder: KeyEncoder[AllocationCategory] = KeyEncoder.encodeKeyString.contramap(_.value)
    implicit val keyDecoder: KeyDecoder[AllocationCategory] = KeyDecoder.instance { str =>
      values.find(_.value === str)
    }
  }

  @derive(encoder, decoder, order, ordering, show)
  case class Allocation(
    id: AllocationId,
    percentage: Percentage
  )

  // latest epoch shall be bigger than early epochs by order / ordering type classes
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

    implicit val keyEncoder: KeyEncoder[MonthlyReference] =
      tuple3KeyEncoder[String, String, String]
        .contramap(mr =>
          (
            epochProgressKeyEncode.apply(mr.firstEpochOfMonth),
            epochProgressKeyEncode.apply(mr.lastEpochOfMonth),
            implicitly[KeyEncoder[NonNegLong]].apply(mr.monthReference)
          )
        )

    implicit val keyDecoder: KeyDecoder[MonthlyReference] =
      tuple3KeyDecoder[EpochProgress, EpochProgress, NonNegLong].map {
        case (firstEpoch, lastEpoch, monthReference) => MonthlyReference(firstEpoch, lastEpoch, monthReference)
      }
  }

  implicit def tuple3KeyEncoder[A, B, C](implicit A: KeyEncoder[A], B: KeyEncoder[B], C: KeyEncoder[C]): KeyEncoder[(A, B, C)] =
    KeyEncoder.instance[(A, B, C)] { case (a, b, c) => A(a) + ":" + B(b) + ":" + C(c) }

  implicit def tuple3KeyDecoder[A, B, C](implicit A: KeyDecoder[A], B: KeyDecoder[B], C: KeyDecoder[C]): KeyDecoder[(A, B, C)] =
    KeyDecoder.instance[(A, B, C)] {
      case s"$as:$bs:$cs" =>
        for {
          a <- A(as)
          b <- B(bs)
          c <- C(cs)
        } yield (a, b, c)
      case _ => None
    }

  @derive(encoder, decoder, order, ordering, show)
  case class FrozenAddressesVotes(
    monthlyReference: MonthlyReference,
    votes: SortedMap[Address, VotingWeight],
    allocationVotes: SortedMap[AllocationId, Percentage]
  )
  object FrozenAddressesVotes {
    val empty: FrozenAddressesVotes = FrozenAddressesVotes(MonthlyReference.empty, SortedMap.empty, SortedMap.empty)
  }

  @derive(encoder, decoder, order, ordering, show)
  case class Allocations(
    monthlyReference: MonthlyReference,
    usersAllocations: SortedMap[Address, UserAllocations],
    frozenUsedUserVotes: FrozenAddressesVotes
  )

  object Allocations {
    def empty: Allocations = Allocations(MonthlyReference.empty, SortedMap.empty, FrozenAddressesVotes.empty)
  }

  /** User allocations / votes for some address
    * @param credits
    *   internal usage, used for preventing vote allocation spam
    * @param reference
    *   reference for previous voting
    * @param allocationEpochProgress
    *   when allocation had been made
    * @param allocations
    *   voting itself in form: votedId -> percentage_of_voting_power therefore allocations.map(allocation => allocation.percentage).sum ===
    *   100.0
    */
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

  @derive(encoder, decoder, order, ordering, show)
  case class AllocationId(
    id: String,
    category: AllocationCategory
  )

  implicit val keyEncoder: KeyEncoder[AllocationId] =
    tupleKeyEncoder[String, String].contramap(aId => (aId.id, implicitly[KeyEncoder[AllocationCategory]].apply(aId.category)))

  implicit val keyDecoder: KeyDecoder[AllocationId] =
    tupleKeyDecoder[String, AllocationCategory].map { case (id, category) => AllocationId(id, category) }
}
