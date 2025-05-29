package org.amm_metagraph.shared_data.types

import java.time._

import cats.Order
import cats.Order._
import cats.effect.kernel.Async
import cats.syntax.all._

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

import derevo.cats.order
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

  @derive(decoder, encoder, order, ordering)
  @newtype
  case class RewardAllocationVoteOrdinal(value: NonNegLong) {
    def next: RewardAllocationVoteOrdinal = RewardAllocationVoteOrdinal(value |+| 1L)
  }

  object RewardAllocationVoteOrdinal {
    val first: RewardAllocationVoteOrdinal = RewardAllocationVoteOrdinal(0L)
  }

  @derive(decoder, encoder, order, ordering)
  case class RewardAllocationVoteReference(ordinal: RewardAllocationVoteOrdinal, hash: Hash)

  object RewardAllocationVoteReference {
    def of(hashedTransaction: Hashed[RewardAllocationVoteUpdate]): RewardAllocationVoteReference =
      RewardAllocationVoteReference(hashedTransaction.ordinal, hashedTransaction.hash)

    def of[F[_]: Async: Hasher](signedTransaction: Signed[RewardAllocationVoteUpdate]): F[RewardAllocationVoteReference] =
      signedTransaction.value.hash.map(RewardAllocationVoteReference(signedTransaction.ordinal, _))

    val empty: RewardAllocationVoteReference = RewardAllocationVoteReference(RewardAllocationVoteOrdinal(0L), Hash.empty)
  }

  @derive(encoder, decoder)
  case class VotingWeightInfo(
    weight: NonNegLong,
    tokenLock: TokenLock,
    votedAtEpochProgress: EpochProgress
  )

  object VotingWeightInfo {
    implicit val order: Order[VotingWeightInfo] = Order.by(_.votedAtEpochProgress)
    implicit val ordering: Ordering[VotingWeightInfo] = order.toOrdering
  }

  @derive(encoder, decoder)
  case class VotingWeight(
    total: NonNegLong,
    info: SortedSet[VotingWeightInfo]
  )

  object VotingWeight {
    def empty: VotingWeight = VotingWeight(NonNegLong.MinValue, SortedSet.empty)

    implicit val order: Order[VotingWeight] = Order.by(_.total)
    implicit val ordering: Ordering[VotingWeight] = order.toOrdering
  }

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

  @derive(encoder, decoder, order, ordering)
  case class Allocation(
    id: String,
    category: AllocationCategory,
    percentage: NonNegDouble
  )

  @derive(encoder, decoder, order, ordering)
  case class MonthlyReference(
    expireGlobalEpochProgress: EpochProgress,
    monthReference: NonNegInt
  )

  object MonthlyReference {
    def empty: MonthlyReference =
      MonthlyReference(
        EpochProgress.MinValue,
        NonNegInt.MinValue
      )

    def getMonthlyReference(
      environment: Environment,
      globalEpochProgress: EpochProgress,
      oneEpochProgressInSeconds: Int
    ): MonthlyReference =
      environment match {
        case ApplicationConfig.Dev =>
          MonthlyReference(
            EpochProgress(NonNegLong.unsafeFrom(globalEpochProgress.value.value + 3L)),
            YearMonth.now(ZoneId.of("UTC")).getMonth.getValue.toNonNegIntUnsafe
          )
        case _ =>
          def getSecondsUntilMonthEnd: Long = {
            val now = LocalDateTime.now(ZoneId.of("UTC"))
            val lastDayOfMonth = YearMonth.now(ZoneId.of("UTC")).atEndOfMonth()
            val monthEnd = LocalDateTime.of(lastDayOfMonth.getYear, lastDayOfMonth.getMonth, lastDayOfMonth.getDayOfMonth, 23, 59, 59)
            val duration = Duration.between(now, monthEnd)
            duration.getSeconds
          }

          val secondsUntilMonthEnd = getSecondsUntilMonthEnd
          val epochProgressesUntilMonthEnd = secondsUntilMonthEnd / oneEpochProgressInSeconds
          val expireGlobalEpochProgressValue = globalEpochProgress.value.value + epochProgressesUntilMonthEnd
          val expireGlobalEpochProgress = EpochProgress(NonNegLong.unsafeFrom(expireGlobalEpochProgressValue))

          MonthlyReference(
            expireGlobalEpochProgress,
            YearMonth.now(ZoneId.of("UTC")).getMonth.getValue.toNonNegIntUnsafe
          )
      }
  }

  @derive(encoder, decoder, order, ordering)
  case class AllocationsRewards(
    monthReference: NonNegInt,
    epochProgressToReward: EpochProgress,
    rewardsInfo: SortedMap[String, Double]
  )

  object AllocationsRewards {
    def empty: AllocationsRewards = AllocationsRewards(0, EpochProgress.MinValue, SortedMap.empty)
  }

  @derive(encoder, decoder, order, ordering)
  case class Allocations(
    monthlyReference: MonthlyReference,
    usersAllocations: SortedMap[Address, UserAllocations],
    allocationsRewards: SortedSet[AllocationsRewards]
  )

  object Allocations {
    def empty: Allocations = Allocations(MonthlyReference.empty, SortedMap.empty, SortedSet.empty)
  }

  @derive(encoder, decoder, order, ordering)
  case class UserAllocations(
    credits: Double,
    reference: RewardAllocationVoteReference,
    allocationGlobalEpochProgress: EpochProgress,
    allocations: SortedSet[Allocation]
  )

  object UserAllocations {
    def empty: UserAllocations =
      UserAllocations(maxCredits, RewardAllocationVoteReference.empty, EpochProgress.MinValue, SortedSet.empty)
  }
}
