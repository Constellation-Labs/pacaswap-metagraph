package org.amm_metagraph.shared_data.types

import java.time._

import cats.Order
import cats.effect.kernel.Async
import cats.syntax.functor._
import cats.syntax.semigroup._

import io.constellationnetwork.ext.crypto._
import io.constellationnetwork.ext.derevo.ordering
import io.constellationnetwork.schema._
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.tokenLock.TokenLock
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hashed, Hasher}

import derevo.cats.order
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import enumeratum.values.{StringCirceEnum, StringEnum, StringEnumEntry}
import eu.timepit.refined.auto.autoRefineV
import eu.timepit.refined.cats._
import eu.timepit.refined.types.numeric.{NonNegDouble, NonNegLong}
import io.circe.refined._
import io.circe.{Decoder, Encoder}
import io.estatico.newtype.macros.newtype
import org.amm_metagraph.shared_data.epochProgress.oneEpochProgressInSeconds
import org.amm_metagraph.shared_data.types.DataUpdates.RewardAllocationVoteUpdate

object Governance {
  val maxCredits = 50.0

  @newtype
  @derive(order, encoder, decoder)
  case class RewardAllocationVoteOrdinal(value: NonNegLong) {
    def next: RewardAllocationVoteOrdinal = RewardAllocationVoteOrdinal(value |+| 1L)
  }

  object RewardAllocationVoteOrdinal {
    val first: RewardAllocationVoteOrdinal = RewardAllocationVoteOrdinal(1L)

    implicit val decoder: Decoder[RewardAllocationVoteOrdinal] = deriving
    implicit val encoder: Encoder[RewardAllocationVoteOrdinal] = deriving
    implicit val orderInstance: Order[RewardAllocationVoteOrdinal] = Order.by(_.value)
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
    weight: NonNegDouble,
    tokenLock: TokenLock
  )

  @derive(encoder, decoder)
  case class VotingWeight(
    total: NonNegDouble,
    info: List[VotingWeightInfo]
  )

  object VotingWeight {
    def empty: VotingWeight = VotingWeight(NonNegDouble.MinValue, List.empty)
  }

  sealed abstract class AllocationCategory(val value: String) extends StringEnumEntry
  object AllocationCategory extends StringEnum[AllocationCategory] with StringCirceEnum[AllocationCategory] {
    val values = findValues

    case object NodeOperator extends AllocationCategory("NodeOperator")
    case object LiquidityPool extends AllocationCategory("LiquidityPool")

    implicit val encoder: Encoder[AllocationCategory] = Encoder.encodeString.contramap(_.value)
    implicit val decoder: Decoder[AllocationCategory] = Decoder.decodeString.emap { str =>
      values.find(_.value == str).toRight(s"Invalid AllocationCategory value: $str")
    }
  }

  @derive(encoder, decoder)
  @newtype
  case class AllocationId(value: String)

  @derive(encoder, decoder)
  case class Allocation(
    id: AllocationId,
    category: AllocationCategory,
    weight: NonNegDouble
  )

  @derive(encoder, decoder)
  case class AllocationEpochProgressInfo(
    allocationGlobalEpochProgress: EpochProgress,
    expireGlobalEpochProgress: EpochProgress,
    monthReference: Int
  )

  object AllocationEpochProgressInfo {
    def empty: AllocationEpochProgressInfo =
      AllocationEpochProgressInfo(
        EpochProgress.MinValue,
        EpochProgress.MinValue,
        Int.MinValue
      )

    def getAllocationEpochProgressInfo(
      globalEpochProgress: EpochProgress
    ): AllocationEpochProgressInfo = {
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

      AllocationEpochProgressInfo(
        globalEpochProgress,
        expireGlobalEpochProgress,
        YearMonth.now(ZoneId.of("UTC")).getMonth.getValue
      )
    }
  }

  @derive(encoder, decoder)
  case class UserAllocations(
    credits: Double,
    reference: RewardAllocationVoteReference,
    allocationEpochProgressInfo: AllocationEpochProgressInfo,
    allocations: List[Allocation]
  )

  object UserAllocations {
    def empty: UserAllocations =
      UserAllocations(maxCredits, RewardAllocationVoteReference.empty, AllocationEpochProgressInfo.empty, List.empty)
  }
}
