package org.amm_metagraph.shared_data.types

import cats.Show
import cats.implicits._

import scala.util.Try

import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.balance.{Amount, BalanceArithmeticError}
import io.constellationnetwork.schema.{tupleKeyDecoder, tupleKeyEncoder}

import derevo.cats.{eqv, show}
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import enumeratum.{Enum, EnumEntry}
import io.circe._
import org.amm_metagraph.shared_data.rewards.RewardDistribution

object Rewards {

  @derive(eqv, show)
  sealed trait RewardType extends EnumEntry

  object RewardType extends Enum[RewardType] with RewardTypeCodecs {
    val values: IndexedSeq[RewardType] = findValues

    case object NodeValidator extends RewardType
    case object VoteBased extends RewardType
    case object Dao extends RewardType
    case object Governance extends RewardType
  }

  trait RewardTypeCodecs {
    implicit val encode: Encoder[RewardType] = Encoder.encodeString.contramap[RewardType](_.entryName)
    implicit val decode: Decoder[RewardType] = Decoder.decodeString.emapTry(s => Try(RewardType.withName(s)))

    implicit val keyEncode: KeyEncoder[RewardType] = KeyEncoder.encodeKeyString.contramap(_.entryName)
    implicit val keyDecode: KeyDecoder[RewardType] = KeyDecoder.instance(RewardType.withNameOption)
  }

  @derive(encoder, decoder, show)
  case class AddressAndRewardType(address: Address, rewardType: RewardType)
  object AddressAndRewardType {
    implicit val keyEncode: KeyEncoder[AddressAndRewardType] =
      tupleKeyEncoder[Address, RewardType].contramap[AddressAndRewardType](v => (v.address, v.rewardType))
    implicit val keyDecode: KeyDecoder[AddressAndRewardType] =
      tupleKeyDecoder[Address, RewardType].map[AddressAndRewardType](t => AddressAndRewardType(t._1, t._2))
  }

  implicit def showIndexedSeqAsList[T: Show]: Show[IndexedSeq[T]] =
    Show.show(_.toList.show)

  @derive(encoder, decoder, show)
  case class RewardsBuffer(data: IndexedSeq[(AddressAndRewardType, Amount)])

  object RewardsBuffer {
    val empty: RewardsBuffer = RewardsBuffer(IndexedSeq.empty)
  }

  @derive(encoder, decoder, show)
  case class RewardInfo(info: Map[AddressAndRewardType, Amount]) {
    def addReward(
      address: Address,
      rewardType: RewardType,
      value: Amount
    ): Either[BalanceArithmeticError, RewardInfo] = {
      val key = AddressAndRewardType(address, rewardType)
      val oldValue = info.getOrElse(key, Amount.empty)
      oldValue.plus(value).map { newValue =>
        val combinedEntry = key -> newValue
        RewardInfo(info + combinedEntry)
      }
    }

    def subtractReward(
      address: Address,
      rewardType: RewardType,
      value: Amount
    ): Either[BalanceArithmeticError, RewardInfo] = {
      val key = AddressAndRewardType(address, rewardType)
      val oldValue = info.getOrElse(key, Amount.empty)
      oldValue.minus(value).map { newValue =>
        val combinedEntry = key -> newValue
        if (newValue.value.value != 0) {
          RewardInfo(info + combinedEntry)
        } else {
          RewardInfo(info - key)
        }
      }
    }

    def currentAmount(address: Address, rewardType: RewardType): Amount = {
      val key = AddressAndRewardType(address, rewardType)
      info.getOrElse(key, Amount.empty)
    }

    def addRewards(that: RewardInfo): Either[BalanceArithmeticError, RewardInfo] =
      addRewards(that.info)

    def addRewards(that: Iterable[(AddressAndRewardType, Amount)]): Either[BalanceArithmeticError, RewardInfo] =
      that.foldLeft(this.asRight[BalanceArithmeticError]) {
        case (acc, (AddressAndRewardType(address, rewardType), amount)) =>
          acc.flatMap(a => a.addReward(address, rewardType, amount))
      }
  }

  object RewardInfo {
    val empty: RewardInfo = RewardInfo(Map.empty)

    def make(that: Iterable[(AddressAndRewardType, Amount)]): Either[BalanceArithmeticError, RewardInfo] = RewardInfo.empty.addRewards(that)

    def fromRewardDistribution(rewardDistribution: RewardDistribution): RewardInfo = {
      val validator = rewardDistribution.nodeValidatorRewards.map {
        case (address, amount) => (address, RewardType.NodeValidator, amount)
      }
      val voting = rewardDistribution.voteBasedRewards.map { case (address, amount) => (address, RewardType.VoteBased, amount) }
      val governance = rewardDistribution.governanceRewards.map { case (address, amount) => (address, RewardType.Governance, amount) }
      val dao = {
        val (address, amount) = rewardDistribution.daoRewards
        List((address, RewardType.Dao, amount))
      }

      val allNewRewards = validator ++ voting ++ governance ++ dao
      val rewardsMap = allNewRewards.map {
        case (address, rewardType, amount) =>
          AddressAndRewardType(address, rewardType) -> amount
      }.toMap
      RewardInfo(rewardsMap)
    }
  }
}
