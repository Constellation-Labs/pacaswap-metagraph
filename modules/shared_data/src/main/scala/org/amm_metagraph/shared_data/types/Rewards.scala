package org.amm_metagraph.shared_data.types

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

    case object ValidatorConsensus extends RewardType
    case object ValidatorBoost extends RewardType
    case object LpBoost extends RewardType
    case object GovernanceVoting extends RewardType
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
      that.info.foldLeft(this.asRight[BalanceArithmeticError]) {
        case (acc, (AddressAndRewardType(address, rewardType), amount)) =>
          acc.flatMap(a => a.addReward(address, rewardType, amount))
      }
  }

  object RewardInfo {
    val empty: RewardInfo = RewardInfo(Map.empty)

    def fromRewardDistribution(rewardDistribution: RewardDistribution): RewardInfo = {
      val validator = rewardDistribution.validatorRewards.map { case (address, amount) => (address, RewardType.ValidatorConsensus, amount) }
      val voting = rewardDistribution.votingRewards.map { case (address, amount) => (address, RewardType.ValidatorBoost, amount) }
      val governance = rewardDistribution.governanceRewards.map { case (address, amount) => (address, RewardType.GovernanceVoting, amount) }
      val dao = {
        val (address, amount) = rewardDistribution.daoRewards
        List((address, RewardType.LpBoost, amount))
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
