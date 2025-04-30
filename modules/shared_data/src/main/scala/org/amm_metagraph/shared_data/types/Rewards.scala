package org.amm_metagraph.shared_data.types

import scala.util.Try

import io.constellationnetwork.schema.balance.Amount

import derevo.cats.{eqv, show}
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import enumeratum.{Enum, EnumEntry}
import io.circe._
import io.circe.generic.auto._

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

  @derive(encoder, decoder)
  case class RewardInformation(amount: Amount)

  @derive(encoder, decoder)
  case class RewardState(reward: Map[RewardType, RewardInformation])
}
