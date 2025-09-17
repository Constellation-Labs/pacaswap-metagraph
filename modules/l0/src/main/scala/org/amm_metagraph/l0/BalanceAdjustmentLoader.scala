package org.amm_metagraph.l0

import scala.collection.immutable.SortedSet
import scala.io.Source
import scala.util.Try

import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact._
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.security.hash.Hash

import derevo.cats.{eqv, show}
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import eu.timepit.refined.types.numeric.NonNegLong
import io.circe.Decoder
import io.circe.parser.decode

object BalanceAdjustmentLoader {

  @derive(encoder, decoder, eqv, show)
  case class RawBalanceAdjustment(
    address: Address,
    reason: String,
    reference: List[Hash],
    increase: Option[Long] = None,
    deduct: Option[Long] = None
  )

  implicit val balanceAdjustmentDecoder: Decoder[BalanceAdjustment] = {
    val rawDecoder = implicitly[Decoder[RawBalanceAdjustment]]

    rawDecoder.emap { raw =>
      val reasonResult = raw.reason match {
        case "SpendTransactionNotApplied"            => Right(SpendTransactionNotApplied)
        case "SpendTransactionSourceNotApplied"      => Right(SpendTransactionSourceNotApplied)
        case "SpendTransactionDestinationNotApplied" => Right(SpendTransactionDestinationNotApplied)
        case other                                   => Left(s"Unknown BalanceAdjustmentReason: $other")
      }

      reasonResult.map { reason =>
        BalanceAdjustment(
          address = raw.address,
          reason = reason,
          reference = SortedSet(raw.reference: _*),
          increase = raw.increase.map(increase => Amount(NonNegLong.unsafeFrom(increase))),
          deduct = raw.deduct.map(deduct => Amount(NonNegLong.unsafeFrom(Math.abs(deduct))))
        )
      }
    }
  }

  def loadBalanceAdjustments(resourcePath: String): Try[List[BalanceAdjustment]] =
    Try {
      val jsonString = Source.fromResource(resourcePath).mkString
      decode[List[BalanceAdjustment]](jsonString) match {
        case Right(adjustments) => adjustments
        case Left(error)        => throw new RuntimeException(s"Failed to parse JSON: $error")
      }
    }
}
