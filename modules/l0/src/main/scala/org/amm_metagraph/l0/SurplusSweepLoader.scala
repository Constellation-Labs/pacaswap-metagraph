package org.amm_metagraph.l0

import cats.data.NonEmptyList

import scala.io.Source
import scala.util.Try

import io.constellationnetwork.schema.address.{Address, DAGAddressRefined}
import io.constellationnetwork.schema.artifact.{SpendAction, SpendTransaction}
import io.constellationnetwork.schema.swap.{CurrencyId, SwapAmount}

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import eu.timepit.refined.refineV
import eu.timepit.refined.types.numeric.PosLong
import io.circe.parser.decode

/** Moves a token surplus out of the metagraph's custody address.
  *
  * The custody address has no private key. It is derived from the metagraph id, nobody holds a
  * key for it, and it has never signed a transaction on any ledger. Value leaves it only through
  * a SpendAction the metagraph itself emits, which in normal operation happens in exactly one
  * place: paying out an LP withdrawal. So a surplus sitting there cannot be transferred back by
  * hand, and cannot be burned by hand either. This is the mechanism for it.
  *
  * Used for The Upsider AI: the wallet holds 71,706,224.58112005 UP that no pool's book claims.
  * Sweeping it leaves the pool's book untouched, so the pool reaches reserve == wallet with no
  * price movement, and the treasury keeps the tokens. The alternative was booking the surplus
  * into the pool, which would have halved that pool's UP price.
  *
  * FAIL CLOSED. Every field is validated and a placeholder destination is rejected. Sending
  * tokens to a wrong address is irreversible: the destination cannot be recovered any more than
  * the source can. If this resource is missing, malformed, or still carries the placeholder, the
  * node must refuse to build the snapshot rather than send anything anywhere.
  *
  * It is also fail-safe in the other direction: if the SpendAction never settles, the pool is
  * merely over-backed, which is harmless. It can never create a shortfall.
  */
object SurplusSweepLoader {

  /** The value shipped in the resource until an operator sets a real destination. */
  val DestinationPlaceholder = "SET_DESTINATION_BEFORE_DEPLOY"

  @derive(encoder, decoder)
  case class RawSurplusSweep(
    currencyId: String,
    amount: Long,
    source: String,
    destination: String,
    reason: String
  )

  def loadSweep(resourcePath: String): Try[SpendAction] =
    Try {
      val source = Source.fromResource(resourcePath)
      val jsonString =
        try source.mkString
        finally source.close()

      val raw = decode[RawSurplusSweep](jsonString) match {
        case Right(v)    => v
        case Left(error) => throw new RuntimeException(s"$resourcePath: failed to parse: $error")
      }

      if (raw.destination == DestinationPlaceholder || raw.destination.trim.isEmpty)
        throw new RuntimeException(
          s"$resourcePath: destination is still the placeholder. Set the address that should " +
            "receive the surplus before deploying. This transfer cannot be reversed."
        )
      if (raw.amount <= 0L)
        throw new RuntimeException(s"$resourcePath: amount must be positive, got ${raw.amount}")
      if (raw.source == raw.destination)
        throw new RuntimeException(s"$resourcePath: source and destination are the same address")

      def address(label: String, value: String): Address =
        refineV[DAGAddressRefined](value) match {
          case Right(a) => Address(a)
          case Left(_)  => throw new RuntimeException(s"$resourcePath: $label is not a valid address: $value")
        }

      val src = address("source", raw.source)
      val dst = address("destination", raw.destination)
      val cur = address("currencyId", raw.currencyId)

      SpendAction(
        NonEmptyList.of(
          SpendTransaction(
            allowSpendRef = None,
            currencyId = Some(CurrencyId(cur)),
            amount = SwapAmount(PosLong.unsafeFrom(raw.amount)),
            source = src,
            destination = dst
          )
        )
      )
    }
}
