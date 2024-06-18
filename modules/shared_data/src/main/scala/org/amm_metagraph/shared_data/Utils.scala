package org.amm_metagraph.shared_data

import cats.MonadThrow
import cats.effect.Async
import cats.syntax.all._
import eu.timepit.refined.types.numeric.PosLong
import org.tessellation.schema.address.Address
import org.tessellation.security.SecurityProvider
import org.tessellation.security.signature.signature.SignatureProof

import scala.collection.SortedSet


object Utils {
  def toAddress[F[_] : Async : SecurityProvider](proof: SignatureProof): F[Address] =
    proof.id.toAddress

  def buildLiquidityPoolUniqueIdentifier[F[_] : MonadThrow](maybeTokenAId: Option[Address], maybeTokenBId: Option[Address]): F[String] =
    SortedSet(maybeTokenAId, maybeTokenBId)
      .flatten
      .mkString("-")
      .pure[F]
      .ensure(new IllegalArgumentException("You should provide at least one currency token identifier"))(_.nonEmpty)

  implicit class PosLongOps(value: Long) {
    def toPosLongUnsafe: PosLong =
      PosLong.unsafeFrom(value)
  }

  implicit class LongOps(value: Long) {
    def fromTokenAmountFormat: Double =
      value / 10e7

    def toTokenAmountFormat: Long =
      (value * 10e7).toLong
  }

  implicit class DoubleOps(value: Double) {
    def toTokenAmountFormat: Long =
      (value * 10e7).toLong
  }
}