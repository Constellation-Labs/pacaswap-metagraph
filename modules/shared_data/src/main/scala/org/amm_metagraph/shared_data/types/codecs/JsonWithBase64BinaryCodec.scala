package org.amm_metagraph.shared_data.types.codecs

import cats.effect.Sync
import cats.syntax.all._

import io.circe.jawn.JawnParser
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Printer}
import org.bouncycastle.util.encoders.Base64

trait JsonWithBase64BinaryCodec[F[_], A] {
  def serialize(content: A): F[Array[Byte]]
  def deserialize(content: Array[Byte]): F[Either[Throwable, A]]
}

object JsonWithBase64BinaryCodec {

  def forSync[F[_]: Sync, A: Encoder: Decoder]: F[JsonWithBase64BinaryCodec[F, A]] = {
    val printer = Printer.noSpaces.copy(dropNullValues = true, sortKeys = true)
    forSync[F, A](printer)
  }

  def forSync[F[_]: Sync, A: Encoder: Decoder](printer: Printer): F[JsonWithBase64BinaryCodec[F, A]] =
    Sync[F].delay {
      new JsonWithBase64BinaryCodec[F, A] {
        private def simpleJsonSerializer(content: A): F[Array[Byte]] =
          Sync[F].delay(content.asJson.printWith(printer).getBytes("UTF-8"))

        override def serialize(content: A): F[Array[Byte]] = for {
          jsonBytes <- simpleJsonSerializer(content)
          base64String <- Sync[F].delay(Base64.toBase64String(jsonBytes))
          prefixedString = s"\u0019Constellation Signed Data:\n${base64String.length}\n$base64String"
        } yield prefixedString.getBytes("UTF-8")

        // D3-02: total deserializer. Every step is captured in Either so malformed/untrusted bytes return Left instead
        // of throwing out of the F. The base64 reconstruction (drop the 2 envelope header lines, join the rest) and
        // therefore the accept/reject outcome for any given input are IDENTICAL to the previous implementation — this
        // is purely a robustness change (no throw escapes), fully backward compatible.
        override def deserialize(content: Array[Byte]): F[Either[Throwable, A]] =
          Sync[F].delay {
            for {
              base64String <- Either.catchNonFatal(new String(content, "UTF-8").split("\n").drop(2).mkString)
              jsonBytes <- Either.catchNonFatal(Base64.decode(base64String))
              a <- JawnParser(false).decodeByteArray[A](jsonBytes).leftWiden[Throwable]
            } yield a
          }
      }
    }
}
