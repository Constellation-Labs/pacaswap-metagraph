package org.amm_metagraph.shared_data.types.codecs

import cats.effect.Sync
import cats.syntax.all._
import io.circe.jawn.JawnParser
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Printer}
import org.tessellation.json.JsonSerializer
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object JsonBinaryCodec {

  def apply[F[_] : JsonSerializer]: JsonSerializer[F] = implicitly

  def forSync[F[_] : Sync]: F[JsonSerializer[F]] = {
    def printer = Printer(dropNullValues = true, indent = "", sortKeys = true)

    forSync[F](printer)
  }

  def forSync[F[_] : Sync](printer: Printer): F[JsonSerializer[F]] =
    Sync[F].delay {
      new JsonSerializer[F] {
        def logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("JsonSerializer")
        override def serialize[A: Encoder](content: A): F[Array[Byte]] = {
          logger.info(s"Serialized JSON: ${content.asJson.printWith(printer)}").as(
          content.asJson.printWith(printer).getBytes("UTF-8")
          )
        }


        override def deserialize[A: Decoder](content: Array[Byte]): F[Either[Throwable, A]] =
          Sync[F]
            .delay(content)
            .map(JawnParser(false).decodeByteArray[A](_))
      }
    }
}
