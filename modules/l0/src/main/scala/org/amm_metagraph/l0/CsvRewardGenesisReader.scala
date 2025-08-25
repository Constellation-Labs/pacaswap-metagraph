package org.amm_metagraph.l0

import java.io.InputStream

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.schema.address.{Address, DAGAddressRefined}

import eu.timepit.refined.refineV
import eu.timepit.refined.types.numeric.PosLong
import fs2.data.csv._
import fs2.data.csv.generic.semiauto._
import fs2.data.csv.lowlevel.rows
import fs2.io.readInputStream
import fs2.{Stream, text}

object CsvRewardGenesisReader {

  private val bufferSize = 64 * 1024 // 64KB buffer for reading input stream

  case class RewardGenesisInfo(address: Address, amount: PosLong)

  implicit val addressCellDecoder: CellDecoder[Address] =
    CellDecoder.stringDecoder.emap { s =>
      refineV[DAGAddressRefined](s.trim)
        .leftMap(err => new DecoderError(s"Invalid DAG address '$s': $err"))
        .map(Address(_))
    }

  implicit val posLongCellDecoder: CellDecoder[PosLong] =
    CellDecoder.longDecoder.emap { n =>
      PosLong.from(n).leftMap(e => new DecoderError(s"Invalid positive amount '$n': $e"))
    }

  implicit val rewardRowDecoder: RowDecoder[RewardGenesisInfo] = deriveRowDecoder

  def readFromResources[F[_]: Async](resourceName: String): F[List[RewardGenesisInfo]] = {
    val getIsF: F[InputStream] =
      Async[F].delay(Option(getClass.getClassLoader.getResourceAsStream(resourceName))).flatMap {
        case Some(is) => is.pure[F]
        case None     => Async[F].raiseError(new RuntimeException(s"Resource $resourceName not found"))
      }

    val stringStream: Stream[F, String] = readInputStream(getIsF, bufferSize, closeAfterUse = true)
      .through(text.utf8.decode)

    stringStream
      .through(rows())
      .evalMap { row =>
        val result: Either[DecoderError, RewardGenesisInfo] = RowDecoder[RewardGenesisInfo].apply(row)
        result match {
          case Right(info) => Async[F].pure[RewardGenesisInfo](info)
          case Left(err)   => Async[F].raiseError[RewardGenesisInfo](new RuntimeException(s"Row decode error: ${err.toString}"))
        }
      }
      .compile
      .toList
  }
}
