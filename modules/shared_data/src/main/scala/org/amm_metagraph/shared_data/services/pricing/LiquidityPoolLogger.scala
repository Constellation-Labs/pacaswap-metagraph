package org.amm_metagraph.shared_data.services.pricing

import java.io.{FileWriter, PrintWriter}
import java.nio.file.{Files, Paths}

import cats.effect.Async
import cats.syntax.all._

import scala.util.{Try, Using}

import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.CurrencyId
import io.constellationnetwork.security.hash.Hash

case class PoolBalanceChange(
  operation: String,
  timestamp: String,
  epochProgress: Option[EpochProgress],
  updateHash: Option[Hash],
  beforeTokenA: (Option[CurrencyId], Long),
  beforeTokenB: (Option[CurrencyId], Long),
  afterTokenA: (Option[CurrencyId], Long),
  afterTokenB: (Option[CurrencyId], Long),
  tokenAChange: Long,
  tokenBChange: Long,
  beforeK: BigInt,
  afterK: BigInt,
  address: Option[Address] = None,
  additionalInfo: Map[String, String] = Map.empty
)

trait LiquidityPoolLogger[F[_]] {
  def logBalanceChange(change: PoolBalanceChange): F[Unit]
}

object LiquidityPoolLogger {
  def make[F[_]: Async](logFilePath: String): LiquidityPoolLogger[F] = new LiquidityPoolLogger[F] {

    private def ensureDirectoryExists(filePath: String): F[Unit] = Async[F].delay {
      val path = Paths.get(filePath)
      val parentDir = path.getParent
      if (parentDir != null && !Files.exists(parentDir)) {
        Files.createDirectories(parentDir)
        ()
      }
    }

    def logBalanceChange(change: PoolBalanceChange): F[Unit] = for {
      _ <- ensureDirectoryExists(logFilePath)
      _ <- Async[F].delay {
        val logEntry = formatLogEntry(change)
        val result = Using(new PrintWriter(new FileWriter(logFilePath, true))) { writer =>
          writer.println(logEntry)
          writer.flush()
        }

        result match {
          case scala.util.Success(_) => ()
          case scala.util.Failure(ex) =>
            System.err.println(s"Failed to write to log file $logFilePath: ${ex.getMessage}")
            ex.printStackTrace()
        }
      }.handleErrorWith { ex =>
        Async[F].delay {
          System.err.println(s"Error in logBalanceChange: ${ex.getMessage}")
          ex.printStackTrace()
        }
      }
    } yield ()

    private def formatLogEntry(change: PoolBalanceChange): String = {
      val basicInfo = s"[${change.timestamp}] POOL_BALANCE_CHANGE " +
        s"operation=${change.operation} " +
        s"epoch=${change.epochProgress.map(_.value).getOrElse("N/A")} " +
        s"hash=${change.updateHash.map(_.value).getOrElse("N/A")}"

      val balanceInfo =
        s"tokenA=[${change.beforeTokenA._1.getOrElse("None")}:${change.beforeTokenA._2}->${change.afterTokenA._2}(${formatChange(change.tokenAChange)})] " +
          s"tokenB=[${change.beforeTokenB._1.getOrElse("None")}:${change.beforeTokenB._2}->${change.afterTokenB._2}(${formatChange(change.tokenBChange)})] " +
          s"k=[${change.beforeK}->${change.afterK}]"

      val addressInfo = change.address.map(addr => s" address=${addr.value}").getOrElse("")
      val additionalInfo = if (change.additionalInfo.nonEmpty) {
        " " + change.additionalInfo.map { case (k, v) => s"$k=$v" }.mkString(" ")
      } else ""

      s"$basicInfo $balanceInfo$addressInfo$additionalInfo"
    }

    private def formatChange(change: Long): String = if (change >= 0) s"+$change" else change.toString
  }
}
