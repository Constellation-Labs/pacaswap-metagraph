package org.amm_metagraph.shared_data.services.pricing

import java.io.{FileWriter, PrintWriter}

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
    def logBalanceChange(change: PoolBalanceChange): F[Unit] = Async[F].delay {
      val logEntry = formatLogEntry(change)
      Try {
        Using(new PrintWriter(new FileWriter(logFilePath, true))) { writer =>
          writer.println(logEntry)
        }
      }
    }.void

    private def formatLogEntry(change: PoolBalanceChange): String = {
      val basicInfo = s"[${change.timestamp}] POOL_BALANCE_CHANGE " +
        s"operation=${change.operation} " +
        s"epoch=${change.epochProgress.map(_.value).getOrElse("N/A")} " +
        s"hash=${change.updateHash.map(_.value).getOrElse("N/A")}"

      val balanceInfo =
        s"tokenA=[${change.beforeTokenA._1}:${change.beforeTokenA._2}->${change.afterTokenA._2}(${formatChange(change.tokenAChange)})] " +
          s"tokenB=[${change.beforeTokenB._1}:${change.beforeTokenB._2}->${change.afterTokenB._2}(${formatChange(change.tokenBChange)})] " +
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
