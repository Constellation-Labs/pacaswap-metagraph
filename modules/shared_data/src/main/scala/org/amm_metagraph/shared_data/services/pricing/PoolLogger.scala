package org.amm_metagraph.shared_data.services.pricing

import java.io.{FileWriter, PrintWriter}
import java.nio.file.{Files, Paths}
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneOffset}

import cats.effect.Async
import cats.syntax.all._

import scala.util.{Failure, Success, Using}

import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.security.hash.Hash

import org.amm_metagraph.shared_data.services.pricing.models.PoolBalanceChange
import org.amm_metagraph.shared_data.types.LiquidityPool.LiquidityPool
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

trait PoolLogger[F[_]] {
  def logBalanceChange(change: PoolBalanceChange): F[Unit]

  def logPoolOperation(
    operation: String,
    beforePool: LiquidityPool,
    afterPool: LiquidityPool,
    epochProgress: Option[EpochProgress] = None,
    updateHash: Option[Hash] = None,
    address: Option[Address] = None,
    additionalInfo: Map[String, String] = Map.empty
  ): F[Unit]
}

object PoolLogger {
  def make[F[_]: Async](logFilePath: String): F[PoolLogger[F]] = {
    val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

    for {
      _ <- logger.info(s"Initializing PoolLogger with file: $logFilePath")
      _ <- logger.info(s"Absolute path: ${Paths.get(logFilePath).toAbsolutePath}")
      poolLogger = new PoolLogger[F] {

        private def ensureFileExists(filePath: String): F[Unit] =
          Async[F].delay {
            val path = Paths.get(filePath)
            val parentDir = path.getParent

            // Create parent directory if needed
            if (parentDir != null && !Files.exists(parentDir)) {
              Files.createDirectories(parentDir)
            }

            if (!Files.exists(path)) {
              Files.createFile(path)
            }
          } *> logger.debug(s"Ensured log file exists: $filePath")

        def logBalanceChange(change: PoolBalanceChange): F[Unit] =
          ensureFileExists(logFilePath) *>
            logger.debug(s"Writing balance change for operation: ${change.operation}") *>
            Async[F].delay {
              val logEntry = formatLogEntry(change)
              Using(new PrintWriter(new FileWriter(logFilePath, true))) { writer =>
                writer.println(logEntry)
                writer.flush()
              } match {
                case Success(_)  => ()
                case Failure(ex) => throw ex
              }
            }.handleErrorWith { ex =>
              logger.error(ex)(s"Failed to write log entry for operation: ${change.operation}")
            } *> logger.debug(s"Successfully wrote balance change for operation: ${change.operation}")

        def logPoolOperation(
          operation: String,
          beforePool: LiquidityPool,
          afterPool: LiquidityPool,
          epochProgress: Option[EpochProgress],
          updateHash: Option[Hash],
          address: Option[Address],
          additionalInfo: Map[String, String]
        ): F[Unit] = {
          val change = createBalanceChangeLog(
            operation,
            beforePool,
            afterPool,
            epochProgress,
            updateHash,
            address,
            additionalInfo
          )
          logBalanceChange(change)
        }

        private def createBalanceChangeLog(
          operation: String,
          beforePool: LiquidityPool,
          afterPool: LiquidityPool,
          epochProgress: Option[EpochProgress],
          updateHash: Option[Hash],
          address: Option[Address],
          additionalInfo: Map[String, String]
        ): PoolBalanceChange = {
          val timestamp = DateTimeFormatter
            .ofPattern("yyyy-MM-dd HH:mm:ss.SSS")
            .withZone(ZoneOffset.UTC)
            .format(Instant.now())

          PoolBalanceChange(
            operation = operation,
            timestamp = timestamp,
            epochProgress = epochProgress,
            updateHash = updateHash,
            beforeTokenA = (beforePool.tokenA.identifier, beforePool.tokenA.amount.value),
            beforeTokenB = (beforePool.tokenB.identifier, beforePool.tokenB.amount.value),
            afterTokenA = (afterPool.tokenA.identifier, afterPool.tokenA.amount.value),
            afterTokenB = (afterPool.tokenB.identifier, afterPool.tokenB.amount.value),
            tokenAChange = afterPool.tokenA.amount.value - beforePool.tokenA.amount.value,
            tokenBChange = afterPool.tokenB.amount.value - beforePool.tokenB.amount.value,
            beforeK = beforePool.k,
            afterK = afterPool.k,
            address = address,
            additionalInfo = additionalInfo
          )
        }

        private def formatLogEntry(change: PoolBalanceChange): String = {
          val basicInfo = s"[${change.timestamp}] POOL_BALANCE_CHANGE " +
            s"operation=${change.operation} " +
            s"epoch=${change.epochProgress.map(_.value).getOrElse("N/A")} " +
            s"hash=${change.updateHash.map(_.value).getOrElse("N/A")}"

          val balanceInfo =
            s"tokenA=[${change.beforeTokenA._1.getOrElse("None")}:${change.beforeTokenA._2}->${change.afterTokenA._2}(${formatChange(change.tokenAChange)})] " +
              s"tokenB=[${change.beforeTokenB._1
                  .getOrElse("None")}:${change.beforeTokenB._2}->${change.afterTokenB._2}(${formatChange(change.tokenBChange)})] " +
              s"k=[${change.beforeK}->${change.afterK}]"

          val addressInfo = change.address.map(addr => s" address=${addr.value}").getOrElse("")
          val additionalInfo = if (change.additionalInfo.nonEmpty) {
            " " + change.additionalInfo.map { case (k, v) => s"$k=$v" }.mkString(" ")
          } else ""

          s"$basicInfo $balanceInfo$addressInfo$additionalInfo"
        }

        private def formatChange(change: Long): String =
          if (change >= 0) s"+$change" else change.toString
      }
    } yield poolLogger
  }
}
