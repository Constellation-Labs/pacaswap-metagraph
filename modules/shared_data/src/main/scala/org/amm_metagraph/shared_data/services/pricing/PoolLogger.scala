package org.amm_metagraph.shared_data.services.pricing

import java.io.{FileWriter, PrintWriter}
import java.nio.file.{Files, Paths}
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneOffset}

import cats.effect.Async
import cats.effect.std.Queue
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
  // Bounded so a slow/full disk can never apply backpressure onto the consensus hot path; excess entries are dropped
  // (this is a diagnostic log, not state).
  private val QueueCapacity = 10000

  def make[F[_]: Async](logFilePath: String): F[PoolLogger[F]] = {
    val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

    def ensureFileExists(filePath: String): F[Unit] =
      Async[F].blocking {
        val path = Paths.get(filePath)
        val parentDir = path.getParent
        if (parentDir != null && !Files.exists(parentDir)) {
          Files.createDirectories(parentDir)
        }
        if (!Files.exists(path)) {
          Files.createFile(path)
        }
      }.void

    def formatChange(change: Long): String =
      if (change >= 0) s"+$change" else change.toString

    def formatLogEntry(change: PoolBalanceChange): String = {
      // Stamp the write time here, on the background writer fiber (not on the consensus hot path).
      val timestamp = DateTimeFormatter
        .ofPattern("yyyy-MM-dd HH:mm:ss.SSS")
        .withZone(ZoneOffset.UTC)
        .format(Instant.now())
      val basicInfo = s"[$timestamp] POOL_BALANCE_CHANGE " +
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

    // Runs on a background fiber, OFF the combine/consensus path: the only place blocking disk I/O and the wall-clock
    // timestamp happen (D5-01/D5-02/D6-05/D1-05). Nothing here can reach AmmCalculatedState.
    def writeEntry(change: PoolBalanceChange): F[Unit] =
      ensureFileExists(logFilePath) *>
        Async[F].blocking {
          val logEntry = formatLogEntry(change)
          Using(new PrintWriter(new FileWriter(logFilePath, true))) { writer =>
            writer.println(logEntry)
            writer.flush()
          } match {
            case Success(_)  => ()
            case Failure(ex) => throw ex
          }
        }.handleErrorWith { (ex: Throwable) =>
          logger.error(ex)(s"Failed to write log entry for operation: ${change.operation}")
        }

    // Drain loop for the background writer fiber. Explicitly typed F[Unit] and stack-safe via flatMap trampolining;
    // a write failure is logged and the loop continues, so the logger can never crash or block consensus.
    def drain(queue: Queue[F, PoolBalanceChange]): F[Unit] =
      queue.take.flatMap(writeEntry).attempt.flatMap {
        case Left(ex) => logger.error(ex)("PoolLogger writer error") >> drain(queue)
        case Right(_) => drain(queue)
      }

    for {
      _ <- logger.info(s"Initializing PoolLogger with file: $logFilePath")
      _ <- logger.info(s"Absolute path: ${Paths.get(logFilePath).toAbsolutePath}")
      queue <- Queue.bounded[F, PoolBalanceChange](QueueCapacity)
      // Background drainer, started fire-and-forget for the app lifetime (off the consensus path).
      _ <- Async[F].start(drain(queue))
      poolLogger = new PoolLogger[F] {

        // Hot-path call: non-blocking enqueue only. Never blocks combine on disk; drops the entry if the buffer is full.
        def logBalanceChange(change: PoolBalanceChange): F[Unit] =
          queue.tryOffer(change).flatMap {
            case true  => Async[F].unit
            case false => logger.debug(s"PoolLogger queue full, dropping entry for operation: ${change.operation}")
          }

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
        ): PoolBalanceChange =
          // Timestamp is intentionally left empty here (hot path): the wall clock is read on the background writer
          // (formatLogEntry) so no Instant.now() executes on the consensus path (D5-02/D6-05).
          PoolBalanceChange(
            operation = operation,
            timestamp = "",
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
    } yield poolLogger
  }
}
