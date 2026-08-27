package org.amm_metagraph.shared_data.services.pricing

import java.io.{BufferedWriter, FileWriter, PrintWriter}
import java.nio.file.{Files, Paths}
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneOffset}

import cats.effect.std.Queue
import cats.effect.{Async, Ref}
import cats.syntax.all._

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

/** Diagnostic log of every pool mutation.
  *
  * This is called from inside `combine`, which runs inside consensus. The previous implementation did, per pool mutation, on the calling
  * fiber: two filesystem syscalls to check the file exists, then open an unbuffered FileWriter, write one line, flush, and close. A slow or
  * full disk therefore applied backpressure straight to the consensus critical path, and a node that stalls there falls behind its peers.
  * Diagnostics must never be able to do that.
  *
  * It is now write-behind. The consensus path pays one non-blocking enqueue. A single background fiber drains the queue, batches, and
  * writes to one long-lived buffered handle on the blocking pool. The queue is bounded and DROPS on overflow rather than waiting - losing
  * diagnostic lines is always preferable to slowing the chain - and drops are counted and reported so the loss is never silent.
  *
  * Ordering is preserved: one producer path, one FIFO queue, one drainer.
  *
  * Nothing here touches state, so none of it can affect consensus or the calculated-state proof.
  */
object PoolLogger {

  /** Bounded so a stalled disk can never grow the heap without limit. At ~200 bytes a line this is well under a megabyte, and far more than
    * one snapshot's worth of mutations.
    */
  private val QueueCapacity: Int = 4096

  /** Lines drained per write before flushing, so a burst costs one flush rather than N. */
  private val BatchSize: Int = 256

  def make[F[_]: Async](logFilePath: String): F[PoolLogger[F]] = {
    val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

    def openWriter: F[PrintWriter] = Async[F].blocking {
      val path = Paths.get(logFilePath)
      Option(path.getParent).filterNot(Files.exists(_)).foreach(Files.createDirectories(_))
      // append, buffered, opened once and kept open for the process lifetime
      new PrintWriter(new BufferedWriter(new FileWriter(logFilePath, true)))
    }

    def drain(queue: Queue[F, PoolBalanceChange], dropped: Ref[F, Long], writer: PrintWriter): F[Unit] = {
      def writeBatch(batch: List[PoolBalanceChange]): F[Unit] =
        Async[F].blocking {
          batch.foreach(c => writer.println(formatLogEntry(c)))
          writer.flush()
        }
          .handleErrorWith(e => logger.error(e)("PoolLogger: failed to write a batch; continuing"))

      def reportDrops: F[Unit] =
        dropped.getAndSet(0L).flatMap { n =>
          logger
            .warn(s"PoolLogger: dropped $n diagnostic line(s) because the write queue was full")
            .whenA(n > 0L)
        }

      val step = for {
        head <- queue.take // semantically blocks this fiber only, never the caller
        rest <- queue.tryTakeN(Some(BatchSize - 1))
        _ <- writeBatch(head :: rest)
        _ <- reportDrops
      } yield ()

      step.foreverM.handleErrorWith(e => logger.error(e)("PoolLogger: drainer died; diagnostics stop here"))
    }

    for {
      _ <- logger.info(s"Initializing PoolLogger with file: $logFilePath")
      _ <- logger.info(s"Absolute path: ${Paths.get(logFilePath).toAbsolutePath}")
      queue <- Queue.bounded[F, PoolBalanceChange](QueueCapacity)
      dropped <- Ref.of[F, Long](0L)
      writer <- openWriter
      _ <- Async[F].start(drain(queue, dropped, writer))
      poolLogger = new PoolLogger[F] {

        def logBalanceChange(change: PoolBalanceChange): F[Unit] =
          // tryOffer, never offer: a full queue must drop the line, not park the combine.
          queue.tryOffer(change).flatMap {
            case true  => Async[F].unit
            case false => dropped.update(_ + 1L)
          }

        def logPoolOperation(
          operation: String,
          beforePool: LiquidityPool,
          afterPool: LiquidityPool,
          epochProgress: Option[EpochProgress],
          updateHash: Option[Hash],
          address: Option[Address],
          additionalInfo: Map[String, String]
        ): F[Unit] =
          logBalanceChange(
            createBalanceChangeLog(operation, beforePool, afterPool, epochProgress, updateHash, address, additionalInfo)
          )
      }
    } yield poolLogger
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

  private def formatChange(change: Long): String =
    if (change >= 0) s"+$change" else change.toString

  def formatLogEntry(change: PoolBalanceChange): String = {
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
}
