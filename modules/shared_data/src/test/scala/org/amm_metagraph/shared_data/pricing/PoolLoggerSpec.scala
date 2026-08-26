package org.amm_metagraph.shared_data.pricing

import java.nio.file.{Files, Paths}

import cats.effect.IO
import cats.syntax.all._

import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

import io.constellationnetwork.schema.epoch.EpochProgress

import eu.timepit.refined.auto._
import org.amm_metagraph.shared_data.services.pricing.PoolLogger
import org.amm_metagraph.shared_data.services.pricing.models.PoolBalanceChange
import weaver.SimpleIOSuite

/** PoolLogger is called from inside `combine`, which runs inside consensus. It must never make the caller wait on a disk.
  */
object PoolLoggerSpec extends SimpleIOSuite {

  private def change(n: Int) = PoolBalanceChange(
    operation = s"OP$n",
    timestamp = "2026-08-26 00:00:00.000",
    epochProgress = EpochProgress.MinValue.some,
    updateHash = None,
    beforeTokenA = (None, 1L),
    beforeTokenB = (None, 2L),
    afterTokenA = (None, 3L),
    afterTokenB = (None, 4L),
    tokenAChange = 2L,
    tokenBChange = 2L,
    beforeK = BigInt(2),
    afterK = BigInt(12),
    address = None,
    additionalInfo = Map("seq" -> n.toString)
  )

  private def withTempLog[A](f: String => IO[A]): IO[A] = {
    val path = Files.createTempFile("pool-logger-spec", ".log")
    f(path.toString).guarantee(IO.blocking(Files.deleteIfExists(path)).void)
  }

  test("the caller never waits on the disk - enqueueing is not the write") {
    withTempLog { path =>
      for {
        pl <- PoolLogger.make[IO](path)
        // If this were still synchronous open+write+flush+close per line, 500 lines would take
        // far longer than this budget on any real filesystem.
        elapsed <- IO.monotonic.flatMap { t0 =>
          (1 to 500).toList.traverse_(i => pl.logBalanceChange(change(i))) >>
            IO.monotonic.map(_ - t0)
        }
      } yield expect(elapsed < 500.millis)
    }
  }

  test("lines reach the file, in order") {
    withTempLog { path =>
      for {
        pl <- PoolLogger.make[IO](path)
        _ <- (1 to 50).toList.traverse_(i => pl.logBalanceChange(change(i)))
        _ <- IO.sleep(1.second) // let the drainer catch up
        lines <- IO.blocking(Files.readAllLines(Paths.get(path)).asScala.toList)
        seqs = lines.flatMap(l => "seq=(\\d+)".r.findFirstMatchIn(l).map(_.group(1).toInt))
      } yield expect.all(seqs.nonEmpty, seqs == seqs.sorted, seqs.take(50) == (1 to seqs.size).toList.take(50))
    }
  }

  test("a burst far beyond the queue is dropped, never blocked") {
    withTempLog { path =>
      for {
        pl <- PoolLogger.make[IO](path)
        // 20k against a 4096 queue. Dropping is the designed behaviour: losing diagnostic
        // lines is always preferable to parking the consensus path on a slow disk.
        elapsed <- IO.monotonic.flatMap { t0 =>
          (1 to 20000).toList.traverse_(i => pl.logBalanceChange(change(i))) >>
            IO.monotonic.map(_ - t0)
        }
      } yield expect(elapsed < 5.seconds)
    }
  }

  pureTest("the formatted line keeps the shape operators grep for") {
    val line = PoolLogger.formatLogEntry(change(7))
    expect.all(
      line.contains("POOL_BALANCE_CHANGE"),
      line.contains("operation=OP7"),
      line.contains("k=[2->12]"),
      line.contains("seq=7")
    )
  }
}
