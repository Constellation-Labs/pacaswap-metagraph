package org.amm_metagraph.l0

import cats.effect.IO

import io.constellationnetwork.schema.artifact.SpendAction

import org.amm_metagraph.l0.SurplusSweepLoader.{DestinationPlaceholder, loadSweep}
import weaver.SimpleIOSuite

/** The surplus sweep moves tokens out of an address nobody holds a key for.
  *
  * That makes it irreversible in both directions: we cannot recall it, and we cannot recover it from a wrong destination. Every one of
  * these assertions exists so that a mistake stops the node instead of sending tokens somewhere they can never be retrieved from.
  */
object SurplusSweepSpec extends SimpleIOSuite {

  private val AMM = "DAG7X5idd4aLfp4XC6WQdG1eDfR3LGPVEwtUUB2W"
  private val UP = "DAG7Ghth1WhWK83SB3MtXnnHYZbCsmiRTwJrgaW1"
  private val SURPLUS = 7170622458112005L
  private val DESTINATION = "DAG471pakWWz3ZAFvX5uhA7uyufSkv7E41Lh77Ge"

  test("the shipped resource loads, and sends the surplus where it was told to") {
    // The destination was validated before it was set: 40 characters, DAG prefix, the checksum
    // digit matches the tessellation rule, it is base58, it is not the custody address, it is
    // not on the deduction list, and it already holds UP on chain.
    IO(loadSweep("up-surplus-sweep.json")).map { r =>
      val legs = r.get.spendTransactions.toList
      expect.all(
        r.isSuccess,
        legs.size == 1,
        legs.head.source.value.value == AMM,
        legs.head.destination.value.value == DESTINATION,
        legs.head.amount.value.value == SURPLUS,
        legs.head.currencyId.exists(_.value.value.value == UP)
      )
    }
  }

  test("a placeholder destination is still refused, so the guard cannot rot") {
    // The shipped resource no longer carries the placeholder, but the check that rejects it must
    // keep working, or a future resource could ship unset and reach production.
    val stillPlaceholder =
      s"""{"currencyId":"$UP","amount":$SURPLUS,"source":"$AMM",
          |"destination":"$DestinationPlaceholder","reason":"x"}""".stripMargin
    IO(writeTemp(stillPlaceholder)).flatMap(pth => IO(loadSweep(pth))).map { r =>
      expect.all(r.isFailure, r.failed.get.getMessage.contains("placeholder"))
    }
  }

  test("the shipped resource carries the measured surplus, source and currency") {
    // Read the raw fields directly, so the numbers are pinned independently of how the loader
    // assembles them into a SpendAction.
    import io.circe.parser.decode
    import org.amm_metagraph.l0.SurplusSweepLoader.RawSurplusSweep
    val json = scala.io.Source.fromResource("up-surplus-sweep.json").mkString
    IO.fromEither(decode[RawSurplusSweep](json)).map { raw =>
      expect.all(
        raw.amount == SURPLUS, // 71,706,224.58112005 UP, the measured surplus
        raw.source == AMM, // out of the custody address
        raw.currencyId == UP, // on The Upsider AI's ledger
        raw.destination == DESTINATION, // the address operations supplied
        raw.destination != AMM // never back into custody
      )
    }
  }

  test("a filled-in destination produces exactly one leg, out of custody") {
    val filled =
      s"""{"currencyId":"$UP","amount":$SURPLUS,"source":"$AMM",
          |"destination":"DAG4y6AFspmw5pzzsTjGeDazjH2EqVvtiYJdzQhH","reason":"UpsiderSurplusSweep"}""".stripMargin
    IO(writeTemp(filled)).flatMap(p => IO(loadSweep(p))).map { r =>
      val action: SpendAction = r.get
      val legs = action.spendTransactions.toList
      expect.all(
        legs.size == 1,
        legs.head.source.value.value == AMM,
        legs.head.destination.value.value == "DAG4y6AFspmw5pzzsTjGeDazjH2EqVvtiYJdzQhH",
        legs.head.amount.value.value == SURPLUS,
        legs.head.currencyId.exists(_.value.value.value == UP),
        legs.head.allowSpendRef.isEmpty // metagraph-initiated, no user allow spend
      )
    }
  }

  test("a destination equal to the source is refused") {
    val same = s"""{"currencyId":"$UP","amount":$SURPLUS,"source":"$AMM","destination":"$AMM","reason":"x"}"""
    IO(writeTemp(same)).flatMap(p => IO(loadSweep(p))).map { r =>
      expect.all(r.isFailure, r.failed.get.getMessage.contains("same address"))
    }
  }

  test("a malformed destination is refused rather than coerced") {
    val bad = s"""{"currencyId":"$UP","amount":$SURPLUS,"source":"$AMM","destination":"not-an-address","reason":"x"}"""
    IO(writeTemp(bad)).flatMap(p => IO(loadSweep(p))).map { r =>
      expect.all(r.isFailure, r.failed.get.getMessage.contains("not a valid address"))
    }
  }

  test("a non-positive amount is refused") {
    val zero = s"""{"currencyId":"$UP","amount":0,"source":"$AMM","destination":"DAG4y6AFspmw5pzzsTjGeDazjH2EqVvtiYJdzQhH","reason":"x"}"""
    IO(writeTemp(zero)).flatMap(p => IO(loadSweep(p))).map { r =>
      expect.all(r.isFailure, r.failed.get.getMessage.contains("must be positive"))
    }
  }

  test("a missing resource is refused") {
    IO(loadSweep("no-such-sweep.json")).map(r => expect(r.isFailure))
  }

  test("the sweep is emitted at 731649, and at no neighbouring ordinal") {
    // 731647 is the remediation, 731648 the normalization. The sweep is its own snapshot so a
    // failure in one cannot take down the others.
    IO {
      val at = (o: Long) => scala.util.Try(Main.customArtifactsAt(o))
      expect.all(
        at(731647L).isSuccess, // deductions, unaffected
        at(731648L).toOption.flatten.isEmpty, // normalization emits no artifacts
        at(731649L).toOption.flatten.exists(_.size == 1), // the sweep, exactly one artifact
        at(731650L).toOption.flatten.isEmpty
      )
    }
  }

  private def writeTemp(content: String): String = {
    val dir = java.nio.file.Files.createTempDirectory("sweep-spec")
    val f = dir.resolve("sweep.json")
    java.nio.file.Files.write(f, content.getBytes("UTF-8"))
    val cl = new java.net.URLClassLoader(Array(dir.toUri.toURL), getClass.getClassLoader)
    Thread.currentThread().setContextClassLoader(cl)
    "sweep.json"
  }
}
