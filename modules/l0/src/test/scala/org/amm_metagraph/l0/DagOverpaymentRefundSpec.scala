package org.amm_metagraph.l0

import cats.effect.IO

import scala.collection.immutable.SortedSet

import io.constellationnetwork.schema.artifact.{SharedArtifact, SpendAction}

import io.circe.parser.parse
import org.amm_metagraph.l0.SurplusSweepLoader.loadSweep
import weaver.SimpleIOSuite

/** The DAG leg of the PROT-1695 recovery was funded twice, by two different wallets, 47 minutes apart. The book counts it once, so the
  * custody wallet ends over-backed by exactly one payment. This refunds one of them.
  *
  * The refund leaves an address nobody holds a key for, so it is irreversible in both directions. These assertions exist so a mistake stops
  * the node rather than sending DAG somewhere it can never be retrieved from.
  */
object DagOverpaymentRefundSpec extends SimpleIOSuite {

  private val AMM = "DAG7X5idd4aLfp4XC6WQdG1eDfR3LGPVEwtUUB2W"
  private val DESTINATION = "DAG0o6WSyvc7XfzujwJB1e25mfyzgXoLYDD6wqnk"

  /** Measured on chain, not copied from the resource under test: the DAG the custody address held once both payments had landed, minus what
    * updated-pools-14.json books across the four DAG legs. Read at global ordinal 6828963.
    */
  private val WalletDagAfterBothPayments = 2160132699225713L
  private val BookedDagAcrossAllPools = 2159365047144895L
  private val Overpayment = WalletDagAfterBothPayments - BookedDagAcrossAllPools

  test("the shipped resource returns exactly the measured overpayment, in DAG, to the first sender") {
    IO(loadSweep("dag-overpayment-refund.json")).map { r =>
      val legs = r.get.spendTransactions.toList
      val leg = legs.head
      expect.all(
        r.isSuccess,
        legs.size == 1,
        // DAG carries no currency id. A Some here would be a metagraph token and would move the
        // wrong ledger entirely.
        leg.currencyId.isEmpty,
        leg.source.value.value == AMM,
        leg.destination.value.value == DESTINATION,
        leg.amount.value.value == Overpayment,
        // Independently: it is the amount of the duplicated transfer as recorded on chain.
        leg.amount.value.value == 767652080818L
      )
    }
  }

  test("the refund closes the gap exactly - no dust left, nothing over-returned") {
    IO(loadSweep("dag-overpayment-refund.json")).map { r =>
      val refunded = r.get.spendTransactions.head.amount.value.value
      expect(WalletDagAfterBothPayments - refunded == BookedDagAcrossAllPools)
    }
  }

  test("the destination is neither the custody address nor a deducted address") {
    val adjustments = parse(
      scala.io.Source.fromResource("balance-adjustments-4.json").mkString
    ).toOption.get
    val deducted = adjustments.asArray.get.flatMap(_.hcursor.get[String]("address").toOption).toSet
    IO(loadSweep("dag-overpayment-refund.json")).map { r =>
      val dst = r.get.spendTransactions.head.destination.value.value
      expect.all(dst != AMM, !deducted.contains(dst))
    }
  }

  test("Main emits it at 731650, and at no neighbouring ordinal") {
    def spendActionsAt(o: Long): List[SpendAction] =
      Main
        .customArtifactsAt(o)
        .getOrElse(SortedSet.empty[SharedArtifact])
        .toList
        .collect { case sa: SpendAction => sa }

    IO {
      val at = spendActionsAt(731650L)
      expect.all(
        at.size == 1,
        at.head.spendTransactions.head.destination.value.value == DESTINATION,
        at.head.spendTransactions.head.currencyId.isEmpty,
        // 731649 is the Upsider sweep: a SpendAction too, but a different ledger and destination.
        spendActionsAt(731649L).forall(_.spendTransactions.head.currencyId.isDefined),
        spendActionsAt(731651L).isEmpty,
        spendActionsAt(731647L).collect { case sa: SpendAction => sa }.isEmpty
      )
    }
  }

  test("a currencyId that is present but blank is refused rather than read as DAG") {
    val blank =
      """{"currencyId":"  ","amount":1,"source":"DAG7X5idd4aLfp4XC6WQdG1eDfR3LGPVEwtUUB2W",
        |"destination":"DAG0o6WSyvc7XfzujwJB1e25mfyzgXoLYDD6wqnk","reason":"x"}""".stripMargin
    IO(writeTemp(blank)).flatMap(p => IO(loadSweep(p))).map(r => expect(r.isFailure))
  }

  private def writeTemp(contents: String): String = {
    val dir = java.nio.file.Files.createTempDirectory("refund-spec")
    val name = s"refund-${java.lang.Long.toHexString(contents.hashCode.toLong & 0xffffffffL)}.json"
    java.nio.file.Files.write(dir.resolve(name), contents.getBytes("UTF-8"))
    val cl = new java.net.URLClassLoader(Array(dir.toUri.toURL), getClass.getClassLoader)
    Thread.currentThread().setContextClassLoader(cl)
    name
  }
}
