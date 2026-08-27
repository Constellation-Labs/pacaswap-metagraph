package org.amm_metagraph.l0

import cats.effect.IO

import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact.FeeTransactionBugDeduction

import eu.timepit.refined.auto._
import org.amm_metagraph.l0.BalanceAdjustmentLoader.loadBalanceAdjustments
import weaver.SimpleIOSuite

object BalanceAdjustment4Spec extends SimpleIOSuite {

  private val mintedAmount = 4611686018427387904L // 2^62, the per-wallet FeeTransaction amount

  private val mintedWallets = Set(
    Address("DAG8uqhyGtFABWSS5KeVB2ia1R4vXop5AeijXeoU"),
    Address("DAG4w5mUqNNxQNS4hgdpx3E8FGgiu2UCRsJxHwhX"),
    Address("DAG7ZjENTP4T36PPSp3skJdTHtQbcuLfpEaAFWdn"),
    Address("DAG1kEmLAgnCVBURHrL4AMsfn9TZdk4QCYQ8tUu3")
  )

  private val pacaswap = Address("DAG7X5idd4aLfp4XC6WQdG1eDfR3LGPVEwtUUB2W")

  /** PACA the swaps pushed into the pool address, taken down to the counterfactual reserve: where the pool would sit had the mint never
    * happened but the ten legitimate purchases had.
    *
    * This is the observed address balance, 360351876219858115, minus the exact reserve updated-pools-13.json writes, 5039524334506729. It
    * has to be the exact reserve and not the figure rounded to the cent: the deduction acts on the address balance while the reserve is
    * written separately, so any gap between them strands PACA on the address that no pool reserve accounts for.
    */
  private val poolSurplus = 355312351884858115L

  /** Phantom PACA removed from the addresses that bought it out of the pool, net of what each of them is entitled to keep at the pre-attack
    * price, and after taking off what they moved into a token lock. Locked phantom is out of reach of a BalanceAdjustment and is not part
    * of this file.
    */
  private val thirdPartyTotal = 139333921117902392L

  /** The pool address's PACA balance as captured from mainnet, and the reserve the remediation writes. Kept side by side so the 1:1 the
    * deduction has to satisfy is visible here.
    */
  private val observedPoolBalance = 360351876219858115L
  private val writtenPacaReserve = 5039524335000000L

  test("balance-adjustments-4.json covers the mint, the pool and every buyer exactly once") {
    IO.fromTry(loadBalanceAdjustments("balance-adjustments-4.json")).map { adjustments =>
      val minted = adjustments.filter(a => mintedWallets.contains(a.address))
      val pool = adjustments.filter(_.address == pacaswap)
      val thirdParty = adjustments.filterNot(a => mintedWallets.contains(a.address) || a.address == pacaswap)

      expect.all(
        adjustments.size == 17,
        // Main folds these into a SortedSet, so two entries for one address would both survive
        // and deduct twice.
        adjustments.groupBy(_.address).forall { case (_, entries) => entries.size == 1 },
        adjustments.forall(_.reason == FeeTransactionBugDeduction),
        adjustments.forall(_.increase.isEmpty),
        adjustments.forall(_.deduct.exists(_.value.value > 0L)),
        adjustments.forall(_.reference.nonEmpty),
        minted.size == 4,
        minted.map(_.address).toSet == mintedWallets,
        // Exact match matters: tessellation's validateRequiredAdjustments compares Amounts
        // exactly, so a float round-trip in the JSON would silently fail the pairing.
        minted.forall(_.deduct.exists(_.value.value == mintedAmount)),
        // Each mint wallet's entry names its own fee transaction alongside the mint snapshot.
        minted.forall(_.reference.size == 2),
        pool.size == 1,
        pool.forall(_.deduct.exists(_.value.value == poolSurplus)),
        // 1:1 with the reserve: what the deduction leaves on the address is exactly what
        // updated-pools-13.json writes as the pool's PACA reserve, to the unit.
        pool.forall(_.deduct.exists(a => observedPoolBalance - a.value.value == writtenPacaReserve)),
        thirdParty.size == 12,
        thirdParty.flatMap(_.deduct.map(_.value.value)).sum == thirdPartyTotal
      )
    }
  }

  pureTest("Main emits the incident artifacts only at ordinal 731647") {
    val atFix = Main.customArtifactsAt(731647L)

    expect.all(
      atFix.exists(_.size == 17),
      atFix.exists(_.forall {
        case adjustment: io.constellationnetwork.schema.artifact.BalanceAdjustment =>
          adjustment.reason == FeeTransactionBugDeduction
        case _ => false
      }),
      Main.customArtifactsAt(731646L).isEmpty,
      Main.customArtifactsAt(731648L).isEmpty
    )
  }

  pureTest("Long.MinValue cannot be normalized into a positive deduction") {
    val json =
      s"""[{"address":"${pacaswap.value}","reason":"FeeTransactionBugDeduction","reference":[],"deduct":${Long.MinValue}}]"""

    expect(io.circe.parser.decode[List[io.constellationnetwork.schema.artifact.BalanceAdjustment]](json).isLeft)
  }
}
