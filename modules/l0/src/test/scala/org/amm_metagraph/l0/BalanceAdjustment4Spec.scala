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

  test("balance-adjustments-4.json deducts exactly 2^62 from each minted wallet") {
    IO.fromTry(loadBalanceAdjustments("balance-adjustments-4.json")).map { adjustments =>
      expect.all(
        adjustments.size == 4,
        adjustments.map(_.address).toSet == mintedWallets,
        adjustments.forall(_.reason == FeeTransactionBugDeduction),
        adjustments.forall(_.increase.isEmpty),
        // Exact match matters: tessellation's validateRequiredAdjustments compares Amounts
        // exactly, so a float round-trip in the JSON would silently fail the pairing.
        adjustments.forall(_.deduct.exists(_.value.value == mintedAmount)),
        adjustments.forall(_.reference.nonEmpty)
      )
    }
  }
}
