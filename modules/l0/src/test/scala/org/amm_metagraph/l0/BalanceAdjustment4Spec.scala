package org.amm_metagraph.l0

import cats.effect.IO

import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact.FeeTransactionBugDeduction

import eu.timepit.refined.auto._
import org.amm_metagraph.l0.BalanceAdjustmentLoader.loadBalanceAdjustments
import weaver.SimpleIOSuite

object BalanceAdjustment4Spec extends SimpleIOSuite {

  private val mintedAmount = 4611686018427387904L

  private val mintedWallets = Set(
    Address("DAG8uqhyGtFABWSS5KeVB2ia1R4vXop5AeijXeoU"),
    Address("DAG4w5mUqNNxQNS4hgdpx3E8FGgiu2UCRsJxHwhX"),
    Address("DAG7ZjENTP4T36PPSp3skJdTHtQbcuLfpEaAFWdn"),
    Address("DAG1kEmLAgnCVBURHrL4AMsfn9TZdk4QCYQ8tUu3")
  )

  private val pacaswap = Address("DAG7X5idd4aLfp4XC6WQdG1eDfR3LGPVEwtUUB2W")

  private val poolSurplus = 355236233753468500L

  private val thirdPartyTotal = 139406347045268L

  test("balance-adjustments-4.json covers the mint, the pool and every buyer exactly once") {
    IO.fromTry(loadBalanceAdjustments("balance-adjustments-4.json")).map { adjustments =>
      val minted = adjustments.filter(a => mintedWallets.contains(a.address))
      val pool = adjustments.filter(_.address == pacaswap)
      val thirdParty = adjustments.filterNot(a => mintedWallets.contains(a.address) || a.address == pacaswap)

      expect.all(
        adjustments.size == 17,
        adjustments.groupBy(_.address).forall { case (_, entries) => entries.size == 1 },
        adjustments.forall(_.reason == FeeTransactionBugDeduction),
        adjustments.forall(_.increase.isEmpty),
        adjustments.forall(_.deduct.exists(_.value.value > 0L)),
        adjustments.forall(_.reference.nonEmpty),
        minted.size == 4,
        minted.map(_.address).toSet == mintedWallets,
        minted.forall(_.deduct.exists(_.value.value == mintedAmount)),
        minted.forall(_.reference.size == 2),
        pool.size == 1,
        pool.forall(_.deduct.exists(_.value.value == poolSurplus)),
        thirdParty.size == 12,
        thirdParty.flatMap(_.deduct.map(_.value.value)).sum == thirdPartyTotal
      )
    }
  }
}
