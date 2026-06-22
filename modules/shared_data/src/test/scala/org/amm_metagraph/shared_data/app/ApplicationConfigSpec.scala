package org.amm_metagraph.shared_data.app

import io.constellationnetwork.schema.epoch.EpochProgress

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.NonNegLong
import weaver.SimpleIOSuite

/** Smoke test: the production application.conf must parse, and the activation epochs must be set to the coordinated
  * rollout value (a malformed HOCON or an accidental change would otherwise only surface at node boot).
  */
object ApplicationConfigSpec extends SimpleIOSuite {

  // Coordinated mainnet rollout epoch (current 486246 + ~5h at 43s/epoch). Update together with application.conf.
  private val rolloutEpoch: EpochProgress = EpochProgress(NonNegLong.unsafeFrom(486666L))

  pureTest("application.conf loads and activation epochs are set to the coordinated rollout epoch") {
    val cfg = ApplicationConfigOps.readDefaultPure
    expect.all(
      cfg.activationEpochs.stakingShareMintFix == rolloutEpoch,
      cfg.activationEpochs.globalSyncDataIntegrity == rolloutEpoch,
      cfg.activationEpochs.rewardEpochCatchUp == rolloutEpoch
    )
  }
}
