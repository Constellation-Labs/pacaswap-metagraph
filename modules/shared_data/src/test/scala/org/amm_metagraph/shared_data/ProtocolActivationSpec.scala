package org.amm_metagraph.shared_data

import io.constellationnetwork.schema.SnapshotOrdinal

import eu.timepit.refined.types.all.NonNegLong
import weaver.SimpleIOSuite

object ProtocolActivationSpec extends SimpleIOSuite {

  private def ord(v: Long) = SnapshotOrdinal(NonNegLong.unsafeFrom(v))

  pureTest("the reserve-accounting fixes activate at 731647 and never before") {
    // 731646 is the last ordinal the metagraph produced before it was stopped. Everything at or
    // below it is already on chain and must replay under the original behaviour, or a node
    // reconstructing from genesis cannot match the recorded calculated-state proofs.
    expect.all(
      ProtocolActivation.reserveAccountingFixes.value.value == 731647L,
      !ProtocolActivation.reserveAccountingFixesActive(SnapshotOrdinal.MinValue),
      !ProtocolActivation.reserveAccountingFixesActive(ord(1L)),
      !ProtocolActivation.reserveAccountingFixesActive(ord(731645L)),
      !ProtocolActivation.reserveAccountingFixesActive(ord(731646L)),
      ProtocolActivation.reserveAccountingFixesActive(ord(731647L)),
      ProtocolActivation.reserveAccountingFixesActive(ord(731648L)),
      ProtocolActivation.reserveAccountingFixesActive(ord(999999L))
    )
  }
}
