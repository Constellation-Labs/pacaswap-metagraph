package org.amm_metagraph.shared_data

import scala.collection.immutable.SortedMap

import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.CurrencyId
import io.constellationnetwork.schema.tokenLock._
import io.constellationnetwork.security.hash.Hash

import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.{NonNegLong, PosLong}
import org.amm_metagraph.shared_data.types.Governance.VotingPower
import org.amm_metagraph.shared_data.types.States.AmmCalculatedState

/** Removes the seven incident-funded TokenLocks from PacaSwap governance without making an ownership claim about their six current holders.
  *
  * The manifest was verified against the active locks in calculated state at currency ordinal 736006. `source` plus the parent reference is
  * already a unique TokenLock identity; the complete values are retained here so an unintended near-match cannot be removed. Together these
  * locks contain 44,019,596,270,815,378 raw SWAP.
  */
object IncidentTokenLockRemediation {

  private val swapCurrencyId = CurrencyId(Address("DAG7X5idd4aLfp4XC6WQdG1eDfR3LGPVEwtUUB2W"))

  private def tokenLock(
    source: Address,
    amount: Long,
    parentOrdinal: Long,
    parentHash: String,
    unlockEpoch: Long
  ): TokenLock =
    TokenLock(
      source = source,
      amount = TokenLockAmount(PosLong.unsafeFrom(amount)),
      fee = TokenLockFee(NonNegLong.MinValue),
      parent = TokenLockReference(TokenLockOrdinal(NonNegLong.unsafeFrom(parentOrdinal)), Hash(parentHash)),
      currencyId = Some(swapCurrencyId),
      unlockEpoch = Some(EpochProgress(NonNegLong.unsafeFrom(unlockEpoch)))
    )

  val incidentTokenLocks: Set[TokenLock] = Set(
    tokenLock(
      Address("DAG6zZakMJrrf25FSvPZAi8QA9wVDdmvFkPvTbKu"),
      20000000000000000L,
      4L,
      "aeaa2b44120991f7f98a71ce0ed2b46f1e96c3b86bec9fdb4ca77f8261b19d20",
      3891938L
    ),
    tokenLock(
      Address("DAG7uHRz6stwzsEnSHB2w1VxVHsCq7PDuDhTbjNP"),
      15000000000000000L,
      2L,
      "712f5009ddc8757599a663c2e5d68902ba0180213bcca8cdeb2a223fd2140f48",
      3114355L
    ),
    tokenLock(
      Address("DAG5434oVLFRRTqVSsTv4Y1qvyoMBkb4Tey21YuZ"),
      6000000000000000L,
      3L,
      "d25cbbfc461d6443e05af6f1e55355fcd43b558cb89560ee058395b5063f672a",
      3891958L
    ),
    tokenLock(
      Address("DAG8Eyr6SGvLorNU4rQspeUXZLZi3wt84CwbV1Ep"),
      138785715000000L,
      2L,
      "ecd25ec8358bb81e183eacb96538e0a1c7de47c8101a7de36dbf91e849f72b43",
      3891877L
    ),
    tokenLock(
      Address("DAG8Eyr6SGvLorNU4rQspeUXZLZi3wt84CwbV1Ep"),
      900000000000000L,
      3L,
      "980e08eedac2bed2acc7d60cd74ce1e1ad0ba117baea02bfa7a4ed8b588a11b9",
      3891879L
    ),
    tokenLock(
      Address("DAG4fVZch1qTY2ccA5eHkxe2RMTFsnNDU6Zu6mUU"),
      1000000000000000L,
      0L,
      "9ac87f6817e147048749548398308bd5775fb0650d29c4a5633449849f057579",
      3891947L
    ),
    tokenLock(
      Address("DAG4kfRPpcPSh4cMn8ZgdMuTEfdu3yz4veZFrv3L"),
      980810555815378L,
      8L,
      "e33a7f4e09fb5f037d57cad23f604352a07de632f7b3f56c682098386f169693",
      3891925L
    )
  )

  def isIncidentTokenLock(tokenLock: TokenLock): Boolean = incidentTokenLocks.contains(tokenLock)

  /** Filters the incident locks after every active-lock import. A holder remains in the map when it has any unrelated lock, and its total is
    * recomputed from the retained `VotingPowerInfo` entries. Below activation this is the identity to preserve historical replay.
    */
  def removeFromVotingPowers(
    votingPowers: SortedMap[Address, VotingPower],
    ordinal: SnapshotOrdinal
  ): SortedMap[Address, VotingPower] =
    if (!ProtocolActivation.incidentTokenLockRemediationActive(ordinal)) votingPowers
    else
      SortedMap.from(
        votingPowers.flatMap {
          case (address, votingPower) =>
            val retainedInfo = votingPower.info.filterNot(info => isIncidentTokenLock(info.tokenLock))
            if (retainedInfo.size == votingPower.info.size) Some(address -> votingPower)
            else if (retainedInfo.isEmpty) None
            else {
              val retainedTotal = retainedInfo.iterator.map(_.votingPower.value).sum
              Some(address -> VotingPower(NonNegLong.unsafeFrom(retainedTotal), retainedInfo))
            }
        }
      )

  def removeFromCalculatedState(state: AmmCalculatedState, ordinal: SnapshotOrdinal): AmmCalculatedState =
    state.copy(votingPowers = removeFromVotingPowers(state.votingPowers, ordinal))
}
