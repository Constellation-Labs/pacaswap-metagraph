package org.amm_metagraph.shared_data

import cats.effect.IO

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.CurrencyId
import io.constellationnetwork.schema.tokenLock._
import io.constellationnetwork.security.hash.Hash

import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.{NonNegLong, PosLong}
import org.amm_metagraph.shared_data.types.Governance.{VotingPower, VotingPowerInfo}
import org.amm_metagraph.shared_data.types.Rewards.{AddressAndRewardType, RewardInfo, RewardType}
import org.amm_metagraph.shared_data.types.States.{AmmCalculatedState, RewardsState}
import weaver.MutableIOSuite

object IncidentTokenLockRemediationSpec extends MutableIOSuite {

  type Res = Unit
  override def sharedResource: cats.effect.Resource[IO, Res] = cats.effect.Resource.pure[IO, Unit](())

  private def ordinal(value: Long): SnapshotOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(value))

  private val owner = Address("DAG6zZakMJrrf25FSvPZAi8QA9wVDdmvFkPvTbKu")
  private val incidentOnlyOwner = Address("DAG4fVZch1qTY2ccA5eHkxe2RMTFsnNDU6Zu6mUU")
  private val bystander = Address("DAG011jH7FMDvKpdb7wewrMWwYtkwq56nHquAHdi")
  private val swapCurrencyId = CurrencyId(Address("DAG7X5idd4aLfp4XC6WQdG1eDfR3LGPVEwtUUB2W"))

  private val ownerIncidentLock = IncidentTokenLockRemediation.incidentTokenLocks.find(_.source == owner).get
  private val incidentOnlyLock = IncidentTokenLockRemediation.incidentTokenLocks.find(_.source == incidentOnlyOwner).get
  private val legitimateLock = TokenLock(
    source = owner,
    amount = TokenLockAmount(PosLong.unsafeFrom(1000L)),
    fee = TokenLockFee(NonNegLong.MinValue),
    parent = TokenLockReference(TokenLockOrdinal(NonNegLong.unsafeFrom(99L)), Hash("legitimate-lock")),
    currencyId = Some(swapCurrencyId),
    unlockEpoch = Some(EpochProgress(NonNegLong.unsafeFrom(4000000L)))
  )

  private val incidentInfo = VotingPowerInfo(
    NonNegLong.unsafeFrom(140000000000000000L),
    ownerIncidentLock,
    EpochProgress(NonNegLong.unsafeFrom(2855078L))
  )
  private val legitimateInfo =
    VotingPowerInfo(NonNegLong.unsafeFrom(7000L), legitimateLock, EpochProgress(NonNegLong.unsafeFrom(2500000L)))
  private val incidentOnlyInfo = VotingPowerInfo(
    NonNegLong.unsafeFrom(7000000000000000L),
    incidentOnlyLock,
    EpochProgress(NonNegLong.unsafeFrom(2855087L))
  )

  private val votingPowers = SortedMap(
    owner -> VotingPower(
      NonNegLong.unsafeFrom(140000000000007000L),
      SortedSet(incidentInfo, legitimateInfo)
    ),
    incidentOnlyOwner -> VotingPower(incidentOnlyInfo.votingPower, SortedSet(incidentOnlyInfo)),
    bystander -> VotingPower.empty
  )

  test("manifest contains the seven exact incident locks across six owners") { _ =>
    val locks = IncidentTokenLockRemediation.incidentTokenLocks
    val total = locks.iterator.map(_.amount.value.value).sum

    IO.pure(
      expect.all(
        locks.size == 7,
        locks.map(_.source).size == 6,
        total == 44019596270815378L
      )
    )
  }

  test("below activation preserves the voting-power map exactly") { _ =>
    val below = ordinal(ProtocolActivation.incidentTokenLockRemediation.value.value - 1L)
    IO.pure(expect(IncidentTokenLockRemediation.removeFromVotingPowers(votingPowers, below) == votingPowers))
  }

  test("activation removes only exact incident locks and recomputes each retained owner total") { _ =>
    val result = IncidentTokenLockRemediation.removeFromVotingPowers(
      votingPowers,
      ProtocolActivation.incidentTokenLockRemediation
    )

    IO.pure(
      expect.all(
        result(owner).total.value == 7000L,
        result(owner).info == SortedSet(legitimateInfo),
        !result.contains(incidentOnlyOwner),
        result.contains(bystander)
      )
    )
  }

  test("subsequent active-lock imports cannot reintroduce an incident lock") { _ =>
    val afterActivation = IncidentTokenLockRemediation.removeFromVotingPowers(
      votingPowers,
      ProtocolActivation.incidentTokenLockRemediation
    )
    val reimported = afterActivation.updated(
      owner,
      VotingPower(
        NonNegLong.unsafeFrom(afterActivation(owner).total.value + incidentInfo.votingPower.value),
        afterActivation(owner).info + incidentInfo
      )
    )
    val later = ordinal(ProtocolActivation.incidentTokenLockRemediation.value.value + 1L)

    IO.pure(expect(IncidentTokenLockRemediation.removeFromVotingPowers(reimported, later) == afterActivation))
  }

  test("calculated-state remediation preserves allocations and earned rewards") { _ =>
    val rewards = RewardsState().copy(
      availableRewards = RewardInfo(
        SortedMap(AddressAndRewardType(owner, RewardType.Governance) -> Amount(NonNegLong.unsafeFrom(123L)))
      )
    )
    val before = AmmCalculatedState(votingPowers = votingPowers, rewards = rewards)
    val result = IncidentTokenLockRemediation.removeFromCalculatedState(
      before,
      ProtocolActivation.incidentTokenLockRemediation
    )

    IO.pure(
      expect.all(
        result.rewards == before.rewards,
        result.allocations == before.allocations,
        result.operations == before.operations,
        result.votingPowers(owner).info == SortedSet(legitimateInfo)
      )
    )
  }
}
