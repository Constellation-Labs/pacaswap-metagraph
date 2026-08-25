package org.amm_metagraph.shared_data.services.combiners

import cats.effect.IO
import cats.syntax.all._

import scala.collection.immutable.SortedMap

import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.swap.CurrencyId
import io.constellationnetwork.security.hash.Hash

import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.{NonNegLong, PosLong}
import org.amm_metagraph.shared_data.FeeDistributor
import org.amm_metagraph.shared_data.types.Governance._
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.Rewards._
import org.amm_metagraph.shared_data.types.States._
import weaver.MutableIOSuite

object FrozenAddressPurgeSpec extends MutableIOSuite {

  type Res = Unit
  override def sharedResource: cats.effect.Resource[IO, Res] = cats.effect.Resource.pure[IO, Unit](())

  // Two of the five addresses the purge targets, plus one that must survive untouched.
  private val frozenA = Address("DAG8uqhyGtFABWSS5KeVB2ia1R4vXop5AeijXeoU")
  private val frozenB = Address("DAG4w5mUqNNxQNS4hgdpx3E8FGgiu2UCRsJxHwhX")
  private val bystander = Address("DAG011jH7FMDvKpdb7wewrMWwYtkwq56nHquAHdi")
  private val frozen: Set[Address] = Set(frozenA, frozenB)

  private def share(v: Long) = ShareAmount(Amount(NonNegLong.unsafeFrom(v)))

  private def stateWithReferences: AmmCalculatedState = {
    val pool = LiquidityPool(
      Hash.empty,
      PoolId("PACA-DAG"),
      TokenInformation(CurrencyId(Address("DAG7X5idd4aLfp4XC6WQdG1eDfR3LGPVEwtUUB2W")).some, PosLong.unsafeFrom(1000L)),
      TokenInformation(none, PosLong.unsafeFrom(1000L)),
      bystander,
      BigInt(1000L) * BigInt(1000L),
      PoolShares(
        totalShares = PosLong.unsafeFrom(1000L),
        addressShares = Map(frozenA -> share(300L), frozenB -> share(200L), bystander -> share(500L))
      ),
      FeeDistributor.empty
    )

    AmmCalculatedState(
      operations = SortedMap[OperationType, AmmOffChainState](
        OperationType.LiquidityPool -> LiquidityPoolCalculatedState.empty.copy(
          confirmed = ConfirmedLiquidityPoolCalculatedState.empty.copy(value = SortedMap("PACA-DAG" -> pool))
        )
      ),
      votingPowers = SortedMap(frozenA -> VotingPower.empty, bystander -> VotingPower.empty),
      allocations = Allocations.empty.copy(
        usersAllocations = SortedMap(frozenB -> UserAllocations.empty, bystander -> UserAllocations.empty)
      ),
      rewards = RewardsState().copy(
        availableRewards = RewardInfo(
          Map(
            AddressAndRewardType(frozenA, RewardType.Governance) -> Amount(NonNegLong.unsafeFrom(111L)),
            AddressAndRewardType(bystander, RewardType.Governance) -> Amount(NonNegLong.unsafeFrom(222L))
          )
        )
      )
    )
  }

  test("purge removes every frozen-address reference and leaves bystanders intact") { _ =>
    val before = stateWithReferences
    val purged = OneTimeFixesHandler.purgeFrozenAddresses(before, frozen)

    val pool = purged.operations(OperationType.LiquidityPool).asInstanceOf[LiquidityPoolCalculatedState].confirmed.value("PACA-DAG")

    IO.pure(
      expect.all(
        // pool shares: both frozen holders gone, bystander untouched
        !pool.poolShares.addressShares.contains(frozenA),
        !pool.poolShares.addressShares.contains(frozenB),
        pool.poolShares.addressShares.get(bystander).map(_.value.value.value).contains(500L),
        // denominator shrinks by exactly what was removed, so the bystander still owns 100%
        pool.poolShares.totalShares.value == 500L,
        // voting power, allocations and rewards
        !purged.votingPowers.contains(frozenA),
        purged.votingPowers.contains(bystander),
        !purged.allocations.usersAllocations.contains(frozenB),
        purged.allocations.usersAllocations.contains(bystander),
        !purged.rewards.availableRewards.info.keys.exists(_.address == frozenA),
        purged.rewards.availableRewards.info.keys.exists(_.address == bystander)
      )
    )
  }

  test("purge is a no-op when no frozen address is referenced") { _ =>
    val clean = AmmCalculatedState(
      votingPowers = SortedMap(bystander -> VotingPower.empty)
    )

    IO.pure(expect(OneTimeFixesHandler.purgeFrozenAddresses(clean, frozen) == clean))
  }
}
