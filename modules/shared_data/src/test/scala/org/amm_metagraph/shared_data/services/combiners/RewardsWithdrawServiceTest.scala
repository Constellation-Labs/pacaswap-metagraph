package org.amm_metagraph.shared_data.services.combiners

import cats.effect.{IO, Resource}
import cats.syntax.all._

import scala.collection.immutable.SortedMap

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.ext.cats.effect.ResourceIO
import io.constellationnetwork.json.JsonSerializer
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.CurrencyId
import io.constellationnetwork.security.{Hasher, KeyPairGenerator, SecurityProvider}

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.NonNegLong
import monocle.Monocle._
import org.amm_metagraph.shared_data.DummyL0Context.buildL0NodeContext
import org.amm_metagraph.shared_data.Shared
import org.amm_metagraph.shared_data.Shared.{config, getFakeSignedUpdate}
import org.amm_metagraph.shared_data.types.DataUpdates.RewardWithdrawUpdate
import org.amm_metagraph.shared_data.types.RewardWithdraw.RewardWithdrawReference
import org.amm_metagraph.shared_data.types.Rewards.RewardType._
import org.amm_metagraph.shared_data.types.Rewards.{AddressAndRewardType, RewardInfo}
import org.amm_metagraph.shared_data.types.States.{AmmCalculatedState, AmmOnChainState}
import org.amm_metagraph.shared_data.types.codecs.HasherSelector
import weaver.MutableIOSuite

object RewardsWithdrawServiceTest extends MutableIOSuite {
  type Res = (Hasher[IO], HasherSelector[IO], SecurityProvider[IO])
  override def sharedResource: Resource[IO, Res] = for {
    implicit0(sp: SecurityProvider[IO]) <- SecurityProvider.forAsync[IO]
    implicit0(j: JsonSerializer[IO]) <- JsonSerializer.forSync[IO].asResource
    h = Hasher.forJson[IO]
    hs = HasherSelector.forSync(h, h)
  } yield (h, hs, sp)
  private val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")

  val a1: Address = Address("DAG7coCMRPJah33MMcfAEZVeB1vYn3vDRe6WqeGU")
  val a2: Address = Address("DAG0y4eLqhhXUafeE3mgBstezPTnr8L3tZjAtMWB")
  val a3: Address = Address("DAG8Yy2enxizZdWoipKKZg6VXwk7rY2Z54mJqUdC")
  val a4: Address = Address("DAG07tqNLYW8jHU9emXcRTT3CfgCUoumwcLghopd")
  val a5: Address = Address("DAG0y4eLqhhXUafeE3mgBstezPTnr8L3tZjAtMWB")

  test("Fail to withdraw from empty / non-existing balance") { implicit res =>
    implicit val (h, hs, sp) = res
    val ammOnChainState = AmmOnChainState(Set.empty, None)
    val ammCalculatedState = AmmCalculatedState()
    val currentEpoch = EpochProgress(NonNegLong(1L))
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(
        keyPair,
        SortedMap.empty,
        EpochProgress.MaxValue,
        SnapshotOrdinal.MinValue,
        SortedMap.empty,
        EpochProgress.MaxValue,
        SnapshotOrdinal.MinValue,
        ownerAddress
      )

      rewardWithdrawService <- RewardsWithdrawService.make(Shared.config.rewards).pure[IO]

      rewardWithdrawUpdate = getFakeSignedUpdate(
        RewardWithdrawUpdate(
          CurrencyId(ownerAddress),
          a1,
          RewardWithdrawReference.empty,
          GovernanceVoting,
          Amount(NonNegLong(100L))
        )
      )

      newState <- rewardWithdrawService.combineNew(rewardWithdrawUpdate, state, currentEpoch)
    } yield expect(newState == state)
  }

  test("Fail to withdraw from empty / non-existing balance because of reward type") { implicit res =>
    implicit val (h, hs, sp) = res
    val ammOnChainState = AmmOnChainState(Set.empty, None)
    val actualRewardType = LpBoost
    val requestedRewardType = GovernanceVoting
    val requestAmount = Amount(NonNegLong(100L))
    val existReward = AddressAndRewardType(a1, actualRewardType) -> requestAmount
    val ammCalculatedState = AmmCalculatedState().focus(_.rewards.availableRewards.info).modify(_ + existReward)
    val currentEpoch = EpochProgress(NonNegLong(1L))
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(
        keyPair,
        SortedMap.empty,
        EpochProgress.MaxValue,
        SnapshotOrdinal.MinValue,
        SortedMap.empty,
        EpochProgress.MaxValue,
        SnapshotOrdinal.MinValue,
        ownerAddress
      )

      rewardWithdrawService <- RewardsWithdrawService.make(Shared.config.rewards).pure[IO]

      rewardWithdrawUpdate = getFakeSignedUpdate(
        RewardWithdrawUpdate(CurrencyId(ownerAddress), a1, RewardWithdrawReference.empty, requestedRewardType, requestAmount)
      )

      newState <- rewardWithdrawService.combineNew(rewardWithdrawUpdate, state, currentEpoch)
    } yield expect(newState == state)
  }

  test("Successfully withdraw from exist balance") { implicit res =>
    implicit val (h, hs, sp) = res
    val ammOnChainState = AmmOnChainState(Set.empty, None)
    val requestedRewardType = GovernanceVoting
    val requestAmount = Amount(NonNegLong(100L))
    val existReward = AddressAndRewardType(a1, requestedRewardType) -> requestAmount
    val ammCalculatedState = AmmCalculatedState().focus(_.rewards.availableRewards.info).modify(_ + existReward)
    val currentEpoch = EpochProgress(NonNegLong(1L))
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(
        keyPair,
        SortedMap.empty,
        EpochProgress.MaxValue,
        SnapshotOrdinal.MinValue,
        SortedMap.empty,
        EpochProgress.MaxValue,
        SnapshotOrdinal.MinValue,
        ownerAddress
      )

      rewardWithdrawService <- RewardsWithdrawService.make(Shared.config.rewards).pure[IO]
      reference = RewardWithdrawReference.empty

      rewardWithdrawUpdate = getFakeSignedUpdate(
        RewardWithdrawUpdate(CurrencyId(ownerAddress), a1, reference, requestedRewardType, requestAmount)
      )

      newState <- rewardWithdrawService.combineNew(rewardWithdrawUpdate, state, currentEpoch)

      expectedReference <- RewardWithdrawReference.of[IO](rewardWithdrawUpdate)
      expectedWithdrawEpoch = currentEpoch.plus(config.rewards.rewardWithdrawDelay).toOption.get
      expectedRewardInfo = RewardInfo.empty.addReward(a1, requestedRewardType, requestAmount).toOption.get
      expectedState = state
        .focus(_.calculated.rewards.availableRewards)
        .replace(RewardInfo.empty)
        .focus(_.calculated.rewards.withdraws.confirmed)
        .replace(Map(a1 -> expectedReference))
        .focus(_.calculated.rewards.withdraws.pending)
        .replace(Map(expectedWithdrawEpoch -> expectedRewardInfo))
    } yield expect.all(newState == expectedState)
  }

  test("Successfully withdraw twice from exist balance") { implicit res =>
    implicit val (h, hs, sp) = res
    val ammOnChainState = AmmOnChainState(Set.empty, None)
    val requestedRewardType = GovernanceVoting
    val firstRequest = Amount(NonNegLong(20L))
    val secondRequest = Amount(NonNegLong(80L))
    val fullAmount = Amount(NonNegLong.unsafeFrom(firstRequest.value + secondRequest.value))

    val existReward = AddressAndRewardType(a1, requestedRewardType) -> fullAmount
    val ammCalculatedState = AmmCalculatedState().focus(_.rewards.availableRewards.info).modify(_ + existReward)
    val currentEpoch = EpochProgress(NonNegLong(1L))
    val state = DataState(ammOnChainState, ammCalculatedState)

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      implicit0(context: L0NodeContext[IO]) = buildL0NodeContext(
        keyPair,
        SortedMap.empty,
        EpochProgress.MaxValue,
        SnapshotOrdinal.MinValue,
        SortedMap.empty,
        EpochProgress.MaxValue,
        SnapshotOrdinal.MinValue,
        ownerAddress
      )

      rewardWithdrawService <- RewardsWithdrawService.make(Shared.config.rewards).pure[IO]
      reference1 = RewardWithdrawReference.empty

      rewardWithdrawUpdate1 = getFakeSignedUpdate(
        RewardWithdrawUpdate(CurrencyId(ownerAddress), a1, reference1, requestedRewardType, firstRequest)
      )
      newState1 <- rewardWithdrawService.combineNew(rewardWithdrawUpdate1, state, currentEpoch)
      reference2 <- RewardWithdrawReference.of[IO](rewardWithdrawUpdate1)

      rewardWithdrawUpdate2 = getFakeSignedUpdate(
        RewardWithdrawUpdate(CurrencyId(ownerAddress), a1, reference2, requestedRewardType, secondRequest)
      )

      newState2 <- rewardWithdrawService.combineNew(rewardWithdrawUpdate2, newState1, currentEpoch)

      expectedReference <- RewardWithdrawReference.of[IO](rewardWithdrawUpdate2)
      expectedWithdrawEpoch = currentEpoch.plus(config.rewards.rewardWithdrawDelay).toOption.get
      expectedRewardInfo = RewardInfo.empty.addReward(a1, requestedRewardType, fullAmount).toOption.get
      expectedState = state
        .focus(_.calculated.rewards.availableRewards)
        .replace(RewardInfo.empty)
        .focus(_.calculated.rewards.withdraws.confirmed)
        .replace(Map(a1 -> expectedReference))
        .focus(_.calculated.rewards.withdraws.pending)
        .replace(Map(expectedWithdrawEpoch -> expectedRewardInfo))
    } yield expect.all(newState2 == expectedState)
  }
}
