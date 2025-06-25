package org.amm_metagraph.shared_data.services.combiners

import java.security.PublicKey

import cats.data.NonEmptySet
import cats.effect.{IO, Resource}
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.currency.schema.currency
import io.constellationnetwork.ext.cats.effect.ResourceIO
import io.constellationnetwork.ext.cats.syntax.next.catsSyntaxNext
import io.constellationnetwork.json.JsonSerializer
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.security.hex.Hex
import io.constellationnetwork.security.key.ops.PublicKeyOps
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.signature.signature.{Signature, SignatureProof}
import io.constellationnetwork.security.{Hasher, KeyPairGenerator, SecurityProvider}
import io.constellationnetwork.syntax.sortedCollection.sortedMapSyntax

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.NonNegLong
import monocle.Monocle._
import org.amm_metagraph.shared_data.DummyL0Context.buildL0NodeContext
import org.amm_metagraph.shared_data.Shared
import org.amm_metagraph.shared_data.refined.Percentage
import org.amm_metagraph.shared_data.rewards._
import org.amm_metagraph.shared_data.types.Governance.{AllocationId, VotingWeight}
import org.amm_metagraph.shared_data.types.LiquidityPool.LiquidityPool
import org.amm_metagraph.shared_data.types.Rewards.RewardType._
import org.amm_metagraph.shared_data.types.Rewards.{AddressAndRewardType, RewardInfo}
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.codecs.HasherSelector
import weaver.MutableIOSuite

object RewardsDistributionServiceTest extends MutableIOSuite {
  private val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")

  private val validatorReward = Amount(NonNegLong.unsafeFrom(1L))
  private val daoReward = Amount(NonNegLong.unsafeFrom(2L))
  private val votingReward = Amount(NonNegLong.unsafeFrom(3L))
  private val governanceReward = Amount(NonNegLong.unsafeFrom(4L))

  private val rewardDistributionCalculator = new RewardCalculator[IO]() {
    override def calculateEpochRewards(
      currentProgress: EpochProgress,
      validators: List[Address],
      frozenVotingPowers: Map[Address, VotingWeight],
      frozenGovernanceVotes: Map[AllocationId, Percentage],
      currentLiquidityPools: Map[String, LiquidityPool],
      approvedValidators: List[Address]
    ): IO[Either[RewardError, RewardDistribution]] = {
      val distribution = RewardDistribution(
        epochProgress = currentProgress,
        month = 1,
        validatorRewards = validators.map(_ -> validatorReward).toMap,
        daoRewards = ownerAddress -> daoReward,
        votingRewards = frozenVotingPowers.map { case (address, _) => address -> votingReward },
        governanceRewards = Map(ownerAddress -> governanceReward)
      )
      distribution.asRight[RewardError].pure[IO]
    }
  }

  private val dummySignature = Signature(
    Hex(
      "3045022100fb26702e976a6569caa3507140756fee96b5ba748719abe1b812b17f7279a3dc0220613db28d5c5a30d7353383358b653aa29772151ccf352a2e67a26a74e49eac57"
    )
  )

  private def distributedReward(r: Amount, multiplier: NonNegLong = Shared.config.rewards.rewardCalculationInterval) =
    Amount(NonNegLong.unsafeFrom(r.value.value * multiplier.value))

  type Res = (Hasher[IO], HasherSelector[IO], SecurityProvider[IO], List[PublicKey])
  override def sharedResource: Resource[IO, Res] = for {
    implicit0(sp: SecurityProvider[IO]) <- SecurityProvider.forAsync[IO]
    implicit0(j: JsonSerializer[IO]) <- JsonSerializer.forSync[IO].asResource
    h = Hasher.forJson[IO]
    hs = HasherSelector.forSync(h, h)
    keys: List[PublicKey] <- (0 to 4).toList.traverse(_ => KeyPairGenerator.makeKeyPair[IO].map(_.getPublic)).toResource
  } yield (h, hs, sp, keys)

  test("Successfully DO NOT update reward distribution in calculated state and on chain because it is skip epoch") { implicit res =>
    implicit val (h, hs, sp, keys) = res
    val List(a1, a2, a3, a4, a5) = keys.map(_.toAddress)
    val List(a1Id, a2Id, a3Id, a4Id, a5Id) = keys.map(_.toId)
    val ammOnChainState = AmmOnChainState(SortedSet.empty, None)
    val ammCalculatedState = AmmCalculatedState()
    val currentEpoch = EpochProgress(Shared.config.rewards.rewardCalculationInterval).next
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

      voters = NonEmptySet.of(
        SignatureProof(a1Id, dummySignature),
        SignatureProof(a2Id, dummySignature),
        SignatureProof(a3Id, dummySignature)
      )
      currencyIncrementalSnapshot: Signed[currency.CurrencyIncrementalSnapshot] <- context.getLastCurrencySnapshot.map(_.get.signed)
      snapShotWithVoters = currencyIncrementalSnapshot.copy(proofs = voters)

      votingPowers = List(a3, a4).map(_ -> VotingWeight(NonNegLong.unsafeFrom(0), SortedSet.empty)).toSortedMap
      stateWithVotingPowers = state.focus(_.calculated.votingWeights).replace(votingPowers)

      rewardService <- RewardsDistributionService.make(rewardDistributionCalculator, Shared.config.rewards).pure[IO]
      newState <- rewardService.updateRewardsDistribution(
        snapShotWithVoters,
        stateWithVotingPowers,
        currentEpoch
      )

      expectedMap = Map.empty[AddressAndRewardType, Amount]
      expectedRewards = RewardsState(
        withdraws = RewardWithdrawCalculatedState.empty,
        availableRewards = RewardInfo(expectedMap),
        lastProcessedEpoch = currentEpoch
      )
    } yield
      expect.all(
        newState.calculated.rewards == expectedRewards,
        newState.onChain.rewardsUpdate.isEmpty
      )

  }

  test("Successfully update reward distribution in calculated state and on chain") { implicit res =>
    implicit val (h, hs, sp, keys) = res
    val List(a1, a2, a3, a4, a5) = keys.map(_.toAddress)
    val List(a1Id, a2Id, a3Id, a4Id, a5Id) = keys.map(_.toId)
    val ammOnChainState = AmmOnChainState(SortedSet.empty, None)
    val ammCalculatedState = AmmCalculatedState()
    val currentEpoch = EpochProgress(Shared.config.rewards.rewardCalculationInterval)
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

      voters = NonEmptySet.of(
        SignatureProof(a1Id, dummySignature),
        SignatureProof(a2Id, dummySignature),
        SignatureProof(a3Id, dummySignature)
      )
      currencyIncrementalSnapshot: Signed[currency.CurrencyIncrementalSnapshot] <- context.getLastCurrencySnapshot.map(_.get.signed)
      snapShotWithVoters = currencyIncrementalSnapshot.copy(proofs = voters)

      votingPowers = List(a3, a4).map(_ -> VotingWeight(NonNegLong.unsafeFrom(0), SortedSet.empty)).toSortedMap
      stateWithVotingPowers = state.focus(_.calculated.allocations.frozenUsedUserVotes.votes).replace(votingPowers)

      rewardService <- RewardsDistributionService.make(rewardDistributionCalculator, Shared.config.rewards).pure[IO]
      newState <- rewardService.updateRewardsDistribution(
        snapShotWithVoters,
        stateWithVotingPowers,
        currentEpoch
      )

      expectedMap: Map[AddressAndRewardType, Amount] = Map(
        AddressAndRewardType(a1, ValidatorConsensus) -> distributedReward(validatorReward),
        AddressAndRewardType(a2, ValidatorConsensus) -> distributedReward(validatorReward),
        AddressAndRewardType(a3, ValidatorConsensus) -> distributedReward(validatorReward),
        AddressAndRewardType(ownerAddress, LpBoost) -> distributedReward(daoReward),
        AddressAndRewardType(a3, ValidatorBoost) -> distributedReward(votingReward),
        AddressAndRewardType(a4, ValidatorBoost) -> distributedReward(votingReward),
        AddressAndRewardType(ownerAddress, GovernanceVoting) -> distributedReward(governanceReward)
      )
      expectedRewards = RewardsState(
        withdraws = RewardWithdrawCalculatedState.empty,
        availableRewards = RewardInfo(expectedMap),
        lastProcessedEpoch = currentEpoch
      )
    } yield
      expect.all(
        newState.calculated.rewards == expectedRewards,
        newState.onChain.rewardsUpdate.get.info == expectedMap
      )

  }

  test("Successfully update reward distribution twice in calculated state and on chain") { implicit res =>
    implicit val (h, hs, sp, keys) = res
    val List(a1, a2, a3, a4, a5) = keys.map(_.toAddress)
    val List(a1Id, a2Id, a3Id, a4Id, a5Id) = keys.map(_.toId)
    val ammOnChainState = AmmOnChainState(SortedSet.empty, None)
    val ammCalculatedState = AmmCalculatedState()
    val currentEpoch1 = EpochProgress(Shared.config.rewards.rewardCalculationInterval)
    val currentEpoch2 = EpochProgress(NonNegLong.unsafeFrom(Shared.config.rewards.rewardCalculationInterval.value * 2))
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

      voters = NonEmptySet.of(
        SignatureProof(a1Id, dummySignature),
        SignatureProof(a2Id, dummySignature),
        SignatureProof(a3Id, dummySignature)
      )
      currencyIncrementalSnapshot: Signed[currency.CurrencyIncrementalSnapshot] <- context.getLastCurrencySnapshot.map(_.get.signed)
      snapShotWithVoters = currencyIncrementalSnapshot.copy(proofs = voters)

      votingPowers = List(a3, a4).map(_ -> VotingWeight(NonNegLong.unsafeFrom(0), SortedSet.empty)).toSortedMap
      stateWithVotingPowers = state.focus(_.calculated.allocations.frozenUsedUserVotes.votes).replace(votingPowers)

      rewardService <- RewardsDistributionService.make(rewardDistributionCalculator, Shared.config.rewards).pure[IO]
      newState1 <- rewardService.updateRewardsDistribution(
        snapShotWithVoters,
        stateWithVotingPowers,
        currentEpoch1
      )

      newState2 <- rewardService.updateRewardsDistribution(
        snapShotWithVoters,
        newState1,
        currentEpoch2
      )

      expectedOnChainMap: Map[AddressAndRewardType, Amount] = Map(
        AddressAndRewardType(a1, ValidatorConsensus) -> distributedReward(validatorReward),
        AddressAndRewardType(a2, ValidatorConsensus) -> distributedReward(validatorReward),
        AddressAndRewardType(a3, ValidatorConsensus) -> distributedReward(validatorReward),
        AddressAndRewardType(ownerAddress, LpBoost) -> distributedReward(daoReward),
        AddressAndRewardType(a3, ValidatorBoost) -> distributedReward(votingReward),
        AddressAndRewardType(a4, ValidatorBoost) -> distributedReward(votingReward),
        AddressAndRewardType(ownerAddress, GovernanceVoting) -> distributedReward(governanceReward)
      )
      expectedMap = expectedOnChainMap.map { case (k, v) => k -> Amount(NonNegLong.unsafeFrom(v.value.value * 2)) }

      expectedRewards = RewardsState(
        withdraws = RewardWithdrawCalculatedState.empty,
        availableRewards = RewardInfo(expectedMap),
        lastProcessedEpoch = currentEpoch2
      )
    } yield
      expect.all(
        newState2.calculated.rewards == expectedRewards,
        newState2.onChain.rewardsUpdate.get.info == expectedOnChainMap
      )

  }

  test("Successfully update reward distribution only once because of the same epoch in calculated state and on chain") { implicit res =>
    implicit val (h, hs, sp, keys) = res
    val List(a1, a2, a3, a4, a5) = keys.map(_.toAddress)
    val List(a1Id, a2Id, a3Id, a4Id, a5Id) = keys.map(_.toId)
    val ammOnChainState = AmmOnChainState(SortedSet.empty, None)
    val ammCalculatedState = AmmCalculatedState()
    val currentEpoch1 = EpochProgress(Shared.config.rewards.rewardCalculationInterval)
    val currentEpoch2 = EpochProgress(NonNegLong.unsafeFrom(Shared.config.rewards.rewardCalculationInterval.value * 2))
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

      voters = NonEmptySet.of(
        SignatureProof(a1Id, dummySignature),
        SignatureProof(a2Id, dummySignature),
        SignatureProof(a3Id, dummySignature)
      )
      currencyIncrementalSnapshot: Signed[currency.CurrencyIncrementalSnapshot] <- context.getLastCurrencySnapshot.map(_.get.signed)
      snapShotWithVoters = currencyIncrementalSnapshot.copy(proofs = voters)

      votingPowers = List(a3, a4).map(_ -> VotingWeight(NonNegLong.unsafeFrom(0), SortedSet.empty)).toSortedMap
      stateWithVotingPowers = state.focus(_.calculated.allocations.frozenUsedUserVotes.votes).replace(votingPowers)

      rewardService <- RewardsDistributionService.make(rewardDistributionCalculator, Shared.config.rewards).pure[IO]
      newState1 <- rewardService.updateRewardsDistribution(
        snapShotWithVoters,
        stateWithVotingPowers,
        currentEpoch1
      )

      newState2 <- rewardService.updateRewardsDistribution(
        snapShotWithVoters,
        newState1,
        currentEpoch2
      )

      newState3 <- rewardService.updateRewardsDistribution(
        snapShotWithVoters,
        newState2,
        currentEpoch2
      )

      expectedOnChainMap: Map[AddressAndRewardType, Amount] = Map(
        AddressAndRewardType(a1, ValidatorConsensus) -> distributedReward(validatorReward),
        AddressAndRewardType(a2, ValidatorConsensus) -> distributedReward(validatorReward),
        AddressAndRewardType(a3, ValidatorConsensus) -> distributedReward(validatorReward),
        AddressAndRewardType(ownerAddress, LpBoost) -> distributedReward(daoReward),
        AddressAndRewardType(a3, ValidatorBoost) -> distributedReward(votingReward),
        AddressAndRewardType(a4, ValidatorBoost) -> distributedReward(votingReward),
        AddressAndRewardType(ownerAddress, GovernanceVoting) -> distributedReward(governanceReward)
      )
      expectedMap = expectedOnChainMap.map { case (k, v) => k -> Amount(NonNegLong.unsafeFrom(v.value.value * 2)) }

      expectedRewards = RewardsState(
        withdraws = RewardWithdrawCalculatedState.empty,
        availableRewards = RewardInfo(expectedMap),
        lastProcessedEpoch = currentEpoch2
      )
    } yield
      expect.all(
        newState3.calculated.rewards == expectedRewards,
        newState3.onChain.rewardsUpdate.isEmpty
      )

  }
}
