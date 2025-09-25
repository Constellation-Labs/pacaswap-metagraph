package org.amm_metagraph.shared_data.services.combiners

import java.security.PublicKey

import cats.data.NonEmptySet
import cats.effect.{IO, Resource}
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}
import scala.concurrent.duration.DurationInt

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
import eu.timepit.refined.types.numeric.NonNegInt
import monocle.Monocle._
import org.amm_metagraph.shared_data.DummyL0Context.buildL0NodeContext
import org.amm_metagraph.shared_data.Shared
import org.amm_metagraph.shared_data.app.ApplicationConfig.EpochMetadata
import org.amm_metagraph.shared_data.refined.Percentage
import org.amm_metagraph.shared_data.rewards._
import org.amm_metagraph.shared_data.services.combiners.operations.RewardsDistributionService
import org.amm_metagraph.shared_data.types.Governance.{AllocationId, MonthlyReference, VotingPower}
import org.amm_metagraph.shared_data.types.LiquidityPool.LiquidityPool
import org.amm_metagraph.shared_data.types.Rewards.RewardType._
import org.amm_metagraph.shared_data.types.Rewards._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.codecs.HasherSelector
import weaver.MutableIOSuite

object RewardsDistributionServiceTest extends MutableIOSuite {
  private val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")

  private val nodeValidatorReward = Amount(NonNegLong.unsafeFrom(1L))
  private val daoReward = Amount(NonNegLong.unsafeFrom(2L))
  private val voteBasedReward = Amount(NonNegLong.unsafeFrom(3L))
  private val governanceReward = Amount(NonNegLong.unsafeFrom(4L))

  private val rewardDistributionCalculator = new RewardCalculator[IO]() {
    override def calculateEpochRewards(
      currentProgress: EpochProgress,
      validators: Seq[Address],
      frozenVotingPowers: Map[Address, VotingPower],
      frozenGovernanceVotes: Map[AllocationId, Percentage],
      currentLiquidityPools: Map[String, LiquidityPool],
      approvedValidators: Seq[Address]
    ): IO[Either[RewardError, RewardsDistribution]] = {
      val validatorsChunk =
        validators.map(address => RewardDistributionChunk(address, RewardTypeExtended.NodeValidator, nodeValidatorReward))
      val daoChunk = Seq(RewardDistributionChunk(ownerAddress, RewardTypeExtended.Dao, daoReward))
      val voteBasedChunk = frozenVotingPowers.map {
        case (address, _) => RewardDistributionChunk(address, RewardTypeExtended.VoteBasedValidator, voteBasedReward)
      }
      val governanceChunk = Seq(RewardDistributionChunk(ownerAddress, RewardTypeExtended.Governance, governanceReward))
      val distribution = RewardsDistribution(validatorsChunk ++ daoChunk ++ voteBasedChunk ++ governanceChunk)

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
    val ammOnChainState = AmmOnChainState.empty
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

      votingPowers = List(a3, a4).map(_ -> VotingPower(NonNegLong.unsafeFrom(0), SortedSet.empty)).toSortedMap
      stateWithVotingPowers = state.focus(_.calculated.votingPowers).replace(votingPowers)

      rewardService <- RewardsDistributionService
        .make(rewardDistributionCalculator, Shared.config.rewards, Shared.config.epochInfo)
        .pure[IO]
      newState <- rewardService.updateRewardsDistribution(
        snapShotWithVoters,
        stateWithVotingPowers,
        currentEpoch
      )

      expectedMap = Map.empty[AddressAndRewardType, Amount]
      expectedRewards = RewardsState(
        withdraws = RewardWithdrawCalculatedState.empty,
        availableRewards = RewardInfo(expectedMap),
        lastProcessedEpoch = currentEpoch,
        rewardsBuffer = RewardsBuffer.empty
      )
    } yield
      expect.all(
        newState.calculated.rewards == expectedRewards,
        newState.onChain.rewardsUpdate.isEmpty
      )

  }

  def buildDistribution(expectedMap: Map[AddressAndRewardType, Amount]): DistributedRewards = {
    val distributionMap =
      expectedMap.toSeq.map { case (AddressAndRewardType(_, rewardType), amount) => rewardType -> amount.value.value }
        .groupMapReduce(_._1)(_._2)(_ + _)
        .map { case (key, amount) => key -> Amount(NonNegLong.unsafeFrom(amount)) }
    DistributedRewards(SortedMap.from(distributionMap))
  }

  test("Successfully update reward distribution in calculated state and on chain") { implicit res =>
    implicit val (h, hs, sp, keys) = res
    val List(a1, a2, a3, a4, a5) = keys.map(_.toAddress)
    val List(a1Id, a2Id, a3Id, a4Id, a5Id) = keys.map(_.toId)
    val ammOnChainState = AmmOnChainState.empty
    val ammCalculatedState = AmmCalculatedState()
    val currentEpoch = EpochProgress(NonNegLong.unsafeFrom(Shared.config.rewards.rewardCalculationInterval.value - 1))
    val currentMonthRef = MonthlyReference.getMonthlyReference(currentEpoch, Shared.config.epochInfo.epochProgress1Month)
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

      votingPowers = List(a3, a4).map(_ -> VotingPower(NonNegLong.unsafeFrom(0), SortedSet.empty)).toSortedMap
      stateWithVotingPowers = state.focus(_.calculated.allocations.frozenUsedUserVotes.votingPowerForAddresses).replace(votingPowers)

      rewardService <- RewardsDistributionService
        .make(rewardDistributionCalculator, Shared.config.rewards, Shared.config.epochInfo)
        .pure[IO]
      newState <- rewardService.updateRewardsDistribution(
        snapShotWithVoters,
        stateWithVotingPowers,
        currentEpoch
      )

      expectedMap: Map[AddressAndRewardType, Amount] = Map(
        AddressAndRewardType(a1, NodeValidator) -> distributedReward(nodeValidatorReward),
        AddressAndRewardType(a2, NodeValidator) -> distributedReward(nodeValidatorReward),
        AddressAndRewardType(a3, NodeValidator) -> distributedReward(nodeValidatorReward),
        AddressAndRewardType(ownerAddress, Dao) -> distributedReward(daoReward),
        AddressAndRewardType(a3, VoteBased) -> distributedReward(voteBasedReward),
        AddressAndRewardType(a4, VoteBased) -> distributedReward(voteBasedReward),
        AddressAndRewardType(ownerAddress, Governance) -> distributedReward(governanceReward)
      )

      distributedRewards = SortedMap(currentMonthRef -> buildDistribution(expectedMap))
      expectedRewards = RewardsState(
        withdraws = RewardWithdrawCalculatedState.empty,
        availableRewards = RewardInfo(expectedMap),
        lastProcessedEpoch = currentEpoch,
        rewardsBuffer = RewardsBuffer.empty,
        distributedRewards = distributedRewards
      )
    } yield
      expect.all(
        newState.calculated.rewards == expectedRewards
        // newState.onChain.rewardsUpdate.get.info == expectedMap
      )
  }

  test("Successfully distributive update reward distribution in calculated state and on chain") { implicit res =>
    implicit val (h, hs, sp, keys) = res
    val List(a1, a2, a3, a4, a5) = keys.map(_.toAddress)
    val List(a1Id, a2Id, a3Id, a4Id, a5Id) = keys.map(_.toId)
    val ammOnChainState = AmmOnChainState.empty
    val ammCalculatedState = AmmCalculatedState()
    val currentEpoch = EpochProgress(NonNegLong.unsafeFrom(Shared.config.rewards.rewardCalculationInterval.value - 1))
    val currentMonthRef = MonthlyReference.getMonthlyReference(currentEpoch, Shared.config.epochInfo.epochProgress1Month)
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

      votingPowers = List(a3, a4).map(_ -> VotingPower(NonNegLong.unsafeFrom(0), SortedSet.empty)).toSortedMap
      stateWithVotingPowers = state.focus(_.calculated.allocations.frozenUsedUserVotes.votingPowerForAddresses).replace(votingPowers)

      rewardTransactionsPerSnapshot = 3
      config = Shared.config.rewards.copy(availableRewardsPerSnapshot = NonNegInt.unsafeFrom(rewardTransactionsPerSnapshot))
      rewardService <- RewardsDistributionService.make(rewardDistributionCalculator, config, Shared.config.epochInfo).pure[IO]
      newState1 <- rewardService.updateRewardsDistribution(
        snapShotWithVoters,
        stateWithVotingPowers,
        currentEpoch
      )
      newState2 <- rewardService.updateRewardsDistribution(
        snapShotWithVoters,
        newState1,
        currentEpoch
      )
      newState3 <- rewardService.updateRewardsDistribution(
        snapShotWithVoters,
        newState2,
        currentEpoch
      )
      expectedMap: Map[AddressAndRewardType, Amount] = Map(
        AddressAndRewardType(a1, NodeValidator) -> distributedReward(nodeValidatorReward),
        AddressAndRewardType(a2, NodeValidator) -> distributedReward(nodeValidatorReward),
        AddressAndRewardType(a3, NodeValidator) -> distributedReward(nodeValidatorReward),
        AddressAndRewardType(ownerAddress, Dao) -> distributedReward(daoReward),
        AddressAndRewardType(a3, VoteBased) -> distributedReward(voteBasedReward),
        AddressAndRewardType(a4, VoteBased) -> distributedReward(voteBasedReward),
        AddressAndRewardType(ownerAddress, Governance) -> distributedReward(governanceReward)
      )
      totalRewardsSize = expectedMap.size

      allRewardsIteration1 =
        RewardInfo(newState1.calculated.rewards.availableRewards.info)
          .addRewards(RewardInfo.fromChunks(newState1.calculated.rewards.rewardsBuffer.data).toOption.get)
          .toOption
          .get
      allRewardsIteration2 =
        RewardInfo(newState2.calculated.rewards.availableRewards.info)
          .addRewards(RewardInfo.fromChunks(newState2.calculated.rewards.rewardsBuffer.data).toOption.get)
          .toOption
          .get

      distributedRewards = SortedMap(currentMonthRef -> buildDistribution(expectedMap))
      expectedRewardsAtEnd = RewardsState(
        withdraws = RewardWithdrawCalculatedState.empty,
        availableRewards = RewardInfo(expectedMap),
        lastProcessedEpoch = currentEpoch,
        distributedRewards = distributedRewards
      )
    } yield
      expect.all(
        newState1.onChain.rewardsUpdate.size == rewardTransactionsPerSnapshot,
        newState1.calculated.rewards.availableRewards.info.size == rewardTransactionsPerSnapshot,
        newState1.calculated.rewards.rewardsBuffer.data.size == totalRewardsSize - rewardTransactionsPerSnapshot,
        allRewardsIteration1 == RewardInfo(expectedMap),
        newState2.onChain.rewardsUpdate.size == rewardTransactionsPerSnapshot,
        newState2.calculated.rewards.availableRewards.info.size == rewardTransactionsPerSnapshot * 2,
        newState2.calculated.rewards.rewardsBuffer.data.size == totalRewardsSize - rewardTransactionsPerSnapshot * 2,
        allRewardsIteration2 == RewardInfo(expectedMap),
        newState3.onChain.rewardsUpdate.size == 1,
        newState3.calculated.rewards == expectedRewardsAtEnd
      )

  }

  test("Successfully append reward to rewards buffer; buffer shall be released at least on 10%") { implicit res =>
    implicit val (h, hs, sp, keys) = res
    val List(a1, a2, a3, a4, a5) = keys.map(_.toAddress)
    val List(a1Id, a2Id, a3Id, a4Id, a5Id) = keys.map(_.toId)
    val ammOnChainState = AmmOnChainState.empty
    val ammCalculatedState = AmmCalculatedState()
    val currentEpoch1 = EpochProgress(NonNegLong.unsafeFrom(Shared.config.rewards.rewardCalculationInterval.value - 1))
    val currentEpoch2 = EpochProgress(NonNegLong.unsafeFrom(Shared.config.rewards.rewardCalculationInterval.value * 2 - 1))

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

      votingPowers = List(a3, a4).map(_ -> VotingPower(NonNegLong.unsafeFrom(0), SortedSet.empty)).toSortedMap
      stateWithVotingPowers = state.focus(_.calculated.allocations.frozenUsedUserVotes.votingPowerForAddresses).replace(votingPowers)

      rewardTransactionsPerSnapshot = 0
      config = Shared.config.rewards.copy(availableRewardsPerSnapshot = NonNegInt.unsafeFrom(rewardTransactionsPerSnapshot))
      rewardService <- RewardsDistributionService.make(rewardDistributionCalculator, config, Shared.config.epochInfo).pure[IO]
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

      expectedMap: Map[AddressAndRewardType, Amount] = Map(
        AddressAndRewardType(a1, NodeValidator) -> distributedReward(nodeValidatorReward),
        AddressAndRewardType(a2, NodeValidator) -> distributedReward(nodeValidatorReward),
        AddressAndRewardType(a3, NodeValidator) -> distributedReward(nodeValidatorReward),
        AddressAndRewardType(ownerAddress, Dao) -> distributedReward(daoReward),
        AddressAndRewardType(a3, VoteBased) -> distributedReward(voteBasedReward),
        AddressAndRewardType(a4, VoteBased) -> distributedReward(voteBasedReward),
        AddressAndRewardType(ownerAddress, Governance) -> distributedReward(governanceReward)
      )

    } yield
      expect.all(
        newState1.onChain.rewardsUpdate.isEmpty,
        newState1.calculated.rewards.availableRewards.info.isEmpty,
        newState1.calculated.rewards.rewardsBuffer.data.size == expectedMap.size,
        newState2.calculated.rewards.rewardsBuffer.data.size == (expectedMap.size * 2),
        newState2.onChain.rewardsUpdate.isEmpty,
        newState2.calculated.rewards.availableRewards.info.isEmpty
      )

  }

  test("Successfully update reward distribution twice in calculated state and on chain") { implicit res =>
    implicit val (h, hs, sp, keys) = res
    val List(a1, a2, a3, a4, a5) = keys.map(_.toAddress)
    val List(a1Id, a2Id, a3Id, a4Id, a5Id) = keys.map(_.toId)
    val ammOnChainState = AmmOnChainState.empty
    val ammCalculatedState = AmmCalculatedState()
    val currentEpoch1 = EpochProgress(NonNegLong.unsafeFrom(Shared.config.rewards.rewardCalculationInterval.value - 1))
    val currentEpoch2 = EpochProgress(NonNegLong.unsafeFrom(Shared.config.rewards.rewardCalculationInterval.value * 2 - 1))
    val currentMonthRef = MonthlyReference.getMonthlyReference(currentEpoch1, Shared.config.epochInfo.epochProgress1Month)
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

      votingPowers = List(a3, a4).map(_ -> VotingPower(NonNegLong.unsafeFrom(0), SortedSet.empty)).toSortedMap
      stateWithVotingPowers = state.focus(_.calculated.allocations.frozenUsedUserVotes.votingPowerForAddresses).replace(votingPowers)

      rewardService <- RewardsDistributionService
        .make(rewardDistributionCalculator, Shared.config.rewards, Shared.config.epochInfo)
        .pure[IO]
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

      // validatorsChunk ++ daoChunk ++ voteBasedChunk ++ governanceChunk
      expectedOnChainSet = Set(
        RewardDistributionChunk(a1, RewardTypeExtended.NodeValidator, distributedReward(nodeValidatorReward)),
        RewardDistributionChunk(a2, RewardTypeExtended.NodeValidator, distributedReward(nodeValidatorReward)),
        RewardDistributionChunk(a3, RewardTypeExtended.NodeValidator, distributedReward(nodeValidatorReward)),
        RewardDistributionChunk(ownerAddress, RewardTypeExtended.Dao, distributedReward(daoReward)),
        RewardDistributionChunk(a3, RewardTypeExtended.VoteBasedValidator, distributedReward(voteBasedReward)),
        RewardDistributionChunk(a4, RewardTypeExtended.VoteBasedValidator, distributedReward(voteBasedReward)),
        RewardDistributionChunk(ownerAddress, RewardTypeExtended.Governance, distributedReward(governanceReward))
      )
      expectedMap = expectedOnChainSet.map {
        case RewardDistributionChunk(address, rewardType, amount) =>
          RewardDistributionChunk(address, rewardType, Amount(NonNegLong.unsafeFrom(amount.value.value * 2)))
      }
      distributedRewards = SortedMap(currentMonthRef -> DistributedRewards.from(expectedMap).toOption.get)
      expectedRewards = RewardsState(
        withdraws = RewardWithdrawCalculatedState.empty,
        availableRewards = RewardInfo.fromChunks(expectedMap).toOption.get,
        lastProcessedEpoch = currentEpoch2,
        distributedRewards = distributedRewards
      )
    } yield
      expect.all(
        newState2.calculated.rewards == expectedRewards,
        newState2.onChain.rewardsUpdate.toSet == expectedOnChainSet
      )

  }

  test("Successfully update reward distribution twice in difference months") { implicit res =>
    implicit val (h, hs, sp, keys) = res
    val List(a1, a2, a3, a4, a5) = keys.map(_.toAddress)
    val List(a1Id, a2Id, a3Id, a4Id, a5Id) = keys.map(_.toId)
    val ammOnChainState = AmmOnChainState.empty
    val ammCalculatedState = AmmCalculatedState()
    val epochConfig = EpochMetadata(1.day, 1)

    val currentEpoch1 = EpochProgress(NonNegLong.unsafeFrom(Shared.config.rewards.rewardCalculationInterval.value - 1))
    val monthRef1 = MonthlyReference.getMonthlyReference(currentEpoch1, epochConfig.epochProgress1Month)

    val currentEpoch2 = EpochProgress(NonNegLong.unsafeFrom(Shared.config.rewards.rewardCalculationInterval.value * 2 - 1))
    val monthRef2 = MonthlyReference.getMonthlyReference(currentEpoch2, epochConfig.epochProgress1Month)
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

      votingPowers = List(a3, a4).map(_ -> VotingPower(NonNegLong.unsafeFrom(0), SortedSet.empty)).toSortedMap
      stateWithVotingPowers = state.focus(_.calculated.allocations.frozenUsedUserVotes.votingPowerForAddresses).replace(votingPowers)

      rewardService <- RewardsDistributionService
        .make(rewardDistributionCalculator, Shared.config.rewards, epochConfig)
        .pure[IO]
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

      expectedOnChainSet = Set(
        RewardDistributionChunk(a1, RewardTypeExtended.NodeValidator, distributedReward(nodeValidatorReward)),
        RewardDistributionChunk(a2, RewardTypeExtended.NodeValidator, distributedReward(nodeValidatorReward)),
        RewardDistributionChunk(a3, RewardTypeExtended.NodeValidator, distributedReward(nodeValidatorReward)),
        RewardDistributionChunk(ownerAddress, RewardTypeExtended.Dao, distributedReward(daoReward)),
        RewardDistributionChunk(a3, RewardTypeExtended.VoteBasedValidator, distributedReward(voteBasedReward)),
        RewardDistributionChunk(a4, RewardTypeExtended.VoteBasedValidator, distributedReward(voteBasedReward)),
        RewardDistributionChunk(ownerAddress, RewardTypeExtended.Governance, distributedReward(governanceReward))
      )
      expectedMap = expectedOnChainSet.map {
        case RewardDistributionChunk(address, rewardType, amount) =>
          RewardDistributionChunk(address, rewardType, Amount(NonNegLong.unsafeFrom(amount.value.value * 2)))
      }

      distributedRewards = SortedMap(
        MonthlyReference(EpochProgress(NonNegLong(99)), EpochProgress(NonNegLong(99)), NonNegLong(99)) ->
          DistributedRewards(
            SortedMap[RewardType, Amount](
              Dao -> Amount(NonNegLong(200)),
              Governance -> Amount(NonNegLong(400)),
              NodeValidator -> Amount(NonNegLong(300)),
              VoteBased -> Amount(NonNegLong(600))
            )
          ),
        MonthlyReference(EpochProgress(NonNegLong(199)), EpochProgress(NonNegLong(199)), NonNegLong(199)) ->
          DistributedRewards(
            SortedMap[RewardType, Amount](
              Dao -> Amount(NonNegLong(200)),
              Governance -> Amount(NonNegLong(400)),
              NodeValidator -> Amount(NonNegLong(300)),
              VoteBased -> Amount(NonNegLong(600))
            )
          )
      )
      expectedRewards = RewardsState(
        withdraws = RewardWithdrawCalculatedState.empty,
        availableRewards = RewardInfo.fromChunks(expectedMap).toOption.get,
        lastProcessedEpoch = currentEpoch2,
        distributedRewards = distributedRewards
      )
    } yield
      expect.all(
        newState2.calculated.rewards == expectedRewards,
        newState2.onChain.rewardsUpdate.toSet == expectedOnChainSet
      )

  }

  test("Successfully update reward distribution only once because of the same epoch in calculated state and on chain") { implicit res =>
    implicit val (h, hs, sp, keys) = res
    val List(a1, a2, a3, a4, a5) = keys.map(_.toAddress)
    val List(a1Id, a2Id, a3Id, a4Id, a5Id) = keys.map(_.toId)
    val ammOnChainState = AmmOnChainState.empty
    val ammCalculatedState = AmmCalculatedState()
    val currentEpoch1 = EpochProgress(NonNegLong.unsafeFrom(Shared.config.rewards.rewardCalculationInterval.value - 1))
    val currentEpoch2 = EpochProgress(NonNegLong.unsafeFrom(Shared.config.rewards.rewardCalculationInterval.value * 2 - 1))
    val currentMonthRef = MonthlyReference.getMonthlyReference(currentEpoch1, Shared.config.epochInfo.epochProgress1Month)
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

      votingPowers = List(a3, a4).map(_ -> VotingPower(NonNegLong.unsafeFrom(0), SortedSet.empty)).toSortedMap
      stateWithVotingPowers = state.focus(_.calculated.allocations.frozenUsedUserVotes.votingPowerForAddresses).replace(votingPowers)

      rewardService <- RewardsDistributionService
        .make(rewardDistributionCalculator, Shared.config.rewards, Shared.config.epochInfo)
        .pure[IO]
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
        AddressAndRewardType(a1, NodeValidator) -> distributedReward(nodeValidatorReward),
        AddressAndRewardType(a2, NodeValidator) -> distributedReward(nodeValidatorReward),
        AddressAndRewardType(a3, NodeValidator) -> distributedReward(nodeValidatorReward),
        AddressAndRewardType(ownerAddress, Dao) -> distributedReward(daoReward),
        AddressAndRewardType(a3, VoteBased) -> distributedReward(voteBasedReward),
        AddressAndRewardType(a4, VoteBased) -> distributedReward(voteBasedReward),
        AddressAndRewardType(ownerAddress, Governance) -> distributedReward(governanceReward)
      )
      expectedMap = expectedOnChainMap.map { case (k, v) => k -> Amount(NonNegLong.unsafeFrom(v.value.value * 2)) }
      distributedRewards = SortedMap(currentMonthRef -> buildDistribution(expectedMap))
      expectedRewards = RewardsState(
        withdraws = RewardWithdrawCalculatedState.empty,
        availableRewards = RewardInfo(expectedMap),
        lastProcessedEpoch = currentEpoch2,
        distributedRewards = distributedRewards
      )
    } yield
      expect.all(
        newState3.calculated.rewards == expectedRewards,
        newState3.onChain.rewardsUpdate.isEmpty
      )

  }

}
