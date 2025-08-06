package org.amm_metagraph.shared_data.services.combiners

import cats.effect.{IO, Resource}
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.currency.dataApplication.{DataApplicationValidationError, DataState, L0NodeContext}
import io.constellationnetwork.ext.cats.effect.ResourceIO
import io.constellationnetwork.json.JsonSerializer
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.CurrencyId
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hasher, KeyPairGenerator, SecurityProvider}

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.NonNegLong
import monocle.Monocle._
import org.amm_metagraph.shared_data.DummyL0Context.buildL0NodeContext
import org.amm_metagraph.shared_data.Shared
import org.amm_metagraph.shared_data.Shared.{config, getFakeSignedUpdate}
import org.amm_metagraph.shared_data.refined.Percentage
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, RewardAllocationVoteUpdate, RewardWithdrawUpdate}
import org.amm_metagraph.shared_data.types.Governance.AllocationCategory.{LiquidityPool, NodeOperator}
import org.amm_metagraph.shared_data.types.Governance._
import org.amm_metagraph.shared_data.types.RewardWithdraw.RewardWithdrawReference
import org.amm_metagraph.shared_data.types.Rewards.RewardType._
import org.amm_metagraph.shared_data.types.Rewards.{AddressAndRewardType, RewardInfo}
import org.amm_metagraph.shared_data.types.States.{AmmCalculatedState, AmmOnChainState}
import org.amm_metagraph.shared_data.types.codecs.{HasherSelector, JsonWithBase64BinaryCodec}
import org.amm_metagraph.shared_data.types.{DataUpdates, States}
import org.amm_metagraph.shared_data.validations.{GovernanceValidations, RewardWithdrawValidations}
import weaver.MutableIOSuite

object GovernanceCombinerServiceTest extends MutableIOSuite {
  type Res = (Hasher[IO], HasherSelector[IO], SecurityProvider[IO])
  override def sharedResource: Resource[IO, Res] = for {
    implicit0(sp: SecurityProvider[IO]) <- SecurityProvider.forAsync[IO]
    implicit0(j: JsonSerializer[IO]) <- JsonSerializer.forSync[IO].asResource
    h = Hasher.forJson[IO]
    hs = HasherSelector.forSync(h, h)
  } yield (h, hs, sp)
  private val ownerAddress = Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ")

  private val dummyGovernanceValidation = new GovernanceValidations[IO] {
    override def l1Validations(
      rewardAllocationVoteUpdate: DataUpdates.RewardAllocationVoteUpdate
    ): IO[DataApplicationValidationErrorOr[Unit]] =
      ().validNec[DataApplicationValidationError].pure[IO]

    override def l0Validations(
      rewardAllocationVoteUpdate: Signed[DataUpdates.RewardAllocationVoteUpdate],
      state: AmmCalculatedState,
      lastSyncGlobalSnapshotEpochProgress: EpochProgress
    )(implicit sp: SecurityProvider[IO]): IO[Either[States.FailedCalculatedState, Signed[DataUpdates.RewardAllocationVoteUpdate]]] =
      rewardAllocationVoteUpdate.asRight.pure[IO]
  }

  private def buildAllocations(
    toAllocate: Map[Address, Map[String, Double]],
    allocationEpoch: Map[Address, Long]
  ): SortedMap[Address, UserAllocations] = {
    val allocationsMap =
      toAllocate.map {
        case (address, allocations) =>
          val allocationsSet =
            SortedSet.from(allocations.map {
              case (id, amount) =>
                val category = if (id.startsWith("val")) AllocationCategory.NodeOperator else AllocationCategory.LiquidityPool
                Allocation(AllocationId(id, category), Percentage.unsafeFrom(amount))
            })
          address -> UserAllocations(
            0,
            RewardAllocationVoteReference.empty,
            EpochProgress(NonNegLong.unsafeFrom(allocationEpoch(address))),
            allocationsSet
          )
      }

    SortedMap.from(allocationsMap)
  }

  test("Votes shall be frozen at the end of the month") { implicit res =>
    implicit val (h, hs, sp) = res
    val ammOnChainState = AmmOnChainState(SortedSet.empty, Seq.empty, None)
    val ammCalculatedState = AmmCalculatedState()
    val nextMonthEpochNumber = config.epochInfo.epochProgress1Month + 1
    val nextMonthEpoch = EpochProgress(NonNegLong.unsafeFrom(nextMonthEpochNumber))
    val currentMonthReference =
      MonthlyReference(EpochProgress(NonNegLong.unsafeFrom(1)), EpochProgress(NonNegLong.unsafeFrom(10)), NonNegLong.unsafeFrom(0))

    val addr1 = Address("DAG7coCMRPJah33MMcfAEZVeB1vYn3vDRe6WqeGU")
    val addr2 = Address("DAG0y4eLqhhXUafeE3mgBstezPTnr8L3tZjAtMWB")
    val addr3 = Address("DAG0DQPuvVThrHnz66S4V6cocrtpg59oesAWyRMb")
    val addr4 = Address("DAG07tqNLYW8jHU9emXcRTT3CfgCUoumwcLghopd")

    val userAllocations: SortedMap[Address, UserAllocations] =
      buildAllocations(
        Map(addr1 -> Map("lp1" -> 0.7, "lp2" -> 0.3), addr2 -> Map("lp3" -> 0.7, "lp2" -> 0.3)),
        Map(addr1 -> 1, addr2 -> 10, addr3 -> 0)
      )
    val clearedUserAllocations = userAllocations.map {
      case (address, allocation) => address -> allocation.copy(allocations = SortedSet.empty)
    }

    // addresses are participated in voting
    val actualVotingWeights: SortedMap[Address, VotingPower] = SortedMap(
      addr1 -> VotingPower(NonNegLong.unsafeFrom(1000), SortedSet.empty[VotingPowerInfo]),
      addr2 -> VotingPower(NonNegLong.unsafeFrom(4000), SortedSet.empty[VotingPowerInfo])
    )

    // addr3 doesn't participate in voting in past month thus shall not be frozen and taken into account at all
    val votingWeights = actualVotingWeights ++
      Map(
        addr3 -> VotingPower(NonNegLong.unsafeFrom(5000), SortedSet.empty[VotingPowerInfo]),
        addr4 -> VotingPower(NonNegLong.unsafeFrom(5000), SortedSet.empty[VotingPowerInfo])
      )

    val currentAllocations = Allocations(currentMonthReference, userAllocations, GovernanceVotingResult.empty)

    val state =
      DataState(ammOnChainState, ammCalculatedState)
        .focus(_.calculated.allocations)
        .replace(currentAllocations)
        .focus(_.calculated.votingPowers)
        .replace(votingWeights)

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

      governanceCombiner = GovernanceCombinerService.make[IO](config, dummyGovernanceValidation)
      res <- governanceCombiner.handleMonthExpiration(state, nextMonthEpoch)

      expectedAllocations = SortedMap(
        AllocationId("lp1", LiquidityPool) -> Percentage.unsafeFrom(0.14),
        AllocationId("lp2", LiquidityPool) -> Percentage.unsafeFrom(0.3),
        AllocationId("lp3", LiquidityPool) -> Percentage.unsafeFrom(0.56)
      )

      voteResult = GovernanceVotingResult(currentMonthReference, actualVotingWeights, expectedAllocations)
    } yield
      expect(res.calculated.allocations.usersAllocations == clearedUserAllocations) &&
        expect(res.calculated.allocations.frozenUsedUserVotes.monthlyReference == currentMonthReference) &&
        expect(res.calculated.allocations.frozenUsedUserVotes.votes == expectedAllocations) &&
        expect(res.calculated.allocations.frozenUsedUserVotes.votingPowerForAddresses == actualVotingWeights) &&
        expect(res.onChain.governanceVotingResult == voteResult.some)
  }
}
