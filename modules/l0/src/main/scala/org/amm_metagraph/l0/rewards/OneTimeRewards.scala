package org.amm_metagraph.l0.rewards

import cats.syntax.all._

import scala.collection.immutable.SortedSet

import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.{Address, DAGAddressRefined}
import io.constellationnetwork.schema.transaction.{RewardTransaction, TransactionAmount}

import eu.timepit.refined.auto._
import eu.timepit.refined.refineV
import eu.timepit.refined.types.all.{NonNegLong, PosLong}
import fs2.data.csv.{CellDecoder, DecoderError}

object OneTimeRewards {
  private case class OneTimeRewardConfig(
    ordinal: SnapshotOrdinal,
    description: String,
    rewards: SortedSet[RewardTransaction]
  )

  private def createReward(address: String, amount: Long): RewardTransaction = {
    val dagAddress = refineV[DAGAddressRefined](address.trim)
      .getOrElse(throw new IllegalArgumentException(s"Invalid DAG address: $address"))

    RewardTransaction(
      Address(dagAddress),
      TransactionAmount(PosLong.unsafeFrom(amount))
    )
  }

  private val rewardRegistry: List[OneTimeRewardConfig] = List(
    OneTimeRewardConfig(
      ordinal = SnapshotOrdinal(NonNegLong.unsafeFrom(140000L)),
      description = "Mint Stardust Wallet Tokens",
      rewards = SortedSet(
        createReward("DAG6qyCvhka9rX9SsAMouHmAoKmADuGW415anB59", 666296625863847L)
      )
    )
  )

  def loadOneTimeRewards(currentOrdinal: SnapshotOrdinal): SortedSet[RewardTransaction] =
    rewardRegistry
      .find(_.ordinal === currentOrdinal)
      .map(_.rewards)
      .getOrElse(SortedSet.empty[RewardTransaction])
}
