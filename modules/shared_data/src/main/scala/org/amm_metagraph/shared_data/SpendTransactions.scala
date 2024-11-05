package org.amm_metagraph.shared_data

import scala.collection.immutable.SortedSet

import io.constellationnetwork.schema.artifact._

import org.amm_metagraph.shared_data.types.States.AmmCalculatedState

object SpendTransactions {
  def getCalculatedStateSpendTransactions(
    calculatedState: AmmCalculatedState
  ): (SortedSet[PendingSpendTransaction], SortedSet[ConcludedSpendTransaction]) =
    getSpendTransactions(calculatedState.spendTransactions)

  def getCombinedSpendTransactions(
    artifacts: SortedSet[SharedArtifact]
  ): (SortedSet[PendingSpendTransaction], SortedSet[ConcludedSpendTransaction]) = {
    val spendTransactions = artifacts.collect {
      case transaction: SpendTransaction => transaction
    }
    getSpendTransactions(spendTransactions)
  }

  private def getSpendTransactions(
    spendTransactions: SortedSet[SpendTransaction]
  ): (SortedSet[PendingSpendTransaction], SortedSet[ConcludedSpendTransaction]) = {
    val (pending, concluded) = spendTransactions.partition {
      case _: PendingSpendTransaction   => true
      case _: ConcludedSpendTransaction => false
    }

    (
      pending.collect { case t: PendingSpendTransaction => t },
      concluded.collect { case t: ConcludedSpendTransaction => t }
    )
  }
}
