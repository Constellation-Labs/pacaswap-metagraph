package org.amm_metagraph.shared_data

import io.constellationnetwork.schema.artifact._
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.AllowSpend
import io.constellationnetwork.security.Hashed
import org.amm_metagraph.shared_data.types.States.AmmCalculatedState

import scala.collection.immutable.SortedSet

object SpendTransactions {

  def generateSpendAction(
    hashedAllowSpend: Hashed[AllowSpend]
  ): SpendAction = {
    SpendAction(
      PendingSpendTransaction(
        SpendTransactionFee(hashedAllowSpend.fee.value),
        EpochProgress.MaxValue,
        hashedAllowSpend.hash,
        hashedAllowSpend.currency,
        hashedAllowSpend.amount
      ),
      PendingSpendTransaction(
        SpendTransactionFee(hashedAllowSpend.fee.value),
        EpochProgress.MaxValue,
        hashedAllowSpend.hash,
        hashedAllowSpend.currency,
        hashedAllowSpend.amount
      )
    )
  }
  
  def getCalculatedStateSpendTransactions(
    calculatedState: AmmCalculatedState
  ): (SortedSet[PendingSpendTransaction], SortedSet[ConcludedSpendTransaction]) =
    getSpendTransactions(calculatedState.spendTransactions)

  def getCombinedSpendTransactions(
    artifacts: SortedSet[SharedArtifact]
  ): (SortedSet[PendingSpendTransaction], SortedSet[ConcludedSpendTransaction]) = {
    val spendActions = artifacts.collect {
      case transaction: SpendAction => transaction
    }
    val spendTransactions = spendActions.flatMap(spendAction => SortedSet(spendAction.input, spendAction.output))

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
