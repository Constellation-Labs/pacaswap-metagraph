package org.amm_metagraph.shared_data

import cats.syntax.all._

import scala.collection.immutable.SortedSet

import io.constellationnetwork.schema.artifact._
import io.constellationnetwork.schema.swap.AllowSpend
import io.constellationnetwork.security.Hashed

object SpendTransactions {

  def generateSpendAction(
    hashedAllowSpend: Hashed[AllowSpend]
  ): SpendAction =
    SpendAction(
      SpendTransaction(
        hashedAllowSpend.hash.some,
        hashedAllowSpend.currency,
        hashedAllowSpend.amount,
        hashedAllowSpend.destination
      ),
      SpendTransaction(
        none,
        hashedAllowSpend.currency,
        hashedAllowSpend.amount,
        hashedAllowSpend.destination
      )
    )

  def getCombinedSpendTransactions(
    artifacts: SortedSet[SharedArtifact]
  ): SortedSet[SpendTransaction] = {
    val spendActions = artifacts.collect {
      case transaction: SpendAction => transaction
    }
    spendActions.flatMap(spendAction => SortedSet(spendAction.input, spendAction.output))
  }
}
