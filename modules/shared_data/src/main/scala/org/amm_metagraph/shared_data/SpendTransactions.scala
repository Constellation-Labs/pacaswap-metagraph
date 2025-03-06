package org.amm_metagraph.shared_data

import cats.syntax.all._

import scala.collection.immutable.SortedSet

import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact._
import io.constellationnetwork.schema.swap.{AllowSpend, CurrencyId, SwapAmount}
import io.constellationnetwork.security.Hashed
import io.constellationnetwork.security.hash.Hash

object SpendTransactions {

  def generateSpendActionWithoutInput(
    token: Option[CurrencyId],
    amount: SwapAmount,
    destination: Address
  ): SpendAction =
    SpendAction(
      SpendTransaction(
        none,
        token,
        amount,
        destination
      ),
      SpendTransaction(
        none,
        token,
        amount,
        destination
      )
    )

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

  def checkIfSpendActionsAcceptedInGl0(
    metagraphGeneratedSpendActionHashes: List[Hash],
    globalSyncSpendActionHashes: List[Hash]
  ) =
    metagraphGeneratedSpendActionHashes.forall { hash =>
      globalSyncSpendActionHashes.contains(hash)
    }

}
