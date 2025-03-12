package org.amm_metagraph.shared_data

import cats.data.NonEmptyList
import cats.syntax.all._

import scala.collection.immutable.SortedSet

import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact._
import io.constellationnetwork.schema.balance.Amount
import io.constellationnetwork.schema.swap.{AllowSpend, CurrencyId, SwapAmount}
import io.constellationnetwork.security.Hashed
import io.constellationnetwork.security.hash.Hash

import eu.timepit.refined.types.numeric.PosLong

object SpendTransactions {

  def generateSpendActionWithoutInput(
    token: Option[CurrencyId],
    amount: SwapAmount,
    destination: Address,
    ammMetagraphId: Address
  ): SpendAction =
    SpendAction(
      NonEmptyList.of(
        SpendTransaction(
          none,
          token,
          amount,
          ammMetagraphId,
          destination
        ),
        SpendTransaction(
          none,
          token,
          amount,
          ammMetagraphId,
          destination
        )
      )
    )

  def generateSpendAction(
    hashedAllowSpend: Hashed[AllowSpend],
    metagraphGeneratedCurrencyId: Option[CurrencyId],
    metagraphGeneratedAmount: Amount,
    ammMetagraphId: Address
  ): SpendAction =
    SpendAction(
      NonEmptyList.of(
        SpendTransaction(
          hashedAllowSpend.hash.some,
          hashedAllowSpend.currency,
          hashedAllowSpend.amount,
          hashedAllowSpend.source,
          hashedAllowSpend.destination
        ),
        SpendTransaction(
          none,
          metagraphGeneratedCurrencyId,
          SwapAmount(PosLong.from(metagraphGeneratedAmount.value.value).getOrElse(PosLong.MinValue)),
          ammMetagraphId,
          hashedAllowSpend.destination
        )
      )
    )

  def generateSpendAction(
    hashedAllowSpendA: Hashed[AllowSpend],
    hashedAllowSpendB: Hashed[AllowSpend]
  ): SpendAction =
    SpendAction(
      NonEmptyList.of(
        SpendTransaction(
          hashedAllowSpendA.hash.some,
          hashedAllowSpendA.currency,
          hashedAllowSpendA.amount,
          hashedAllowSpendA.source,
          hashedAllowSpendA.destination
        ),
        SpendTransaction(
          hashedAllowSpendB.hash.some,
          hashedAllowSpendB.currency,
          hashedAllowSpendB.amount,
          hashedAllowSpendB.source,
          hashedAllowSpendB.destination
        )
      )
    )

  def getCombinedSpendTransactions(
    artifacts: SortedSet[SharedArtifact]
  ): SortedSet[SpendTransaction] = {
    val spendActions = artifacts.collect {
      case transaction: SpendAction => transaction
    }
    spendActions.flatMap(spendAction => spendAction.spendTransactions.toList.to(SortedSet))
  }

  def checkIfSpendActionAcceptedInGl0(
    metagraphGeneratedSpendActionHash: Hash,
    globalSyncSpendActionHashes: List[Hash]
  ) =
    globalSyncSpendActionHashes.contains(metagraphGeneratedSpendActionHash)

}
