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

  def generateSpendActionWithoutAllowSpends(
    fromTokenAId: Option[CurrencyId],
    amountA: SwapAmount,
    fromTokenBId: Option[CurrencyId],
    amountB: SwapAmount,
    toDestination: Address,
    ammMetagraphId: CurrencyId
  ): SpendAction =
    SpendAction(
      NonEmptyList.of(
        SpendTransaction(
          none,
          fromTokenAId,
          amountA,
          ammMetagraphId.value,
          toDestination
        ),
        SpendTransaction(
          none,
          fromTokenBId,
          amountB,
          ammMetagraphId.value,
          toDestination
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
          hashedAllowSpend.currencyId,
          hashedAllowSpend.amount,
          hashedAllowSpend.source,
          hashedAllowSpend.destination
        ),
        SpendTransaction(
          none,
          metagraphGeneratedCurrencyId,
          SwapAmount(PosLong.from(metagraphGeneratedAmount.value.value).getOrElse(PosLong.MinValue)),
          ammMetagraphId,
          hashedAllowSpend.source
        )
      )
    )

  def generateSpendAction(
    hashedAllowSpendA: Hashed[AllowSpend],
    tokenAAmount: SwapAmount,
    hashedAllowSpendB: Hashed[AllowSpend],
    tokenBAmount: SwapAmount
  ): SpendAction =
    SpendAction(
      NonEmptyList.of(
        SpendTransaction(
          hashedAllowSpendA.hash.some,
          hashedAllowSpendA.currencyId,
          tokenAAmount,
          hashedAllowSpendA.source,
          hashedAllowSpendA.destination
        ),
        SpendTransaction(
          hashedAllowSpendB.hash.some,
          hashedAllowSpendB.currencyId,
          tokenBAmount,
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
