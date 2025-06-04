package org.amm_metagraph.shared_data

import java.security.KeyPair

import cats.Parallel
import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.currency.dataApplication.L0NodeContext
import io.constellationnetwork.currency.schema.currency.{CurrencyIncrementalSnapshot, CurrencySnapshot, CurrencySnapshotInfo}
import io.constellationnetwork.schema._
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact.SpendAction
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.{AllowSpend, CurrencyId}
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hashed, Hasher, SecurityProvider}

import monocle.syntax.all._

object DummyL0Context {
  def buildGlobalIncrementalSnapshot[F[_]: Parallel: Async: SecurityProvider](
    keyPair: KeyPair,
    gsEpochProgress: EpochProgress
  )(
    implicit hs: Hasher[F]
  ): F[Hashed[GlobalIncrementalSnapshot]] =
    GlobalIncrementalSnapshot
      .fromGlobalSnapshot[F](
        GlobalSnapshot.mkGenesis(Map.empty, gsEpochProgress)
      )
      .flatMap { globalIncrementalSnapshot =>
        Signed
          .forAsyncHasher[F, GlobalIncrementalSnapshot](globalIncrementalSnapshot, keyPair)
          .flatMap(_.toHashed[F])
      }

  def buildCurrencyIncrementalSnapshot[F[_]: Parallel: Async: SecurityProvider](
    keyPair: KeyPair
  )(
    implicit hs: Hasher[F]
  ): F[Hashed[CurrencyIncrementalSnapshot]] =
    CurrencyIncrementalSnapshot
      .fromCurrencySnapshot[F](
        CurrencySnapshot.mkGenesis(Map.empty, None)
      )
      .flatMap { currencyIncrementalSnapshot =>
        Signed
          .forAsyncHasher[F, CurrencyIncrementalSnapshot](currencyIncrementalSnapshot, keyPair)
          .flatMap(_.toHashed[F])
      }

  def buildGlobalSnapshotInfo(
    activeAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]]
  ): GlobalSnapshotInfo =
    GlobalSnapshotInfo(
      SortedMap.empty,
      SortedMap.empty,
      SortedMap.empty,
      SortedMap.empty,
      SortedMap.empty,
      activeAllowSpends.some,
      none,
      none,
      none,
      none,
      none,
      none,
      none,
      none,
      none,
      none,
      none
    )

  def buildCurrencySnapshotInfo(
    activeAllowSpends: SortedMap[Address, SortedSet[Signed[AllowSpend]]]
  ): CurrencySnapshotInfo =
    CurrencySnapshotInfo(
      lastTxRefs = SortedMap.empty,
      balances = SortedMap.empty,
      lastMessages = None,
      None,
      None,
      activeAllowSpends.some,
      None,
      None,
      None
    )

  def buildL0NodeContext[F[_]: Async: Parallel: Hasher: SecurityProvider](
    keyPair: KeyPair,
    gsAllowSpends: SortedMap[Option[Address], SortedMap[Address, SortedSet[Signed[AllowSpend]]]],
    gsEpochProgress: EpochProgress = EpochProgress.MinValue,
    gsSnapshotOrdinal: SnapshotOrdinal = SnapshotOrdinal.MinValue,
    csAllowSpends: SortedMap[Address, SortedSet[Signed[AllowSpend]]],
    csEpochProgress: EpochProgress = EpochProgress.MinValue,
    csSnapshotOrdinal: SnapshotOrdinal = SnapshotOrdinal.MinValue,
    ammMetagraphAddress: Address,
    spendActions: Option[SortedMap[Address, List[SpendAction]]] = None
  ): L0NodeContext[F] =
    new L0NodeContext[F] {
      def getLastCurrencySnapshot: F[Option[Hashed[CurrencyIncrementalSnapshot]]] = for {
        currencyIncrementalSnapshot <- buildCurrencyIncrementalSnapshot[F](keyPair)
        updated = currencyIncrementalSnapshot
          .focus(_.signed.value.epochProgress)
          .replace(csEpochProgress)
          .focus(_.signed.value.ordinal)
          .replace(csSnapshotOrdinal)
      } yield Some(updated)

      def getCurrencySnapshot(ordinal: SnapshotOrdinal): F[Option[Hashed[CurrencyIncrementalSnapshot]]] = ???
      def getLastCurrencySnapshotCombined: F[Option[(Hashed[CurrencyIncrementalSnapshot], CurrencySnapshotInfo)]] = for {
        currencyIncrementalSnapshot <- buildCurrencyIncrementalSnapshot[F](keyPair)
        updated = currencyIncrementalSnapshot
          .focus(_.signed.value.epochProgress)
          .replace(csEpochProgress)
          .focus(_.signed.value.ordinal)
          .replace(csSnapshotOrdinal)
        currencySnapshotInfo = buildCurrencySnapshotInfo(csAllowSpends)
      } yield Some((updated, currencySnapshotInfo))

      def securityProvider: SecurityProvider[F] = ???
      def getCurrencyId: F[CurrencyId] = CurrencyId(ammMetagraphAddress).pure[F]
      def getLastSynchronizedGlobalSnapshot: F[Option[Hashed[GlobalIncrementalSnapshot]]] = for {
        globalIncrementalSnapshot <- buildGlobalIncrementalSnapshot[F](keyPair, gsEpochProgress)
        updated = globalIncrementalSnapshot
          .focus(_.signed.value.ordinal)
          .replace(gsSnapshotOrdinal)
          .focus(_.signed.value.spendActions)
          .replace(spendActions)
      } yield Some(updated)

      def getLastSynchronizedGlobalSnapshotCombined: F[Option[(Hashed[GlobalIncrementalSnapshot], GlobalSnapshotInfo)]] = for {
        globalIncrementalSnapshot <- buildGlobalIncrementalSnapshot[F](keyPair, gsEpochProgress)
        updated = globalIncrementalSnapshot
          .focus(_.signed.value.ordinal)
          .replace(gsSnapshotOrdinal)
          .focus(_.signed.value.spendActions)
          .replace(spendActions)
        globalSnapshotInfo = buildGlobalSnapshotInfo(gsAllowSpends)
      } yield Some((updated, globalSnapshotInfo))
    }
}
