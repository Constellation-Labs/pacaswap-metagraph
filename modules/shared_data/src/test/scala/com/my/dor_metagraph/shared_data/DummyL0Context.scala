package com.my.dor_metagraph.shared_data

import java.security.KeyPair

import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.currency.dataApplication.L0NodeContext
import io.constellationnetwork.currency.schema.currency
import io.constellationnetwork.schema._
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.AllowSpend
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hashed, Hasher, SecurityProvider}

import eu.timepit.refined.auto._

object DummyL0Context {
  val metagraphAddress = Address("DAG88yethVdWM44eq5riNB65XF3rfE3rGFJN15Ks")

  def buildGlobalIncrementalSnapshot[F[_]: Async: SecurityProvider](
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

  def buildGlobalSnapshotInfo(activeAllowSpends: SortedMap[Address, SortedSet[Signed[AllowSpend]]]): GlobalSnapshotInfo =
    GlobalSnapshotInfo(
      SortedMap.empty,
      SortedMap.empty,
      SortedMap.empty,
      SortedMap.empty,
      SortedMap.empty,
      SortedMap(metagraphAddress -> activeAllowSpends)
    )

  def buildL0NodeContext[F[_]: Async: Hasher: SecurityProvider](
    keyPair: KeyPair,
    allowSpends: SortedMap[Address, SortedSet[Signed[AllowSpend]]],
    gsEpochProgress: EpochProgress = EpochProgress.MinValue
  ): L0NodeContext[F] =
    new L0NodeContext[F] {
      override def getLastSynchronizedGlobalSnapshot: F[Option[Hashed[GlobalIncrementalSnapshot]]] = ???

      override def getLastSynchronizedGlobalSnapshotCombined: F[Option[(Hashed[GlobalIncrementalSnapshot], GlobalSnapshotInfo)]] = for {
        globalIncrementalSnapshot <- buildGlobalIncrementalSnapshot[F](keyPair, gsEpochProgress)
        globalSnapshotInfo = buildGlobalSnapshotInfo(allowSpends)
      } yield Some((globalIncrementalSnapshot, globalSnapshotInfo))

      override def getLastCurrencySnapshot: F[Option[Hashed[currency.CurrencyIncrementalSnapshot]]] = ???

      override def getCurrencySnapshot(ordinal: SnapshotOrdinal): F[Option[Hashed[currency.CurrencyIncrementalSnapshot]]] = ???

      override def getLastCurrencySnapshotCombined
        : F[Option[(Hashed[currency.CurrencyIncrementalSnapshot], currency.CurrencySnapshotInfo)]] = ???

      override def securityProvider: SecurityProvider[F] = ???

      override def getMetagraphId: F[Address] = metagraphAddress.pure[F]
    }

}
