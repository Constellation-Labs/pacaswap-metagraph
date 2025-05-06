package com.my.amm_metagraph.shared_data

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.L1NodeContext
import io.constellationnetwork.currency.schema.currency.{CurrencyIncrementalSnapshot, CurrencySnapshotInfo}
import io.constellationnetwork.schema.GlobalIncrementalSnapshot
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.swap.CurrencyId
import io.constellationnetwork.security.{Hashed, SecurityProvider}

object DummyL1Context {

  def buildL1NodeContext[F[_]: Async](
    ammMetagraphAddress: Address
  ): L1NodeContext[F] =
    new L1NodeContext[F] {
      def getLastCurrencySnapshot: F[Option[Hashed[CurrencyIncrementalSnapshot]]] = ???
      def getLastCurrencySnapshotCombined: F[Option[(Hashed[CurrencyIncrementalSnapshot], CurrencySnapshotInfo)]] = ???
      def securityProvider: SecurityProvider[F] = ???
      def getCurrencyId: F[CurrencyId] = CurrencyId(ammMetagraphAddress).pure[F]
      def getLastGlobalSnapshot: F[Option[Hashed[GlobalIncrementalSnapshot]]] = ???
    }
}
