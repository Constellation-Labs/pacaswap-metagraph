package com.my.project_template.l0

import cats.effect.{IO, Resource}
import io.constellationnetwork.currency.dataApplication._
import io.constellationnetwork.currency.l0.CurrencyL0App
import io.constellationnetwork.currency.schema.currency.{CurrencyIncrementalSnapshot, CurrencySnapshotStateProof}
import io.constellationnetwork.node.shared.domain.rewards.Rewards
import io.constellationnetwork.node.shared.snapshot.currency.CurrencySnapshotEvent
import io.constellationnetwork.schema.cluster.ClusterId
import io.constellationnetwork.schema.semver.{MetagraphVersion, TessellationVersion}
import io.constellationnetwork.security.SecurityProvider

import java.util.UUID

object Main
  extends CurrencyL0App(
    "currency-l0",
    "currency L0 node",
    ClusterId(UUID.fromString("517c3a05-9219-471b-a54c-21b7d72f4ae5")),
    tessellationVersion = TessellationVersion.unsafeFrom("1.0.0"),
    metagraphVersion = MetagraphVersion.unsafeFrom("1.0.0")
  ) {

  override def dataApplication: Option[Resource[IO, BaseDataApplicationL0Service[IO]]] =
    None

  override def rewards(implicit sp: SecurityProvider[IO]): Option[Rewards[IO, CurrencySnapshotStateProof, CurrencyIncrementalSnapshot, CurrencySnapshotEvent]] =
    None
}
