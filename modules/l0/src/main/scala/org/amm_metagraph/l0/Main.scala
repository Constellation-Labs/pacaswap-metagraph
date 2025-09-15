package org.amm_metagraph.l0

import java.util.UUID

import cats.effect.{IO, Resource}
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication._
import io.constellationnetwork.currency.l0.CurrencyL0App
import io.constellationnetwork.currency.schema.currency.{CurrencyIncrementalSnapshot, CurrencySnapshotStateProof}
import io.constellationnetwork.ext.cats.effect.ResourceIO
import io.constellationnetwork.json.{JsonSerializer => JsonBrotliBinaryCodec}
import io.constellationnetwork.node.shared.domain.rewards.Rewards
import io.constellationnetwork.node.shared.snapshot.currency.CurrencySnapshotEvent
import io.constellationnetwork.schema.cluster.ClusterId
import io.constellationnetwork.schema.semver.{MetagraphVersion, TessellationVersion}
import io.constellationnetwork.security.{Hasher, SecurityProvider}

import org.amm_metagraph.l0.rewards.RewardsService
import org.amm_metagraph.shared_data.app.ApplicationConfigOps
import org.amm_metagraph.shared_data.calculated_state.CalculatedStateService
import org.amm_metagraph.shared_data.rewards.RewardCalculator
import org.amm_metagraph.shared_data.services.combiners._
import org.amm_metagraph.shared_data.services.pricing.PricingService
import org.amm_metagraph.shared_data.storages.GlobalSnapshotsStorage
import org.amm_metagraph.shared_data.types.DataUpdates.AmmUpdate
import org.amm_metagraph.shared_data.types.codecs.{HasherSelector, JsonBinaryCodec, JsonWithBase64BinaryCodec}
import org.amm_metagraph.shared_data.validations._
import org.typelevel.log4cats.SelfAwareStructuredLogger

object Main
    extends CurrencyL0App(
      "currency-l0",
      "currency L0 node",
      ClusterId(UUID.fromString("517c3a05-9219-471b-a54c-21b7d72f4ae5")),
      tessellationVersion = TessellationVersion.unsafeFrom(io.constellationnetwork.BuildInfo.version),
      metagraphVersion = MetagraphVersion.unsafeFrom(org.amm_metagraph.l0.BuildInfo.version)
    ) {

  implicit val implicitLogger: SelfAwareStructuredLogger[IO] = logger
  override def dataApplication: Option[Resource[IO, BaseDataApplicationL0Service[IO]]] = (for {
    implicit0(sp: SecurityProvider[IO]) <- SecurityProvider.forAsync[IO]
    jsonBrotliBinaryCodec <- JsonBrotliBinaryCodec.forSync[IO].asResource
    jsonBase64BinaryCodec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate].asResource
    jsonBinaryCodec <- JsonBinaryCodec.forSync[IO].asResource
    hasherBrotli = {
      implicit val serializer: JsonBrotliBinaryCodec[IO] = jsonBrotliBinaryCodec
      Hasher.forJson[IO]
    }
    hasherCurrent = {
      implicit val serializer: JsonBrotliBinaryCodec[IO] = jsonBinaryCodec
      Hasher.forJson[IO]
    }
    implicit0(hasherSelector: HasherSelector[IO]) = HasherSelector.forSync(hasherBrotli, hasherCurrent)
    config <- ApplicationConfigOps.readDefault[IO].asResource
    _ <- logger.info(show"Start L0 with config: $config").asResource
    calculatedStateService <- CalculatedStateService.make[IO].asResource
    globalSnapshotsStorage: GlobalSnapshotsStorage[IO] <- GlobalSnapshotsStorage.make[IO].asResource

    liquidityPoolValidations = LiquidityPoolValidations.make[IO](config, jsonBase64BinaryCodec)
    stakingValidations = StakingValidations.make[IO](config, jsonBase64BinaryCodec)
    swapValidations = SwapValidations.make[IO](config, jsonBase64BinaryCodec)
    withdrawalValidations = WithdrawalValidations.make[IO](config, jsonBase64BinaryCodec)
    governanceValidations = GovernanceValidations.make[IO](config, jsonBase64BinaryCodec)
    rewardWithdrawValidations = RewardWithdrawValidations.make[IO](config, jsonBase64BinaryCodec)
    validationService = ValidationService.make[IO](
      config,
      liquidityPoolValidations,
      stakingValidations,
      swapValidations,
      withdrawalValidations,
      governanceValidations,
      rewardWithdrawValidations
    )

    pricingService = PricingService.make[IO](config, calculatedStateService)
    governanceCombinerService = GovernanceCombinerService.make[IO](config, governanceValidations)
    liquidityPoolCombinerService = LiquidityPoolCombinerService.make[IO](config, liquidityPoolValidations, jsonBase64BinaryCodec)
    stakingCombinerService = StakingCombinerService.make[IO](config, pricingService, stakingValidations, jsonBase64BinaryCodec)
    swapCombinerService = SwapCombinerService.make[IO](config, pricingService, swapValidations, jsonBase64BinaryCodec)
    withdrawalCombinerService = WithdrawalCombinerService.make[IO](config, pricingService, withdrawalValidations, jsonBase64BinaryCodec)
    rewardsCalculator <- RewardCalculator.make[IO](config.rewards, config.epochInfo).toResource
    rewardsCombinerService = RewardsDistributionService.make[IO](rewardsCalculator, config.rewards, config.epochInfo)
    rewardsWithdrawService = RewardsWithdrawService.make[IO](config.rewards, rewardWithdrawValidations, jsonBase64BinaryCodec)

    combinerService <- L0CombinerService
      .make[IO](
        globalSnapshotsStorage,
        governanceCombinerService,
        liquidityPoolCombinerService,
        stakingCombinerService,
        swapCombinerService,
        withdrawalCombinerService,
        rewardsCombinerService,
        rewardsWithdrawService
      )
      .toResource

    l0Service = MetagraphL0Service
      .make[IO](
        calculatedStateService,
        validationService,
        combinerService,
        jsonBase64BinaryCodec,
        jsonBinaryCodec,
        globalSnapshotsStorage,
        pricingService,
        config
      )

  } yield l0Service).some

  override def rewards(
    implicit sp: SecurityProvider[IO]
  ): Option[Rewards[IO, CurrencySnapshotStateProof, CurrencyIncrementalSnapshot, CurrencySnapshotEvent]] =
    RewardsService.make[IO].some
}
