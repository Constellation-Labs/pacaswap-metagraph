package org.amm_metagraph.l0

import java.util.UUID

import cats.effect.{IO, Resource}
import cats.syntax.all._

import scala.collection.immutable.SortedSet
import scala.util.{Failure, Success}

import io.constellationnetwork.currency.dataApplication._
import io.constellationnetwork.currency.l0.CurrencyL0App
import io.constellationnetwork.currency.schema.currency.{CurrencyIncrementalSnapshot, CurrencySnapshotStateProof}
import io.constellationnetwork.ext.cats.effect.ResourceIO
import io.constellationnetwork.json.{JsonSerializer => JsonBrotliBinaryCodec}
import io.constellationnetwork.node.shared.domain.rewards.Rewards
import io.constellationnetwork.node.shared.snapshot.currency.CurrencySnapshotEvent
import io.constellationnetwork.schema.artifact.SharedArtifact
import io.constellationnetwork.schema.cluster.ClusterId
import io.constellationnetwork.schema.semver.{MetagraphVersion, TessellationVersion}
import io.constellationnetwork.schema.{SnapshotOrdinal, artifact}
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hasher, SecurityProvider}

import eu.timepit.refined.types.numeric.NonNegLong
import org.amm_metagraph.l0.BalanceAdjustmentLoader.loadBalanceAdjustments
import org.amm_metagraph.l0.SurplusSweepLoader.loadSweep
import org.amm_metagraph.l0.rewards.RewardsService
import org.amm_metagraph.shared_data.app.ApplicationConfigOps
import org.amm_metagraph.shared_data.calculated_state.CalculatedStateService
import org.amm_metagraph.shared_data.rewards.RewardCalculator
import org.amm_metagraph.shared_data.services.combiners._
import org.amm_metagraph.shared_data.services.combiners.operations._
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

  override def customArtifacts(
    lastCurrencySnapshot: Signed[CurrencyIncrementalSnapshot]
  ): Option[SortedSet[SharedArtifact]] =
    customArtifactsAt(lastCurrencySnapshot.ordinal.value.value + 1)

  private[l0] def customArtifactsAt(nextOrdinal: Long): Option[SortedSet[SharedArtifact]] = {
    val ordinalToPerformBalanceAdjustments1 = 109991L
    val ordinalToPerformBalanceAdjustments2 = 145000L
    val ordinalToPerformBalanceAdjustments3 = 472325L
    val ordinalToPerformBalanceAdjustments4 = 731647L
    // 731648 applies updated-pools-14.json; this is the snapshot after it.
    val ordinalToSweepUpsiderSurplus = 731649L
    val ordinalToRefundDagOverpayment = 731650L
    if (nextOrdinal == ordinalToPerformBalanceAdjustments1) {
      loadBalanceAdjustments("balance-adjustments.json") match {
        case Failure(_) => None
        case Success(adjustments) =>
          val artifactSet: SortedSet[SharedArtifact] = SortedSet(adjustments: _*)
          Some(artifactSet)
      }
    } else if (nextOrdinal == ordinalToPerformBalanceAdjustments2) {
      loadBalanceAdjustments("balance-adjustments-2.json") match {
        case Failure(_) => None
        case Success(adjustments) =>
          val artifactSet: SortedSet[SharedArtifact] = SortedSet(adjustments: _*)
          Some(artifactSet)
      }
    } else if (nextOrdinal == ordinalToPerformBalanceAdjustments3) {
      loadBalanceAdjustments("balance-adjustments-3.json") match {
        case Failure(_) => None
        case Success(adjustments) =>
          val artifactSet: SortedSet[SharedArtifact] = SortedSet(adjustments: _*)
          Some(artifactSet)
      }
    } else if (nextOrdinal == ordinalToPerformBalanceAdjustments4) {
      loadBalanceAdjustments("balance-adjustments-4.json") match {
        // At the remediation ordinal, emitting no artifacts would let calculated-state changes
        // proceed without the paired balance deductions. A packaging/resource failure must halt.
        case Failure(exception) => throw exception
        case Success(adjustments) =>
          val artifactSet: SortedSet[SharedArtifact] = SortedSet(adjustments: _*)
          Some(artifactSet)
      }
    } else if (nextOrdinal == ordinalToSweepUpsiderSurplus) {
      // The custody address has no private key, so a surplus sitting there cannot be moved by an
      // ordinary transfer. This SpendAction is the only mechanism. It leaves the pool's book
      // untouched, so The Upsider AI reaches reserve == wallet with no price movement.
      //
      // Fails closed for the same reason the deductions do: a resource problem must halt rather
      // than silently emit nothing. It is fail-safe the other way too - if the SpendAction never
      // settles the pool is merely over-backed, which is harmless and can never create a
      // shortfall.
      loadSweep("up-surplus-sweep.json") match {
        case Failure(exception) => throw exception
        case Success(spendAction) =>
          val artifactSet: SortedSet[SharedArtifact] = SortedSet[SharedArtifact](spendAction)
          Some(artifactSet)
      }
    } else if (nextOrdinal == ordinalToRefundDagOverpayment) {
      // The DAG leg of the PROT-1695 recovery was funded twice, by two different wallets. The book
      // counts it once, so the wallet ends over-backed by exactly that leg. This returns one of the
      // two payments and restores reserve == wallet without touching any pool's book.
      //
      // Fails closed for the same reason the others do. Over-backing is the harmless direction, so
      // if this never settles nothing is at risk; a silent no-op that we mistake for success is
      // worse than a halt.
      loadSweep("dag-overpayment-refund.json") match {
        case Failure(exception) => throw exception
        case Success(spendAction) =>
          val artifactSet: SortedSet[SharedArtifact] = SortedSet[SharedArtifact](spendAction)
          Some(artifactSet)
      }
    } else {
      None
    }
  }

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

    pricingService <- PricingService.make[IO](config, calculatedStateService).toResource
    governanceCombinerService = GovernanceCombinerService.make[IO](config, governanceValidations)
    liquidityPoolCombinerService = LiquidityPoolCombinerService.make[IO](config, liquidityPoolValidations, jsonBase64BinaryCodec)
    stakingCombinerService = StakingCombinerService.make[IO](config, pricingService, stakingValidations, jsonBase64BinaryCodec)
    swapCombinerService = SwapCombinerService.make[IO](config, pricingService, swapValidations, jsonBase64BinaryCodec)
    withdrawalCombinerService = WithdrawalCombinerService.make[IO](config, pricingService, withdrawalValidations, jsonBase64BinaryCodec)
    rewardsCalculator <- RewardCalculator.make[IO](config.rewards, config.epochInfo).toResource
    rewardsCombinerService = RewardsDistributionService
      .make[IO](rewardsCalculator, config.rewards, config.epochInfo, config.activationEpochs.rewardEpochCatchUp)
    rewardsWithdrawService = RewardsWithdrawService.make[IO](config.rewards, rewardWithdrawValidations, jsonBase64BinaryCodec)

    combinerService <- L0CombinerServiceFactory
      .make[IO](
        globalSnapshotsStorage,
        governanceCombinerService,
        liquidityPoolCombinerService,
        stakingCombinerService,
        swapCombinerService,
        withdrawalCombinerService,
        rewardsCombinerService,
        rewardsWithdrawService,
        config.activationEpochs.globalSyncDataIntegrity
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
