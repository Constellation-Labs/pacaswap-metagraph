package org.amm_metagraph.shared_data.pricing

import cats.effect.IO

import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.CurrencyId
import io.constellationnetwork.security.hash.Hash

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.{NonNegLong, PosLong}
import org.amm_metagraph.shared_data.Shared._
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.services.pricing.{LiquidityPoolOperations, PoolLogger}
import org.amm_metagraph.shared_data.types.DataUpdates.StakingUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.Staking.StakingReference
import org.amm_metagraph.shared_data.types.States.{FailedCalculatedState, StakingTokenInfo}
import org.amm_metagraph.shared_data.validations.Errors.StakingAmountTooSmall
import weaver.SimpleIOSuite

/** Regression tests for D2-01 (a dust stake that mints 0 LP shares donates the staker's tokens to incumbent LPs) and D2-02 (the unsafe
  * PosLong conversion throws and the top-level combine catch drops the whole ordinal's batch).
  *
  * The deterministic BigInt math + dust rejection is gated by `stakingShareMintFix` so a rolling upgrade does not fork: below the
  * activation epoch the legacy behavior is preserved byte-for-byte.
  */
object StakingShareMintSpec extends SimpleIOSuite {

  private val tokenAId: Option[CurrencyId] = Some(CurrencyId(sourceAddress))
  private val tokenBId: Option[CurrencyId] = Some(ammMetagraphIdAsCurrencyId)

  // A pool with a huge primary reserve (1e12) and only 1e8 total shares: a tiny deposit floors to 0 shares.
  private val tokenA = TokenInformation(tokenAId, PosLong.unsafeFrom(toFixedPoint(10000.0)))
  private val tokenB = TokenInformation(tokenBId, PosLong.unsafeFrom(toFixedPoint(10000.0)))
  private val owner = sourceAddress

  private val (_, lpState) = buildLiquidityPoolCalculatedState(tokenA, tokenB, owner)
  private val pool: LiquidityPool = lpState.confirmed.value.head._2

  private val activationEpoch = EpochProgress(NonNegLong.unsafeFrom(1L))
  private val activeConfig: ApplicationConfig =
    config.copy(activationEpochs = ApplicationConfig.ActivationEpochs(stakingShareMintFix = activationEpoch))

  private def stakingUpdate(amount: PosLong): StakingUpdate =
    StakingUpdate(
      ammMetagraphIdAsCurrencyId,
      sourceAddress,
      Hash.empty,
      Hash.empty,
      tokenAId,
      amount,
      tokenBId,
      StakingReference.empty,
      EpochProgress.MaxValue
    )

  private def ops(cfg: ApplicationConfig): IO[LiquidityPoolOperations[IO]] =
    PoolLogger.make[IO]("/dev/null").map(new LiquidityPoolOperations[IO](cfg, _))

  // 1 unit deposit -> floor(1 * 1e8 / 1e12) = 0 shares
  private val dust: PosLong = PosLong.unsafeFrom(1L)
  // 100-token deposit -> floor(1e10 * 1e8 / 1e12) = 1e6 shares
  private val normal: PosLong = PosLong.unsafeFrom(toFixedPoint(100.0))

  test("D2-01/D2-02 (active): a dust deposit is REJECTED as StakingAmountTooSmall, not absorbed with 0 shares") {
    ops(activeConfig).map { o =>
      val result = o.calculateStakingInfo(getFakeSignedUpdate(stakingUpdate(dust)), Hash.empty, pool, activationEpoch)
      matches(result) {
        case Left(FailedCalculatedState(_: StakingAmountTooSmall, _, _, _)) => success
      }
    }
  }

  test("active: a normal deposit mints >= 1 share and succeeds") {
    ops(activeConfig).map { o =>
      val result = o.calculateStakingInfo(getFakeSignedUpdate(stakingUpdate(normal)), Hash.empty, pool, activationEpoch)
      matches(result) {
        case Right(info: StakingTokenInfo) => expect(info.newlyIssuedShares >= 1L)
      }
    }
  }

  test("legacy (pre-activation): the SAME dust deposit is silently absorbed with 0 shares (the bug the fix removes)") {
    // Default config => stakingShareMintFix = MaxValue, so any real epoch is below activation.
    ops(config).map { o =>
      val result = o.calculateStakingInfo(getFakeSignedUpdate(stakingUpdate(dust)), Hash.empty, pool, EpochProgress.MinValue)
      matches(result) {
        case Right(info: StakingTokenInfo) => expect(info.newlyIssuedShares == 0L)
      }
    }
  }
}
