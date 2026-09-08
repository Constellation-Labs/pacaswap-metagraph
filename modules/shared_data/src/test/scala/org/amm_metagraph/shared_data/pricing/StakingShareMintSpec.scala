package org.amm_metagraph.shared_data.pricing

import cats.effect.IO

import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.{CurrencyId, SwapAmount}
import io.constellationnetwork.security.hash.Hash

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.{NonNegLong, PosLong}
import org.amm_metagraph.shared_data.ProtocolActivation
import org.amm_metagraph.shared_data.Shared._
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.refined._
import org.amm_metagraph.shared_data.services.pricing.{LiquidityPoolOperations, PoolLogger}
import org.amm_metagraph.shared_data.types.DataUpdates.StakingUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.Staking.StakingReference
import org.amm_metagraph.shared_data.types.States.{FailedCalculatedState, StakingTokenInfo}
import org.amm_metagraph.shared_data.validations.Errors.{ArithmeticError, StakingAmountTooSmall}
import weaver.SimpleIOSuite

/** Regression tests for D2-01 (a dust stake that mints 0 LP shares donates the staker's tokens to incumbent LPs) and D2-02 (the unsafe
  * PosLong conversion throws and the top-level combine catch drops the whole ordinal's batch).
  *
  * The deterministic BigInt math + dust rejection is gated on the currency ordinal, not on `stakingShareMintFix`. That config epoch is
  * compared against the GLOBAL epoch, which mainnet passed long ago, so it would be active for every already-signed snapshot and replaying
  * them would compute different share issuance. `ProtocolActivation.reserveAccountingFixes` (731647, the first ordinal after the stall) is
  * the only gate that reproduces what was actually signed.
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

  private val anyEpoch = EpochProgress(NonNegLong.unsafeFrom(1L))
  private val activeOrdinal = ProtocolActivation.reserveAccountingFixes
  private val beforeOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(ProtocolActivation.reserveAccountingFixes.value.value - 1L))

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

  // This witness reaches 2^64 + 448,384 on the pair leg. The legacy BigInt.toLong narrowing therefore charged
  // only 448,384 while issuing 184,467,440,737,100 shares. Crucially, all of the subsequently updated reserves and
  // share totals remain representable, so the vulnerable pool mutation succeeds instead of failing downstream.
  private val overflowPrimaryReserve = PosLong.unsafeFrom(10_000_000L)
  private val overflowPairReserve = PosLong.unsafeFrom(10_000_000_000_000L)
  private val overflowPrimary = TokenInformation(tokenAId, overflowPrimaryReserve)
  private val overflowPair = TokenInformation(tokenBId, overflowPairReserve)
  private val (_, overflowLpState) = buildLiquidityPoolCalculatedState(overflowPrimary, overflowPair, owner)
  private val overflowPool: LiquidityPool = overflowLpState.confirmed.value.head._2
  private val overflowDeposit = PosLong.unsafeFrom(18_446_744_073_710L)
  private val exactPairAmount =
    (BigInt(overflowDeposit.value) * BigInt(overflowPairReserve.value)) / BigInt(overflowPrimaryReserve.value)
  private val wrappedPairAmount = PosLong.unsafeFrom(exactPairAmount.toLong)
  private val exactIssuedShares =
    (BigInt(overflowDeposit.value) * BigInt(overflowPool.poolShares.totalShares.value)) / BigInt(overflowPrimaryReserve.value)

  // A separate fixture reaches the minted-share guard while keeping the proportional pair leg inside Long.
  private val shareOverflowPrimary = TokenInformation(tokenAId, PosLong.unsafeFrom(1L))
  private val shareOverflowPair = TokenInformation(tokenBId, PosLong.unsafeFrom(1L))
  private val (_, shareOverflowLpState) = buildLiquidityPoolCalculatedState(shareOverflowPrimary, shareOverflowPair, owner)
  private val shareOverflowPool: LiquidityPool = shareOverflowLpState.confirmed.value.head._2
  private val shareOverflowDeposit = PosLong.unsafeFrom(Long.MaxValue - 1L)

  test("D2-01/D2-02 (active): a dust deposit is REJECTED as StakingAmountTooSmall, not absorbed with 0 shares") {
    ops(config).map { o =>
      val result = o.calculateStakingInfo(getFakeSignedUpdate(stakingUpdate(dust)), Hash.empty, pool, anyEpoch, activeOrdinal)
      matches(result) {
        case Left(FailedCalculatedState(_: StakingAmountTooSmall, _, _, _)) => success
      }
    }
  }

  test("active: a normal deposit mints >= 1 share and succeeds") {
    ops(config).map { o =>
      val result = o.calculateStakingInfo(getFakeSignedUpdate(stakingUpdate(normal)), Hash.empty, pool, anyEpoch, activeOrdinal)
      matches(result) {
        case Right(info: StakingTokenInfo) => expect(info.newlyIssuedShares >= 1L)
      }
    }
  }

  test("active: reject a pair-leg overflow whose legacy wrapped result could complete the pool mutation") {
    val signedUpdate = getFakeSignedUpdate(stakingUpdate(overflowDeposit))
    val vulnerableInfo = StakingTokenInfo(
      overflowPrimary.copy(amount = overflowDeposit),
      overflowPair.copy(amount = wrappedPairAmount),
      SwapAmount(wrappedPairAmount),
      exactIssuedShares.toLong
    )

    ops(config).flatMap { o =>
      val rejected = o.calculateStakingInfo(signedUpdate, Hash.empty, overflowPool, anyEpoch, activeOrdinal)

      o.updatePoolForStaking(overflowPool, signedUpdate, Hash.empty, owner, vulnerableInfo, anyEpoch, activeOrdinal).map { legacyMutation =>
        expect.all(
          exactPairAmount == (BigInt(1) << 64) + BigInt(448_384L),
          wrappedPairAmount.value == 448_384L,
          exactIssuedShares == BigInt(184_467_440_737_100L),
          legacyMutation.isRight
        ) && matches(rejected) {
          case Left(FailedCalculatedState(_: ArithmeticError, _, _, _)) => success
        }
      }
    }
  }

  test("active: reject an exact minted-share count outside Long even when the pair leg is representable") {
    ops(config).map { o =>
      val result = o.calculateStakingInfo(
        getFakeSignedUpdate(stakingUpdate(shareOverflowDeposit)),
        Hash.empty,
        shareOverflowPool,
        anyEpoch,
        activeOrdinal
      )

      matches(result) {
        case Left(FailedCalculatedState(_: ArithmeticError, _, _, _)) => success
      }
    }
  }

  test("legacy (pre-activation): the SAME dust deposit is silently absorbed with 0 shares (the bug the fix removes)") {
    // One ordinal below the activation: the pre-731647 behaviour every signed snapshot was produced under.
    ops(config).map { o =>
      val result = o.calculateStakingInfo(getFakeSignedUpdate(stakingUpdate(dust)), Hash.empty, pool, anyEpoch, beforeOrdinal)
      matches(result) {
        case Right(info: StakingTokenInfo) => expect(info.newlyIssuedShares == 0L)
      }
    }
  }
}
