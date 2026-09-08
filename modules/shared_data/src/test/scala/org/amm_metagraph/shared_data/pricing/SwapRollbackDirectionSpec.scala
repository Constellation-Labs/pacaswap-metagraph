package org.amm_metagraph.shared_data.pricing

import cats.effect.IO
import cats.syntax.all._

import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.{CurrencyId, SwapAmount}
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.{NonNegLong, PosLong}
import org.amm_metagraph.shared_data.ProtocolActivation
import org.amm_metagraph.shared_data.Shared._
import org.amm_metagraph.shared_data.services.pricing.models.PoolBalanceChange
import org.amm_metagraph.shared_data.services.pricing.{PoolLogger, RollbackOperations}
import org.amm_metagraph.shared_data.types.DataUpdates.SwapUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool._
import org.amm_metagraph.shared_data.types.Swap.SwapReference
import org.amm_metagraph.shared_data.validations.Errors.ArithmeticError
import weaver.SimpleIOSuite

/** A failed swap must put the pool back exactly where it was, whichever side of the pair it sold.
  *
  * Before `ProtocolActivation.rollbackDirectionFix` the rollback only reversed swaps from token A into token B; a swap from token B into
  * token A returned the pool unchanged as a success. That is how the USDC.dag pool came to carry a phantom 50 USDC.dag at currency ordinal
  * 747127. The pre-activation cases below pin that behaviour so history replays; the post-activation cases pin the fix.
  */
object SwapRollbackDirectionSpec extends SimpleIOSuite {

  private def ord(v: Long): SnapshotOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(v))
  private val BEFORE = ord(ProtocolActivation.rollbackDirectionFix.value.value - 1)
  private val ACTIVE = ProtocolActivation.rollbackDirectionFix

  private val silentLogger: PoolLogger[IO] = new PoolLogger[IO] {
    def logBalanceChange(change: PoolBalanceChange): IO[Unit] = IO.unit
    def logPoolOperation(
      operation: String,
      beforePool: LiquidityPool,
      afterPool: LiquidityPool,
      epochProgress: Option[EpochProgress],
      updateHash: Option[Hash],
      address: Option[Address],
      additionalInfo: Map[String, String]
    ): IO[Unit] = IO.unit
  }

  private val rollback = new RollbackOperations[IO](config, silentLogger)

  // The live USDC.dag pool: token A is native DAG (no identifier), token B is USDC.dag.
  private val usdc = CurrencyId(Address("DAG0S16WDgdAvh8VvroR6MWLdjmHYdzAF5S181xh")).some
  private val dag: Option[CurrencyId] = None
  private val owner = Address("DAG62QdFnvW8xX3uGmo6F3yB2CT5i25hZoVmN6za")

  private def pool(dagAmount: Long, usdcAmount: Long): LiquidityPool = {
    val (_, state) = buildLiquidityPoolCalculatedState(
      TokenInformation(dag, PosLong.unsafeFrom(dagAmount)),
      TokenInformation(usdc, PosLong.unsafeFrom(usdcAmount)),
      owner
    )
    state.confirmed.value.values.head
  }

  private def swap(from: Option[CurrencyId], to: Option[CurrencyId], amountIn: Long): Signed[SwapUpdate] =
    getFakeSignedUpdate(
      SwapUpdate(
        ammMetagraphIdAsCurrencyId,
        sourceAddress,
        from,
        to,
        Hash.empty,
        SwapAmount(PosLong.unsafeFrom(amountIn)),
        SwapAmount(PosLong.unsafeFrom(1L)),
        none,
        EpochProgress.MaxValue,
        SwapReference.empty
      )
    )

  private def run(update: Signed[SwapUpdate], after: LiquidityPool, amountIn: Long, netReceived: Long, at: SnapshotOrdinal) =
    rollback.rollbackSwap(
      update,
      Hash.empty,
      EpochProgress.MinValue,
      after,
      SwapAmount(PosLong.unsafeFrom(amountIn)),
      SwapAmount(PosLong.unsafeFrom(netReceived)),
      ammMetagraphIdAsCurrencyId,
      at
    )

  // The incident figures: 50 USDC.dag sold for 7,036.36031393 DAG, booked at 747126 and failed at 747127.
  private val usdcIn = 5000000000L
  private val dagOut = 703636031393L
  private val originalDag = 289435420387451L
  private val originalUsdc = 1986030900105L
  private val afterUsdcToDag = pool(originalDag - dagOut, originalUsdc + usdcIn)

  pureTest("the gate is a fresh ordinal, later than every earlier activation") {
    expect.all(
      ProtocolActivation.rollbackDirectionFix.value.value == 752000L,
      ProtocolActivation.rollbackDirectionFix.value.value > ProtocolActivation.evidenceCompletenessFirst.value.value,
      !ProtocolActivation.rollbackDirectionFixActive(SnapshotOrdinal.MinValue),
      !ProtocolActivation.rollbackDirectionFixActive(BEFORE),
      ProtocolActivation.rollbackDirectionFixActive(ACTIVE)
    )
  }

  test("before activation a token B -> token A rollback leaves the pool unchanged, as history recorded it") {
    run(swap(usdc, dag, usdcIn), afterUsdcToDag, usdcIn, dagOut, BEFORE).map { result =>
      expect(result.isRight, s"legacy rollback must not fail, got $result")
        .and(expect(result.exists(_.tokenA.amount == afterUsdcToDag.tokenA.amount), "legacy rollback left DAG untouched"))
        .and(expect(result.exists(_.tokenB.amount == afterUsdcToDag.tokenB.amount), "legacy rollback left USDC.dag untouched"))
    }
  }

  test("from activation a token B -> token A rollback restores both reserves and k") {
    run(swap(usdc, dag, usdcIn), afterUsdcToDag, usdcIn, dagOut, ACTIVE).map { result =>
      expect(result.isRight, s"rollback must succeed, got $result")
        .and(expect(result.exists(_.tokenA.amount.value == originalDag), s"DAG reserve should return to $originalDag"))
        .and(expect(result.exists(_.tokenB.amount.value == originalUsdc), s"USDC.dag reserve should return to $originalUsdc"))
        .and(expect(result.exists(_.k == BigInt(originalDag) * BigInt(originalUsdc)), "k should be the product of the restored reserves"))
    }
  }

  test("from activation a token A -> token B rollback still restores both reserves") {
    val dagIn = 703636031393L
    val usdcOut = 4900000000L
    val after = pool(originalDag + dagIn, originalUsdc - usdcOut)
    run(swap(dag, usdc, dagIn), after, dagIn, usdcOut, ACTIVE).map { result =>
      expect(result.isRight, s"rollback must succeed, got $result")
        .and(expect(result.exists(_.tokenA.amount.value == originalDag), s"DAG reserve should return to $originalDag"))
        .and(expect(result.exists(_.tokenB.amount.value == originalUsdc), s"USDC.dag reserve should return to $originalUsdc"))
    }
  }

  test("from activation a swap whose pair does not match the pool is refused rather than silently ignored") {
    val other = CurrencyId(Address("DAG0CyySf35ftDQDQBnd1bdQ9aPyUdacMghpnCuM")).some
    run(swap(other, dag, usdcIn), afterUsdcToDag, usdcIn, dagOut, ACTIVE).map { result =>
      expect(result.isLeft, "a pool that matches neither side of the swap cannot be rolled back").and(
        expect(
          result.swap.exists(_.reason.isInstanceOf[ArithmeticError]),
          s"the refusal should be an ArithmeticError, got $result"
        )
      )
    }
  }

  test("from activation a rollback that would take a reserve below one datum is refused") {
    val tiny = pool(1L, originalUsdc + usdcIn)
    run(swap(usdc, dag, usdcIn), tiny, usdcIn, dagOut, ACTIVE).map { result =>
      expect(result.isRight, "adding back the bought side is always safe")
    } *> run(swap(usdc, dag, usdcIn), pool(originalDag, usdcIn), usdcIn, dagOut, ACTIVE).map { result =>
      expect(result.isLeft, "removing the whole sold side would leave zero, which is not a valid reserve")
    }
  }
}
