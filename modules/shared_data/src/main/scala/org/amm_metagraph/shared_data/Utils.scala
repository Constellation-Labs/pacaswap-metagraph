package org.amm_metagraph.shared_data

import cats.effect.Async
import cats.syntax.applicativeError.catsSyntaxApplicativeErrorId
import eu.timepit.refined.types.numeric.PosLong
import org.amm_metagraph.shared_data.types.DataUpdates.StakingUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool.{LiquidityPool, TokenInformation}
import org.tessellation.schema.address.Address
import org.tessellation.security.SecurityProvider
import org.tessellation.security.signature.signature.SignatureProof

import scala.collection.SortedSet


object Utils {
  def toAddress[F[_] : Async : SecurityProvider](proof: SignatureProof): F[Address] =
    proof.id.toAddress

  def toTokenAmountFormat(
    balance: Double
  ): Long = {
    (balance * 10e7).toLong
  }

  def buildLiquidityPoolUniqueIdentifier[F[_] : Async](maybeTokenAId: Option[Address], maybeTokenBId: Option[Address]): F[String] =
    (maybeTokenAId, maybeTokenBId) match {
      case (None, None) =>
        new IllegalArgumentException("You should provide at least one currency token identifier").raiseError[F, String]
      case _ =>
        Async[F].delay {
          val tokenAId = maybeTokenAId.fold("")(address => address.value.value)
          val tokenBId = maybeTokenBId.fold("")(address => address.value.value)
          val sortedSet = SortedSet(tokenAId, tokenBId)
          sortedSet.mkString("-")
        }
    }

  def getUpdatedTokenInformation(
    stakingUpdate: StakingUpdate,
    liquidityPool: LiquidityPool
  ): (TokenInformation, TokenInformation, Long) = {
    val primaryToken = if (stakingUpdate.primaryTokenId == liquidityPool.tokenA.identifier) liquidityPool.tokenA else liquidityPool.tokenB
    val pairToken = if (stakingUpdate.pairTokenId == liquidityPool.tokenA.identifier) liquidityPool.tokenA else liquidityPool.tokenB

    val currentPrimaryTokenAmount = primaryToken.amount.value
    val currentPairTokenAmount = pairToken.amount.value

    val incomingPrimaryAmount = stakingUpdate.primaryTokenAmount.value
    val incomingPairAmount = (incomingPrimaryAmount * currentPairTokenAmount) / currentPrimaryTokenAmount // Calculate equivalent pair needed to maintain the invariant

    val liquidityMinted = math.min(incomingPrimaryAmount * liquidityPool.totalLiquidity / primaryToken.amount.value, incomingPairAmount * liquidityPool.totalLiquidity / pairToken.amount.value)

    (
      primaryToken.copy(amount = incomingPrimaryAmount.toPosLongUnsafe),
      pairToken.copy(amount = incomingPairAmount.toPosLongUnsafe),
      liquidityMinted
    )
  }

  implicit class PosLongOps(value: Long) {
    def toPosLongUnsafe: PosLong =
      PosLong.unsafeFrom(value)
  }
}