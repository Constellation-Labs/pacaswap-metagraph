package org.amm_metagraph.shared_data.loaders

import scala.io.Source
import scala.util.Try

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import io.circe.parser._
import io.circe.refined._
import org.amm_metagraph.shared_data.types.LiquidityPool.TokenInformation

/** Reserves without the share ledger. LiquidityPoolFromJson carries poolShares and every historical updated-pools-*.json replaces them,
  * which is right when the shares themselves drifted. The fee transaction bug never touched shares, so restoring the reserves has to leave
  * them where they are.
  */
@derive(encoder, decoder)
case class PoolReservesFromJson(
  poolId: String,
  tokenA: TokenInformation,
  tokenB: TokenInformation,
  k: BigInt
)

object PoolReservesLoader {
  def loadReserves(resourcePath: String): Try[Map[String, PoolReservesFromJson]] =
    Try {
      val source = Source.fromResource(resourcePath)
      val jsonString =
        try
          source.mkString
        finally
          source.close()

      decode[Map[String, PoolReservesFromJson]](jsonString) match {
        case Right(pools) => pools
        case Left(error)  => throw new RuntimeException(s"JSON parsing failed: $error")
      }
    }
}
