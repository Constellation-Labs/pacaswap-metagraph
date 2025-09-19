package org.amm_metagraph.shared_data.loaders

import scala.io.Source
import scala.util.Try

import io.constellationnetwork.schema.address.Address

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import eu.timepit.refined.types.numeric.PosLong
import io.circe.parser._
import io.circe.refined._
import org.amm_metagraph.shared_data.types.LiquidityPool.{PoolShares, TokenInformation}

@derive(encoder, decoder)
case class LiquidityPoolFromJson(
  poolId: String,
  tokenA: TokenInformation,
  tokenB: TokenInformation,
  owner: Address,
  k: BigInt,
  poolShares: PoolShares
)

object LiquidityPoolLoader {
  def loadPools(resourcePath: String): Try[Map[String, LiquidityPoolFromJson]] =
    Try {
      val source = Source.fromResource(resourcePath)
      val jsonString =
        try
          source.mkString
        finally
          source.close()

      decode[Map[String, LiquidityPoolFromJson]](jsonString) match {
        case Right(pools) => pools
        case Left(error)  => throw new RuntimeException(s"JSON parsing failed: $error")
      }
    }
}
