package org.amm_metagraph.l0.genesisRewards

import cats.effect.IO

import io.constellationnetwork.schema.address.Address

import eu.timepit.refined.auto._
import org.amm_metagraph.l0.rewards.RewardsService
import weaver.SimpleIOSuite

object GenesisRewardsSpec extends SimpleIOSuite {
  test("Should return empty rewards for ordinal >= limit") {
    for {
      content <- RewardsService.buildGenesisRewards[IO](12L)
    } yield expect(content.isEmpty)
  }

  test("Should build rewards correctly for valid ordinal") {
    for {
      content <- RewardsService.buildGenesisRewards[IO](2L)
    } yield
      expect.all(
        content.nonEmpty
      )
  }

  test("Should distribute genesis rewards across ordinals correctly") {
    for {
      ordinal2 <- RewardsService.buildGenesisRewards[IO](2L)
      ordinal3 <- RewardsService.buildGenesisRewards[IO](3L)
      ordinal11 <- RewardsService.buildGenesisRewards[IO](11L) // Last valid ordinal
      ordinal12 <- RewardsService.buildGenesisRewards[IO](12L) // Should be empty
      ordinal13 <- RewardsService.buildGenesisRewards[IO](13L) // Should be empty
    } yield
      expect.all(
        ordinal2.nonEmpty,
        ordinal3.nonEmpty,
        ordinal11.nonEmpty,
        ordinal12.isEmpty,
        ordinal13.isEmpty,
        ordinal2.intersect(ordinal3).isEmpty
      )
  }

  test("Should build rewards correctly with specific values") {
    for {
      content <- RewardsService.buildGenesisRewards[IO](2L)
    } yield
      if (content.nonEmpty) {
        expect.all(
          content.nonEmpty,
          content.exists(_.destination == Address("DAG01S8hdo9BXJhY6yicJYmaxBo7ZDDXYDM68ZCW")),
          content.forall(_.amount.value.value > 0L)
        )
      } else {
        expect(false)
      }
  }

  test("Should handle CSV loading errors correctly") {
    for {
      result <- RewardsService.buildGenesisRewards[IO](-1L)
    } yield expect(result.isEmpty)
  }
}
