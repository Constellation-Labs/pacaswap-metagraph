package org.amm_metagraph.shared_data.validations

import cats.data.Validated.Valid
import cats.effect.{IO, Resource}

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.ext.cats.effect.ResourceIO
import io.constellationnetwork.json.JsonSerializer
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.security.key.ops.PublicKeyOps
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hasher, KeyPairGenerator, SecurityProvider}

import eu.timepit.refined.types.all.{NonNegLong, PosLong}
import org.amm_metagraph.shared_data.Shared.{ammMetagraphIdAsCurrencyId, config}
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, RewardAllocationVoteUpdate}
import org.amm_metagraph.shared_data.types.Governance.{RewardAllocationVoteReference, VotingPower}
import org.amm_metagraph.shared_data.types.States.AmmCalculatedState
import org.amm_metagraph.shared_data.types.codecs.{HasherSelector, JsonWithBase64BinaryCodec}
import weaver.MutableIOSuite

object GovernanceValidationTest extends MutableIOSuite {
  type Res = (Hasher[IO], HasherSelector[IO], SecurityProvider[IO])

  override def sharedResource: Resource[IO, Res] = for {
    implicit0(sp: SecurityProvider[IO]) <- SecurityProvider.forAsync[IO]
    implicit0(j: JsonSerializer[IO]) <- JsonSerializer.forSync[IO].asResource
    h = Hasher.forJson[IO]
    hs = HasherSelector.forSync(h, h)
  } yield (h, hs, sp)

  private val overflowWeights = Seq(
    "NodeValidators" -> PosLong.unsafeFrom(Long.MaxValue),
    "NodeValidators" -> PosLong.unsafeFrom(Long.MaxValue),
    "NodeValidators" -> PosLong.unsafeFrom(2L)
  )

  test("overflow-crafted governance weights are normalizable at both L1 and L0 without Long division") { implicit res =>
    implicit val (h, hs, sp) = res

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      source = keyPair.getPublic.toAddress
      update = RewardAllocationVoteUpdate(
        ammMetagraphIdAsCurrencyId,
        source,
        RewardAllocationVoteReference.empty,
        overflowWeights
      )
      signedUpdate <- Signed.forAsyncHasher[IO, RewardAllocationVoteUpdate](update, keyPair)
      codec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      validations = GovernanceValidations.make[IO](config, codec)
      state = AmmCalculatedState(
        votingPowers = SortedMap(source -> VotingPower(NonNegLong.unsafeFrom(1L), SortedSet.empty))
      )
      l1Result <- validations.l1Validations(update)
      l0Result <- validations.l0Validations(signedUpdate, state, EpochProgress.MinValue)
    } yield expect.all(
      l1Result == Valid(()),
      l0Result == Right(signedUpdate)
    )
  }

  test("empty governance weights retain their historical L1 and L0 behavior") { implicit res =>
    implicit val (h, hs, sp) = res

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      source = keyPair.getPublic.toAddress
      update = RewardAllocationVoteUpdate(
        ammMetagraphIdAsCurrencyId,
        source,
        RewardAllocationVoteReference.empty,
        Seq.empty
      )
      signedUpdate <- Signed.forAsyncHasher[IO, RewardAllocationVoteUpdate](update, keyPair)
      codec <- JsonWithBase64BinaryCodec.forSync[IO, AmmUpdate]
      validations = GovernanceValidations.make[IO](config, codec)
      state = AmmCalculatedState(
        votingPowers = SortedMap(source -> VotingPower(NonNegLong.unsafeFrom(1L), SortedSet.empty))
      )
      l1Result <- validations.l1Validations(update)
      l0Result <- validations.l0Validations(signedUpdate, state, EpochProgress.MinValue)
    } yield expect.all(
      l1Result == Valid(()),
      l0Result == Right(signedUpdate)
    )
  }
}
