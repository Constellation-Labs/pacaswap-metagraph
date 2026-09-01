package org.amm_metagraph.shared_data

import cats.effect.{IO, Resource}
import cats.syntax.all._

import scala.collection.immutable.SortedMap

import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.currency.schema.globalSnapshotSync.GlobalSyncView
import io.constellationnetwork.ext.cats.effect.ResourceIO
import io.constellationnetwork.json.JsonSerializer
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.artifact.SpendAction
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.{CurrencyId, SwapAmount}
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.{Hasher, KeyPairGenerator, SecurityProvider}

import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.{NonNegLong, PosLong}
import io.circe.parser.decode
import io.circe.syntax._
import org.amm_metagraph.shared_data.Shared.getFakeSignedUpdate
import org.amm_metagraph.shared_data.services.combiners.SpendActionEvidence
import org.amm_metagraph.shared_data.types.DataUpdates.SwapUpdate
import org.amm_metagraph.shared_data.types.States.{AmmCalculatedState, AmmOnChainState, PendingSpendAction}
import org.amm_metagraph.shared_data.types.Swap.SwapReference
import weaver.MutableIOSuite

/** Pins the two SpendActions incorrectly rolled back at currency ordinal 741789 against their signed GL0 JSON representation.
  *
  * Paca's pending-state JSON omits `None` fields while the GL0 artifact JSON renders them as null. Both must decode to the same protocol
  * value and therefore have the same current hash; otherwise an exact action present in the scanned range would look absent.
  */
object IncidentSpendActionEvidenceSpec extends MutableIOSuite {

  type Res = (Hasher[IO], SecurityProvider[IO])

  override def sharedResource: Resource[IO, Res] = for {
    sp <- SecurityProvider.forAsync[IO]
    implicit0(serializer: JsonSerializer[IO]) <- JsonSerializer.forSync[IO].asResource
  } yield (Hasher.forJson[IO], sp)

  private val pending700ae9df =
    """{"spendTransactions":[{"allowSpendRef":"da1905ce97b6a1b4ee12c9209ac1cf3602207753a5cf48fc1f0baca3c9644951","amount":1933266495245,"currencyId":"DAG7X5idd4aLfp4XC6WQdG1eDfR3LGPVEwtUUB2W","destination":"DAG7X5idd4aLfp4XC6WQdG1eDfR3LGPVEwtUUB2W","source":"DAG832uu4PTXa1rjjHVcCnMA3FrYGuS4hVKuEqjm"},{"amount":432254469442,"destination":"DAG832uu4PTXa1rjjHVcCnMA3FrYGuS4hVKuEqjm","source":"DAG7X5idd4aLfp4XC6WQdG1eDfR3LGPVEwtUUB2W"}]}"""

  private val global700ae9df =
    """{"spendTransactions":[{"allowSpendRef":"da1905ce97b6a1b4ee12c9209ac1cf3602207753a5cf48fc1f0baca3c9644951","amount":1933266495245,"currencyId":"DAG7X5idd4aLfp4XC6WQdG1eDfR3LGPVEwtUUB2W","destination":"DAG7X5idd4aLfp4XC6WQdG1eDfR3LGPVEwtUUB2W","source":"DAG832uu4PTXa1rjjHVcCnMA3FrYGuS4hVKuEqjm"},{"allowSpendRef":null,"amount":432254469442,"currencyId":null,"destination":"DAG832uu4PTXa1rjjHVcCnMA3FrYGuS4hVKuEqjm","source":"DAG7X5idd4aLfp4XC6WQdG1eDfR3LGPVEwtUUB2W"}]}"""

  private val pending96e16834 =
    """{"spendTransactions":[{"allowSpendRef":"afa0f2c7df764aa4b6e6d5b15f85b9aff7f246fdba5582ac665e06d1723c28eb","amount":21999052080,"currencyId":"DAG7X5idd4aLfp4XC6WQdG1eDfR3LGPVEwtUUB2W","destination":"DAG7X5idd4aLfp4XC6WQdG1eDfR3LGPVEwtUUB2W","source":"DAG5NSQA1FPW6Nehd1rC9hqDW6eZt2NbK7fPCMnP"},{"amount":4916836003,"destination":"DAG5NSQA1FPW6Nehd1rC9hqDW6eZt2NbK7fPCMnP","source":"DAG7X5idd4aLfp4XC6WQdG1eDfR3LGPVEwtUUB2W"}]}"""

  private val global96e16834 =
    """{"spendTransactions":[{"allowSpendRef":"afa0f2c7df764aa4b6e6d5b15f85b9aff7f246fdba5582ac665e06d1723c28eb","amount":21999052080,"currencyId":"DAG7X5idd4aLfp4XC6WQdG1eDfR3LGPVEwtUUB2W","destination":"DAG7X5idd4aLfp4XC6WQdG1eDfR3LGPVEwtUUB2W","source":"DAG5NSQA1FPW6Nehd1rC9hqDW6eZt2NbK7fPCMnP"},{"allowSpendRef":null,"amount":4916836003,"currencyId":null,"destination":"DAG5NSQA1FPW6Nehd1rC9hqDW6eZt2NbK7fPCMnP","source":"DAG7X5idd4aLfp4XC6WQdG1eDfR3LGPVEwtUUB2W"}]}"""

  private def decodeAction(value: String): SpendAction =
    decode[SpendAction](value).fold(throw _, identity)

  test("incident SpendActions decode and hash identically across pending and GL0 JSON") {
    case (hasher, _) =>
      val pairs = List(
        decodeAction(pending700ae9df) -> decodeAction(global700ae9df),
        decodeAction(pending96e16834) -> decodeAction(global96e16834)
      )

      pairs.traverse {
        case (pending, global) =>
          for {
            pendingHash <- hasher.hash(pending)
            globalHash <- hasher.hash(global)
          } yield expect.all(pending == global, pendingHash == globalHash)
      }.map(_.combineAll)
  }

  test("pending actions written before evidence provenance existed still decode fail-closed") { _ =>
    val update = getFakeSignedUpdate(
      SwapUpdate(
        CurrencyId(Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9z5")),
        Address("DAG6t89ps7G8bfS2WuTcNUAy9Pg8xWqiEHjrrLAZ"),
        None,
        Some(CurrencyId(Address("DAG7X5idd4aLfp4XC6WQdG1eDfR3LGPVEwtUUB2W"))),
        Hash.empty,
        SwapAmount(PosLong.unsafeFrom(1L)),
        SwapAmount(PosLong.unsafeFrom(1L)),
        None,
        EpochProgress.MaxValue,
        SwapReference.empty
      )
    )
    val pending = PendingSpendAction(
      update,
      Hash.empty,
      decodeAction(pending700ae9df),
      generatedAfterGlobalOrdinal = Some(SnapshotOrdinal(NonNegLong.unsafeFrom(6855978L)))
    )
    val legacyJson = pending.asJson.mapObject(_.remove("generatedAfterGlobalOrdinal")).noSpaces

    IO.pure(decode[PendingSpendAction[SwapUpdate]](legacyJson)).map { decoded =>
      expect.all(decoded.isRight, decoded.exists(_.generatedAfterGlobalOrdinal.isEmpty))
    }
  }

  test("new SpendActions bind provenance to the exact signed predecessor snapshot") { implicit resources =>
    implicit val (hasher, securityProvider) = resources
    val predecessor = SnapshotOrdinal(NonNegLong.unsafeFrom(749999L))
    val globalViewOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(6855978L))
    val metagraphId = Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9z5")
    val state = DataState(
      AmmOnChainState.empty,
      AmmCalculatedState(lastProcessedCurrencyOrdinal = predecessor.some)
    )

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      implicit0(context: L0NodeContext[IO]) = DummyL0Context.buildL0NodeContext[IO](
        keyPair = keyPair,
        gsAllowSpends = SortedMap.empty,
        csAllowSpends = SortedMap.empty,
        csSnapshotOrdinal = predecessor,
        csGlobalSyncView = GlobalSyncView(globalViewOrdinal, Hash.empty, EpochProgress.MinValue).some,
        ammMetagraphAddress = metagraphId
      )
      provenance <- SpendActionEvidence.generatedAfterGlobalOrdinal[IO](state)
    } yield expect(provenance.contains(globalViewOrdinal))
  }

  test("new SpendActions fail closed when the signed predecessor is unavailable") { implicit resources =>
    implicit val (hasher, securityProvider) = resources
    val predecessor = SnapshotOrdinal(NonNegLong.unsafeFrom(749999L))
    val metagraphId = Address("DAG0KpQNqMsED4FC5grhFCBWG8iwU8Gm6aLhB9z5")
    val state = DataState(
      AmmOnChainState.empty,
      AmmCalculatedState(lastProcessedCurrencyOrdinal = predecessor.some)
    )

    for {
      keyPair <- KeyPairGenerator.makeKeyPair[IO]
      implicit0(context: L0NodeContext[IO]) = DummyL0Context.buildL0NodeContext[IO](
        keyPair = keyPair,
        gsAllowSpends = SortedMap.empty,
        csAllowSpends = SortedMap.empty,
        csSnapshotOrdinal = SnapshotOrdinal(NonNegLong.unsafeFrom(749998L)),
        ammMetagraphAddress = metagraphId
      )
      result <- SpendActionEvidence.generatedAfterGlobalOrdinal[IO](state).attempt
    } yield expect(result.isLeft)
  }
}
