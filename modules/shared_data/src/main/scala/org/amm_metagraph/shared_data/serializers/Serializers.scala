package org.amm_metagraph.shared_data.serializers

import io.circe.Encoder
import io.circe.syntax.EncoderOps
import org.amm_metagraph.shared_data.types.DataUpdates.AmmUpdate
import org.amm_metagraph.shared_data.types.States.{AmmCalculatedState, AmmOnChainState}
import org.tessellation.currency.dataApplication.DataUpdate
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationBlock
import org.tessellation.security.signature.Signed

import java.nio.charset.StandardCharsets

object Serializers {
  private def serialize[A: Encoder](
    serializableData: A
  ): Array[Byte] =
    serializableData.asJson.deepDropNullValues.noSpaces.getBytes(StandardCharsets.UTF_8)

  def serializeUpdate(
    update: AmmUpdate
  ): Array[Byte] =
    serialize[AmmUpdate](update)

  def serializeState(
    state: AmmOnChainState
  ): Array[Byte] =
    serialize[AmmOnChainState](state)

  def serializeBlock(
    block: Signed[DataApplicationBlock]
  )(implicit e: Encoder[DataUpdate]): Array[Byte] =
    serialize[Signed[DataApplicationBlock]](block)

  def serializeCalculatedState(
    calculatedState: AmmCalculatedState
  ): Array[Byte] =
    serialize[AmmCalculatedState](calculatedState)
}