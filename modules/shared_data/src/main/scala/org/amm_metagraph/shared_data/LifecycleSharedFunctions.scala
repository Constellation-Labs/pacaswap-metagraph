package org.amm_metagraph.shared_data

import cats.data.{NonEmptyList, OptionT}
import cats.effect.Async
import cats.syntax.all._
import org.amm_metagraph.shared_data.Utils.toAddress
import org.amm_metagraph.shared_data.combiners.StakingCombiner.combineStaking
import org.amm_metagraph.shared_data.combiners.WithdrawCombiner.combineWithdraw
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, AmmUpdateProof, StakingUpdate, WithdrawUpdate}
import org.amm_metagraph.shared_data.types.States.{AmmCalculatedState, AmmOnChainState}
import org.amm_metagraph.shared_data.validations.Validations.{stakingValidationsL0, stakingValidationsL1, withdrawValidationsL0, withdrawValidationsL1}
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import org.tessellation.currency.dataApplication.{DataState, L0NodeContext}
import org.tessellation.ext.cats.syntax.next.catsSyntaxNext
import org.tessellation.schema.SnapshotOrdinal
import org.tessellation.security.SecurityProvider
import org.tessellation.security.signature.Signed
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object LifecycleSharedFunctions {
  def logger[F[_] : Async]: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("LifecycleSharedFunctions")

  def validateUpdate[F[_] : Async](
    update: AmmUpdate
  ): F[DataApplicationValidationErrorOr[Unit]] = update match {
    case stakingUpdate: StakingUpdate =>
      stakingValidationsL1(stakingUpdate)
    case withdrawUpdate: WithdrawUpdate =>
      withdrawValidationsL1(withdrawUpdate)
  }

  def validateData[F[_] : Async](
    oldState: DataState[AmmOnChainState, AmmCalculatedState],
    updates : NonEmptyList[Signed[AmmUpdate]]
  )(implicit context: L0NodeContext[F]): F[DataApplicationValidationErrorOr[Unit]] = {
    implicit val sp: SecurityProvider[F] = context.securityProvider
    updates.traverse { signedUpdate =>
      signedUpdate.value match {
        case stakingUpdate: StakingUpdate =>
          stakingValidationsL0(stakingUpdate, signedUpdate.proofs, oldState.calculated)
        case withdrawUpdate: WithdrawUpdate =>
          withdrawValidationsL0(withdrawUpdate, signedUpdate.proofs, oldState.calculated)
      }
    }.map(_.reduce)
  }

  def combine[F[_] : Async](
    oldState: DataState[AmmOnChainState, AmmCalculatedState],
    updates : List[Signed[AmmUpdate]]
  )(implicit context: L0NodeContext[F]): F[DataState[AmmOnChainState, AmmCalculatedState]] = {
    val newState = DataState(AmmOnChainState(List.empty), AmmCalculatedState(oldState.calculated.addresses))
    if (updates.isEmpty) {
      logger.info("Snapshot without any updates, updating the state to empty updates").as(newState)
    } else {
      implicit val sp: SecurityProvider[F] = context.securityProvider
      updates.foldLeftM(newState) { (acc, signedUpdate) =>
        for {
          address <- toAddress(signedUpdate.proofs.head)
          currentSnapshotOrdinal <- OptionT(context.getLastCurrencySnapshot)
            .map(_.ordinal.next)
            .getOrElseF {
              val message = "Could not get the ordinal from currency snapshot. lastCurrencySnapshot not found"
              logger.error(message) >> new Exception(message).raiseError[F, SnapshotOrdinal]
            }
          ammProof = AmmUpdateProof(signedUpdate.proofs.head.id.toString, signedUpdate.proofs.head.signature.value.toString)
          updatedState <- signedUpdate.value match {
            case stakingUpdate: StakingUpdate =>
              logger.info(s"Received a new staking update: $stakingUpdate").as(
                combineStaking(acc, stakingUpdate, address, ammProof, currentSnapshotOrdinal)
              )

            case withdrawUpdate: WithdrawUpdate =>
              logger.info(s"Received a new withdraw update: $withdrawUpdate").as(
                combineWithdraw(acc, withdrawUpdate, address, ammProof)
              )
          }
        } yield updatedState
      }
    }
  }
}