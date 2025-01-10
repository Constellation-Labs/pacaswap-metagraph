package org.amm_metagraph.shared_data.validations

import cats.data.{NonEmptyList, OptionT}
import cats.effect.kernel.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.currency.dataApplication.{DataState, L0NodeContext}
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hasher, SecurityProvider}

import org.amm_metagraph.shared_data.types.DataUpdates._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.validations.GovernanceValidations.{rewardAllocationValidationsL0, rewardAllocationValidationsL1}
import org.amm_metagraph.shared_data.validations.LiquidityPoolValidations.{liquidityPoolValidationsL0, liquidityPoolValidationsL1}
import org.amm_metagraph.shared_data.validations.StakingValidations.{stakingValidationsL0, stakingValidationsL1}
import org.amm_metagraph.shared_data.validations.SwapValidations.{swapValidationsL0, swapValidationsL1}

trait ValidationService[F[_]] {
  def validateUpdate(
    update: AmmUpdate
  ): F[DataApplicationValidationErrorOr[Unit]]

  def validateData(
    signedUpdates: NonEmptyList[Signed[AmmUpdate]],
    state: DataState[AmmOnChainState, AmmCalculatedState]
  )(implicit context: L0NodeContext[F]): F[DataApplicationValidationErrorOr[Unit]]
}

object ValidationService {
  def make[F[_]: Async: SecurityProvider: Hasher]: F[ValidationService[F]] = Async[F].delay {
    new ValidationService[F] {
      private def validateL1(update: AmmUpdate): F[DataApplicationValidationErrorOr[Unit]] = update match {
        case stakingUpdate: StakingUpdate                           => stakingValidationsL1(stakingUpdate)
        case liquidityPoolUpdate: LiquidityPoolUpdate               => liquidityPoolValidationsL1(liquidityPoolUpdate)
        case swapUpdate: SwapUpdate                                 => swapValidationsL1(swapUpdate)
        case rewardAllocationVoteUpdate: RewardAllocationVoteUpdate => rewardAllocationValidationsL1(rewardAllocationVoteUpdate)
      }

      private def validateL0(
        signedUpdate: Signed[AmmUpdate],
        state: AmmCalculatedState
      )(implicit context: L0NodeContext[F]): F[DataApplicationValidationErrorOr[Unit]] =
        for {
          lastSyncGlobalSnapshot <- OptionT(context.getLastSynchronizedGlobalSnapshot).getOrRaise(
            new IllegalStateException("lastSyncGlobalSnapshot unavailable")
          )
          result <- signedUpdate.value match {
            case stakingUpdate: StakingUpdate             => stakingValidationsL0(Signed(stakingUpdate, signedUpdate.proofs), state)
            case liquidityPoolUpdate: LiquidityPoolUpdate => liquidityPoolValidationsL0(liquidityPoolUpdate, state)
            case swapUpdate: SwapUpdate                   => swapValidationsL0(swapUpdate, state)
            case rewardAllocationVoteUpdate: RewardAllocationVoteUpdate =>
              rewardAllocationValidationsL0(
                Signed(rewardAllocationVoteUpdate, signedUpdate.proofs),
                state,
                lastSyncGlobalSnapshot.epochProgress
              )
          }
        } yield result

      override def validateUpdate(
        update: AmmUpdate
      ): F[DataApplicationValidationErrorOr[Unit]] = validateL1(update)

      override def validateData(
        signedUpdates: NonEmptyList[Signed[AmmUpdate]],
        state: DataState[AmmOnChainState, AmmCalculatedState]
      )(implicit context: L0NodeContext[F]): F[DataApplicationValidationErrorOr[Unit]] = signedUpdates
        .traverse(validateL0(_, state.calculated))
        .map(_.combineAll)
    }
  }
}
