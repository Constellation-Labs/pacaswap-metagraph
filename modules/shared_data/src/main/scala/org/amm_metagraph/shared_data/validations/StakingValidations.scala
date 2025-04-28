package org.amm_metagraph.shared_data.validations

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.{AllowSpend, CurrencyId}
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hashed, SecurityProvider}

import eu.timepit.refined.types.numeric.NonNegLong
import org.amm_metagraph.shared_data.AllowSpends.getAllAllowSpendsInUseFromState
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.types.DataUpdates.StakingUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool.{LiquidityPool, buildLiquidityPoolUniqueIdentifier, getConfirmedLiquidityPools}
import org.amm_metagraph.shared_data.types.Staking._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.validations.Errors._
import org.amm_metagraph.shared_data.validations.SharedValidations._

trait StakingValidations[F[_]] {
  def l1Validations(
    stakingUpdate: StakingUpdate
  ): F[DataApplicationValidationErrorOr[Unit]]

  def l0Validations(
    signedStakingUpdate: Signed[StakingUpdate],
    state: AmmCalculatedState
  )(implicit sp: SecurityProvider[F]): F[DataApplicationValidationErrorOr[Unit]]

  def combinerContextualValidations(
    oldState: AmmCalculatedState,
    signedUpdate: Signed[StakingUpdate],
    lastSyncGlobalEpochProgress: EpochProgress,
    confirmedStakings: Set[StakingCalculatedStateAddress],
    pendingStakings: Set[Signed[StakingUpdate]],
    isNewUpdate: Boolean
  ): Either[FailedCalculatedState, Signed[StakingUpdate]]

  def combinerValidations(
    oldState: AmmCalculatedState,
    signedUpdate: Signed[StakingUpdate],
    lastSyncGlobalEpochProgress: EpochProgress,
    confirmedStakings: Set[StakingCalculatedStateAddress],
    pendingStakings: Set[Signed[StakingUpdate]],
    isNewUpdate: Boolean,
    currencyId: CurrencyId,
    tokenInformation: StakingTokenInformation,
    allowSpendTokenA: Hashed[AllowSpend],
    allowSpendTokenB: Hashed[AllowSpend]
  ): Either[FailedCalculatedState, Signed[StakingUpdate]]
}

object StakingValidations {
  def make[F[_]: Async](
    applicationConfig: ApplicationConfig
  ): StakingValidations[F] = new StakingValidations[F] {
    def l1Validations(
      stakingUpdate: StakingUpdate
    ): F[DataApplicationValidationErrorOr[Unit]] = Async[F].delay {
      validateIfTokenIdsAreTheSame(stakingUpdate.tokenAId, stakingUpdate.tokenBId)
    }

    def l0Validations(
      signedStakingUpdate: Signed[StakingUpdate],
      state: AmmCalculatedState
    )(implicit sp: SecurityProvider[F]): F[DataApplicationValidationErrorOr[Unit]] = {
      val stakingUpdate = signedStakingUpdate.value

      val stakingCalculatedState = getStakingCalculatedState(state)
      val liquidityPoolsCalculatedState = getConfirmedLiquidityPools(state)

      for {
        l1Validations <- l1Validations(stakingUpdate)
        signatures <- signatureValidations(signedStakingUpdate, signedStakingUpdate.source)
        sourceAddress = signedStakingUpdate.source

        transactionAlreadyExists = validateIfTransactionAlreadyExists(
          stakingUpdate,
          stakingCalculatedState.confirmed.value.getOrElse(sourceAddress, Set.empty),
          stakingCalculatedState.getPendingUpdates
        )

        liquidityPoolExists <- validateIfLiquidityPoolExists(
          stakingUpdate,
          liquidityPoolsCalculatedState
        )

        allAllowSpendsInUse = getAllAllowSpendsInUseFromState(state)

        tokenAAllowSpendIsDuplicated = validateIfAllowSpendsAreDuplicated(
          stakingUpdate.tokenAAllowSpend,
          allAllowSpendsInUse
        )
        tokenBAllowSpendIsDuplicated = validateIfAllowSpendsAreDuplicated(
          stakingUpdate.tokenBAllowSpend,
          allAllowSpendsInUse
        )

        lastRef = lastRefValidation(stakingCalculatedState, signedStakingUpdate, sourceAddress)

      } yield
        l1Validations
          .productR(signatures)
          .productR(transactionAlreadyExists)
          .productR(liquidityPoolExists)
          .productR(tokenAAllowSpendIsDuplicated)
          .productR(tokenBAllowSpendIsDuplicated)
          .productR(lastRef)
    }

    def combinerContextualValidations(
      oldState: AmmCalculatedState,
      signedUpdate: Signed[StakingUpdate],
      lastSyncGlobalEpochProgress: EpochProgress,
      confirmedStakings: Set[StakingCalculatedStateAddress],
      pendingStakings: Set[Signed[StakingUpdate]],
      isNewUpdate: Boolean
    ): Either[FailedCalculatedState, Signed[StakingUpdate]] = {
      val allAllowSpendsInUse = getAllAllowSpendsInUseFromState(oldState)

      val tokenAAllowSpendIsDuplicated = validateIfAllowSpendsAreDuplicated(
        signedUpdate.tokenAAllowSpend,
        allAllowSpendsInUse
      )
      val tokenBAllowSpendIsDuplicated = validateIfAllowSpendsAreDuplicated(
        signedUpdate.tokenBAllowSpend,
        allAllowSpendsInUse
      )

      val isDuplicated = confirmedStakings.exists(staking =>
        staking.tokenAAllowSpend === signedUpdate.tokenAAllowSpend || staking.tokenBAllowSpend === signedUpdate.tokenBAllowSpend
      ) || pendingStakings.exists(staking =>
        staking.value.tokenAAllowSpend === signedUpdate.tokenAAllowSpend || staking.value.tokenBAllowSpend === signedUpdate.tokenBAllowSpend
      )

      val expireEpochProgress = EpochProgress(
        NonNegLong
          .from(
            lastSyncGlobalEpochProgress.value.value + applicationConfig.failedOperationsExpirationEpochProgresses.value.value
          )
          .getOrElse(NonNegLong.MinValue)
      )

      def failWith(reason: FailedCalculatedStateReason): Left[FailedCalculatedState, Signed[StakingUpdate]] =
        Left(FailedCalculatedState(reason, expireEpochProgress, signedUpdate))

      if (isNewUpdate && (tokenAAllowSpendIsDuplicated.isInvalid || tokenBAllowSpendIsDuplicated.isInvalid)) {
        failWith(DuplicatedAllowSpend(signedUpdate))
      } else if (signedUpdate.maxValidGsEpochProgress < lastSyncGlobalEpochProgress) {
        failWith(OperationExpired(signedUpdate))
      } else if (isDuplicated) {
        failWith(DuplicatedStakingRequest(signedUpdate))
      } else {
        signedUpdate.asRight
      }

    }

    def combinerValidations(
      oldState: AmmCalculatedState,
      signedUpdate: Signed[StakingUpdate],
      lastSyncGlobalEpochProgress: EpochProgress,
      confirmedStakings: Set[StakingCalculatedStateAddress],
      pendingStakings: Set[Signed[StakingUpdate]],
      isNewUpdate: Boolean,
      currencyId: CurrencyId,
      tokenInformation: StakingTokenInformation,
      allowSpendTokenA: Hashed[AllowSpend],
      allowSpendTokenB: Hashed[AllowSpend]
    ): Either[FailedCalculatedState, Signed[StakingUpdate]] =
      combinerContextualValidations(
        oldState,
        signedUpdate,
        lastSyncGlobalEpochProgress,
        confirmedStakings,
        pendingStakings,
        isNewUpdate
      ) match {
        case Left(failedCalculatedState) => failedCalculatedState.asLeft
        case Right(_) =>
          val expireEpochProgress = EpochProgress(
            NonNegLong
              .from(
                lastSyncGlobalEpochProgress.value.value + applicationConfig.failedOperationsExpirationEpochProgresses.value.value
              )
              .getOrElse(NonNegLong.MinValue)
          )

          val (tokenA, tokenB) = if (tokenInformation.primaryTokenInformation.identifier == allowSpendTokenA.currencyId) {
            (tokenInformation.primaryTokenInformation, tokenInformation.pairTokenInformation)
          } else {
            (tokenInformation.pairTokenInformation, tokenInformation.primaryTokenInformation)
          }
          if (allowSpendTokenA.source =!= signedUpdate.source || allowSpendTokenB.source =!= signedUpdate.source) {
            failWith(SourceAddressBetweenUpdateAndAllowSpendDifferent(signedUpdate), expireEpochProgress, signedUpdate)
          } else if (allowSpendTokenA.destination =!= currencyId.value || allowSpendTokenB.destination =!= currencyId.value) {
            failWith(AllowSpendsDestinationAddressInvalid(), expireEpochProgress, signedUpdate)
          } else if (allowSpendTokenA.currencyId =!= signedUpdate.tokenAId || allowSpendTokenB.currencyId =!= signedUpdate.tokenBId) {
            failWith(InvalidCurrencyIdsBetweenAllowSpendsAndDataUpdate(signedUpdate), expireEpochProgress, signedUpdate)
          } else if (tokenA.amount.value > allowSpendTokenA.amount.value.value) {
            failWith(AmountGreaterThanAllowSpendLimit(allowSpendTokenA.signed.value), expireEpochProgress, signedUpdate)
          } else if (tokenB.amount.value > allowSpendTokenB.amount.value.value) {
            failWith(AmountGreaterThanAllowSpendLimit(allowSpendTokenB.signed.value), expireEpochProgress, signedUpdate)
          } else if (allowSpendTokenA.lastValidEpochProgress.value.value < lastSyncGlobalEpochProgress.value.value) {
            failWith(AllowSpendExpired(allowSpendTokenA.signed.value), expireEpochProgress, signedUpdate)
          } else if (allowSpendTokenB.lastValidEpochProgress.value.value < lastSyncGlobalEpochProgress.value.value) {
            failWith(AllowSpendExpired(allowSpendTokenB.signed.value), expireEpochProgress, signedUpdate)
          } else {
            Right(signedUpdate)
          }
      }

    private def validateIfTransactionAlreadyExists(
      stakingUpdate: StakingUpdate,
      confirmedStakings: Set[StakingCalculatedStateAddress],
      pendingStakings: Set[Signed[StakingUpdate]]
    ): DataApplicationValidationErrorOr[Unit] =
      StakingTransactionAlreadyExists.whenA(
        confirmedStakings.exists(staking =>
          staking.tokenAAllowSpend === stakingUpdate.tokenAAllowSpend || staking.tokenBAllowSpend === stakingUpdate.tokenBAllowSpend
        ) ||
          pendingStakings.exists(staking =>
            staking.value.tokenAAllowSpend === stakingUpdate.tokenAAllowSpend || staking.value.tokenBAllowSpend === stakingUpdate.tokenBAllowSpend
          )
      )

    private def validateIfLiquidityPoolExists(
      stakingUpdate: StakingUpdate,
      currentLiquidityPools: Map[String, LiquidityPool]
    ): F[DataApplicationValidationErrorOr[Unit]] = for {
      poolId <- buildLiquidityPoolUniqueIdentifier(stakingUpdate.tokenAId, stakingUpdate.tokenBId)
      result = StakingLiquidityPoolDoesNotExists.unlessA(currentLiquidityPools.contains(poolId.value))
    } yield result

    private def lastRefValidation(
      stakingCalculatedState: StakingCalculatedState,
      signedStaking: Signed[StakingUpdate],
      address: Address
    ): DataApplicationValidationErrorOr[Unit] = {
      val lastConfirmed: Option[StakingReference] = stakingCalculatedState.confirmed.value
        .get(address)
        .flatMap(_.maxByOption(_.parent.ordinal))
        .map(_.parent)

      lastConfirmed match {
        case Some(last) if signedStaking.ordinal =!= last.ordinal.next || signedStaking.parent =!= last =>
          InvalidStakingParent.invalid
        case _ => valid
      }
    }
  }
}
