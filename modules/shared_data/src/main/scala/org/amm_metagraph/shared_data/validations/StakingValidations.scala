package org.amm_metagraph.shared_data.validations

import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.SortedSet

import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.{AllowSpend, CurrencyId}
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hashed, SecurityProvider}

import org.amm_metagraph.shared_data.AllowSpends.getAllAllowSpendsInUseFromState
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.epochProgress.getFailureExpireEpochProgress
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, StakingUpdate}
import org.amm_metagraph.shared_data.types.LiquidityPool.{LiquidityPool, buildLiquidityPoolUniqueIdentifier, getConfirmedLiquidityPools}
import org.amm_metagraph.shared_data.types.Staking._
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.codecs.{HasherSelector, JsonWithBase64BinaryCodec}
import org.amm_metagraph.shared_data.validations.Errors._
import org.amm_metagraph.shared_data.validations.SharedValidations._

trait StakingValidations[F[_]] {
  def l1Validations(
    stakingUpdate: StakingUpdate
  ): F[DataApplicationValidationErrorOr[Unit]]

  def l0Validations(
    signedStakingUpdate: Signed[StakingUpdate],
    state: AmmCalculatedState,
    lastSyncGlobalEpochProgress: EpochProgress
  ): F[Either[FailedCalculatedState, Signed[StakingUpdate]]]

  def newUpdateValidations(
    oldState: AmmCalculatedState,
    signedUpdate: Signed[StakingUpdate],
    lastSyncGlobalEpochProgress: EpochProgress,
    confirmedStakings: SortedSet[StakingCalculatedStateValue],
    pendingStakings: SortedSet[Signed[StakingUpdate]]
  ): F[Either[FailedCalculatedState, Signed[StakingUpdate]]]

  def pendingAllowSpendsValidations(
    signedUpdate: Signed[StakingUpdate],
    lastSyncGlobalEpochProgress: EpochProgress,
    currencyId: CurrencyId,
    tokenInformation: StakingTokenInformation,
    allowSpendTokenA: Hashed[AllowSpend],
    allowSpendTokenB: Hashed[AllowSpend]
  ): F[Either[FailedCalculatedState, Signed[StakingUpdate]]]

  def pendingSpendActionsValidation(
    signedUpdate: Signed[StakingUpdate],
    lastSyncGlobalEpochProgress: EpochProgress
  ): F[Either[FailedCalculatedState, Signed[StakingUpdate]]]
}

object StakingValidations {
  def make[F[_]: Async: SecurityProvider: HasherSelector](
    applicationConfig: ApplicationConfig,
    dataUpdateCodec: JsonWithBase64BinaryCodec[F, AmmUpdate]
  ): StakingValidations[F] = new StakingValidations[F] {
    def l1Validations(
      stakingUpdate: StakingUpdate
    ): F[DataApplicationValidationErrorOr[Unit]] = Async[F].delay {
      validateIfTokenIdsAreTheSame(stakingUpdate.tokenAId, stakingUpdate.tokenBId)
    }

    def l0Validations(
      signedStakingUpdate: Signed[StakingUpdate],
      state: AmmCalculatedState,
      lastSyncGlobalEpochProgress: EpochProgress
    ): F[Either[FailedCalculatedState, Signed[StakingUpdate]]] = {
      val stakingUpdate = signedStakingUpdate.value

      val stakingCalculatedState = getStakingCalculatedState(state)
      val liquidityPoolsCalculatedState = getConfirmedLiquidityPools(state)

      for {
        signatures <- signatureValidations(signedStakingUpdate, signedStakingUpdate.source)
        sourceAddress = signedStakingUpdate.source

        transactionAlreadyExists = validateIfTransactionAlreadyExists(
          stakingUpdate,
          stakingCalculatedState.confirmed.value
            .get(sourceAddress)
            .map(_.values)
            .getOrElse(SortedSet.empty),
          stakingCalculatedState.getPendingUpdates
        )

        liquidityPoolExists <- validateIfLiquidityPoolExists(
          stakingUpdate,
          liquidityPoolsCalculatedState
        )

        hashedUpdate <- HasherSelector[F].withCurrent(implicit hs => signedStakingUpdate.toHashed(dataUpdateCodec.serialize))
        updateHash = hashedUpdate.hash
        duplicatedUpdate = validateDuplicatedUpdate(state, hashedUpdate)

        lastRef = lastRefValidation(stakingCalculatedState, signedStakingUpdate, sourceAddress)
        expireEpochProgress = getFailureExpireEpochProgress(applicationConfig, lastSyncGlobalEpochProgress)
        result =
          if (duplicatedUpdate.isInvalid) {
            failWith(DuplicatedUpdate(stakingUpdate), expireEpochProgress, signedStakingUpdate, updateHash)
          } else if (liquidityPoolExists.isInvalid) {
            failWith(InvalidLiquidityPool(), expireEpochProgress, signedStakingUpdate, updateHash)
          } else if (signatures.isInvalid) {
            failWith(InvalidSignatures(signatures.map(_.show).mkString_(",")), expireEpochProgress, signedStakingUpdate, updateHash)
          } else if (transactionAlreadyExists.isInvalid) {
            failWith(TransactionAlreadyExists(stakingUpdate), expireEpochProgress, signedStakingUpdate, updateHash)
          } else if (lastRef.isInvalid) {
            failWith(InvalidLastReference(), expireEpochProgress, signedStakingUpdate, updateHash)
          } else {
            signedStakingUpdate.asRight
          }
      } yield result
    }

    def newUpdateValidations(
      oldState: AmmCalculatedState,
      signedUpdate: Signed[StakingUpdate],
      lastSyncGlobalEpochProgress: EpochProgress,
      confirmedStakings: SortedSet[StakingCalculatedStateValue],
      pendingStakings: SortedSet[Signed[StakingUpdate]]
    ): F[Either[FailedCalculatedState, Signed[StakingUpdate]]] = {
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
        staking.value.tokenAAllowSpend === signedUpdate.tokenAAllowSpend || staking.value.tokenBAllowSpend === signedUpdate.tokenBAllowSpend
      ) || pendingStakings.exists(staking =>
        staking.value.tokenAAllowSpend === signedUpdate.tokenAAllowSpend || staking.value.tokenBAllowSpend === signedUpdate.tokenBAllowSpend
      )

      val expireEpochProgress = getFailureExpireEpochProgress(applicationConfig, lastSyncGlobalEpochProgress)
      for {
        hashedUpdate <- HasherSelector[F].withCurrent(implicit hs => signedUpdate.toHashed(dataUpdateCodec.serialize))
        updateHash = hashedUpdate.hash
        result =
          if (tokenAAllowSpendIsDuplicated.isInvalid || tokenBAllowSpendIsDuplicated.isInvalid) {
            failWith(DuplicatedAllowSpend(signedUpdate), expireEpochProgress, signedUpdate, updateHash)
          } else if (signedUpdate.maxValidGsEpochProgress < lastSyncGlobalEpochProgress) {
            failWith(OperationExpired(signedUpdate), expireEpochProgress, signedUpdate, updateHash)
          } else if (isDuplicated) {
            failWith(DuplicatedStakingRequest(signedUpdate), expireEpochProgress, signedUpdate, updateHash)
          } else {
            signedUpdate.asRight
          }
      } yield result
    }

    def pendingAllowSpendsValidations(
      signedUpdate: Signed[StakingUpdate],
      lastSyncGlobalEpochProgress: EpochProgress,
      currencyId: CurrencyId,
      tokenInformation: StakingTokenInformation,
      allowSpendTokenA: Hashed[AllowSpend],
      allowSpendTokenB: Hashed[AllowSpend]
    ): F[Either[FailedCalculatedState, Signed[StakingUpdate]]] = {
      val expireEpochProgress = getFailureExpireEpochProgress(applicationConfig, lastSyncGlobalEpochProgress)

      val (tokenA, tokenB) = if (tokenInformation.primaryTokenInformation.identifier == allowSpendTokenA.currencyId) {
        (tokenInformation.primaryTokenInformation, tokenInformation.pairTokenInformation)
      } else {
        (tokenInformation.pairTokenInformation, tokenInformation.primaryTokenInformation)
      }
      val allowSpendDelay = applicationConfig.allowSpendEpochBufferDelay.value.value

      for {
        hashedUpdate <- HasherSelector[F].withCurrent(implicit hs => signedUpdate.toHashed(dataUpdateCodec.serialize))
        updateHash = hashedUpdate.hash
        result =
          if (signedUpdate.maxValidGsEpochProgress < lastSyncGlobalEpochProgress) {
            failWith(OperationExpired(signedUpdate), expireEpochProgress, signedUpdate, updateHash)
          } else if (allowSpendTokenA.source =!= signedUpdate.source || allowSpendTokenB.source =!= signedUpdate.source) {
            failWith(SourceAddressBetweenUpdateAndAllowSpendDifferent(signedUpdate), expireEpochProgress, signedUpdate, updateHash)
          } else if (allowSpendTokenA.destination =!= currencyId.value || allowSpendTokenB.destination =!= currencyId.value) {
            failWith(AllowSpendsDestinationAddressInvalid(), expireEpochProgress, signedUpdate, updateHash)
          } else if (allowSpendTokenA.currencyId =!= signedUpdate.tokenAId || allowSpendTokenB.currencyId =!= signedUpdate.tokenBId) {
            failWith(InvalidCurrencyIdsBetweenAllowSpendsAndDataUpdate(signedUpdate), expireEpochProgress, signedUpdate, updateHash)
          } else if (tokenA.amount.value > allowSpendTokenA.amount.value.value) {
            failWith(AmountGreaterThanAllowSpendLimit(allowSpendTokenA.signed.value), expireEpochProgress, signedUpdate, updateHash)
          } else if (tokenB.amount.value > allowSpendTokenB.amount.value.value) {
            failWith(AmountGreaterThanAllowSpendLimit(allowSpendTokenB.signed.value), expireEpochProgress, signedUpdate, updateHash)
          } else if (allowSpendTokenA.lastValidEpochProgress.value.value + allowSpendDelay < lastSyncGlobalEpochProgress.value.value) {
            failWith(AllowSpendExpired(allowSpendTokenA.signed.value), expireEpochProgress, signedUpdate, updateHash)
          } else if (allowSpendTokenB.lastValidEpochProgress.value.value + allowSpendDelay < lastSyncGlobalEpochProgress.value.value) {
            failWith(AllowSpendExpired(allowSpendTokenB.signed.value), expireEpochProgress, signedUpdate, updateHash)
          } else {
            Right(signedUpdate)
          }
      } yield result
    }

    def pendingSpendActionsValidation(
      signedUpdate: Signed[StakingUpdate],
      lastSyncGlobalEpochProgress: EpochProgress
    ): F[Either[FailedCalculatedState, Signed[StakingUpdate]]] = {
      val expireEpochProgress = getFailureExpireEpochProgress(applicationConfig, lastSyncGlobalEpochProgress)

      for {
        hashedUpdate <- HasherSelector[F].withCurrent(implicit hs => signedUpdate.toHashed(dataUpdateCodec.serialize))
        updateHash = hashedUpdate.hash
        result =
          if (signedUpdate.maxValidGsEpochProgress < lastSyncGlobalEpochProgress) {
            failWith(OperationExpired(signedUpdate), expireEpochProgress, signedUpdate, updateHash)
          } else {
            signedUpdate.asRight
          }
      } yield result
    }

    private def validateIfTransactionAlreadyExists(
      stakingUpdate: StakingUpdate,
      confirmedStakings: SortedSet[StakingCalculatedStateValue],
      pendingStakings: SortedSet[Signed[StakingUpdate]]
    ): DataApplicationValidationErrorOr[Unit] =
      StakingTransactionAlreadyExists.whenA(
        confirmedStakings.exists(staking =>
          staking.value.tokenAAllowSpend === stakingUpdate.tokenAAllowSpend || staking.value.tokenBAllowSpend === stakingUpdate.tokenBAllowSpend
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
        .map(_.lastReference)

      lastConfirmed match {
        case Some(last) if signedStaking.ordinal =!= last.ordinal.next || signedStaking.parent =!= last =>
          InvalidStakingParent.invalid
        case _ => valid
      }
    }
    private def validateDuplicatedUpdate(
      calculatedState: AmmCalculatedState,
      hashedUpdate: Hashed[StakingUpdate]
    ): DataApplicationValidationErrorOr[Unit] = {
      val updateHash = hashedUpdate.hash
      val pendingAllowSpends = getPendingAllowSpendsStakingUpdates(calculatedState)
      val pendingSpendActions = getPendingSpendActionStakingUpdates(calculatedState)

      if (
        pendingAllowSpends.exists(_.updateHash === updateHash) ||
        pendingSpendActions.exists(_.updateHash === updateHash)
      ) {
        DuplicatedOperation.invalidNec
      } else {
        valid
      }
    }
  }
}
