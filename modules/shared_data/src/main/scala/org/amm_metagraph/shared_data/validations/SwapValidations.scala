package org.amm_metagraph.shared_data.validations

import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.SortedSet

import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.ext.cats.syntax.next._
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.{AllowSpend, CurrencyId}
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hashed, SecurityProvider}

import eu.timepit.refined.cats.refTypeEq
import org.amm_metagraph.shared_data.AllowSpends.getAllAllowSpendsInUseFromState
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.epochProgress.getFailureExpireEpochProgress
import org.amm_metagraph.shared_data.types.DataUpdates.{AmmUpdate, SwapUpdate}
import org.amm_metagraph.shared_data.types.LiquidityPool.{LiquidityPool, buildLiquidityPoolUniqueIdentifier, getConfirmedLiquidityPools}
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.Swap._
import org.amm_metagraph.shared_data.types.codecs.{HasherSelector, JsonWithBase64BinaryCodec}
import org.amm_metagraph.shared_data.validations.Errors._
import org.amm_metagraph.shared_data.validations.SharedValidations._

trait SwapValidations[F[_]] {
  def l1Validations(
    swapUpdate: SwapUpdate
  ): F[DataApplicationValidationErrorOr[Unit]]

  def l0Validations(
    signedSwapUpdate: Signed[SwapUpdate],
    state: AmmCalculatedState,
    lastSyncGlobalEpochProgress: EpochProgress
  ): F[Either[FailedCalculatedState, Signed[SwapUpdate]]]

  def newUpdateValidations(
    oldState: AmmCalculatedState,
    signedUpdate: Signed[SwapUpdate],
    lastSyncGlobalEpochProgress: EpochProgress,
    confirmedSwaps: SortedSet[SwapCalculatedStateValue],
    pendingSwaps: SortedSet[Signed[SwapUpdate]]
  ): F[Either[FailedCalculatedState, Signed[SwapUpdate]]]

  def pendingAllowSpendsValidations(
    signedUpdate: Signed[SwapUpdate],
    lastSyncGlobalEpochProgress: EpochProgress,
    currencyId: CurrencyId,
    tokenInformation: SwapTokenInfo,
    allowSpendToken: Hashed[AllowSpend]
  ): F[Either[FailedCalculatedState, Signed[SwapUpdate]]]

  def pendingSpendActionsValidation(
    signedUpdate: Signed[SwapUpdate],
    lastSyncGlobalEpochProgress: EpochProgress
  ): F[Either[FailedCalculatedState, Signed[SwapUpdate]]]
}

object SwapValidations {
  def make[F[_]: Async: SecurityProvider: HasherSelector](
    applicationConfig: ApplicationConfig,
    dataUpdateCodec: JsonWithBase64BinaryCodec[F, AmmUpdate]
  ): SwapValidations[F] = new SwapValidations[F] {
    def l1Validations(
      swapUpdate: SwapUpdate
    ): F[DataApplicationValidationErrorOr[Unit]] = Async[F].delay {
      validateIfTokenIdsAreTheSame(swapUpdate.swapFromPair, swapUpdate.swapToPair)
    }

    def l0Validations(
      signedSwapUpdate: Signed[SwapUpdate],
      state: AmmCalculatedState,
      lastSyncGlobalEpochProgress: EpochProgress
    ): F[Either[FailedCalculatedState, Signed[SwapUpdate]]] = {
      val liquidityPoolsCalculatedState = getConfirmedLiquidityPools(state)
      val swapUpdate = signedSwapUpdate.value

      for {
        signatures <- signatureValidations(signedSwapUpdate, signedSwapUpdate.source)
        liquidityPoolExists <- validateIfLiquidityPoolExists(
          swapUpdate,
          liquidityPoolsCalculatedState
        )
        sourceAddress = signedSwapUpdate.source

        swapCalculatedState = getSwapCalculatedState(state)
        transactionAlreadyExists = validateIfTransactionAlreadyExists(
          swapUpdate,
          swapCalculatedState.confirmed.value
            .get(sourceAddress)
            .map(_.values)
            .getOrElse(SortedSet.empty),
          swapCalculatedState.getPendingUpdates
        )

        hashedUpdate <- HasherSelector[F].withCurrent(implicit hs => signedSwapUpdate.toHashed(dataUpdateCodec.serialize))
        updateHash = hashedUpdate.hash
        duplicatedUpdate = validateDuplicatedUpdate(state, hashedUpdate)

        lastRef = lastRefValidation(swapCalculatedState, signedSwapUpdate, sourceAddress)
        expireEpochProgress = getFailureExpireEpochProgress(applicationConfig, lastSyncGlobalEpochProgress)

        result =
          if (duplicatedUpdate.isInvalid) {
            failWith(DuplicatedUpdate(swapUpdate), expireEpochProgress, signedSwapUpdate, updateHash)
          } else if (liquidityPoolExists.isInvalid) {
            failWith(InvalidLiquidityPool(), expireEpochProgress, signedSwapUpdate, updateHash)
          } else if (signatures.isInvalid) {
            failWith(InvalidSignatures(signatures.map(_.show).mkString_(",")), expireEpochProgress, signedSwapUpdate, updateHash)
          } else if (transactionAlreadyExists.isInvalid) {
            failWith(TransactionAlreadyExists(swapUpdate), expireEpochProgress, signedSwapUpdate, updateHash)
          } else if (lastRef.isInvalid) {
            failWith(InvalidLastReference(), expireEpochProgress, signedSwapUpdate, updateHash)
          } else {
            signedSwapUpdate.asRight
          }
      } yield result
    }

    def newUpdateValidations(
      oldState: AmmCalculatedState,
      signedUpdate: Signed[SwapUpdate],
      lastSyncGlobalEpochProgress: EpochProgress,
      confirmedSwaps: SortedSet[SwapCalculatedStateValue],
      pendingSwaps: SortedSet[Signed[SwapUpdate]]
    ): F[Either[FailedCalculatedState, Signed[SwapUpdate]]] = {
      val allAllowSpendsInUse = getAllAllowSpendsInUseFromState(oldState)

      val allowSpendIsDuplicated = validateIfAllowSpendsAreDuplicated(
        signedUpdate.allowSpendReference,
        allAllowSpendsInUse
      )

      val expireEpochProgress = getFailureExpireEpochProgress(applicationConfig, lastSyncGlobalEpochProgress)
      for {
        hashedUpdate <- HasherSelector[F].withCurrent(implicit hs => signedUpdate.toHashed(dataUpdateCodec.serialize))
        updateHash = hashedUpdate.hash
        result =
          if (allowSpendIsDuplicated.isInvalid) {
            failWith(DuplicatedAllowSpend(signedUpdate), expireEpochProgress, signedUpdate, updateHash)
          } else if (signedUpdate.maxValidGsEpochProgress < lastSyncGlobalEpochProgress) {
            failWith(OperationExpired(signedUpdate), expireEpochProgress, signedUpdate, updateHash)
          } else if (
            confirmedSwaps.exists(swap => swap.value.allowSpendReference === signedUpdate.allowSpendReference) ||
            pendingSwaps.exists(swap => swap.allowSpendReference === signedUpdate.allowSpendReference)
          ) {
            failWith(DuplicatedSwapRequest(signedUpdate), expireEpochProgress, signedUpdate, updateHash)
          } else {
            signedUpdate.asRight
          }
      } yield result
    }

    def pendingAllowSpendsValidations(
      signedUpdate: Signed[SwapUpdate],
      lastSyncGlobalEpochProgress: EpochProgress,
      currencyId: CurrencyId,
      tokenInformation: SwapTokenInfo,
      allowSpendToken: Hashed[AllowSpend]
    ): F[Either[FailedCalculatedState, Signed[SwapUpdate]]] = {
      val expireEpochProgress = getFailureExpireEpochProgress(applicationConfig, lastSyncGlobalEpochProgress)
      for {
        hashedUpdate <- HasherSelector[F].withCurrent(implicit hs => signedUpdate.toHashed(dataUpdateCodec.serialize))
        updateHash = hashedUpdate.hash
        result =
          if (allowSpendToken.source =!= signedUpdate.source) {
            failWith(SourceAddressBetweenUpdateAndAllowSpendDifferent(signedUpdate), expireEpochProgress, signedUpdate, updateHash)
          } else if (allowSpendToken.destination =!= currencyId.value) {
            failWith(AllowSpendsDestinationAddressInvalid(), expireEpochProgress, signedUpdate, updateHash)
          } else if (allowSpendToken.currencyId =!= signedUpdate.swapFromPair) {
            failWith(InvalidCurrencyIdsBetweenAllowSpendsAndDataUpdate(signedUpdate), expireEpochProgress, signedUpdate, updateHash)
          } else if (signedUpdate.amountIn.value.value > allowSpendToken.amount.value.value) {
            failWith(AmountGreaterThanAllowSpendLimit(allowSpendToken.signed.value), expireEpochProgress, signedUpdate, updateHash)
          } else if (tokenInformation.netReceived < signedUpdate.amountOutMinimum) {
            failWith(SwapLessThanMinAmount(), expireEpochProgress, signedUpdate, updateHash)
          } else if (signedUpdate.amountOutMaximum.exists(_ < tokenInformation.netReceived)) {
            failWith(SwapHigherThanMaxAmount(), expireEpochProgress, signedUpdate, updateHash)
          } else if (allowSpendToken.lastValidEpochProgress < lastSyncGlobalEpochProgress) {
            failWith(AllowSpendExpired(allowSpendToken.signed.value), expireEpochProgress, signedUpdate, updateHash)
          } else if (
            tokenInformation.primaryTokenInformationUpdated.amount.value < applicationConfig.tokenLimits.minTokens.value ||
            tokenInformation.pairTokenInformationUpdated.amount.value < applicationConfig.tokenLimits.minTokens.value
          ) {
            failWith(SwapWouldDrainPoolBalance(), expireEpochProgress, signedUpdate, updateHash)
          } else if (
            tokenInformation.grossReceived.value.value > applicationConfig.tokenLimits.maxTokens.value ||
            signedUpdate.amountIn.value.value > applicationConfig.tokenLimits.maxTokens.value
          ) {
            failWith(
              SwapExceedsMaxTokensLimit(signedUpdate.value, tokenInformation.grossReceived),
              expireEpochProgress,
              signedUpdate,
              updateHash
            )
          } else {
            Right(signedUpdate)
          }
      } yield result
    }

    def pendingSpendActionsValidation(
      signedUpdate: Signed[SwapUpdate],
      lastSyncGlobalEpochProgress: EpochProgress
    ): F[Either[FailedCalculatedState, Signed[SwapUpdate]]] = {
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

    private def validateIfLiquidityPoolExists(
      swapUpdate: SwapUpdate,
      currentLiquidityPools: Map[String, LiquidityPool]
    ): F[DataApplicationValidationErrorOr[Unit]] = for {
      poolId <- buildLiquidityPoolUniqueIdentifier(swapUpdate.swapFromPair, swapUpdate.swapToPair)
      result = SwapLiquidityPoolDoesNotExists.unlessA(currentLiquidityPools.contains(poolId.value))
    } yield result

    private def lastRefValidation(
      swapCalculatedState: SwapCalculatedState,
      signedSwap: Signed[SwapUpdate],
      address: Address
    ): DataApplicationValidationErrorOr[Unit] = {
      val lastConfirmed: Option[SwapReference] = swapCalculatedState.confirmed.value
        .get(address)
        .map(_.lastReference)

      lastConfirmed match {
        case Some(last) if signedSwap.ordinal.value =!= last.ordinal.next.value || signedSwap.parent =!= last =>
          InvalidSwapParent.invalid
        case _ => valid
      }
    }

    private def validateIfTransactionAlreadyExists(
      swapUpdate: SwapUpdate,
      confirmedSwaps: SortedSet[SwapCalculatedStateValue],
      pendingSwaps: SortedSet[Signed[SwapUpdate]]
    ): DataApplicationValidationErrorOr[Unit] =
      SwapTransactionAlreadyExists.whenA(
        confirmedSwaps.exists(swap => swap.value.allowSpendReference === swapUpdate.allowSpendReference) ||
          pendingSwaps.exists(swap => swap.value.allowSpendReference === swapUpdate.allowSpendReference)
      )

    private def validateDuplicatedUpdate(
      calculatedState: AmmCalculatedState,
      hashedUpdate: Hashed[SwapUpdate]
    ): DataApplicationValidationErrorOr[Unit] = {
      val updateHash = hashedUpdate.hash
      val pendingAllowSpends = getPendingAllowSpendsSwapUpdates(calculatedState)
      val pendingSpendActions = getPendingSpendActionSwapUpdates(calculatedState)

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
