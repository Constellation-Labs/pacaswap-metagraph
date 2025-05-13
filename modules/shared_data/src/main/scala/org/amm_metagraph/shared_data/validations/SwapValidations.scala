package org.amm_metagraph.shared_data.validations

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.ext.cats.syntax.next._
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.schema.epoch.EpochProgress
import io.constellationnetwork.schema.swap.{AllowSpend, CurrencyId}
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.{Hashed, SecurityProvider}

import eu.timepit.refined.cats.refTypeEq
import eu.timepit.refined.types.numeric.NonNegLong
import org.amm_metagraph.shared_data.AllowSpends.getAllAllowSpendsInUseFromState
import org.amm_metagraph.shared_data.app.ApplicationConfig
import org.amm_metagraph.shared_data.types.DataUpdates.SwapUpdate
import org.amm_metagraph.shared_data.types.LiquidityPool.{LiquidityPool, buildLiquidityPoolUniqueIdentifier, getConfirmedLiquidityPools}
import org.amm_metagraph.shared_data.types.States._
import org.amm_metagraph.shared_data.types.Swap._
import org.amm_metagraph.shared_data.validations.Errors._
import org.amm_metagraph.shared_data.validations.SharedValidations._

trait SwapValidations[F[_]] {
  def l1Validations(
    swapUpdate: SwapUpdate
  ): F[DataApplicationValidationErrorOr[Unit]]

  def l0Validations(
    signedSwapUpdate: Signed[SwapUpdate],
    state: AmmCalculatedState
  ): F[DataApplicationValidationErrorOr[Unit]]

  def newUpdateValidations(
    oldState: AmmCalculatedState,
    signedUpdate: Signed[SwapUpdate],
    lastSyncGlobalEpochProgress: EpochProgress,
    confirmedSwaps: Set[SwapCalculatedStateAddress],
    pendingSwaps: Set[Signed[SwapUpdate]]
  ): Either[FailedCalculatedState, Signed[SwapUpdate]]

  def pendingAllowSpendsValidations(
    signedUpdate: Signed[SwapUpdate],
    lastSyncGlobalEpochProgress: EpochProgress,
    currencyId: CurrencyId,
    tokenInformation: SwapTokenInfo,
    allowSpendToken: Hashed[AllowSpend]
  ): Either[FailedCalculatedState, Signed[SwapUpdate]]

  def pendingSpendActionsValidation(
    signedUpdate: Signed[SwapUpdate],
    lastSyncGlobalEpochProgress: EpochProgress
  ): Either[FailedCalculatedState, Signed[SwapUpdate]]
}

object SwapValidations {
  def make[F[_]: Async: SecurityProvider](
    applicationConfig: ApplicationConfig
  ): SwapValidations[F] = new SwapValidations[F] {
    def l1Validations(
      swapUpdate: SwapUpdate
    ): F[DataApplicationValidationErrorOr[Unit]] = Async[F].delay {
      validateIfTokenIdsAreTheSame(swapUpdate.swapFromPair, swapUpdate.swapToPair)
    }

    def l0Validations(
      signedSwapUpdate: Signed[SwapUpdate],
      state: AmmCalculatedState
    ): F[DataApplicationValidationErrorOr[Unit]] = {
      val liquidityPoolsCalculatedState = getConfirmedLiquidityPools(state)
      val swapUpdate = signedSwapUpdate.value

      for {
        l1Validations <- l1Validations(swapUpdate)
        signatures <- signatureValidations(signedSwapUpdate, signedSwapUpdate.source)
        liquidityPoolExists <- validateIfLiquidityPoolExists(
          swapUpdate,
          liquidityPoolsCalculatedState
        )
        sourceAddress = signedSwapUpdate.source
        poolHaveEnoughTokens <- validateIfPoolHaveEnoughTokens(
          swapUpdate,
          liquidityPoolsCalculatedState
        )
        swapCalculatedState = getSwapCalculatedState(state)
        allAllowSpendsInUse = getAllAllowSpendsInUseFromState(state)
        allowSpendIsDuplicated = validateIfAllowSpendsAreDuplicated(
          swapUpdate.allowSpendReference,
          allAllowSpendsInUse
        )
        transactionAlreadyExists = validateIfTransactionAlreadyExists(
          swapUpdate,
          swapCalculatedState.confirmed.value.getOrElse(sourceAddress, Set.empty),
          swapCalculatedState.getPendingUpdates
        )
        lastRef = lastRefValidation(swapCalculatedState, signedSwapUpdate, sourceAddress)
      } yield
        l1Validations
          .productR(signatures)
          .productR(liquidityPoolExists)
          .productR(poolHaveEnoughTokens)
          .productR(allowSpendIsDuplicated)
          .productR(transactionAlreadyExists)
          .productR(lastRef)
    }

    def newUpdateValidations(
      oldState: AmmCalculatedState,
      signedUpdate: Signed[SwapUpdate],
      lastSyncGlobalEpochProgress: EpochProgress,
      confirmedSwaps: Set[SwapCalculatedStateAddress],
      pendingSwaps: Set[Signed[SwapUpdate]]
    ): Either[FailedCalculatedState, Signed[SwapUpdate]] = {
      val allAllowSpendsInUse = getAllAllowSpendsInUseFromState(oldState)

      val allowSpendIsDuplicated = validateIfAllowSpendsAreDuplicated(
        signedUpdate.allowSpendReference,
        allAllowSpendsInUse
      )

      val expireEpochProgress = EpochProgress(
        NonNegLong
          .from(
            lastSyncGlobalEpochProgress.value.value + applicationConfig.failedOperationsExpirationEpochProgresses.value.value
          )
          .getOrElse(NonNegLong.MinValue)
      )

      def failWith(reason: FailedCalculatedStateReason): Left[FailedCalculatedState, Signed[SwapUpdate]] =
        Left(FailedCalculatedState(reason, expireEpochProgress, signedUpdate))

      if (allowSpendIsDuplicated.isInvalid) {
        failWith(DuplicatedAllowSpend(signedUpdate))
      } else if (signedUpdate.maxValidGsEpochProgress < lastSyncGlobalEpochProgress) {
        failWith(OperationExpired(signedUpdate))
      } else if (
        confirmedSwaps.exists(swap => swap.allowSpendReference === signedUpdate.allowSpendReference) ||
        pendingSwaps.exists(swap => swap.allowSpendReference === signedUpdate.allowSpendReference)
      ) {
        failWith(DuplicatedSwapRequest(signedUpdate))
      } else {
        signedUpdate.asRight
      }
    }

    def pendingAllowSpendsValidations(
      signedUpdate: Signed[SwapUpdate],
      lastSyncGlobalEpochProgress: EpochProgress,
      currencyId: CurrencyId,
      tokenInformation: SwapTokenInfo,
      allowSpendToken: Hashed[AllowSpend]
    ): Either[FailedCalculatedState, Signed[SwapUpdate]] = {
      val expireEpochProgress = EpochProgress(
        NonNegLong
          .from(
            lastSyncGlobalEpochProgress.value.value + applicationConfig.failedOperationsExpirationEpochProgresses.value.value
          )
          .getOrElse(NonNegLong.MinValue)
      )

      if (allowSpendToken.source =!= signedUpdate.source) {
        failWith(SourceAddressBetweenUpdateAndAllowSpendDifferent(signedUpdate), expireEpochProgress, signedUpdate)
      } else if (allowSpendToken.destination =!= currencyId.value) {
        failWith(AllowSpendsDestinationAddressInvalid(), expireEpochProgress, signedUpdate)
      } else if (allowSpendToken.currencyId =!= signedUpdate.swapFromPair) {
        failWith(InvalidCurrencyIdsBetweenAllowSpendsAndDataUpdate(signedUpdate), expireEpochProgress, signedUpdate)
      } else if (signedUpdate.amountIn.value.value > allowSpendToken.amount.value.value) {
        failWith(AmountGreaterThanAllowSpendLimit(allowSpendToken.signed.value), expireEpochProgress, signedUpdate)
      } else if (tokenInformation.netReceived < signedUpdate.amountOutMinimum) {
        failWith(SwapLessThanMinAmount(), expireEpochProgress, signedUpdate)
      } else if (signedUpdate.amountOutMaximum.exists(_ < tokenInformation.netReceived)) {
        failWith(SwapHigherThanMaxAmount(), expireEpochProgress, signedUpdate)
      } else if (allowSpendToken.lastValidEpochProgress < lastSyncGlobalEpochProgress) {
        failWith(AllowSpendExpired(allowSpendToken.signed.value), expireEpochProgress, signedUpdate)
      } else if (
        tokenInformation.primaryTokenInformationUpdated.amount.value < applicationConfig.tokenLimits.minTokens.value ||
        tokenInformation.pairTokenInformationUpdated.amount.value < applicationConfig.tokenLimits.minTokens.value
      ) {
        failWith(SwapWouldDrainPoolBalance(), expireEpochProgress, signedUpdate)
      } else if (
        tokenInformation.grossReceived.value.value > applicationConfig.tokenLimits.maxTokens.value ||
        signedUpdate.amountIn.value.value > applicationConfig.tokenLimits.maxTokens.value
      ) {
        failWith(SwapExceedsMaxTokensLimit(signedUpdate.value, tokenInformation.grossReceived), expireEpochProgress, signedUpdate)
      } else {
        Right(signedUpdate)
      }
    }

    def pendingSpendActionsValidation(
      signedUpdate: Signed[SwapUpdate],
      lastSyncGlobalEpochProgress: EpochProgress
    ): Either[FailedCalculatedState, Signed[SwapUpdate]] = {
      val expireEpochProgress = EpochProgress(
        NonNegLong
          .from(
            lastSyncGlobalEpochProgress.value.value + applicationConfig.failedOperationsExpirationEpochProgresses.value.value
          )
          .getOrElse(NonNegLong.MinValue)
      )

      if (signedUpdate.maxValidGsEpochProgress < lastSyncGlobalEpochProgress) {
        failWith(OperationExpired(signedUpdate), expireEpochProgress, signedUpdate)
      } else {
        signedUpdate.asRight
      }
    }

    private def validateIfLiquidityPoolExists(
      swapUpdate: SwapUpdate,
      currentLiquidityPools: Map[String, LiquidityPool]
    ): F[DataApplicationValidationErrorOr[Unit]] = for {
      poolId <- buildLiquidityPoolUniqueIdentifier(swapUpdate.swapFromPair, swapUpdate.swapToPair)
      result = SwapLiquidityPoolDoesNotExists.unlessA(currentLiquidityPools.contains(poolId.value))
    } yield result

    private def validateIfPoolHaveEnoughTokens(
      swapUpdate: SwapUpdate,
      currentLiquidityPools: Map[String, LiquidityPool]
    ): F[DataApplicationValidationErrorOr[Unit]] = {
      val hasEnoughTokens: LiquidityPool => Boolean = lp => {
        val maybeToken = swapUpdate.swapToPair match {
          case None => Option.when(lp.tokenA.identifier.isEmpty)(lp.tokenA).orElse(lp.tokenB.some)
          case Some(value) if lp.tokenA.identifier.contains(value) => lp.tokenA.some
          case Some(value)                                         => Option.when(lp.tokenB.identifier.contains(value))(lp.tokenB)
        }
        maybeToken.exists(_.amount.value > swapUpdate.amountIn.value.value)
      }

      buildLiquidityPoolUniqueIdentifier(swapUpdate.swapFromPair, swapUpdate.swapToPair)
        .map(poolId => currentLiquidityPools.get(poolId.value))
        .map(maybePool => SwapLiquidityPoolNotEnoughTokens.unlessA(maybePool.exists(hasEnoughTokens)))
    }

    private def lastRefValidation(
      swapCalculatedState: SwapCalculatedState,
      signedSwap: Signed[SwapUpdate],
      address: Address
    ): DataApplicationValidationErrorOr[Unit] = {
      val lastConfirmed: Option[SwapReference] = swapCalculatedState.confirmed.value
        .get(address)
        .flatMap(_.maxByOption(_.parent.ordinal))
        .map(_.parent)

      lastConfirmed match {
        case Some(last) if signedSwap.ordinal.value =!= last.ordinal.next.value || signedSwap.parent =!= last =>
          InvalidSwapParent.invalid
        case _ => valid
      }
    }

    private def validateIfTransactionAlreadyExists(
      swapUpdate: SwapUpdate,
      confirmedSwaps: Set[SwapCalculatedStateAddress],
      pendingSwaps: Set[Signed[SwapUpdate]]
    ): DataApplicationValidationErrorOr[Unit] =
      SwapTransactionAlreadyExists.whenA(
        confirmedSwaps.exists(swap => swap.allowSpendReference === swapUpdate.allowSpendReference) ||
          pendingSwaps.exists(swap => swap.value.allowSpendReference === swapUpdate.allowSpendReference)
      )
  }
}
