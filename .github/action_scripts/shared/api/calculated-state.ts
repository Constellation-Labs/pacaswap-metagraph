import axios from "axios";
import { throwInContext } from "../log";
import { Logger } from "../retry";
import { Signed } from "./signed";
import { ConfirmedLiquidityPoolCalculatedState, LiquidityPoolUpdate } from "./liquidity-pool";
import { ConfirmedStakingCalculatedState, StakingUpdate } from "./staking";
import { ConfirmedWithdrawalCalculatedState, WithdrawalUpdate } from "./withdrawal";

type OperationCalculatedState<C, U> = {
    confirmed: C
    pending: PendingAction<U>[]
    failed: FailedCalculatedState<U>[]
}

type TokenInformation = {
    identifier?: string | null
    amount: number
}

type CalculatedState = {
    lastSyncGlobalSnapshotOrdinal: number;
    operations: {
        LiquidityPool: {
            LiquidityPoolCalculatedState: OperationCalculatedState<ConfirmedLiquidityPoolCalculatedState, LiquidityPoolUpdate>
        };
        Staking: {
            StakingCalculatedState: OperationCalculatedState<ConfirmedStakingCalculatedState, StakingUpdate>
        };
        Withdrawal: {
            WithdrawalCalculatedState: OperationCalculatedState<ConfirmedWithdrawalCalculatedState, WithdrawalUpdate>
        };
    }
}

type FailedCalculatedState<T> = {
    update: Signed<T>
    expiringEpochProgress: number
    reason: string
}

type SpendAction = any

type PendingAllowSpend<T> = {
    PendingAllowSpend: {
        update: Signed<T>
    }
}

type PendingSpendAction<T> = {
    PendingSpendAction: {
        update: Signed<T>
        generatedSpendAction: SpendAction
    }
}

const isPendingAllowSpend = (pendingAction: PendingAction<any>): pendingAction is PendingAllowSpend<any> => {
    return 'PendingAllowSpend' in pendingAction;
}

const isPendingSpendAction = (pendingAction: PendingAction<any>): pendingAction is PendingSpendAction<any> => {
    return 'PendingSpendAction' in pendingAction;
}

type PendingAction<T> = PendingAllowSpend<T> | PendingSpendAction<T>

const getCalculatedState = async (
    ammL0Url: string,
): Promise<CalculatedState> => {
    const { data } = await axios.get(`${ammL0Url}/v1/calculated-state/latest`);
    return data.calculatedState;
}

const validateIfSyncedToGlobalOrdinal = async (ammL0Url: string, expectedOrdinal: number, logger: Logger) => {
    const calculatedState = await getCalculatedState(ammL0Url);
    if (calculatedState.lastSyncGlobalSnapshotOrdinal < expectedOrdinal) {
        logger(`Calculated state is not synced to global ordinal ${expectedOrdinal}`, "ERROR", "AMM");
        throwInContext('AMM')(`Calculated state is not synced to global ordinal ${expectedOrdinal} (current: ${calculatedState.lastSyncGlobalSnapshotOrdinal})`);
    } else {
        logger(`Calculated state is synced to global ordinal ${expectedOrdinal} (current: ${calculatedState.lastSyncGlobalSnapshotOrdinal})`, "INFO", "AMM");
    }
}

export { validateIfSyncedToGlobalOrdinal, getCalculatedState, type PendingSpendAction, type FailedCalculatedState, type CalculatedState, type TokenInformation, type SpendAction, type PendingAction, isPendingAllowSpend, isPendingSpendAction }