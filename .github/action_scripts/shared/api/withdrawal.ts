import axios from "axios";
import { createAccount, getPublicKey } from "./account";
import { log, Logger, throwInContext } from "../log";
import { serializeBase64 } from "../serialize";
import { dag4 } from "@stardust-collective/dag4";
import { getCalculatedState, isPendingAllowSpend } from "./calculated-state";
import { singleResponseSchema } from "./response";
import { LastRef, lastRefSchema } from "./last-ref";
import { Signed } from "./signed";

type WithdrawalUpdate = {
    metagraphId: string
    source: string
    tokenAId: string | null
    tokenBId: string | null
    shareToWithdraw: number
    minAmountAOut: number | null
    minAmountBOut: number | null
    maxAmountAOut: number | null
    maxAmountBOut: number | null
    parent: LastRef
    maxValidGsEpochProgress: number
}

type WithdrawalCalculatedStateAddress = {
    tokenAId: string | null
    tokenAAmount: number
    tokenBId: string | null
    tokenBAmount: number
    shareToWithdraw: number
    minAmountAOut: number | null
    minAmountBOut: number | null
    maxAmountAOut: number | null
    maxAmountBOut: number | null
    parent: LastRef
}

type WithdrawalUpdateBody = {
    WithdrawalUpdate: WithdrawalUpdate
}

type WithdrawalCalculatedStateValue = {
    expiringEpochProgress: number,
    value: WithdrawalCalculatedStateAddress
}

type WithdrawalCalculatedStateInfo = {
    lastReference: LastRef,
    values: WithdrawalCalculatedStateValue[]
}

type ConfirmedWithdrawalCalculatedState = {
    value: Record<string, WithdrawalCalculatedStateInfo>
}

const getLastWithdrawalReference = async (
    address: string,
    l0Url: string
): Promise<LastRef> => {
    const { data } = await axios.get(`${l0Url}/v1/addresses/${address}/withdrawals/last-reference`);
    const lastRefResponse = singleResponseSchema(lastRefSchema).parse(data);
    return lastRefResponse.data
}

const createWithdrawalUpdate = async (
    tokenAId: string | null,
    tokenBId: string | null,
    ammMetagraphId: string,
    shareToWithdraw: number,
    account: ReturnType<typeof createAccount>,
    privateKey: string,
    l0Url: string,
    context: string
): Promise<Signed<WithdrawalUpdateBody>> => {
    log(`Fetching last withdrawal reference for wallet: ${account.address}`, "INFO", context);

    const lastRef = await getLastWithdrawalReference(account.address, l0Url);

    log(`Last withdrawal reference for wallet: ${account.address}: ${JSON.stringify(lastRef, null, 2)}`, "INFO", context);

    const body: WithdrawalUpdateBody = {
        WithdrawalUpdate: {
            metagraphId: ammMetagraphId,
            source: account.address,
            maxValidGsEpochProgress: 1000,
            tokenAId,
            tokenBId,
            shareToWithdraw,
            minAmountAOut: null,
            minAmountBOut: null,
            maxAmountAOut: null,
            maxAmountBOut: null,
            parent: lastRef,
        }
    };

    const serialized = await serializeBase64(body)
    const signature = await dag4.keyStore.dataSign(
        privateKey,
        serialized
    );

    const publicKey = getPublicKey(account)

    const withdrawalUpdate = {
        value: body,
        proofs: [{ id: publicKey, signature }]
    };

    log(`Signed withdrawal update generated for wallet: ${account.address}: ${JSON.stringify(withdrawalUpdate, null, 2)}`, "INFO", context);

    return withdrawalUpdate;
}

const validateWithdrawalCreated = async (
    ammL0Url: string,
    tokenAId: string | null,
    tokenBId: string | null,
    shareToWithdraw: number,
    account: ReturnType<typeof createAccount>,
    logger: Logger = log
) => {

    const calculatedState = await getCalculatedState(ammL0Url);
    logger("Pulled calculated state", "INFO", 'AMM')

    const withdrawalCalculatedState = calculatedState?.operations.Withdrawal?.WithdrawalCalculatedState

    const confirmedWithdrawals = withdrawalCalculatedState?.confirmed.value || {}
    const pendingWithdrawals = withdrawalCalculatedState?.pending || []
    const failedWithdrawals = withdrawalCalculatedState?.failed || []

    log(`Looking for confirmed withdrawals for wallet: ${account.address}`, "INFO", 'AMM')

    const confirmedAddressWithdrawals = confirmedWithdrawals[account.address]?.values?.map(info => info.value) || []
    const confirmedWithdrawal = confirmedAddressWithdrawals.find(
        (withdrawal) =>
            withdrawal.tokenAId === tokenAId
            && withdrawal.tokenBId === tokenBId
            && withdrawal.shareToWithdraw === shareToWithdraw
    );

    const isPendingWithdrawal = pendingWithdrawals.some(
        (pendingAction) => {
            const updateValue = isPendingAllowSpend(pendingAction)
                ? pendingAction.PendingAllowSpend.update.value
                : pendingAction.PendingSpendAction.update.value;

            return updateValue.tokenAId === tokenAId && updateValue.tokenBId === tokenBId;
        }
    );

    const isFailedWithdrawal = failedWithdrawals.some(
        (withdrawal) =>
            withdrawal.update.value.tokenAId === tokenAId && withdrawal.update.value.tokenBId === tokenBId
    );

    if (isFailedWithdrawal) {
        throwInContext('AMM')("Withdrawal creation failed.");
    } else if (isPendingWithdrawal) {
        throwInContext('AMM')("Withdrawal creation is pending.");
    } else if (confirmedWithdrawal) {
        logger("Withdrawal creation validated!", "INFO", 'AMM')
        return confirmedWithdrawal
    } else {
        throwInContext('AMM')("Withdrawal not found.");
    }
}

export { createWithdrawalUpdate, validateWithdrawalCreated, type WithdrawalUpdate, type WithdrawalCalculatedStateAddress, type ConfirmedWithdrawalCalculatedState }
