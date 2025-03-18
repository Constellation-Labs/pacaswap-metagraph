import axios from "axios";
import { createAccount, getPublicKey } from "./account";
import { log, throwInContext } from "../log";
import { serializeBase64 } from "../serialize";
import { dag4 } from "@stardust-collective/dag4";
import { LastRef, Signed } from "./data-update";
import { getCalculatedState, isPendingAllowSpend, TokenInformation } from "./calculated-state";

type StakingUpdate = {
    tokenAAllowSpend: string
    tokenBAllowSpend: string
    tokenAId: string | null
    tokenAAmount: number
    tokenBId: string | null
    parent: string
    maxValidGsEpochProgress: number
}

type StakingCalculatedStateAddress = {
    tokenAAllowSpend: string
    tokenBAllowSpend: string
    tokenA: TokenInformation
    tokenB: TokenInformation
    parent: LastRef
}

type StakingUpdateBody = {
    StakingUpdate: StakingUpdate
}

type ConfirmedStakingCalculatedState = {
    value: Record<string, StakingCalculatedStateAddress[]>
}

const createStakingUpdate = async (
    tokenAAllowSpendHash: string,
    tokenBAllowSpendHash: string,
    tokenAId: string | null,
    tokenBId: string | null,
    tokenAAllowSpendAmount: number,
    account: ReturnType<typeof createAccount>,
    privateKey: string,
    l0Url: string,
    context: string
): Promise<Signed<StakingUpdateBody>> => {
    log(`Fetching last staking reference for wallet: ${account.address}`, "INFO", context);

    const { data: lastRef } = await axios.get(
        `${l0Url}/v1/addresses/${account.address}/stakings/last-reference`
    );

    log(`Last staking reference for wallet: ${account.address}: ${JSON.stringify(lastRef, null, 2)}`, "INFO", context);

    const body: StakingUpdateBody = {
        StakingUpdate: {
            maxValidGsEpochProgress: 1000,
            tokenAAllowSpend: tokenAAllowSpendHash,
            tokenAAmount: tokenAAllowSpendAmount,
            tokenAId,
            tokenBAllowSpend: tokenBAllowSpendHash,
            tokenBId,
            parent: lastRef,
        }
    };

    const serialized = await serializeBase64(body)
    const signature = await dag4.keyStore.dataSign(
        privateKey,
        serialized
    );

    const publicKey = getPublicKey(account)

    const stakingUpdate = {
        value: body,
        proofs: [{ id: publicKey, signature }]
    };

    log(`Signed staking update generated for wallet: ${account.address}: ${JSON.stringify(stakingUpdate, null, 2)}`, "INFO", context);

    return stakingUpdate;
}

const validateStakingCreated = async (
    ammL0Url: string,
    tokenAId: string | null,
    tokenBId: string | null,
    tokenAAllowSpendHash: string,
    tokenBAllowSpendHash: string,
    account: ReturnType<typeof createAccount>,
    logger: (message: string, type?: string, context?: string) => void = log
) => {

    const calculatedState = await getCalculatedState(ammL0Url);
    logger("Pulled calculated state", "INFO", 'AMM')

    const stakingCalculatedState = calculatedState?.operations.Staking?.StakingCalculatedState

    const confirmedStakings = stakingCalculatedState?.confirmed.value || {}
    const pendingStakings = stakingCalculatedState?.pending || []
    const failedStakings = stakingCalculatedState?.failed || []

    log(`Looking for confirmed stakings for wallet: ${account.address}`, "INFO", 'AMM')

    const isConfirmedStaking = (confirmedStakings[account.address] || []).some(
        (staking) =>
            staking.tokenA.identifier === tokenAId
            && staking.tokenB.identifier === tokenBId
            && staking.tokenAAllowSpend === tokenAAllowSpendHash
            && staking.tokenBAllowSpend === tokenBAllowSpendHash
    );

    const isPendingStaking = pendingStakings.some(
        (pendingAction) => {
            const updateValue = isPendingAllowSpend(pendingAction)
                ? pendingAction.PendingAllowSpend.update.value
                : pendingAction.PendingSpendAction.update.value;

            return updateValue.tokenAId === tokenAId && updateValue.tokenBId === tokenBId;
        }
    );

    const isFailedStaking = failedStakings.some(
        (staking) =>
            staking.update.value.tokenAId === tokenAId && staking.update.value.tokenBId === tokenBId
    );

    if (isFailedStaking) {
        throwInContext('AMM')("Staking creation failed.");
    } else if (isPendingStaking) {
        throwInContext('AMM')("Staking creation is pending.");
    } else if (isConfirmedStaking) {
        logger("Staking creation validated!", "INFO", 'AMM')
    } else {
        throwInContext('AMM')("Staking not found.");
    }
}

export { createStakingUpdate, validateStakingCreated, type StakingUpdate, type StakingCalculatedStateAddress, type ConfirmedStakingCalculatedState }
