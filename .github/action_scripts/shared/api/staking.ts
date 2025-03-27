import axios from "axios";
import { createAccount, getPublicKey } from "./account";
import { log, Logger, logObject, throwInContext } from "../log";
import { serializeBase64 } from "../serialize";
import { dag4 } from "@stardust-collective/dag4";
import { getCalculatedState, isPendingAllowSpend, isPendingSpendAction, TokenInformation } from "./calculated-state";
import { LastRef, lastRefSchema } from "./last-ref";
import { Signed } from "./signed";
import { singleResponseSchema } from "./response";

type StakingUpdate = {
    source: string
    tokenAAllowSpend: string
    tokenBAllowSpend: string
    tokenAId: string | null
    tokenAAmount: number
    tokenBId: string | null
    parent: LastRef
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

const getLastStakingReference = async (
    address: string,
    l0Url: string
): Promise<LastRef> => {
    const { data } = await axios.get(`${l0Url}/v1/addresses/${address}/stakings/last-reference`);
    const lastRefResponse = singleResponseSchema(lastRefSchema).parse(data);
    return lastRefResponse.data
}

const createStakingUpdate = async (
    tokenAAllowSpendHash: string,
    tokenBAllowSpendHash: string,
    tokenAId: string | null,
    tokenBId: string | null,
    tokenAAmount: number,
    account: ReturnType<typeof createAccount>,
    privateKey: string,
    l0Url: string,
    context: string
): Promise<Signed<StakingUpdateBody>> => {
    log(`Fetching last staking reference for wallet: ${account.address}`, "INFO", context);

    const lastRef = await getLastStakingReference(account.address, l0Url);

    log(`Last staking reference for wallet: ${account.address}: ${JSON.stringify(lastRef, null, 2)}`, "INFO", context);

    const body: StakingUpdateBody = {
        StakingUpdate: {
            maxValidGsEpochProgress: 1000,
            source: account.address,
            tokenAAllowSpend: tokenAAllowSpendHash,
            tokenAAmount,
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
    logger: Logger = log
) => {

    const calculatedState = await getCalculatedState(ammL0Url);
    logger("Pulled calculated state", "INFO", 'AMM')

    const stakingCalculatedState = calculatedState?.operations.Staking?.StakingCalculatedState

    const confirmedStakings = stakingCalculatedState?.confirmed.value || {}
    const pendingStakings = stakingCalculatedState?.pending || []
    const failedStakings = stakingCalculatedState?.failed || []

    log(`Looking for confirmed stakings for wallet: ${account.address}`, "INFO", 'AMM')

    const confirmedStaking = (confirmedStakings[account.address] || []).find(
        (staking) =>
            staking.tokenA.identifier === tokenAId
            && staking.tokenB.identifier === tokenBId
            && staking.tokenAAllowSpend === tokenAAllowSpendHash
            && staking.tokenBAllowSpend === tokenBAllowSpendHash
    );

    const isPendingAllowSpendStaking = pendingStakings.some(
        (pendingAction) => {
            return isPendingAllowSpend(pendingAction)
                && pendingAction.PendingAllowSpend.update.value.tokenAId === tokenAId
                && pendingAction.PendingAllowSpend.update.value.tokenBId === tokenBId
        }
    );

    const isPendingSpendActionStaking = pendingStakings.some(
        (pendingAction) => {
            return isPendingSpendAction(pendingAction)
                && pendingAction.PendingSpendAction.update.value.tokenAId === tokenAId
                && pendingAction.PendingSpendAction.update.value.tokenBId === tokenBId
        }
    );

    const isFailedStaking = failedStakings.some(
        (staking) =>
            staking.update.value.tokenAId === tokenAId && staking.update.value.tokenBId === tokenBId
    );

    if (isFailedStaking) {
        throwInContext('AMM')("Staking creation failed.");
    } else if (isPendingAllowSpendStaking) {
        throwInContext('AMM')("Staking creation is pending (allow spend).");
    } else if (isPendingSpendActionStaking) {
        throwInContext('AMM')("Staking creation is pending (spend action).");
    } else if (confirmedStaking) {
        logger("Staking creation validated!", "INFO", 'AMM')
        return confirmedStaking
    } else {
        throwInContext('AMM')("Staking not found.");
    }
}

export { createStakingUpdate, validateStakingCreated, type StakingUpdate, type StakingCalculatedStateAddress, type ConfirmedStakingCalculatedState }
