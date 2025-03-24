import axios from "axios";
import { log, logObject, throwInContext } from "../log";
import { createAccount, getPublicKey } from "./account";
import { serializeBase64 } from "../serialize";
import { dag4 } from "@stardust-collective/dag4";
import { z } from "zod";
import { Signed } from "./data-update";
import { getCalculatedState, isPendingAllowSpend, TokenInformation } from "./calculated-state";

type LiquidityPool = {
    poolId: string
    tokenA: TokenInformation
    tokenB: TokenInformation
    owner: string
    k: number
    poolShares: {
        totalShares: number
        addressShares: Record<string, number>
    }
}

type LiquidityPoolUpdate = {
    source: string
    tokenAAllowSpend: string
    tokenBAllowSpend: string
    tokenAId?: string | null
    tokenBId?: string | null
    tokenAAmount: number
    tokenBAmount: number
    maxValidGsEpochProgress: number
}

type LiquidityUpdateBody = {
    LiquidityPoolUpdate: LiquidityPoolUpdate
}

type ConfirmedLiquidityPoolCalculatedState = {
    value: Record<string, LiquidityPool>
}

const createLiquidityPoolUpdate = async (
    tokenAAllowSpendHash: string,
    tokenBAllowSpendHash: string,
    tokenAId: string | null,
    tokenBId: string | null,
    tokenAAllowSpendAmount: number,
    tokenBAllowSpendAmount: number,
    privateKey: string,
    account: ReturnType<typeof createAccount>,
    ammMl0Url: string,
): Promise<Signed<LiquidityUpdateBody>> => {
    const body: LiquidityUpdateBody = {
        LiquidityPoolUpdate: {
            maxValidGsEpochProgress: 1000,
            source: account.address,
            tokenAAllowSpend: tokenAAllowSpendHash,
            tokenAAmount: tokenAAllowSpendAmount,
            tokenAId,
            tokenBAllowSpend: tokenBAllowSpendHash,
            tokenBAmount: tokenBAllowSpendAmount,
            tokenBId,
        }
    };
    const serialized = await serializeBase64(body)
    const signature = await dag4.keyStore.dataSign(
        privateKey,
        serialized
    );

    const publicKey = getPublicKey(account)

    const liquidityPoolUpdate = {
        value: body,
        proofs: [{ id: publicKey, signature }]

    };
    log(`Signed liquidity pool update generated for wallet: ${account.address}: ${JSON.stringify(liquidityPoolUpdate, null, 2)}`, "INFO", 'AMM');

    return liquidityPoolUpdate;
};


const buildLiquidityPoolUniqueIdentifier = (tokenAId: string | null, tokenBId: string | null) => {
    return [tokenAId, tokenBId]
        .filter(id => id !== null && id !== undefined)
        .sort()
        .join('-');
};

const getLiquidityPool = async (
    ammL0Url: string,
    poolId: string,
    logger: (message: string, level: string, context: string) => void
) => {
    logger(`Getting liquidity pool data for ${poolId}`, "INFO", 'AMM');

    try {

        const { data } = await axios.get(
            `${ammL0Url}/v1/liquidity-pools/${poolId}`
        );
        logger(`Liquidity pool data received for ${poolId}`, "INFO", 'AMM');
        logObject(data, "AMM");

        const TokenInfoResponseSchema = z.object({
            id: z.string().nullable(),
            amount: z.number(),
            price: z.number()
        });

        const LiquidityPoolResponseSchema = z.object({
            data: z.object({
                poolId: z.string(),
                tokenA: TokenInfoResponseSchema,
                tokenB: TokenInfoResponseSchema,
                owner: z.string(),
                k: z.number(),
                totalShared: z.number()
            })
        });

        return LiquidityPoolResponseSchema.parse(data);
    } catch (error) {
        logger(`Error getting liquidity pool data for ${poolId}: ${error}`, "ERROR", 'AMM');
        throwInContext('AMM')(`Error getting liquidity pool data for ${poolId}`);
    }
}

const validateLiquidityPoolCreated = async (
    ammL0Url: string,
    account: ReturnType<typeof createAccount>,
    tokenAId: string | null,
    tokenBId: string | null,
    logger: (message: string, type?: string, context?: string) => void
) => {

    const calculatedState = await getCalculatedState(ammL0Url);
    logger("Pulled calculated state", "INFO", 'AMM')

    const liquidityPoolCalculatedState = calculatedState?.operations.LiquidityPool?.LiquidityPoolCalculatedState
    const confirmedLiquidityPool = liquidityPoolCalculatedState?.confirmed.value ?? {}
    const pendingLiquidityPool = liquidityPoolCalculatedState?.pending ?? []
    const failedLiquidityPool = liquidityPoolCalculatedState?.failed ?? []

    const isConfirmedLiquidityPool = Object.values(confirmedLiquidityPool).some(
        (liquidityPool) => liquidityPool.tokenA.identifier === tokenAId && liquidityPool.tokenB.identifier === tokenBId
    );

    const isPendingLiquidityPool = pendingLiquidityPool.some(
        (pendingAction) => {
            const updateValue = isPendingAllowSpend(pendingAction)
                ? pendingAction.PendingAllowSpend.update.value
                : pendingAction.PendingSpendAction.update.value;

            return updateValue.tokenAId === tokenAId && updateValue.tokenBId === tokenBId
        }
    );

    const isFailedLiquidityPool = failedLiquidityPool.some(
        (failed) => {
            failed.update.value.tokenAId === tokenAId && failed.update.value.tokenBId === tokenBId
        }
    );

    if (isFailedLiquidityPool) {
        throwInContext('AMM')("Liquidity pool creation failed.");
    } else if (isPendingLiquidityPool) {
        throwInContext('AMM')("Liquidity pool creation is pending.");
    } else if (isConfirmedLiquidityPool) {
        logger("Liquidity pool creation validated!", "INFO", 'AMM')
    } else {
        throwInContext('AMM')("Liquidity pool not found.");
    }
}

const validateLiquidityPoolAmountChanged = async (
    ammMl0Url: string,
    poolId: string,
    initialTokenABalance: number,
    initialTokenBBalance: number,
    expectedTokenABalance: number,
    expectedTokenBBalance: number,
    logger: (message: string, level: string, context: string) => void
) => {
    const liquidityPool = await getLiquidityPool(ammMl0Url, poolId, logger);
    if (!liquidityPool) {
        throwInContext('AMM')("Liquidity pool not found.");
        return
    }
    const tokenAPoolBalance = liquidityPool.data.tokenA.amount;
    const tokenBPoolBalance = liquidityPool.data.tokenB.amount;

    if (tokenAPoolBalance === expectedTokenABalance && tokenBPoolBalance === expectedTokenBBalance) {
        logger(`Liquidity pool token A amount changed from ${initialTokenABalance} to ${tokenAPoolBalance}`, "INFO", 'AMM');
        logger(`Liquidity pool token B amount changed from ${initialTokenBBalance} to ${tokenBPoolBalance}`, "INFO", 'AMM');
    } else {
        throwInContext('AMM')(`Liquidity pool token amount did not change.`);
    }
}

export {
    createLiquidityPoolUpdate,
    validateLiquidityPoolCreated,
    type LiquidityPool,
    type LiquidityPoolUpdate,
    getLiquidityPool,
    validateLiquidityPoolAmountChanged,
    buildLiquidityPoolUniqueIdentifier,
    type ConfirmedLiquidityPoolCalculatedState
}
