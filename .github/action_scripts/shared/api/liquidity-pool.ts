import axios from "axios";
import { log, Logger, logObject, throwInContext } from "../log";
import { createAccount, getPublicKey } from "./account";
import { serializeBase64 } from "../serialize";
import { dag4 } from "@stardust-collective/dag4";
import { z } from "zod";
import { getCalculatedState, isPendingAllowSpend, isPendingSpendAction, TokenInformation } from "./calculated-state";
import { Signed } from "./signed";

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

type PoolFees = {
    total: number
    providers: number
    operators: number
}

type LiquidityPoolUpdate = {
    metagraphId: string
    source: string
    tokenAAllowSpend: string
    tokenBAllowSpend: string
    tokenAId?: string | null
    tokenBId?: string | null
    tokenAAmount: number
    tokenBAmount: number
    maxValidGsEpochProgress: number,
    poolFees: PoolFees
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
    ammMetagraphId: string,
    tokenAAmount: number,
    tokenBAmount: number,
    privateKey: string,
    account: ReturnType<typeof createAccount>,
): Promise<Signed<LiquidityUpdateBody>> => {
    const body: LiquidityUpdateBody = {
        LiquidityPoolUpdate: {
            maxValidGsEpochProgress: 1000,
            metagraphId: ammMetagraphId,
            source: account.address,
            tokenAAllowSpend: tokenAAllowSpendHash,
            tokenAAmount,
            tokenAId,
            tokenBAllowSpend: tokenBAllowSpendHash,
            tokenBAmount,
            tokenBId,
            poolFees: {
                total: 0.0,
                providers: 0.0,
                operators: 0.0
            }
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

const getLiquidityPoolShares = async (
    ammL0Url: string,
    poolId: string,
    address: string,
    logger: Logger
) => {
    const liquidityPoolSharesResponseSchema = z.object({
        data: z.object({
            poolId: z.string(),
            address: z.string(),
            shares: z.number()
        })
    });
    const { data } = await axios.get(
        `${ammL0Url}/v1/liquidity-pools/${poolId}/shares/${address}`
    );
    logger(`Liquidity pool shares received for ${poolId}`, "INFO", 'AMM');
    return liquidityPoolSharesResponseSchema.parse(data);
}

const getLiquidityPool = async (
    ammL0Url: string,
    poolId: string,
    logger: Logger
) => {
    logger(`Getting liquidity pool data for ${poolId}`, "INFO", 'AMM');

    try {

        const { data } = await axios.get(
            `${ammL0Url}/v1/liquidity-pools/${poolId}`
        );
        logger(`Liquidity pool data received for ${poolId}`, "INFO", 'AMM');

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
    tokenAId: string | null,
    tokenBId: string | null,
    logger: Logger
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

    const isPendingAllowSpendLiquidityPool = pendingLiquidityPool.some(
        (pendingAction) => {
            return isPendingAllowSpend(pendingAction)
                && pendingAction.PendingAllowSpend.update.value.tokenAId === tokenAId
                && pendingAction.PendingAllowSpend.update.value.tokenBId === tokenBId
        }
    );

    const isPendingSpendActionLiquidityPool = pendingLiquidityPool.some(
        (pendingAction) => {
            return isPendingSpendAction(pendingAction)
                && pendingAction.PendingSpendAction.update.value.tokenAId === tokenAId
                && pendingAction.PendingSpendAction.update.value.tokenBId === tokenBId
        }
    );

    const isFailedLiquidityPool = failedLiquidityPool.some(
        (failed) => {
            failed.update.value.tokenAId === tokenAId && failed.update.value.tokenBId === tokenBId
        }
    );

    if (isFailedLiquidityPool) {
        throwInContext('AMM')("Liquidity pool creation failed.");
    } else if (isPendingAllowSpendLiquidityPool) {
        throwInContext('AMM')("Liquidity pool creation is pending (allow spend).");
    } else if (isPendingSpendActionLiquidityPool) {
        throwInContext('AMM')("Liquidity pool creation is pending (spend action).");
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
    logger: Logger
) => {
    const liquidityPool = await getLiquidityPool(ammMl0Url, poolId, logger);
    if (!liquidityPool) {
        throwInContext('AMM')("Liquidity pool not found.");
        return
    }
    const tokenAPoolBalance = liquidityPool.data.tokenA.amount;
    const tokenBPoolBalance = liquidityPool.data.tokenB.amount;

    if (tokenAPoolBalance !== expectedTokenABalance) {
        const msg = `Liquidity pool token A amount did not change. Actual: ${tokenAPoolBalance} (Δ ${tokenAPoolBalance - initialTokenABalance}). Expected: ${expectedTokenABalance} (Δ ${expectedTokenABalance - initialTokenABalance})`;
        throwInContext('AMM')(msg);
    }

    if (tokenBPoolBalance !== expectedTokenBBalance) {
        const msg = `Liquidity pool token B amount did not change. Actual: ${tokenBPoolBalance} (Δ ${tokenBPoolBalance - initialTokenABalance}). Expected: ${expectedTokenBBalance} (Δ ${expectedTokenBBalance - initialTokenBBalance})`;
        throwInContext('AMM')(msg);
    }
}

export {
    createLiquidityPoolUpdate,
    validateLiquidityPoolCreated,
    type LiquidityPool,
    type LiquidityPoolUpdate,
    getLiquidityPool,
    validateLiquidityPoolAmountChanged,
    getLiquidityPoolShares,
    buildLiquidityPoolUniqueIdentifier,
    type ConfirmedLiquidityPoolCalculatedState
}
