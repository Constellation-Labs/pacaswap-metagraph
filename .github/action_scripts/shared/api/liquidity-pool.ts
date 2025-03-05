import axios from "axios";
import { log, throwInContext } from "../log";
import { createAccount, getPublicKey } from "./account";
import { serializeBase64 } from "../serialize";
import { dag4 } from "@stardust-collective/dag4";
import { z } from "zod";
import { Signed, TokenInformation } from "./data-update";

type LiquidityPool = {
    tokenA: TokenInformation
    tokenB: TokenInformation
}

type LiquidityPoolUpdate = {
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

const createLiquidityPoolUpdate = async (
    tokenAAllowSpendHash: string,
    tokenBAllowSpendHash: string,
    tokenAId: string | null,
    tokenBId: string | null,
    tokenAAllowSpendAmount: number,
    tokenBAllowSpendAmount: number,
    privateKey: string,
    account: ReturnType<typeof createAccount>,
): Promise<Signed<LiquidityUpdateBody>> => {
    const body: LiquidityUpdateBody = {
        LiquidityPoolUpdate: {
            maxValidGsEpochProgress: 50,
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
        console.log(JSON.stringify(data, null, 2));

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
        throwInContext('AMM')("Error getting liquidity pool data.");
    }
}

const validateLiquidityPoolCreated = async (
    ammL0Url: string,
    poolId: string,
    logger: (message: string, type?: string, context?: string) => void = log
) => {
    try {
        const liquidityPool = await getLiquidityPool(ammL0Url, poolId, logger)
        if (liquidityPool) {
            logger(`Liquidity pool found with id ${liquidityPool.data.poolId}`, "INFO", 'AMM')
            return liquidityPool
        } else {
            logger(`Liquidity pool not found with id ${poolId}`, "INFO", 'AMM')
            throwInContext('AMM')("Liquidity pool not found.");
        }
    } catch (error) {
        logger(`Liquidity pool not found with id ${poolId}`, "INFO", 'AMM')
        throwInContext('AMM')("Liquidity pool not found.");
    }
}

const validateLiquidityPoolAmountChanged = async (
    ammMl0Url: string,
    poolId: string,
    initialTokenABalance: number,
    initialTokenBBalance: number,
    logger: (message: string, level: string, context: string) => void
) => {
    const liquidityPoolAfterStaking = await getLiquidityPool(ammMl0Url, poolId, logger);
    if (!liquidityPoolAfterStaking) {
        throwInContext('AMM')("Liquidity pool not found.");
        return
    }
    const tokenAPoolBalanceAfterStaking = liquidityPoolAfterStaking.data.tokenA.amount;
    const tokenBPoolBalanceAfterStaking = liquidityPoolAfterStaking.data.tokenB.amount;

    if (tokenAPoolBalanceAfterStaking !== initialTokenABalance && tokenBPoolBalanceAfterStaking !== initialTokenBBalance) {
        logger(`Liquidity pool token A amount changed from ${initialTokenABalance} to ${tokenAPoolBalanceAfterStaking}`, "INFO", 'AMM');
        logger(`Liquidity pool token B amount changed from ${initialTokenBBalance} to ${tokenBPoolBalanceAfterStaking}`, "INFO", 'AMM');
    } else {
        throwInContext('AMM')(`Liquidity pool token amount did not change. Expected ${initialTokenABalance} vs ${tokenAPoolBalanceAfterStaking} and ${initialTokenBBalance} vs ${tokenBPoolBalanceAfterStaking}.`);
    }
}

export {
    createLiquidityPoolUpdate,
    validateLiquidityPoolCreated,
    type LiquidityPool,
    type TokenInformation,
    getLiquidityPool,
    validateLiquidityPoolAmountChanged,
    buildLiquidityPoolUniqueIdentifier,
}