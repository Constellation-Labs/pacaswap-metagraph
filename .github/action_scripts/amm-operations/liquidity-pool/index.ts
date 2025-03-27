import { z } from 'zod';
import {
    BaseConfig,
    buildLiquidityPoolUniqueIdentifier,
    createAccount,
    createLiquidityPoolUpdate,
    createSignedAllowSpend, createTokenConfig,
    log,
    sendDataUpdate,
    sendSignedAllowSpend,
    throwInContext,
    TokenConfig, validateIfAllowSpendAcceptedOnCL1,
    validateIfAllowSpendAcceptedOnGL0,
    validateIfAllowSpendAcceptedOnML0,
    validateIfHasEnoughBalanceForAllowSpend,
    validateIfSyncedToGlobalOrdinal,
    validateLiquidityPoolCreated,
    getBalance,
    validateLiquidityPoolAmountChanged,
    validateIfAmountHasBeenSpentCorrectly
} from "../../shared";
import { retry } from "../../shared/retry";
import { lastGlobalSnapshotIncreasingTest } from '../../shared/tests/last-global-snapshot-increasing';

const InputsSchema = z
    .object({
        privateKey: z.string().min(1, "key cannot be empty"),
        tokenAAllowSpendAmount: z.number().min(1, "amount must be greater than 0"),
        tokenBAllowSpendAmount: z.number().min(1, "amount must be greater than 0"),
        tokenAToSpend: z.number().min(1, "amount must be greater than 0"),
        tokenBToSpend: z.number().min(1, "amount must be greater than 0"),
    })
    .refine(({ tokenAToSpend, tokenAAllowSpendAmount }) => tokenAToSpend <= tokenAAllowSpendAmount, {
        message: `Amount to spend on token A cannot exceed the allowed spend amount`,
        path: ["tokenAToSpend"]
    })
    .refine(({ tokenBToSpend, tokenBAllowSpendAmount }) => tokenBToSpend <= tokenBAllowSpendAmount, {
        message: `Amount to spend on token B cannot exceed the allowed spend amount`,
        path: ["tokenBToSpend"]
    })

const inputs = InputsSchema.parse({
    privateKey: "8971dcc9a07f2db7fa769582139768fd5d73c56501113472977eca6200c679c8",
    tokenAAllowSpendAmount: 100,
    tokenBAllowSpendAmount: 200,
    tokenAToSpend: 50,
    tokenBToSpend: 100,
});

type LiquidityPoolConfig = BaseConfig & {
    inputs: z.infer<typeof InputsSchema>
}

const createConfig = (baseConfig: BaseConfig): LiquidityPoolConfig => {
    const config = { ...baseConfig, inputs };
    return config;
};

const processLiquidityPoolCreation = async (
    config: LiquidityPoolConfig,
    tokenA: TokenConfig<{
        allowSpendAmount: number;
        amountToSpend: number;
    }>,
    tokenB: TokenConfig<{
        allowSpendAmount: number;
        amountToSpend: number;
    }>,
) => {
    const privateKey = config.inputs.privateKey;
    const lpProviderAccount = createAccount(privateKey, config.ammMl0Url, config.ammMl0Url);

    const initialBalanceA = await getBalance(tokenA);
    log(`Initial balance: ${initialBalanceA}`, "INFO", tokenA.context);
    const initialBalanceB = await getBalance(tokenB);
    log(`Initial balance: ${initialBalanceB}`, "INFO", tokenB.context);

    log("Created LP provider account", "INFO", 'AMM');

    const signedAllowSpendA = await createSignedAllowSpend(
        privateKey,
        tokenA,
        config.ammMetagraphId,
    );

    const signedAllowSpendB = await createSignedAllowSpend(
        privateKey,
        tokenB,
        config.ammMetagraphId,
        tokenA.tokenId === tokenB.tokenId ? signedAllowSpendA : undefined // Note: For same currency we reference the previous ref directly
    );

    await validateIfHasEnoughBalanceForAllowSpend(tokenA, signedAllowSpendA.value.amount);

    const { hash: tokenAAllowSpendHash } = await sendSignedAllowSpend(
        tokenA.l1Url,
        signedAllowSpendA,
        tokenA.context
    );

    await validateIfHasEnoughBalanceForAllowSpend(tokenB, signedAllowSpendB.value.amount);

    const { hash: tokenBAllowSpendHash } = await sendSignedAllowSpend(
        tokenB.l1Url,
        signedAllowSpendB,
        tokenB.context
    );

    const update = await createLiquidityPoolUpdate(
        tokenAAllowSpendHash,
        tokenBAllowSpendHash,
        tokenA.tokenId,
        tokenB.tokenId,
        tokenA.amountToSpend,
        tokenB.amountToSpend,
        privateKey,
        lpProviderAccount,
    );

    await sendDataUpdate(config.ammDl1Url, update);

    await retry('Validate if allow spends accepted on CL1')(async (logger) => {
        await validateIfAllowSpendAcceptedOnCL1(tokenA.l1Url, tokenAAllowSpendHash, tokenA.context, logger);
        await validateIfAllowSpendAcceptedOnCL1(tokenB.l1Url, tokenBAllowSpendHash, tokenB.context, logger);
    });

    await retry('Validate if allow spends accepted on ML0')(async (logger) => {
        if (tokenA.tokenId !== null) {
            await validateIfAllowSpendAcceptedOnML0(tokenA.l0Url, tokenA.account.address, tokenAAllowSpendHash, tokenA.tokenId, tokenA.context, logger);
        }
        if (tokenB.tokenId !== null) {
            await validateIfAllowSpendAcceptedOnML0(tokenB.l0Url, tokenB.account.address, tokenBAllowSpendHash, tokenB.tokenId, tokenB.context, logger);
        }
    });

    const globalOrdinal = await retry<number>('Validate if allow spends accepted on GL0')(async (logger) => {
        const foundAInOrdinal: number | null = await validateIfAllowSpendAcceptedOnGL0(config.gl0Url, tokenA.account.address, tokenAAllowSpendHash, tokenA.tokenId, tokenA.context, logger);
        const foundBInOrdinal: number | null = await validateIfAllowSpendAcceptedOnGL0(config.gl0Url, tokenB.account.address, tokenBAllowSpendHash, tokenB.tokenId, tokenB.context, logger);
        if (foundAInOrdinal == null && foundBInOrdinal == null) {
            throwInContext('AMM')(`No allow spend found for token A or B on GL0`);
        }
        return Math.min(foundAInOrdinal!, foundBInOrdinal!)
    });

    await retry('Validate if calculated state is synced to global ordinal', { delayMs: 5000 })(async (logger) => {
        await validateIfSyncedToGlobalOrdinal(config.ammMl0Url, globalOrdinal, logger);
    });

    const maxAttempts = tokenA.tokenId === tokenB.tokenId ? 1 : 60;

    await retry('Validate if liquidity pool created', { maxAttempts, delayMs: 5000 })(async (logger) => {
        await validateLiquidityPoolCreated(config.ammMl0Url, tokenA.tokenId, tokenB.tokenId, logger);
    });

    await retry('Validate if liquidity pool amount changed', { maxAttempts: 1 })(async (logger) => {
        const poolId = buildLiquidityPoolUniqueIdentifier(tokenA.tokenId, tokenB.tokenId);
        await validateLiquidityPoolAmountChanged(
            config.ammMl0Url,
            poolId,
            0,
            0,
            tokenA.amountToSpend,
            tokenB.amountToSpend,
            logger
        );
    });

    await retry('Validate if LP owner balance changed', { delayMs: 5000 })(async (logger) => {
        const results = await Promise.allSettled([
            validateIfAmountHasBeenSpentCorrectly(
                initialBalanceA,
                tokenA.amountToSpend,
                signedAllowSpendA,
                tokenA,
                logger
            ),
            validateIfAmountHasBeenSpentCorrectly(
                initialBalanceB,
                tokenB.amountToSpend,
                signedAllowSpendB,
                tokenB,
                logger
            )
        ]);

        const failedResults = results.filter(result => result.status === 'rejected');
        if (failedResults.length > 0) {
            throwInContext('AMM')(`Balance validation failed! Checking if whole allow spends have been spent...`);
        }
    });
}

const lpCreationCurrencyToCurrencyTest = async (config: LiquidityPoolConfig) => {
    log("Starting liquidity pool creation test (Currency to Currency)".toUpperCase());

    const tokenA = await createTokenConfig(
        config.inputs.privateKey,
        config.tokenAMl0Url,
        config.tokenACl1Url,
        'A',
        config.tokenAId,
        true,
        {
            allowSpendAmount: config.inputs.tokenAAllowSpendAmount,
            amountToSpend: config.inputs.tokenAToSpend
        }
    );

    const tokenB = await createTokenConfig(
        config.inputs.privateKey,
        config.tokenBMl0Url,
        config.tokenBCl1Url,
        'B',
        config.tokenBId,
        true,
        {
            allowSpendAmount: config.inputs.tokenBAllowSpendAmount,
            amountToSpend: config.inputs.tokenBToSpend
        }
    );

    await processLiquidityPoolCreation(config, tokenA, tokenB);
}

const lpCreationDagToCurrencyTest = async (config: ReturnType<typeof createConfig>) => {
    log("Starting liquidity pool creation test (DAG to Currency)".toUpperCase());

    const tokenA = await createTokenConfig(
        config.inputs.privateKey,
        config.gl0Url,
        config.dagCl1Url,
        'DAG',
        null,
        false,
        {
            allowSpendAmount: config.inputs.tokenAAllowSpendAmount,
            amountToSpend: config.inputs.tokenAToSpend
        }
    );

    const tokenB = await createTokenConfig(
        config.inputs.privateKey,
        config.tokenBMl0Url,
        config.tokenBCl1Url,
        'B',
        config.tokenBId,
        true,
        {
            allowSpendAmount: config.inputs.tokenBAllowSpendAmount,
            amountToSpend: config.inputs.tokenBToSpend
        }
    );

    await processLiquidityPoolCreation(config, tokenA, tokenB);
}

const lpCreationDagToDagTest = async (config: ReturnType<typeof createConfig>) => {
    log("Starting liquidity pool creation test (DAG to DAG - should fail)".toUpperCase());

    const tokenA = await createTokenConfig(
        config.inputs.privateKey,
        config.gl0Url,
        config.dagCl1Url,
        'DAG',
        null,
        false,
        {
            allowSpendAmount: config.inputs.tokenAAllowSpendAmount,
            amountToSpend: config.inputs.tokenAToSpend
        }
    );

    const tokenB = await createTokenConfig(
        config.inputs.privateKey,
        config.gl0Url,
        config.dagCl1Url,
        'DAG',
        null,
        false,
        {
            allowSpendAmount: config.inputs.tokenBAllowSpendAmount,
            amountToSpend: config.inputs.tokenBToSpend
        }
    );

    try {
        await processLiquidityPoolCreation(config, tokenA, tokenB);
        throw new Error("Expected DAG to DAG liquidity pool creation to fail, but it succeeded");
    } catch (error) {
        log("DAG to DAG liquidity pool creation failed as expected", "INFO", 'AMM');
    }
}

const lpCreationSameCurrencyTest = async (config: ReturnType<typeof createConfig>) => {
    log("Starting liquidity pool creation test (Same Currency to Same Currency - should fail)".toUpperCase());

    const tokenA = await createTokenConfig(
        config.inputs.privateKey,
        config.tokenAMl0Url,
        config.tokenACl1Url,
        'A',
        config.tokenAId,
        true,
        {
            allowSpendAmount: config.inputs.tokenAAllowSpendAmount,
            amountToSpend: config.inputs.tokenAToSpend
        }
    );

    const tokenB = await createTokenConfig(
        config.inputs.privateKey,
        config.tokenAMl0Url,
        config.tokenACl1Url,
        'A',
        config.tokenAId,
        true,
        {
            allowSpendAmount: config.inputs.tokenBAllowSpendAmount,
            amountToSpend: config.inputs.tokenBToSpend
        }
    );

    try {
        await processLiquidityPoolCreation(config, tokenA, tokenB);
        throw new Error("Expected same currency liquidity pool creation to fail, but it succeeded");
    } catch (error) {
        log("Same currency liquidity pool creation failed as expected", "INFO", 'AMM');
    }
}

export default async (baseConfig: BaseConfig) => {
    const config = createConfig(baseConfig);

    await lastGlobalSnapshotIncreasingTest({ ammMl0Url: config.ammMl0Url });
    log("=".repeat(80), "INFO");

    await lpCreationCurrencyToCurrencyTest(config);
    log("=".repeat(80), "INFO");

    await lpCreationDagToCurrencyTest(config);
    log("=".repeat(80), "INFO");

    await lpCreationSameCurrencyTest(config);
    log("=".repeat(80), "INFO");

    await lpCreationDagToDagTest(config);
    log("=".repeat(80), "INFO");

    log("All liquidity pool creation tests passed!", "INFO", 'AMM');
};
