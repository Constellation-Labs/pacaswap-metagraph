import { z } from "zod";
import { BaseConfig, buildLiquidityPoolUniqueIdentifier, createAccount, createSignedAllowSpend, createStakingUpdate, createTokenConfig, delay, getBalance, getCalculatedState, getLiquidityPool, log, retry, sendDataUpdate, sendSignedAllowSpend, throwInContext, TokenConfig, TokenConfigWithAllowSpend, validateIfAllowSpendAcceptedOnCL1, validateIfAllowSpendAcceptedOnGL0, validateIfAllowSpendAcceptedOnML0, validateIfBalanceChangedByAllowSpend, validateIfHasEnoughBalanceForAllowSpend, validateIfSyncedToGlobalOrdinal, validateLiquidityPoolAmountChanged, validateStakingCreated } from "../../shared";
import { lastGlobalSnapshotIncreasingTest } from "../../shared/tests/last-global-snapshot-increasing";

const InputsSchema = z
    .object({
        privateKey: z.string().min(1, "key cannot be empty"),
        tokenAAllowSpendAmount: z.number().min(1, "amount must be greater than 0"),
        tokenBAllowSpendAmount: z.number().min(1, "amount must be greater than 0"),
    })

const inputs = InputsSchema.parse({
    privateKey: "8971dcc9a07f2db7fa769582139768fd5d73c56501113472977eca6200c679c8",
    tokenAAllowSpendAmount: 50,
    tokenBAllowSpendAmount: 100,
});

type StakingConfig = BaseConfig & {
    inputs: z.infer<typeof InputsSchema>
}

const createConfig = (baseConfig: BaseConfig): StakingConfig => {
    return { ...baseConfig, inputs };
};

const processStakingCreation = async (
    config: StakingConfig,
    tokenA: TokenConfigWithAllowSpend,
    tokenB: TokenConfigWithAllowSpend,
) => {
    const privateKey = config.inputs.privateKey;
    const stakingProviderAccount = createAccount(privateKey, config.ammMl0Url, config.ammMl0Url);
    log("Created staking provider account", "INFO", 'AMM');

    const initialBalanceA = await getBalance(tokenA);
    log(`Initial balance: ${initialBalanceA}`, "INFO", tokenA.context);
    const initialBalanceB = await getBalance(tokenB);
    log(`Initial balance: ${initialBalanceB}`, "INFO", tokenB.context);

    const poolId = buildLiquidityPoolUniqueIdentifier(tokenA.tokenId, tokenB.tokenId);
    const createdLiquidityPool = await getLiquidityPool(config.ammMl0Url, poolId, log);

    if (!createdLiquidityPool) {
        throwInContext('AMM')("Liquidity pool not found.");
        return
    }

    const tokenAPoolBalance = createdLiquidityPool.data.tokenA.amount;
    const tokenBPoolBalance = createdLiquidityPool.data.tokenB.amount;

    const signedAllowSpendA = await createSignedAllowSpend(
        privateKey,
        tokenA,
        config.ammMetagraphId,
    );

    await validateIfHasEnoughBalanceForAllowSpend(tokenA, signedAllowSpendA.value.amount);

    const { hash: tokenAAllowSpendHash } = await sendSignedAllowSpend(
        tokenA.l1Url,
        signedAllowSpendA,
        tokenA.context
    );

    const signedAllowSpendB = await createSignedAllowSpend(
        privateKey,
        tokenB,
        config.ammMetagraphId,
        tokenA.tokenId === tokenB.tokenId ? signedAllowSpendA : undefined // Note: For same currency we reference the previous ref directly
    );

    await validateIfHasEnoughBalanceForAllowSpend(tokenB, signedAllowSpendB.value.amount);

    const { hash: tokenBAllowSpendHash } = await sendSignedAllowSpend(
        tokenB.l1Url,
        signedAllowSpendB,
        tokenB.context
    );

    const stakingUpdate = await createStakingUpdate(
        tokenAAllowSpendHash,
        tokenBAllowSpendHash,
        tokenA.tokenId,
        tokenB.tokenId,
        tokenA.allowSpendAmount,
        stakingProviderAccount,
        privateKey,
        config.ammMl0Url,
        'AMM'
    );

    await sendDataUpdate(config.ammDl1Url, stakingUpdate);

    await retry('Validate if allow spends accepted on CL1')(async (logger) => {
        await validateIfAllowSpendAcceptedOnCL1(tokenA.l1Url, tokenAAllowSpendHash, tokenA.context, logger);
        await validateIfAllowSpendAcceptedOnCL1(tokenB.l1Url, tokenBAllowSpendHash, tokenB.context, logger);
    });

    delay(5000);

    await retry('Validate if allow spends accepted on ML0')(async (logger) => {
        if (tokenA.tokenId !== null) {
            await validateIfAllowSpendAcceptedOnML0(tokenA.l0Url, stakingProviderAccount.address, tokenAAllowSpendHash, tokenA.tokenId, tokenA.context, logger);
        }
        await validateIfAllowSpendAcceptedOnML0(tokenB.l0Url, stakingProviderAccount.address, tokenBAllowSpendHash, tokenB.tokenId, tokenB.context, logger);
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

    await retry('Validate if staking created', { delayMs: 5000 })(async (logger) => {
        await validateStakingCreated(config.ammMl0Url, tokenA.tokenId, tokenB.tokenId, tokenAAllowSpendHash, tokenBAllowSpendHash, stakingProviderAccount, logger);
    });

    await retry('Validate if balance changed')(async (logger) => {
        await validateIfBalanceChangedByAllowSpend(initialBalanceA, signedAllowSpendA, tokenA, logger);
        await validateIfBalanceChangedByAllowSpend(initialBalanceB, signedAllowSpendB, tokenB, logger);
    });

    await retry('Validate if liquidity pool token amount changed')(async (logger) => {
        const tokenBPrice = signedAllowSpendB.value.amount / signedAllowSpendA.value.amount;
        const expectedTokenABalance = tokenAPoolBalance + signedAllowSpendA.value.amount
        const expectedTokenBBalance = tokenBPoolBalance + signedAllowSpendA.value.amount * tokenBPrice;

        await validateLiquidityPoolAmountChanged(
            config.ammMl0Url,
            poolId,
            tokenAPoolBalance,
            tokenBPoolBalance,
            expectedTokenABalance,
            expectedTokenBBalance,
            logger
        );
    });
}

const stakingCurrencyToCurrencyTest = async (config: StakingConfig) => {
    log("Starting staking creation test (Currency to Currency)".toUpperCase(), 'INFO', 'AMM');

    const tokenA = await createTokenConfig(
        config.inputs.privateKey,
        config.tokenAMl0Url,
        config.tokenACl1Url,
        'A',
        config.tokenAId,
        true,
        { allowSpendAmount: config.inputs.tokenAAllowSpendAmount }
    );

    const tokenB = await createTokenConfig(
        config.inputs.privateKey,
        config.tokenBMl0Url,
        config.tokenBCl1Url,
        'B',
        config.tokenBId,
        true,
        { allowSpendAmount: config.inputs.tokenBAllowSpendAmount }
    );

    await processStakingCreation(config, tokenA, tokenB);
}

const stakingDagToCurrencyTest = async (config: StakingConfig) => {
    log("Starting staking creation test (DAG to Currency)".toUpperCase(), 'INFO', 'AMM');

    const tokenA = await createTokenConfig(
        config.inputs.privateKey,
        config.gl0Url,
        config.dagCl1Url,
        'DAG',
        null,
        false,
        { allowSpendAmount: config.inputs.tokenAAllowSpendAmount }
    );

    const tokenB = await createTokenConfig(
        config.inputs.privateKey,
        config.tokenBMl0Url,
        config.tokenBCl1Url,
        'B',
        config.tokenBId,
        true,
        { allowSpendAmount: config.inputs.tokenBAllowSpendAmount }
    );

    await processStakingCreation(config, tokenA, tokenB);
}

const stakingDagToDagTest = async (config: ReturnType<typeof createConfig>) => {
    log("Starting staking creation test (DAG to DAG - should fail)".toUpperCase(), 'INFO', 'AMM');

    const tokenA = await createTokenConfig(
        config.inputs.privateKey,
        config.gl0Url,
        config.dagCl1Url,
        'DAG',
        null,
        false,
        { allowSpendAmount: config.inputs.tokenAAllowSpendAmount }
    );

    const tokenB = await createTokenConfig(
        config.inputs.privateKey,
        config.gl0Url,
        config.dagCl1Url,
        'DAG',
        null,
        false,
        { allowSpendAmount: config.inputs.tokenBAllowSpendAmount }
    );

    try {
        await processStakingCreation(config, tokenA, tokenB);
        throw new Error("Expected DAG to DAG staking creation to fail, but it succeeded");
    } catch (error) {
        log("DAG to DAG staking creation failed as expected", "INFO", 'AMM');
    }
}

const stakingSameCurrencyTest = async (config: ReturnType<typeof createConfig>) => {
    log("Starting staking creation test (Same Currency to Same Currency - should fail)".toUpperCase(), 'INFO', 'AMM');

    const tokenA = await createTokenConfig(
        config.inputs.privateKey,
        config.tokenAMl0Url,
        config.tokenACl1Url,
        'A',
        config.tokenAId,
        true,
        { allowSpendAmount: config.inputs.tokenAAllowSpendAmount }
    );

    const tokenB = await createTokenConfig(
        config.inputs.privateKey,
        config.tokenAMl0Url,
        config.tokenACl1Url,
        'A',
        config.tokenAId,
        true,
        { allowSpendAmount: config.inputs.tokenBAllowSpendAmount }
    );

    try {
        await processStakingCreation(config, tokenA, tokenB);
        throw new Error("Expected same currency staking creation to fail, but it succeeded");
    } catch (error) {
        log("Same currency staking creation failed as expected", "INFO", 'AMM');
    }
}

export default async (baseConfig: BaseConfig) => {
    const config = createConfig(baseConfig);

    await lastGlobalSnapshotIncreasingTest({ ammMl0Url: config.ammMl0Url });
    log("=".repeat(80), "INFO");

    await stakingCurrencyToCurrencyTest(config);
    log("=".repeat(80), "INFO");

    await stakingDagToCurrencyTest(config);
    log("=".repeat(80), "INFO");

    await stakingDagToDagTest(config);
    log("=".repeat(80), "INFO");

    await stakingSameCurrencyTest(config);
    log("=".repeat(80), "INFO");

    log("All staking creation tests passed!", "INFO", 'AMM');
}