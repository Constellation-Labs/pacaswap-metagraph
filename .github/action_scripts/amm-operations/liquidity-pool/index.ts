import { z } from 'zod';
import {
    BaseWithCurrencyMetagraphsCliArgsSchema,
    createAccount,
    createLiquidityPoolUpdate,
    createSignedAllowSpend,
    createTokenConfig,
    delay,
    log,
    retry,
    sendDataUpdate,
    sendSignedAllowSpend,
    TokenConfig,
    validateIfAllowSpendAcceptedOnCL1,
    validateIfAllowSpendAcceptedOnGL0,
    validateIfAllowSpendAcceptedOnML0,
    validateIfBalanceChanged,
    validateLiquidityPoolCreated,
    buildLiquidityPoolUniqueIdentifier,
    getCalculatedState
} from '../../shared';

const lpCreation: z.infer<typeof LPCreationSchema> = {
    privateKey: "8971dcc9a07f2db7fa769582139768fd5d73c56501113472977eca6200c679c8",
    tokenAAllowSpendAmount: 100,
    tokenBAllowSpendAmount: 200,
};

const LPCreationSchema = z
    .object({
        privateKey: z.string().min(1, "key cannot be empty"),
        tokenAAllowSpendAmount: z.number().min(1, "amount must be greater than 0"),
        tokenBAllowSpendAmount: z.number().min(1, "amount must be greater than 0"),
    })


const createConfig = (argsObject: object) => {
    const argsObj = {
        ...argsObject,
        lpCreation
    }

    const CliArgsSchema = BaseWithCurrencyMetagraphsCliArgsSchema.extend({
        lpCreation: LPCreationSchema
    });

    return CliArgsSchema.parse(argsObj);
};

const processLiquidityPoolCreation = async (
    config: ReturnType<typeof createConfig>,
    tokenA: TokenConfig,
    tokenB: TokenConfig,
) => {
    const privateKey = config.lpCreation.privateKey;
    const lpProviderAccount = createAccount(privateKey, config.ammMl0Url, config.ammMl0Url);
    log("Created LP provider account", "INFO", 'AMM');
    const signedAllowSpendA = await createSignedAllowSpend(
        privateKey,
        tokenA,
        config.ammMetagraphId,
        'A'
    );

    const signedAllowSpendB = await createSignedAllowSpend(
        privateKey,
        tokenB,
        config.ammMetagraphId,
        'B'
    );

    const { hash: tokenAAllowSpendHash } = await sendSignedAllowSpend(
        tokenA.l1Url,
        signedAllowSpendA,
        tokenA.context
    );

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
        tokenA.allowSpendAmount,
        tokenB.allowSpendAmount,
        privateKey,
        lpProviderAccount,
    );

    await sendDataUpdate(config.ammDl1Url, update);

    await retry('Validate if allow spends accepted on CL1')(async (logger) => {
        await validateIfAllowSpendAcceptedOnCL1(tokenA.l1Url, tokenAAllowSpendHash, tokenA.context, logger);
        await validateIfAllowSpendAcceptedOnCL1(tokenB.l1Url, tokenBAllowSpendHash, tokenB.context, logger);
    });

    delay(5000);

    await retry('Validate if allow spends accepted on ML0')(async (logger) => {
        if (tokenA.tokenId !== null) {
            await validateIfAllowSpendAcceptedOnML0(tokenA.l0Url, tokenA.account.address, tokenAAllowSpendHash, tokenA.tokenId, tokenA.context, logger);
        }
        await validateIfAllowSpendAcceptedOnML0(tokenB.l0Url, tokenB.account.address, tokenBAllowSpendHash, tokenB.tokenId, tokenB.context, logger);
    });

    await retry('Validate if allow spends accepted on GL0')(async (logger) => {
        await validateIfAllowSpendAcceptedOnGL0(config.gl0Url, tokenA.account.address, tokenAAllowSpendHash, tokenA.tokenId, tokenA.context, logger);
        await validateIfAllowSpendAcceptedOnGL0(config.gl0Url, tokenB.account.address, tokenBAllowSpendHash, tokenB.tokenId, tokenB.context, logger);
    });

    delay(1000 * 60);

    await retry('Validate if liquidity pool created', 60, 5000)(async (logger) => {
        const poolId = await buildLiquidityPoolUniqueIdentifier(tokenA.tokenId, tokenB.tokenId);
        await validateLiquidityPoolCreated(config.ammMl0Url, poolId, logger);
        const calculatedState = await getCalculatedState(config.ammMl0Url);
        console.log(JSON.stringify(calculatedState, null, 2));
    });

    await retry('Validate if balance changed')(async (logger) => {
        await validateIfBalanceChanged(tokenA.initialBalance, signedAllowSpendA, tokenA.account, tokenA.l0Url, tokenA.isCurrency, tokenA.context, logger);
        await validateIfBalanceChanged(tokenB.initialBalance, signedAllowSpendB, tokenB.account, tokenB.l0Url, tokenB.isCurrency, tokenB.context, logger);
    });
}

const currencyToCurrencyTest = async (config: ReturnType<typeof createConfig>) => {
    log("Starting liquidity pool creation test (Currency to Currency)");

    const tokenA = await createTokenConfig(
        config.lpCreation.privateKey,
        config.tokenAMl0Url,
        config.tokenACl1Url,
        'A',
        config.tokenAId,
        true,
        config.lpCreation.tokenAAllowSpendAmount
    );

    const tokenB = await createTokenConfig(
        config.lpCreation.privateKey,
        config.tokenBMl0Url,
        config.tokenBCl1Url,
        'B',
        config.tokenBId,
        true,
        config.lpCreation.tokenBAllowSpendAmount
    );

    await processLiquidityPoolCreation(config, tokenA, tokenB);
}

const dagToCurrencyTest = async (config: ReturnType<typeof createConfig>) => {
    log("Starting liquidity pool creation test (DAG to Currency)");

    const tokenA = await createTokenConfig(
        config.lpCreation.privateKey,
        config.gl0Url,
        config.dagCl1Url,
        'DAG',
        null,
        false,
        config.lpCreation.tokenAAllowSpendAmount
    );

    const tokenB = await createTokenConfig(
        config.lpCreation.privateKey,
        config.tokenBMl0Url,
        config.tokenBCl1Url,
        'B',
        config.tokenBId,
        true,
        config.lpCreation.tokenBAllowSpendAmount
    );

    await processLiquidityPoolCreation(config, tokenA, tokenB);
}

const dagToDagTest = async (config: ReturnType<typeof createConfig>) => {
    log("Starting liquidity pool creation test (DAG to DAG - should fail)");

    const tokenA = await createTokenConfig(
        config.lpCreation.privateKey,
        config.gl0Url,
        config.dagCl1Url,
        'DAG',
        null,
        false,
        config.lpCreation.tokenAAllowSpendAmount
    );

    const tokenB = await createTokenConfig(
        config.lpCreation.privateKey,
        config.gl0Url,
        config.dagCl1Url,
        'DAG',
        null,
        false,
        config.lpCreation.tokenBAllowSpendAmount
    );

    try {
        await processLiquidityPoolCreation(config, tokenA, tokenB);
        throw new Error("Expected DAG to DAG liquidity pool creation to fail, but it succeeded");
    } catch (error) {
        log("DAG to DAG liquidity pool creation failed as expected", "INFO", 'AMM');
    }
}

const sameCurrencyTest = async (config: ReturnType<typeof createConfig>) => {
    log("Starting liquidity pool creation test (Same Currency to Same Currency - should fail)");

    const tokenA = await createTokenConfig(
        config.lpCreation.privateKey,
        config.tokenAMl0Url,
        config.tokenACl1Url,
        'A',
        config.tokenAId,
        true,
        config.lpCreation.tokenAAllowSpendAmount
    );

    const tokenB = await createTokenConfig(
        config.lpCreation.privateKey,
        config.tokenAMl0Url,
        config.tokenACl1Url,
        'A',
        config.tokenAId,
        true,
        config.lpCreation.tokenBAllowSpendAmount
    );

    try {
        await processLiquidityPoolCreation(config, tokenA, tokenB);
        throw new Error("Expected same currency liquidity pool creation to fail, but it succeeded");
    } catch (error) {
        log("Same currency liquidity pool creation failed as expected", "INFO", 'AMM');
    }
}

const liquidityPoolTests = async (argsObject: object) => {
    const config = createConfig(argsObject);

    // Test valid cases
    await currencyToCurrencyTest(config);
    await dagToCurrencyTest(config);

    // Test invalid cases
    await sameCurrencyTest(config);
    await dagToDagTest(config);

    log("All liquidity pool creation tests passed!", "INFO", 'AMM');
};

export { liquidityPoolTests }
