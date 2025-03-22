import { z } from "zod";
import {
    BaseConfig,
    buildLiquidityPoolUniqueIdentifier,
    createAccount,
    createSignedAllowSpend,
    createSwapUpdate,
    createTokenConfig,
    delay,
    getBalance,
    getBalanceForAddress,
    getCalculatedState,
    getLiquidityPool,
    getSwapQuote,
    log,
    logObject,
    retry,
    sendDataUpdate,
    sendSignedAllowSpend,
    SwapQuoteRequest,
    throwInContext,
    TokenConfig,
    validateIfAllowSpendAcceptedOnCL1,
    validateIfAllowSpendAcceptedOnGL0,
    validateIfAllowSpendAcceptedOnML0,
    validateIfAmountHasBeenSpentCorrectly,
    validateIfBalanceChanged,
    validateIfHasEnoughBalanceForAllowSpend,
    validateIfSyncedToGlobalOrdinal,
    validateLiquidityPoolAmountChanged,
    validateSwapCreated
} from "../../shared";
import { lastGlobalSnapshotIncreasingTest } from "../../shared/tests/last-global-snapshot-increasing";

const InputsSchema = z
    .object({
        privateKey: z.string().min(1, "key cannot be empty"),
        tokenAAllowSpendAmount: z.number().min(1, "amount must be greater than 0"),
        tokenBAllowSpendAmount: z.number().min(1, "amount must be greater than 0"),
        tokenAToSwapAmount: z.number().min(1, "amount must be greater than 0"),
    })
    .refine(({ tokenAAllowSpendAmount, tokenAToSwapAmount }) => tokenAToSwapAmount <= tokenAAllowSpendAmount, {
        message: "tokenAToSwapAmount must be less than or equal to tokenAAllowSpendAmount",
        path: ["tokenASpendAmount"]
    })

const inputs = InputsSchema.parse({
    privateKey: "8971dcc9a07f2db7fa769582139768fd5d73c56501113472977eca6200c679c8",
    tokenAAllowSpendAmount: 50,
    tokenBAllowSpendAmount: 100,
    tokenAToSwapAmount: 25,
});

type SwapConfig = BaseConfig & {
    inputs: z.infer<typeof InputsSchema>
}

const createConfig = (baseConfig: BaseConfig): SwapConfig => {
    return { ...baseConfig, inputs };
};

const processSwapCreation = async (
    config: SwapConfig,
    tokenA: TokenConfig,
    tokenB: TokenConfig,
) => {
    log("Starting swap creation process", "INFO");
    
    const privateKey = config.inputs.privateKey;
    const swapProviderAccount = createAccount(privateKey, config.ammMl0Url, config.ammMl0Url);

    const poolId = buildLiquidityPoolUniqueIdentifier(tokenA.tokenId, tokenB.tokenId);
    log(`Looking for liquidity pool with ID: ${poolId}`, "INFO", 'AMM');
    
    const createdLiquidityPool = await getLiquidityPool(config.ammMl0Url, poolId, log);
    log(`Found liquidity pool: ${JSON.stringify(createdLiquidityPool, null, 2)}`, "INFO", 'AMM');

    if (!createdLiquidityPool) {
        throwInContext('AMM')("Liquidity pool not found.");
        return
    }

    log("Fetching initial balances...", "INFO");
    const initialBalanceA = await getBalance(tokenA);
    const initialBalanceB = await getBalance(tokenB);
    const initialAmmBalanceA = await getBalanceForAddress(config.ammMetagraphId, tokenA.l0Url, tokenA.isCurrency, 'AMM');
    const initialAmmBalanceB = await getBalanceForAddress(config.ammMetagraphId, tokenB.l0Url, tokenB.isCurrency, 'AMM');
    log(`Initial balance for Token: ${initialBalanceA}`, "INFO", tokenA.context);
    log(`Initial balance for Token: ${initialBalanceB}`, "INFO", tokenB.context);
    log(`Initial balance for Token A: ${initialAmmBalanceA}`, "INFO", tokenA.context);
    log(`Initial balance for Token B: ${initialAmmBalanceB}`, "INFO", tokenB.context);

    const signedAllowSpend = await createSignedAllowSpend(
        privateKey,
        { ...tokenA, allowSpendAmount: config.inputs.tokenAAllowSpendAmount },
        config.ammMetagraphId,
    );

    await validateIfHasEnoughBalanceForAllowSpend(tokenA, signedAllowSpend.value.amount);
    log("Balance validation passed", "INFO", tokenA.context);

    const { hash: allowSpendHash } = await sendSignedAllowSpend(
        tokenA.l1Url,
        signedAllowSpend,
        tokenA.context
    );

    log("Fetching swap quote...", "INFO", 'AMM');
    const swapQuoteRequest: SwapQuoteRequest = {
        fromTokenId: tokenA.tokenId,
        toTokenId: tokenB.tokenId,
        amount: config.inputs.tokenAToSwapAmount,
        slippagePercent: 0.5
    };
    
    const swapQuote = await getSwapQuote(config.ammMl0Url, swapQuoteRequest);
    log(`Swap quote: ${JSON.stringify(swapQuote, null, 2)}`, "INFO", 'AMM');

    const estimatedReceived = swapQuote.estimatedReceived;
    log(`Estimated received: ${estimatedReceived}`, "INFO", 'AMM');
        
    const swapUpdate = await createSwapUpdate(
        tokenA.tokenId,
        tokenB.tokenId,
        allowSpendHash,
        swapQuote.amount,
        swapQuote.minimumReceived,
        swapProviderAccount,
        privateKey,
        config.ammMl0Url,
        'AMM'
    );

    await sendDataUpdate(config.ammDl1Url, swapUpdate);

    log("Starting validation process...", "INFO", 'AMM');

    await retry('Validate if allow spend accepted on CL1')(async (logger) => {
        await validateIfAllowSpendAcceptedOnCL1(tokenA.l1Url, allowSpendHash, tokenA.context, logger);
    });

    log("Waiting for propagation...", "INFO", 'AMM');
    delay(5000);

    await retry('Validate if allow spend accepted on ML0')(async (logger) => {
        if (tokenA.tokenId !== null) {
            await validateIfAllowSpendAcceptedOnML0(tokenA.l0Url, swapProviderAccount.address, allowSpendHash, tokenA.tokenId, tokenA.context, logger);
        }
    });

    const globalOrdinal = await retry<number>('Validate if allow spend accepted on GL0')(async (logger) => {
        const foundInOrdinal: number | null = await validateIfAllowSpendAcceptedOnGL0(config.gl0Url, tokenA.account.address, allowSpendHash, tokenA.tokenId, tokenA.context, logger);
        if (foundInOrdinal == null) {
            return throwInContext('AMM')(`No allow spend found for token A on GL0`);
        }
        log(`Found allow spend in ordinal: ${foundInOrdinal}`, "INFO", 'AMM');
        return foundInOrdinal;
    });

    await retry('Validate if calculated state is synced to global ordinal', { delayMs: 5000 })(async (logger) => {
        await validateIfSyncedToGlobalOrdinal(config.ammMl0Url, globalOrdinal, logger);
    });

    await retry('Validate if swap created', { delayMs: 5000 })(async (logger) => {
        await validateSwapCreated(config.ammMl0Url, tokenA.tokenId, tokenB.tokenId, allowSpendHash, swapProviderAccount, logger);
    });

    const finalBalanceA = await getBalance(tokenA);
    const finalBalanceB = await getBalance(tokenB);
    
    log(`Final balance for Token: ${finalBalanceA}`, "INFO", tokenA.context);
    log(`Final balance for Token: ${finalBalanceB}`, "INFO", tokenB.context);
    
    const allowSpendFee = signedAllowSpend.value.fee;
    
    const expectedTokenABalance = Number(initialBalanceA) - swapQuote.amount - allowSpendFee;
    const expectedTokenBBalance = Number(initialBalanceB) + estimatedReceived;
    
    await validateIfBalanceChanged(Number(initialBalanceA), expectedTokenABalance, tokenA);
    await validateIfBalanceChanged(Number(initialBalanceB), expectedTokenBBalance, tokenB);
    
    log("Fetching metagraph balances...", "INFO", 'AMM');
    try {
        const ammAddress = config.ammMetagraphId;

        const ammBalanceA = await getBalanceForAddress(config.ammMetagraphId, tokenA.l0Url, tokenA.isCurrency, 'AMM');
        const ammBalanceB = await getBalanceForAddress(config.ammMetagraphId, tokenB.l0Url, tokenB.isCurrency, 'AMM');

        log(`AMM metagraph balance for Token A: ${ammBalanceA}`, "INFO", 'AMM');
        log(`AMM metagraph balance for Token B: ${ammBalanceB}`, "INFO", 'AMM');

        const expectedAMMTokenABalance = initialAmmBalanceA + swapQuote.amount;
        const expectedAMMTokenBBalance = initialAmmBalanceB - estimatedReceived;

        await validateIfBalanceChanged(Number(initialAmmBalanceA), expectedAMMTokenABalance, tokenA, ammAddress);
        await validateIfBalanceChanged(Number(initialAmmBalanceB), expectedAMMTokenBBalance, tokenB, ammAddress);
    } catch (error) {
        throwInContext('AMM')(`Error fetching AMM metagraph balances: ${error}`);
    }

    log("Swap creation process completed successfully!", "INFO", 'AMM');
}

const swapCurrencyToCurrencyTest = async (config: SwapConfig) => {
    log("Starting swap creation test (Currency to Currency)".toUpperCase(), 'INFO', 'AMM');

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

    await processSwapCreation(config, tokenA, tokenB);
}

const swapDagToCurrencyTest = async (config: SwapConfig) => {
    log("Starting swap creation test (DAG to Currency)".toUpperCase(), 'INFO', 'AMM');

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

    await processSwapCreation(config, tokenA, tokenB);
}

export default async (baseConfig: BaseConfig) => {
    const config = createConfig(baseConfig);

    await lastGlobalSnapshotIncreasingTest({ ammMl0Url: config.ammMl0Url });
    log("=".repeat(80), "INFO");

    await swapCurrencyToCurrencyTest(config);
    log("=".repeat(80), "INFO");

    await swapDagToCurrencyTest(config);
    log("=".repeat(80), "INFO");

    log("All swap creation tests passed!", "INFO", 'AMM');
}