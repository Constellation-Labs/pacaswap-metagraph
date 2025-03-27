import { z } from "zod";
import { BaseConfig, buildLiquidityPoolUniqueIdentifier, createAccount, createTokenConfig, getBalance, getLiquidityPool, getLiquidityPoolShares, log, retry, sendDataUpdate, throwInContext, TokenConfig, validateIfBalanceChanged, validateLiquidityPoolAmountChanged } from "../../shared";
import { lastGlobalSnapshotIncreasingTest } from "../../shared/tests/last-global-snapshot-increasing";
import { createWithdrawalUpdate, validateWithdrawalCreated, WithdrawalCalculatedStateAddress } from "../../shared/api/withdrawal";

const InputsSchema = z
    .object({
        privateKey: z.string().min(1, "key cannot be empty"),
    })

const inputs = InputsSchema.parse({
    privateKey: "8971dcc9a07f2db7fa769582139768fd5d73c56501113472977eca6200c679c8",
});

type WithdrawalConfig = BaseConfig & {
    inputs: z.infer<typeof InputsSchema>
}

const createConfig = (baseConfig: BaseConfig): WithdrawalConfig => {
    return { ...baseConfig, inputs };
};

const processWithdrawal = async (
    config: WithdrawalConfig,
    tokenA: TokenConfig,
    tokenB: TokenConfig,
) => {
    const privateKey = config.inputs.privateKey;
    const withdrawalAccount = createAccount(privateKey, config.ammMl0Url, config.ammMl0Url);

    const poolId = buildLiquidityPoolUniqueIdentifier(tokenA.tokenId, tokenB.tokenId);
    const createdLiquidityPool = await getLiquidityPool(config.ammMl0Url, poolId, log);

    if (!createdLiquidityPool) {
        throwInContext('AMM')(`Error getting liquidity pool data for ${poolId}`);
        return
    }

    const initialBalanceA = await getBalance(tokenA);
    log(`Initial balance: ${initialBalanceA}`, "INFO", tokenA.context);
    const initialBalanceB = await getBalance(tokenB);
    log(`Initial balance: ${initialBalanceB}`, "INFO", tokenB.context);

    const initialTokenAPoolBalance = createdLiquidityPool.data.tokenA.amount;
    const initialTokenBPoolBalance = createdLiquidityPool.data.tokenB.amount;

    const shares = await getLiquidityPoolShares(config.ammMl0Url, poolId, withdrawalAccount.address, log);

    const addressShares = shares.data.shares;

    if (addressShares === 0) {
        throwInContext('AMM')("No shares found for address");
        return;
    }

    const percentageToWithdraw = 0.5; // NOTE: Withdraw half of the shares

    const sharesToWithdraw = Math.round(addressShares * percentageToWithdraw);

    const withdrawalUpdate = await createWithdrawalUpdate(
        tokenA.tokenId,
        tokenB.tokenId,
        sharesToWithdraw,
        withdrawalAccount,
        privateKey,
        config.ammMl0Url,
        'AMM'
    );

    await sendDataUpdate(config.ammDl1Url, withdrawalUpdate);

    const confirmedWithdrawal = await retry<WithdrawalCalculatedStateAddress>('Validate if withdrawal created', { delayMs: 5000 })(async (logger) => {
        const confirmed = await validateWithdrawalCreated(config.ammMl0Url, tokenA.tokenId, tokenB.tokenId, sharesToWithdraw, withdrawalAccount, logger);
        return confirmed!
    });

    await retry('Validate if liquidity pool token amount changed')(async (logger) => {
        const expectedTokenAPoolBalance = initialTokenAPoolBalance - confirmedWithdrawal.tokenAAmount;
        const expectedTokenBPoolBalance = initialTokenBPoolBalance - confirmedWithdrawal.tokenBAmount;

        await validateLiquidityPoolAmountChanged(
            config.ammMl0Url,
            poolId,
            initialTokenAPoolBalance,
            initialTokenBPoolBalance,
            expectedTokenAPoolBalance,
            expectedTokenBPoolBalance,
            logger
        );
    });

    await retry('Validate if balance changed')(async (logger) => {
        const expectedBalanceA = initialBalanceA + confirmedWithdrawal.tokenAAmount;
        const expectedBalanceB = initialBalanceB + confirmedWithdrawal.tokenBAmount;
        const results = await Promise.allSettled([
            validateIfBalanceChanged(initialBalanceA, expectedBalanceA, tokenA, logger),
            validateIfBalanceChanged(initialBalanceB, expectedBalanceB, tokenB, logger),
        ]);

        const failedResults = results.filter(result => result.status === 'rejected');
        if (failedResults.length > 0) {
            throwInContext('AMM')(`Balance validation failed!`);
        }
    });
}

const withdrawalCurrencyToCurrencyTest = async (config: WithdrawalConfig) => {
    log("Starting withdrawal creation test (Currency to Currency)".toUpperCase(), 'INFO', 'AMM');

    const tokenA = await createTokenConfig(
        config.inputs.privateKey,
        config.tokenAMl0Url,
        config.tokenACl1Url,
        'A',
        config.tokenAId,
        true,
    )

    const tokenB = await createTokenConfig(
        config.inputs.privateKey,
        config.tokenBMl0Url,
        config.tokenBCl1Url,
        'B',
        config.tokenBId,
        true,
    )
    await processWithdrawal(config, tokenA, tokenB);
}

const withdrawalDagToCurrencyTest = async (config: WithdrawalConfig) => {
    log("Starting withdrawal creation test (DAG to Currency)".toUpperCase(), 'INFO', 'AMM');

    const tokenA = await createTokenConfig(
        config.inputs.privateKey,
        config.gl0Url,
        config.dagCl1Url,
        'DAG',
        null,
        false,
    )

    const tokenB = await createTokenConfig(
        config.inputs.privateKey,
        config.tokenBMl0Url,
        config.tokenBCl1Url,
        'B',
        config.tokenBId,
        true,
    )

    await processWithdrawal(config, tokenA, tokenB);
}

export default async (baseConfig: BaseConfig) => {
    const config = createConfig(baseConfig);

    await lastGlobalSnapshotIncreasingTest({ ammMl0Url: config.ammMl0Url });
    log("=".repeat(80), "INFO");

    await withdrawalCurrencyToCurrencyTest(config);
    log("=".repeat(80), "INFO");

    await withdrawalDagToCurrencyTest(config);
    log("=".repeat(80), "INFO");

    log("All withdrawal creation tests passed!", "INFO", 'AMM');
}