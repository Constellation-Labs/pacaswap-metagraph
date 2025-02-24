import { dag4 } from '@stardust-collective/dag4';
import axios from 'axios';
import { z } from 'zod';
import { serializeBrotli, getPublicKey, log, throwInContext, serializeBase64, delay, retry, createAccount, getHash, TokenConfig, validateIfAllowSpendAcceptedOnCL1, validateIfAllowSpendAcceptedOnML0, validateIfAllowSpendAcceptedOnGL0, validateIfBalanceChanged, createTokenConfig, createSignedAllowSpend, sendSignedAllowSpend } from '../shared';

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

const CliArgsSchema = z.object({
  gl0Url: z.string().url("GL0 URL must be a valid URL"),
  dagCl1Url: z.string().url("DAG CL1 URL must be a valid URL"),
  ammMl0Url: z.string().url("AMM ML0 URL must be a valid URL"),
  ammCl1Url: z.string().url("AMM CL1 URL must be a valid URL"),
  ammDl1Url: z.string().url("AMM DL1 URL must be a valid URL"),
  tokenACl1Url: z.string().url("TokenA CL1 URL must be a valid URL"),
  tokenBCl1Url: z.string().url("TokenB CL1 URL must be a valid URL"),
  tokenAMl0Url: z.string().url("TokenA ML0 URL must be a valid URL"),
  tokenBMl0Url: z.string().url("TokenB ML0 URL must be a valid URL"),
  ammMetagraphId: z.string().min(1, "AMM Metagraph ID cannot be empty"),
  tokenAId: z.string().min(1, "tokenAId cannot be empty"),
  tokenBId: z.string().min(1, "tokenBId cannot be empty"),
  lpCreation: LPCreationSchema
});

const createConfig = () => {
  const args = process.argv.slice(2);

  const [
    gl0Url,
    dagCl1Url,
    ammMl0Url,
    ammCl1Url,
    ammDl1Url,
    tokenACl1Url,
    tokenBCl1Url,
    tokenAMl0Url,
    tokenBMl0Url,
    ammMetagraphId,
    tokenAId,
    tokenBId,
  ] = args;

  if (args.length < 11) {
    throw new Error(
      "Usage: npx tsx liquidity-pool/liquidity-pool-creation_test.ts <gl0-url> <dagcl1-url> <aml0-url> <acl1-url> <adl1-url> <tacl1-url> <tbcl1-url> <taml0-url> <tbml0-url> <amm-metagraph-id> <tokenAId> <tokenBId>"
    );
  }

  const argsObj = {
    gl0Url,
    dagCl1Url,
    ammMl0Url,
    ammCl1Url,
    ammDl1Url,
    tokenACl1Url,
    tokenBCl1Url,
    tokenAMl0Url,
    tokenBMl0Url,
    ammMetagraphId,
    tokenAId,
    tokenBId,
    lpCreation
  }

  return CliArgsSchema.parse(argsObj);
};

const createLiquidityPoolUpdate = async (
  tokenAAllowSpendHash: string,
  tokenBAllowSpendHash: string,
  tokenAId: string | null,
  tokenBId: string | null,
  tokenAAllowSpendAmount: number,
  tokenBAllowSpendAmount: number,
  privateKey: string,
  account: ReturnType<typeof createAccount>,
) => {
  const body = {
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

const sendLiquidityPoolUpdate = async (
  dataL1Url: string,
  update: Awaited<ReturnType<typeof createLiquidityPoolUpdate>>,
) => {
  try {
    log(`Sending liquidity pool update...`, "INFO", 'AMM');
    await axios.post(`${dataL1Url}/data`, update);
    log(`Liquidity pool update sent successfully`, "INFO", 'AMM');
    return;
  } catch (error) {
    throwInContext('AMM')(`Failed to send liquidity pool update: ${error.message}`);
  }
}

const validateLiquidityPoolCreated = async (
  ammL0Url: string,
  tokenAId: string | null,
  tokenBId: string | null,
  logger: (message: string, type?: string, context?: string) => void = log
) => {
  const { data } = await axios.get(
    `${ammL0Url}/v1/calculated-state/latest`
  );
  logger("Validating liquidity pool creation...", "INFO", 'AMM')
  const lpCalculatedState = data.calculatedState.operations.LiquidityPool?.LiquidityPoolCalculatedState

  type LiquidityPool = {
    tokenA: {
      identifier?: string | null
    }
    tokenB: {
      identifier?: string | null
    }
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

  type FailedCalculatedState = {
    value: LiquidityPoolUpdate
  }

  const confirmedLiquidityPools: Record<string, LiquidityPool> = lpCalculatedState?.confirmed.value || {}
  const pendingLiquidityPools: LiquidityPoolUpdate[] = lpCalculatedState?.pending || []
  const failedLiquidityPools: FailedCalculatedState[] = lpCalculatedState?.failed || []

  const isConfirmedLiquidityPool = Object.values(confirmedLiquidityPools).some(
    (lp) =>
      lp.tokenA.identifier === tokenAId && lp.tokenB.identifier === tokenBId
  );

  const isPendingLiquidityPool = pendingLiquidityPools.some(
    (lp) =>
      lp.tokenAId === tokenAId && lp.tokenBId === tokenBId
  );

  if (!isConfirmedLiquidityPool && !isPendingLiquidityPool && failedLiquidityPools.length > 0) {
    throwInContext('AMM')("Liquidity pool not found but there are failed liquidity pools.");
  }

  if (!isConfirmedLiquidityPool && isPendingLiquidityPool) {
    throwInContext('AMM')("Liquidity pool is pending.");
  }
  logger("Liquidity pool creation validated!", "INFO", 'AMM')
}

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
    tokenA.account,
    tokenA.l1Url,
    config.ammMetagraphId,
    tokenA.allowSpendAmount,
    tokenA.context
  );

  const signedAllowSpendB = await createSignedAllowSpend(
    privateKey,
    tokenB.account,
    tokenB.l1Url,
    config.ammMetagraphId,
    tokenB.allowSpendAmount,
    tokenB.context
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

  await sendLiquidityPoolUpdate(config.ammDl1Url, update);

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

  delay(5000);

  await retry('Validate if liquidity pool created')(async (logger) => {
    await validateLiquidityPoolCreated(config.ammMl0Url, tokenA.tokenId, tokenB.tokenId, logger);
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

const main = async () => {
  const config = createConfig();

  // Test valid cases
  await dagToCurrencyTest(config);
  await currencyToCurrencyTest(config);

  // Test invalid cases
  await dagToDagTest(config);
  await sameCurrencyTest(config);

  log("All liquidity pool creation tests passed!", "INFO", 'AMM');
};

main().catch((error) => {
  log(`Error: ${error.message}`, "ERROR");
  process.exit(1);
});

