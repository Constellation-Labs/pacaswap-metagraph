import { dag4 } from '@stardust-collective/dag4';
import jsSha256 from 'js-sha256';
import axios from 'axios';
import { log, delay, getPublicKey, parseSharedArgs } from '../shared';
import { z } from 'zod';
import { compress } from 'brotli';

// TODO: Should be in secrets?
const LP_CREATION = {
  privateKey: "8971dcc9a07f2db7fa769582139768fd5d73c56501113472977eca6200c679c8",
  tokenAAllowSpendAmount: 100000000000, // 1000
  tokenBAllowSpendAmount: 200000000000, // 2000
};

const LPCreationSchema = z
  .object({
    privateKey: z.string().min(1, "key cannot be empty"),
    tokenAAllowSpendAmount: z.number().min(1, "amount must be greater than 0"),
    tokenBAllowSpendAmount: z.number().min(1, "amount must be greater than 0"),
  })

const CliArgsSchema = z.object({
  gl0Url: z.string().url("GL0 URL must be a valid URL"),
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
  lpCreation: z
    .string()
    .transform((str, ctx) => {
      try {
        return JSON.parse(str);
      } catch (e) {
        ctx.addIssue({
          code: z.ZodIssueCode.custom,
          message: "Invalid JSON string",
        });
        return z.NEVER;
      }
    })
    .pipe(LPCreationSchema),
});

const createConfig = () => {
  const args = process.argv.slice(2);

  const [
    gl0Url,
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

  if (args.length < 6) {
    throw new Error(
      "Usage: node lp_creation_test.js <gl0-url> <aml0-url> <acl1-url> <adl1-url> <tacl1-url> <tbcl1-url> <taml0-url> <tbml0-url> <amm-metagraph-id> <tokenAId> <tokenBId>"
    );
  }

  return CliArgsSchema.parse({
    gl0Url,
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
    lpCreation: LP_CREATION,
  });
};

const createAccount = (privateKey: string, l0Url: string, l1Url: string, metagraphId: string) => {
  const account = dag4.createAccount(privateKey);

  if (metagraphId) {
    account.createMetagraphTokenClient({
      id: metagraphId,
      l0Url,
      l1Url,
      beUrl: '', // Note: We do not use any of these urls
      metagraphId: metagraphId,
    });
  } else {
    account.connect({
      networkVersion: '2.0',
      l0Url,
      l1Url,
      testnet: true,
    });
  }

  return account;
};

const brotliSerialize = async (content, compressionLevel = 2) => {
  log("Serializing data with Brotli...");
  const jsonString = JSON.stringify(content);
  const encoder = new TextEncoder();
  const utf8Bytes = encoder.encode(jsonString);
  return compress(utf8Bytes, { quality: compressionLevel });
};

const createSignedAllowSpend = async (
  privateKey: string,
  publicKey: string,
  lpProviderAddress: string,
  l1Url: string,
  ammMetagraphId: string,
  tokenId: string,
  amount: number,
) => {
  log(`Fetching last allow spend reference for wallet: ${lpProviderAddress}`);

  const { data: lastRef } = await axios.get(
    `${l1Url}/allow-spends/last-reference/${lpProviderAddress}`
  );

  const body = {
    amount,
    approvers: [],
    destination: ammMetagraphId,
    fee: 0,
    lastValidEpochProgress: Number.MAX_SAFE_INTEGER,
    source: lpProviderAddress,
    parent: lastRef,
    currency: tokenId,
  };

  const hash = await getHash(body);
  const signature = await dag4.keyStore.sign(privateKey, hash);

  log(`Signed allow spend generated for wallet: ${lpProviderAddress}`);

  return {
    value: body,
    proofs: [{ id: publicKey, signature }],
  };
};

const sendSignedAllowSpend = async (l1Url, signedAllowSpend) => {
  try {
    log("Sending signed allow spend...");
    const { data } = await axios.post(`${l1Url}/data`, signedAllowSpend);
    log("Signed allow spend sent successfully.");
    return data;
  } catch (error) {
    log(`Error sending signed allow spend: ${error.message}`, "ERROR");
    throw error;
  }
};

const getHash = async (body: any) => {
  const serialized = await brotliSerialize(body);
  return jsSha256.sha256(Buffer.from(serialized as any, "hex"));
};

const createLiquidityPoolUpdate = async ({
  tokenAAllowSpend,
  tokenBAllowSpend,
  tokenAId,
  tokenBId,
  tokenAAllowSpendAmount,
  tokenBAllowSpendAmount,
  privateKey,
  publicKey,
}) => {
  const body = {
    tokenAAllowSpend,
    tokenBAllowSpend,
    tokenAId,
    tokenBId,
    tokenAAllowSpendAmount,
    tokenBAllowSpendAmount,
    maxValidGsEpochProgress: Number.MAX_SAFE_INTEGER,
  };

  const hash = await getHash(body);
  const signature = await dag4.keyStore.sign(privateKey, hash);

  return { value: body, proofs: [{ id: publicKey, signature }] };
};

const sendLiquidityPoolUpdate = async (l1Url, liquidityPoolUpdate) => {
  try {
    log("Sending liquidity pool update...");
    await axios.post(`${l1Url}/data`, liquidityPoolUpdate);
    log("Liquidity pool update sent successfully.");
  } catch (error) {
    log(`Error sending liquidity pool update: ${error.message}`, "ERROR");
    throw error;
  }
};

const validateLiquidityPoolCreation = async (ammL0Url: string, tokenAId: string, tokenBId: string) => {
  const { data } = await axios.get(
    `${ammL0Url}/calculated-state/latest`
  );
  const liquidityPools = data.state.operations.liquidityPools.confirmed

  const liquidityPool = liquidityPools.find(
    (lp) =>
      lp.tokenA.id === tokenAId && lp.tokenB.id === tokenBId
  );

  if (!liquidityPool) {
    throw new Error("Liquidity pool not found");
  }
}

const validateIfTokenBalancesChanged = async (
  initialBalanceA: number,
  initialBalanceB: number,
  tokenAAllowSpendAmount: number,
  tokenBAllowSpendAmount: number,
  accountA: ReturnType<typeof createAccount>,
  accountB: ReturnType<typeof createAccount>
) => {
  const balanceA = await accountA.getBalance();
  const balanceB = await accountB.getBalance();

  const expectedBalanceA = initialBalanceA - tokenAAllowSpendAmount;
  const expectedBalanceB = initialBalanceB - tokenBAllowSpendAmount;
  if (balanceA !== expectedBalanceA) {
    throw new Error(`Token A balance didn't change`);
  }
  if (balanceB !== expectedBalanceB) {
    throw new Error(`Token B balance didn't change`);
  }
}

const main = async () => {
  const config = createConfig();

  const privateKey = config.lpCreation.privateKey;
  const lpProviderAccount = createAccount(privateKey, config.ammMl0Url, config.ammMl0Url, config.ammMetagraphId);
  const tokenAAccount = createAccount(privateKey, config.tokenAMl0Url, config.tokenACl1Url, config.tokenAId);
  const tokenBAccount = createAccount(privateKey, config.tokenBMl0Url, config.tokenBCl1Url, config.tokenBId);

  const tokenAInitialBalance = await tokenAAccount.getBalance();
  const tokenBInitialBalance = await tokenBAccount.getBalance();

  const signedAllowSpendA = await createSignedAllowSpend(
    privateKey,
    getPublicKey(tokenAAccount),
    tokenAAccount.address,
    config.tokenACl1Url,
    config.ammMetagraphId,
    config.tokenAId,
    config.lpCreation.tokenAAllowSpendAmount,
  );

  const signedAllowSpendB = await createSignedAllowSpend(
    privateKey,
    getPublicKey(tokenBAccount),
    tokenBAccount.address,
    config.tokenBCl1Url,
    config.ammMetagraphId,
    config.tokenBId,
    config.lpCreation.tokenBAllowSpendAmount,
  );

  const { hash: tokenAAllowSpend } = await sendSignedAllowSpend(
    config.tokenACl1Url,
    signedAllowSpendA
  );
  const { hash: tokenBAllowSpend } = await sendSignedAllowSpend(
    config.tokenBCl1Url,
    signedAllowSpendB
  );

  const liquidityPoolUpdate = await createLiquidityPoolUpdate({
    tokenAAllowSpend,
    tokenBAllowSpend,
    tokenAId: config.tokenAId,
    tokenBId: config.tokenBId,
    tokenAAllowSpendAmount: config.lpCreation.tokenAAllowSpendAmount,
    tokenBAllowSpendAmount: config.lpCreation.tokenBAllowSpendAmount,
    privateKey,
    publicKey: getPublicKey(lpProviderAccount),
  });

  await sendLiquidityPoolUpdate(config.ammDl1Url, liquidityPoolUpdate);


  // TODO: Set delays and maxAttempts
  await validateLiquidityPoolCreation(config.ammMl0Url, config.tokenAId, config.tokenBId);

  await validateIfTokenBalancesChanged(
    tokenAInitialBalance,
    tokenBInitialBalance,
    config.lpCreation.tokenAAllowSpendAmount,
    config.lpCreation.tokenBAllowSpendAmount,
    tokenAAccount,
    tokenBAccount
  );

  log("Liquidity pool creation test passed");
};

main().catch((error) => {
  log(`Error: ${error.message}`, "ERROR");
  process.exit(1);
});
