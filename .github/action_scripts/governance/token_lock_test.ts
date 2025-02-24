import { dag4 } from "@stardust-collective/dag4";
import jsSha256 from "js-sha256";
import axios from "axios";
import { compress } from "brotli";
import { log, delay, getPublicKey, parseSharedArgs } from "../shared";
import { z } from 'zod';

const TokenLockSchema = z.object({
  privateKey: z.string()
    .min(1, "Private key cannot be empty")
    .nullish()
    .refine(val => val != null, {
      message: "privateKey is required"
    }),
  lockAmount: z.number()
    .nullish()
    .refine(val => val != null, {
      message: "lockAmount is required"
    }),
  multiplier: z.number()
    .nullish()
    .refine(val => val != null, {
      message: "multiplier is required"
    }),
  expireEpochProgress: z.number()
    .nullish()
    .refine(val => val != null, {
      message: "expireEpochProgress is required"
    })
});

const CliArgsSchema = z.object({
  tokenLocks: z.string()
    .transform((str, ctx) => {
      try {
        return JSON.parse(str);
      } catch (e) {
        ctx.addIssue({
          code: z.ZodIssueCode.custom,
          message: "Invalid JSON string"
        });
        return z.NEVER;
      }
    })
    .pipe(z.array(TokenLockSchema)
      .min(1, "Token locks array cannot be empty")
      .refine(data => data.every(lock =>
        lock.privateKey &&
        lock.lockAmount &&
        lock.multiplier &&
        lock.expireEpochProgress
      ), {
        message: "Each token lock must include privateKey, lockAmount, multiplier, and expireEpochProgress"
      })
    )
});

const createConfig = () => {
  const args = process.argv.slice(2);

  if (args.length < 6) {
    throw new Error(
      "Usage: npx tsx governance/token_lock_test.ts <gl0-url> <aml0-url> <acl1-url> <adl1-url> <metagraph-id> <token-locks-json>"
    );
  }

  const sharedArgs = parseSharedArgs(args.slice(0, 5));
  const [tokenLocks] = args.slice(5);

  const specificArgs = CliArgsSchema.parse({ tokenLocks });

  return { ...sharedArgs, ...specificArgs };
};

const brotliSerialize = async (content, compressionLevel = 2) => {
  log("Serializing data with Brotli...");
  const jsonString = JSON.stringify(content);
  const encoder = new TextEncoder();
  const utf8Bytes = encoder.encode(jsonString);
  return compress(utf8Bytes, { quality: compressionLevel });
};

const getSignedTokenLock = async (config, { privateKey, publicKey, address, lockAmount, expireEpochProgress }) => {
  log("Generating signed token locks...");
  const { metagraphId, ammCl1Url } = config;

  log(`Fetching last reference for wallet: ${address}`);
  const { data: lastRef } = await axios.get(`${ammCl1Url}/token-locks/last-reference/${address}`);
  const { hash, ordinal } = lastRef;

  const body = {
    amount: lockAmount,
    currencyId: metagraphId,
    parent: { hash, ordinal },
    source: address,
    unlockEpoch: expireEpochProgress,
  };

  const serializedTx = await brotliSerialize(body);
  const messageHash = jsSha256.sha256(Buffer.from(serializedTx, "hex"));
  const signature = await dag4.keyStore.sign(privateKey, messageHash);

  log(`Signed token lock generated for wallet: ${address}`);

  return {
    value: body,
    proofs: [{ id: publicKey, signature }],
  };
};

const sendSignedTokenLock = async (config, signedTokenLock) => {
  const { ammCl1Url } = config;
  try {
    log("Sending signed token lock...");
    await axios.post(`${ammCl1Url}/token-locks`, signedTokenLock);
    log("Signed token lock sent successfully.");
  } catch (error) {
    log(`Error sending signed token lock: ${error.message}`, "ERROR");
    throw error;
  }
};

const validateTokenLockInGL0 = async (config, walletAddress) => {
  const { gl0Url, metagraphId } = config;

  const maxAttempts = 60
  for (let attempt = 1; attempt <= maxAttempts; attempt++) {
    try {
      log(`Checking token lock status in GL0 (Attempt ${attempt}/${maxAttempts}) for wallet: ${walletAddress}`);
      const { data: snapshot } = await axios.get(`${gl0Url}/global-snapshots/latest/combined`);
      const tokenLockBalances = snapshot[1]?.tokenLockBalances;

      if (tokenLockBalances?.[metagraphId]?.[walletAddress]) {
        log("Token lock successfully accepted in GL0.");
        return;
      }
    } catch (error) {
      log(`Error checking token lock in GL0: ${error.message}`, "ERROR");
    }
    await delay(1000);
  }
  throw new Error(`Token lock not accepted in GL0 after ${maxAttempts} attempts.`);
};

const validateVotingWeight = async (config, address, lockAmount, multiplier) => {
  const { ammMl0Url } = config;
  const expectedWeight = lockAmount * multiplier;

  const maxAttempts = 60
  for (let attempt = 1; attempt <= maxAttempts; attempt++) {
    try {
      log(`Validating voting weight (Attempt ${attempt}/${maxAttempts}) for wallet: ${address}`);
      const { data: state } = await axios.get(`${ammMl0Url}/v1/addresses/${address}/vote-weight`);
      const actualWeight = state.total;

      if (actualWeight === expectedWeight) {
        log(`Voting weight validated for ${address}: ${actualWeight}`);
        return;
      }
      log(`Voting weight mismatch for ${address}. Retrying...`);
    } catch (error) {
      log(`Error validating voting weight: ${error.message}`, "ERROR");
    }
    await delay(1000);
  }
  throw new Error(`Voting weight validation failed after ${maxAttempts} attempts.`);
};

const main = async () => {
  const config = createConfig()

  log("Building token locks information...");
  const tokenLocksInfo = config.tokenLocks.map((data) => {
    const account = dag4.createAccount();
    account.loginPrivateKey(data.privateKey);

    return {
      privateKey: data.privateKey,
      publicKey: getPublicKey(account),
      address: account.address,
      lockAmount: data.lockAmount,
      multiplier: data.multiplier,
      expireEpochProgress: data.expireEpochProgress,
    };
  });
  log("Token lock information built successfully.");

  for (const tokenLockInfo of tokenLocksInfo) {
    const { address, lockAmount, multiplier } = tokenLockInfo;

    const signedTokenLock = await getSignedTokenLock(config, tokenLockInfo);
    await sendSignedTokenLock(config, signedTokenLock);
    await validateTokenLockInGL0(config, address);
    await validateVotingWeight(config, address, lockAmount, multiplier);
    await delay(10000)
  }

  log("All token locks validated successfully.");
};

main().catch((error) => {
  log(`Error: ${error.message}`, "ERROR");
  process.exit(1);
});