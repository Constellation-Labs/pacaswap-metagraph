import { dag4 } from "@stardust-collective/dag4";
import jsSha256 from "js-sha256";
import axios from "axios";
import { BaseAmmMetagraphCliArgsSchema, delay, getGlobalSnapshotCombined, getPublicKey, log, retry, serializeBrotli, validateIfSyncedToGlobalOrdinal } from "../../shared";
import { z } from 'zod';

const tokenLocks = [
    {
        "privateKey": "8971dcc9a07f2db7fa769582139768fd5d73c56501113472977eca6200c679c8",
        "lockAmount": 1000,
        "multiplier": 0.25,
        "expireEpochProgress": 5000
    }, {
        "privateKey": "7bf1b37351f164ae4cdc5f10a55987163d42bf68e607383e7fa5121f7e7624ed",
        "lockAmount": 1000,
        "multiplier": 0.25,
        "expireEpochProgress": 5000
    }]

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

const createConfig = (argsObject: object) => {
    const argsObj = {
        ...argsObject,
        tokenLocks
    }

    const CliArgsSchema = BaseAmmMetagraphCliArgsSchema.extend({
        tokenLocks: z.array(TokenLockSchema)
    });

    return CliArgsSchema.parse(argsObj);
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
        fee: 0,
        parent: { hash, ordinal },
        source: address,
        unlockEpoch: expireEpochProgress,
    };

    const serializedTx = await serializeBrotli(body);
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

const validateTokenLockInGL0 = async (
    config: ReturnType<typeof createConfig>,
    walletAddress: string,
    logger: (message: string, type?: string, context?: string) => void = log
) => {
    const { gl0Url, metagraphId } = config;

    try {
        const combined = await getGlobalSnapshotCombined(gl0Url);
        const [snapshot, info] = combined;
        const tokenLockBalances = info?.tokenLockBalances;

        if (tokenLockBalances?.[metagraphId]?.[walletAddress]) {
            const ordinal = snapshot.value.ordinal;
            logger(`Token lock successfully accepted in GL0 with ordinal ${ordinal}.`);
            return ordinal;
        } else {
            logger(`Token lock not found in GL0 for wallet: ${walletAddress}.`);
            throw new Error(`Token lock not found in GL0 for wallet: ${walletAddress}.`);
        }
    } catch (error) {
        logger(`Error checking token lock in GL0: ${error.message}`, "ERROR");
        throw error
    }
};

const validateVotingWeight = async (
    config: ReturnType<typeof createConfig>,
    address: string,
    lockAmount: number,
    multiplier: number,
    logger: (message: string, type?: string, context?: string) => void = log
) => {
    const { ammMl0Url } = config;
    const expectedWeight = lockAmount * multiplier;
    try {
        const { data: state } = await axios.get(`${ammMl0Url}/v1/addresses/${address}/vote-weight`);
        const actualWeight = state.total;
        if (actualWeight === expectedWeight) {
            logger(`Voting weight validated for ${address}: ${actualWeight}`);
            return;
        }
        logger(`Voting weight mismatch for ${address}. Retrying...`);
    } catch (error) {
        logger(`Error validating voting weight: ${error.message}`, "ERROR");
        throw error
    }
};

const tokenLockTests = async (argsObject: object) => {
    const config = createConfig(argsObject)

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

        const globalOrdinal = await retry<number>('Validate token lock in GL0')(async (logger) => {
            return await validateTokenLockInGL0(config, address, logger);
        })

        await retry('Validate if calculated state is synced to global ordinal', { delayMs: 5000 })(async (logger) => {
            await validateIfSyncedToGlobalOrdinal(config.ammMl0Url, globalOrdinal, logger);
        });

        await retry('Validate voting weight')(async (logger) => {
            await validateVotingWeight(config, address, lockAmount, multiplier, logger);
        })

        await delay(30 * 1000)
    }

    log("All token locks validated successfully.", "SUCCESS");
};

export { tokenLockTests }