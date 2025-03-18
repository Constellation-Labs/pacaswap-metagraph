import axios from "axios";
import { getPublicKey } from "./account";
import { log, throwInContext } from "../log";
import { getHash, serializeBrotli } from "../serialize";
import { dag4 } from "@stardust-collective/dag4";
import { TokenConfig } from "./token";
import { getCurrencySnapshotCombined, getCurrentEpochProgress, getGlobalSnapshotCombined } from "./snapshot";
import { getBalance } from "./balances";
import { Signed } from "./signed";
import { Logger } from "../retry";

type AllowSpend = {
    amount: number;
    approvers: string[];
    currency: string | null;
    destination: string;
    fee: number;
    lastValidEpochProgress: number;
    parent: AllowSpendReference;
    source: string;
};

type AllowSpendReference = {
    hash: string;
    ordinal: number;
}

const getLastAllowSpendReference = async (l1Url: string, address: string): Promise<AllowSpendReference> => {
    const { data: lastRef } = await axios.get<AllowSpendReference>(
        `${l1Url}/allow-spends/last-reference/${address}`
    );
    return lastRef;
}

const getAllowSpendHash = async (allowSpend: AllowSpend) => {
    const serializedAllowSpend = await serializeBrotli(allowSpend);
    return getHash(serializedAllowSpend);
}


const createSignedAllowSpend = async (
    privateKey: string,
    tokenConf: TokenConfig,
    ammMetagraphId: string,
    parent?: Signed<AllowSpend>
): Promise<Signed<AllowSpend>> => {
    log(`Fetching last allow spend reference for wallet: ${tokenConf.account.address}`, "INFO", tokenConf.context);

    const getLastRef = parent
        ? getAllowSpendHash(parent.value).then(hash => ({ hash, ordinal: parent.value.parent.ordinal + 1 }))
        : getLastAllowSpendReference(tokenConf.l1Url, tokenConf.account.address)

    const [lastRef, currentEpochProgress] = await Promise.all([
        getLastRef,
        getCurrentEpochProgress(tokenConf.l0Url, tokenConf.isCurrency)
    ]);

    if (!currentEpochProgress) {
        throwInContext(tokenConf.context)(`Current epoch progress is missing. Still syncing...`);
    }

    log(`Last allow spend reference for wallet from (${parent ? "parent" : "last-reference"}): ${tokenConf.account.address}: ${JSON.stringify(lastRef, null, 2)}`, "INFO", tokenConf.context);

    const EPOCH_PROGRESS_VALIDITY = 50

    const body: AllowSpend = {
        amount: tokenConf.allowSpendAmount,
        approvers: [ammMetagraphId],
        currency: tokenConf.tokenId,
        destination: ammMetagraphId,
        fee: 1,
        lastValidEpochProgress: currentEpochProgress! + EPOCH_PROGRESS_VALIDITY,
        parent: lastRef,
        source: tokenConf.account.address,
    };

    const serializedAllowSpend = await serializeBrotli(body);
    const hash = getHash(serializedAllowSpend);
    const signature = await dag4.keyStore.sign(privateKey, hash);
    const publicKey = getPublicKey(tokenConf.account)

    log(`Signed allow spend generated for wallet: ${tokenConf.account.address}: ${JSON.stringify(body, null, 2)}`, "INFO", tokenConf.context);

    return {
        value: body,
        proofs: [{ id: publicKey, signature }],
    };
};

const sendSignedAllowSpend = async (l1Url: string, signedAllowSpend, context: string) => {
    try {
        log(`Sending signed allow spend`, "INFO", context);
        const { data } = await axios.post(`${l1Url}/allow-spends`, signedAllowSpend);
        log(`Signed allow spend sent successfully. Received hash: ${data.hash}`, "INFO", context);
        return data;
    } catch (error) {
        log(`Error sending signed allow spend: ${error.message}`, "ERROR", context);
        throw error;
    }
};

const validateIfHasEnoughBalanceForAllowSpend = async (
    tokenConfig: TokenConfig,
    allowSpendAmount: number,
) => {
    const balance = await getBalance(tokenConfig.account, tokenConfig.l0Url, tokenConfig.isCurrency, tokenConfig.context);
    if (balance < allowSpendAmount) {
        throwInContext(tokenConfig.context)(`Not enough balance for allow spend: ${balance} < ${allowSpendAmount}`);
    }
}

const validateIfAllowSpendAcceptedOnGL0 = async (
    gL0Url: string,
    address: string,
    tokenAllowSpendHash: string,
    tokenId: string | null,
    context: string,
    logger: (message: string, type?: string, context?: string) => void
) => {
    logger(`Validating allow spend with hash ${tokenAllowSpendHash} in GL0`, "INFO", context);
    try {
        const snapshot = await getGlobalSnapshotCombined(gL0Url);
        return await validateIfAllowSpendAcceptedinSnapshot(address, tokenAllowSpendHash, snapshot, tokenId, false, context, logger);
    } catch (error) {
        throwInContext(context)(`Error validating allow spend with hash ${tokenAllowSpendHash} in GL0: ${error.message}`);
    }
}

const validateIfAllowSpendAcceptedOnML0 = async (
    mL0Url: string,
    address: string,
    tokenAllowSpendHash: string,
    tokenId: string | null,
    context: string,
    logger: (message: string, type?: string, context?: string) => void
) => {
    logger(`Validating allow spend with hash ${tokenAllowSpendHash} in ML0...`, "INFO", context);
    try {
        const snapshot = await getCurrencySnapshotCombined(mL0Url);
        await validateIfAllowSpendAcceptedinSnapshot(address, tokenAllowSpendHash, snapshot, tokenId, true, context, logger);
    } catch (error) {
        throwInContext(context)(`Error validating allow spend with hash ${tokenAllowSpendHash} in ML0: ${error.message}`);
    }
}


const validateIfAllowSpendAcceptedinSnapshot = async (
    address: string,
    hash: string,
    combinedSnapshot,
    tokenId: string | null,
    isCurrencySnapshot: boolean,
    context: string,
    logger: Logger = log
) => {
    const findMatchingHash = async (allowSpends, targetHash) => {
        return allowSpends.reduce(async (acc, allowSpend) => {
            const prevResult = await acc;
            if (prevResult) return true;

            const message = await serializeBrotli(allowSpend.value);
            const allowSpendHash = getHash(message);
            return allowSpendHash === targetHash;
        }, Promise.resolve(false));
    };

    const [snapshot, info] = combinedSnapshot
    const ordinal = snapshot.value.ordinal
    const activeAllowSpends = info?.activeAllowSpends;

    logger(`Active allow spends found in snapshot of ordinal: ${ordinal}`, "INFO", context);

    const activeAllowSpendsForAddress = isCurrencySnapshot
        ? activeAllowSpends?.[address]
        : activeAllowSpends?.[tokenId || '']?.[address]

    if (!activeAllowSpends || Object.keys(activeAllowSpends).length === 0) {
        throwInContext(context)(`No active allow spends found in snapshot`);
    }

    if (!activeAllowSpendsForAddress) {
        if (isCurrencySnapshot) {
            throwInContext(context)(`No active allow spends for address ${address} in ML0 snapshot ${ordinal}, but there are active allow spends for different addresses...`);
        } else {
            throwInContext(context)(`No active allow spends for address ${address} in GL0 snapshot ${ordinal}, but there are active allow spends for different addresses...`);
        }
    }

    const hasMatchingHash = await findMatchingHash(activeAllowSpendsForAddress, hash);
    if (!hasMatchingHash) {
        if (isCurrencySnapshot) {
            throwInContext(context)(`Found active allow spends for ${address} in ML0 snapshot ${ordinal} but couldn't find hash ${hash}`);
        } else {
            throwInContext(context)(`Found active allow spends for ${address} in GL0 snapshot ${ordinal} but couldn't find hash ${hash}`);
        }
    }
    if (isCurrencySnapshot) {
        logger(`Allow spend with hash ${hash} found in ML0 snapshot ${ordinal}`, "INFO", context);
    } else {
        logger(`Allow spend with hash ${hash} for ${tokenId} found in GL0 snapshot ${ordinal}!`, "INFO", context);
    }

    return ordinal
}

const validateIfAllowSpendAcceptedOnCL1 = async (
    l1Url: string,
    tokenAllowSpendHash: string,
    context: string,
    logger: (message: string, type?: string, context?: string) => void
) => {
    logger(`Validating allow spend with hash ${tokenAllowSpendHash} in CL1...`, "INFO", context);

    try {
        const { data } = await axios.get(`${l1Url}/allow-spends/${tokenAllowSpendHash}`);

        if (!data) {
            throwInContext(context)(`Allow spend with hash ${tokenAllowSpendHash} not found in CL1`);
        }

        logger(`Allow spend with hash ${tokenAllowSpendHash} validated in CL1!`, "INFO", context);
    } catch (error) {
        throwInContext(context)(`Error validating allow spend with hash ${tokenAllowSpendHash} in CL1: ${error.message}`);
    }
}

export { createSignedAllowSpend, sendSignedAllowSpend, validateIfAllowSpendAcceptedOnCL1, validateIfAllowSpendAcceptedOnGL0, validateIfAllowSpendAcceptedOnML0, validateIfAllowSpendAcceptedinSnapshot, validateIfHasEnoughBalanceForAllowSpend };
