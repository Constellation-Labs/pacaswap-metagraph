import axios from "axios";
import { createAccount, getPublicKey } from "./account";
import { log, throwInContext } from "../log";
import { getHash, serializeBrotli } from "../serialize";
import { dag4 } from "@stardust-collective/dag4";

const createSignedAllowSpend = async (
    privateKey: string,
    account: ReturnType<typeof createAccount>,
    l1Url: string,
    ammMetagraphId: string,
    amount: number,
    context: string
) => {
    log(`Fetching last allow spend reference for wallet: ${account.address}`, "INFO", context);

    const { data: lastRef } = await axios.get(
        `${l1Url}/allow-spends/last-reference/${account.address}`
    );

    log(`Last allow spend reference for wallet: ${account.address}: ${JSON.stringify(lastRef, null, 2)}`, "INFO", context);

    const body = {
        amount,
        approvers: [ammMetagraphId],
        // currency: tokenId, // NOTE: Not supported yet, handle DAG and Currency case
        destination: ammMetagraphId,
        fee: 1,
        lastValidEpochProgress: 50,
        parent: lastRef,
        source: account.address,
    };

    const serializedAllowSpend = await serializeBrotli(body);
    const hash = getHash(serializedAllowSpend);
    const signature = await dag4.keyStore.sign(privateKey, hash);
    const publicKey = getPublicKey(account)

    log(`Signed allow spend generated for wallet: ${account.address}: ${JSON.stringify(body, null, 2)}`, "INFO", context);

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

const validateIfAllowSpendAcceptedOnGL0 = async (
    gL0Url: string,
    address: string,
    tokenAllowSpendHash: string,
    tokenId: string | null,
    context: string,
    logger: (message: string, type?: string, context?: string) => void = log
) => {
    logger(`Validating allow spend with hash ${tokenAllowSpendHash} in GL0`, "INFO", context);
    try {
        const { data: snapshot } = await axios.get(
            `${gL0Url}/global-snapshots/latest/combined`
        );
        await validateIfAllowSpendAcceptedinSnapshot(address, tokenAllowSpendHash, snapshot, tokenId, false, context, logger);
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
    logger: (message: string, type?: string, context?: string) => void = log
) => {
    logger(`Validating allow spend with hash ${tokenAllowSpendHash} in ML0...`, "INFO", context);
    try {
        const { data: snapshot } = await axios.get(
            `${mL0Url}/snapshots/latest/combined`
        );
        await validateIfAllowSpendAcceptedinSnapshot(address, tokenAllowSpendHash, snapshot, tokenId, true, context, logger);
    } catch (error) {
        throwInContext(context)(`Error validating allow spend with hash ${tokenAllowSpendHash} in ML0: ${error.message}`);
    }
}


const validateIfAllowSpendAcceptedinSnapshot = async (
    address: string,
    hash: string,
    snapshot,
    tokenId: string | null,
    isCurrencySnapshot: boolean,
    context: string,
    logger: (message: string, type?: string, context?: string) => void = log
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

    const activeAllowSpends = snapshot[1]?.activeAllowSpends;

    const activeAllowSpendsForAddress = isCurrencySnapshot
        ? activeAllowSpends?.[address]
        : activeAllowSpends?.[tokenId || '']?.[address]

    if (!activeAllowSpends || Object.keys(activeAllowSpends).length === 0) {
        throwInContext(context)(`No active allow spends found in snapshot`);
    }

    if (!activeAllowSpendsForAddress) {
        if (isCurrencySnapshot) {
            throwInContext(context)(`No active allow spends for address ${address} in ML0 snapshot, but there are active allow spends for different addresses...`);
        } else {
            throwInContext(context)(`No active allow spends for address ${address} in GL0 snapshot, but there are active allow spends for different addresses...`);
        }
    }

    const hasMatchingHash = await findMatchingHash(activeAllowSpendsForAddress, hash);
    if (!hasMatchingHash) {
        if (isCurrencySnapshot) {
            throwInContext(context)(`Found active allow spends for ${address} in ML0 snapshot but count not find hash ${hash}, but there are active allow spends for that address: ${JSON.stringify(activeAllowSpendsForAddress, null, 2)}`);
        } else {
            throwInContext(context)(`Found active allow spends for ${address} in GL0 snapshot but count not find hash ${hash}, but there are active allow spends for that address: ${JSON.stringify(activeAllowSpendsForAddress, null, 2)}`);
        }
    }
    if (isCurrencySnapshot) {
        logger(`Allow spend with hash ${hash} found in ML0 snapshot`, "INFO", context);
    } else {
        logger(`Allow spend with hash ${hash} for ${tokenId} found in GL0 snapshot!`, "INFO", context);
    }
}

const validateIfAllowSpendAcceptedOnCL1 = async (
    l1Url: string,
    tokenAllowSpendHash: string,
    context: string,
    logger: (message: string, type?: string, context?: string) => void = log
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

export { createSignedAllowSpend, sendSignedAllowSpend, validateIfAllowSpendAcceptedOnCL1, validateIfAllowSpendAcceptedOnGL0, validateIfAllowSpendAcceptedOnML0, validateIfAllowSpendAcceptedinSnapshot };