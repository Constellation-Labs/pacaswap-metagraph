import axios from "axios";
import { createAccount } from "./account";
import { log, logObject, throwInContext } from "../log";
import { Logger } from "../retry";
import { Signed } from "./signed";
import { AllowSpend } from "./allow-spends";
import { TokenConfig } from "./token";

const getBalance = async (tokenConfig: TokenConfig) =>
    getBalanceForAddress(tokenConfig.account.address, tokenConfig.l0Url, tokenConfig.isCurrency, tokenConfig.context)

const getBalanceForAddress = async (address: string, l0Url: string, isCurrency: boolean, context: string) => {
    log(`Getting balance for account: ${address} `, "INFO", context);

    try {
        const snapshotUrl = isCurrency
            ? `${l0Url}/snapshots/latest/combined`
            : `${l0Url}/global-snapshots/latest/combined`;

        const { data } = await axios.get(snapshotUrl);

        const [, snapshotInfo] = data;

        const balance = snapshotInfo?.balances?.[address];

        if (balance === undefined) {
            throwInContext(context)(`Balance for account: ${address} is undefined`);
        }
        log(`Balance for account: ${address} is ${balance}`, "INFO", context);
        return balance;
    } catch (error) {
        console.log(error)
        log(`Error getting balance for account: ${address}: ${error}`, "ERROR", context);
        throw error;
    }
}

const validateIfBalanceChanged = async (
    initialBalance: number,
    expectedBalance: number,
    tokenConfig: TokenConfig,
    address ?: string
) => {
    const balance = address ? await getBalanceForAddress(address, tokenConfig.l0Url, tokenConfig.isCurrency, tokenConfig.context) : await getBalance(tokenConfig);
    if (balance !== expectedBalance) {
        const msg = `Balance different than expected. Actual: ${balance} (Δ ${balance - initialBalance}). Expected: ${expectedBalance} (Δ ${expectedBalance - initialBalance})`;
        throwInContext(tokenConfig.context)(msg);
    }
}

export { getBalance, getBalanceForAddress, validateIfBalanceChanged };
