import axios from "axios";
import { createAccount } from "./account";
import { log, logObject, throwInContext } from "../log";
import { Logger } from "../retry";
import { Signed } from "./signed";
import { AllowSpend } from "./allow-spends";
import { TokenConfig } from "./token";

const getBalance = async (tokenConfig: TokenConfig) => {
    try {
        const snapshotUrl = tokenConfig.isCurrency
            ? `${tokenConfig.l0Url}/snapshots/latest/combined`
            : `${tokenConfig.l0Url}/global-snapshots/latest/combined`;

        const { data } = await axios.get(snapshotUrl);

        const [, snapshotInfo] = data;

        const balance = snapshotInfo?.balances?.[tokenConfig.account.address];

        if (balance === undefined) {
            throwInContext(tokenConfig.context)(`Balance for account: ${tokenConfig.account.address} is undefined`);
        }
        log(`Balance for account: ${tokenConfig.account.address} is ${balance}`, "INFO", tokenConfig.context);
        return balance;
    } catch (error) {
        console.log(error)
        log(`Error getting balance for account: ${tokenConfig.account.address}: ${error}`, "ERROR", tokenConfig.context);
        throw error;
    }
}

const validateIfBalanceChanged = async (
    initialBalance: number,
    expectedBalance: number,
    tokenConfig: TokenConfig,
    logger: Logger
) => {
    const balance = await getBalance(tokenConfig);
    if (balance !== expectedBalance) {
        const msg = `Balance different than expected. Actual: ${balance} (Δ ${balance - initialBalance}). Expected: ${expectedBalance} (Δ ${expectedBalance - initialBalance})`;
        throwInContext(tokenConfig.context)(msg);
    }
}

export { getBalance, validateIfBalanceChanged };