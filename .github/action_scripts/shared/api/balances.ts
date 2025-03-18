import axios from "axios";
import { createAccount } from "./account";
import { log, logObject, throwInContext } from "../log";
import { Logger } from "../retry";
import { Signed } from "./signed";
import { AllowSpend } from "./allow-spends";
import { TokenConfig } from "./token";

const getBalance = async (tokenConfig: TokenConfig) => {
    log(`Getting balance for account: ${tokenConfig.account.address} `, "INFO", tokenConfig.context);

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


const validateIfBalanceChangedByAllowSpend = async (
    initialBalance: number,
    tokenAllowSpend: Signed<AllowSpend>,
    tokenConfig: TokenConfig,
    logger: Logger
) => {
    const expectedBalance = initialBalance - tokenAllowSpend.value.amount - tokenAllowSpend.value.fee;
    await validateIfBalanceChanged(initialBalance, expectedBalance, tokenConfig, logger);
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
        logger(msg, "ERROR", tokenConfig.context);
        throwInContext(tokenConfig.context)(msg);
    }
    logger(`Balance change validated!`, "INFO", tokenConfig.context);
}

export { getBalance, validateIfBalanceChangedByAllowSpend, validateIfBalanceChanged };