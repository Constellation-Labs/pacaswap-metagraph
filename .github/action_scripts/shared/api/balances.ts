import axios from "axios";
import { createAccount } from "./account";
import { log, throwInContext } from "../log";
import { Logger } from "../retry";

const getBalance = async (account: ReturnType<typeof createAccount>, l0Url: string, isCurrency: boolean, context: string) => {
    log(`Getting balance for account: ${account.address} `, "INFO", context);

    try {
        const snapshotUrl = isCurrency
            ? `${l0Url}/snapshots/latest/combined`
            : `${l0Url}/global-snapshots/latest/combined`;

        const { data } = await axios.get(snapshotUrl);

        const [, snapshotInfo] = data;

        const balance = snapshotInfo?.balances?.[account.address];

        if (balance === undefined) {
            throwInContext(context)(`Balance for account: ${account.address} is undefined`);
        }
        log(`Balance for account: ${account.address} is ${balance}`, "INFO", context);
        return balance;
    } catch (error) {
        console.log(error)
        log(`Error getting balance for account: ${account.address}: ${error}`, "ERROR", context);
        throw error;
    }
}

const validateIfBalanceChanged = async (
    initialBalance: number,
    tokenAllowSpend: any,
    account: ReturnType<typeof createAccount>,
    l0Url: string,
    isCurrency: boolean,
    context: string,
    logger: Logger = log
) => {
    const balance = await getBalance(account, l0Url, isCurrency, context);
    const expectedBalance = initialBalance - tokenAllowSpend.value.amount - tokenAllowSpend.value.fee;
    if (balance !== expectedBalance) {
        throwInContext(context)(`Balance different than expected. Expected: ${expectedBalance}, Actual: ${balance}`);
    }
    logger(`Balance change validated!`, "INFO", context);
}


export { getBalance, validateIfBalanceChanged };