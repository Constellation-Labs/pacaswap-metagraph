import axios from "axios";
import { createAccount } from "./account";
import { log, logObject, throwInContext } from "../log";
import { Logger } from "../retry";
import { Signed } from "./signed";
import { AllowSpend } from "./allow-spends";
import { TokenConfig } from "./token";

const getBalance = async (tokenConfig: TokenConfig) =>
    getBalanceForAddress(tokenConfig.account.address, tokenConfig.l0Url, tokenConfig.isCurrency, tokenConfig.context)


const getSnapshotBalanceForAddress = async (address: string, l0Url: string, context: string, currencyId: string) => {
    log(`Getting snapshot balance for account: ${address} `, "INFO", context);

    try {
        const snapshotUrl = `${l0Url}/global-snapshots/latest/combined`;

        const { data } = await axios.get(snapshotUrl);


        const [, snapshotInfo] = data;

        const snapshotBalances = snapshotInfo.lastCurrencySnapshots[currencyId].Right[1].balances;
        console.warn(JSON.stringify(snapshotBalances, null, 2))

        const balance = snapshotBalances[address];

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

const getSnapshotRewardForAddress = async (address: string, l0Url: string, context: string, currencyId: string) => {
    log(`Getting snapshot balance for account: ${address} `, "INFO", context);

    try {
        const snapshotUrl = `${l0Url}/global-snapshots/latest/combined`;

        const { data } = await axios.get(snapshotUrl);
        const [, snapshotInfo] = data;

        const rewardInfo = snapshotInfo.lastCurrencySnapshots[currencyId].Right[0].value.rewards;
        const rewards = rewardInfo.filter(r => r.destination === address).map(r => r.amount).sort((a, b) => a - b);

        if (rewards === undefined) {
            throwInContext(context)(`Rewards for account: ${address} is undefined`);
        }
        log(`Rewards for account: ${address} is ${rewards}`, "INFO", context);
        return rewards;
    } catch (error) {
        console.log(error)
        log(`Error getting rewards for account: ${address}: ${error}`, "ERROR", context);
        throw error;
    }
}

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

export { getBalance, getBalanceForAddress, validateIfBalanceChanged, getSnapshotBalanceForAddress, getSnapshotRewardForAddress };
