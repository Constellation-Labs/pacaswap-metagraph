import { log } from "../log";
import { createAccount } from "./account";
import { getBalance } from "./balances";

type TokenConfig = {
    account: ReturnType<typeof createAccount>;
    l1Url: string;
    l0Url: string;
    context: string;
    tokenId: string | null;
    allowSpendAmount: number;
    isCurrency: boolean;
}

const createTokenConfig = async (
    privateKey: string,
    l0Url: string,
    l1Url: string,
    context: string,
    tokenId: string | null,
    isCurrency: boolean,
    allowSpendAmount: number
): Promise<TokenConfig> => {
    const account = createAccount(privateKey, l0Url, l1Url);
    log(`Created token account`, "INFO", context);

    return {
        account,
        l1Url,
        l0Url,
        context,
        tokenId,
        allowSpendAmount,
        isCurrency
    };
}

export { createTokenConfig, type TokenConfig };