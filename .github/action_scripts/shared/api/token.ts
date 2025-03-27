import { log } from "../log";
import { createAccount } from "./account";
import { getBalance } from "./balances";

type TokenConfig<T extends object = {}> = {
    account: ReturnType<typeof createAccount>;
    l1Url: string;
    l0Url: string;
    context: string;
    tokenId: string | null;
    isCurrency: boolean;
} & T

const createTokenConfig = async <T extends object = {}>(
    privateKey: string,
    l0Url: string,
    l1Url: string,
    context: string,
    tokenId: string | null,
    isCurrency: boolean,
    custom: T = {} as T
): Promise<TokenConfig<T>> => {
    const account = createAccount(privateKey, l0Url, l1Url);
    log(`Created token account`, "INFO", context);

    return {
        account,
        l1Url,
        l0Url,
        context,
        tokenId,
        isCurrency,
        ...custom
    };
}

export { createTokenConfig, type TokenConfig };