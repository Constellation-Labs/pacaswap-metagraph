import { dag4 } from "@stardust-collective/dag4";
import { log } from "./log";

const createAccount = (privateKey: string, l0Url: string, l1Url: string) => {
    const account = dag4.createAccount(privateKey);

    account.connect({
        networkVersion: '2.0',
        l0Url,
        l1Url,
    });

    return account;
}

const getPublicKey = (account) => {
    const publicKey = account.publicKey;
    if (publicKey.length === 130) {
        log(`Public key: ${publicKey} has length 130. Removing 2 characters...`);
        return publicKey.substring(2);
    } else if (publicKey.length === 128) {
        log(`Public key: ${publicKey} has length 128. Returning as is...`);
        return publicKey;
    } else {
        throw new Error("Public key has wrong length");
    }
};

export { createAccount, getPublicKey };