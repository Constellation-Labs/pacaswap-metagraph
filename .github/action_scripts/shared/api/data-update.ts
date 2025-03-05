import axios from "axios"
import { log, throwInContext } from "../log"

type Signed<T> = {
    proofs: {
        id: string
        signature: string
    }[]
    value: T
}

type FailedCalculatedState<T> = {
    update: Signed<T>
}

type TokenInformation = {
    identifier?: string | null
    amount: number
}

const sendDataUpdate = async <T>(
    dataL1Url: string,
    update: Signed<T>,
) => {
    try {
        log(`Sending data update...`, "INFO", 'AMM');
        await axios.post(`${dataL1Url}/data`, update);
        log(`Data update sent successfully`, "INFO", 'AMM');
        return;
    } catch (error) {
        throwInContext('AMM')(`Failed to send data update: ${error.message}`);
    }
}

export { sendDataUpdate, type Signed, type FailedCalculatedState, type TokenInformation }