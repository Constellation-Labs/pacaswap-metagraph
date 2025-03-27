import axios from "axios"
import { log, throwInContext } from "../log"
import { Signed } from "./signed";

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

export { sendDataUpdate }