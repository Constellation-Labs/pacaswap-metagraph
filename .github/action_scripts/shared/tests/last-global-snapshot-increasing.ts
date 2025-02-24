import { getCalculatedState } from "../api";
import { log } from "../log";
import { retry } from "../retry";


type Config = {
    ammMl0Url: string;
}

const lastGlobalSnapshotIncreasingTest = async (config: Config) => {
    log("Starting lastGlobalSnapshotIncreasingTest".toUpperCase());
    const calculatedState = await getCalculatedState(config.ammMl0Url)
    const initialLastSyncGlobalSnapshotOrdinal = calculatedState.lastSyncGlobalSnapshotOrdinal

    log(`Initial lastSyncGlobalSnapshotOrdinal: ${initialLastSyncGlobalSnapshotOrdinal}`)

    await retry('Validate if last sync global snapshot ordinal is increasing')(async (logger) => {
        const latestCalculatedState = await getCalculatedState(config.ammMl0Url)
        const lastSyncGlobalSnapshotOrdinal = latestCalculatedState.lastSyncGlobalSnapshotOrdinal
        if (lastSyncGlobalSnapshotOrdinal > initialLastSyncGlobalSnapshotOrdinal) {
            logger(`Last sync global snapshot ordinal is increasing`)
        } else {
            throw new Error(`Last sync global snapshot ordinal not increased yet`)
        }
    })
}

export { lastGlobalSnapshotIncreasingTest };