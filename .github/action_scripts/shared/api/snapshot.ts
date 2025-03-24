import axios from "axios";

type Snapshot = {
    value: {
        ordinal: number;
        epochProgress: number;
    };
}

type CurrencySnapshot = Snapshot & {
    value: {
        globalSyncView: {
            epochProgress?: number | null;
            ordinal: number;
        };
    };
}

type SnapshotInfo = {
    activeAllowSpends: {
        [key: string]: string[];
    };
    tokenLockBalances: {
        [key: string]: {
            [key: string]: number;
        };
    };
}

type CombinedSnapshot<S extends Snapshot> = [S, SnapshotInfo]

const getCurrencySnapshot = async (ml0Url: string): Promise<CurrencySnapshot> => {
    const { data: snapshot } = await axios.get(
        `${ml0Url}/snapshots/latest`
    );
    return snapshot;
}

const getGlobalSnapshot = async (gl0Url: string): Promise<Snapshot> => {
    const { data: snapshot } = await axios.get(
        `${gl0Url}/global-snapshots/latest`
    );
    return snapshot;
}

const getCurrencySnapshotCombined = async (ml0Url: string): Promise<CombinedSnapshot<CurrencySnapshot>> => {
    const { data: combinedSnapshot } = await axios.get(
        `${ml0Url}/snapshots/latest/combined`
    );
    return combinedSnapshot;
}

const getGlobalSnapshotCombined = async (gl0Url: string): Promise<CombinedSnapshot<Snapshot>> => {
    const { data: combinedSnapshot } = await axios.get(
        `${gl0Url}/global-snapshots/latest/combined`
    );
    return combinedSnapshot;
}

const getCurrentEpochProgress = async (l0Url: string, isCurrency: boolean) => {
    if (isCurrency) {
        const snapshot = await getCurrencySnapshot(l0Url);
        return snapshot.value.globalSyncView.epochProgress;
    } else {
        const snapshot = await getGlobalSnapshot(l0Url);
        return snapshot.value.epochProgress;
    }
}

const getCurrentOrdinal = async (l0Url: string, isCurrency: boolean) => {
    if (isCurrency) {
        const snapshot = await getCurrencySnapshot(l0Url);
        return snapshot.value.globalSyncView.ordinal;
    } else {
        const snapshot = await getGlobalSnapshot(l0Url);
        return snapshot.value.ordinal;
    }
}

export { getCurrencySnapshotCombined, getGlobalSnapshotCombined, getGlobalSnapshot, getCurrencySnapshot, getCurrentEpochProgress, getCurrentOrdinal }
