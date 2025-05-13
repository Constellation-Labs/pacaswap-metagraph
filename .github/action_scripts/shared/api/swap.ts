import axios from "axios";
import { createAccount, getPublicKey } from "./account";
import { log, logObject, throwInContext } from "../log";
import { serializeBase64 } from "../serialize";
import { dag4 } from "@stardust-collective/dag4";
import { getCalculatedState, isPendingAllowSpend, TokenInformation } from "./calculated-state";
import { LastRef } from "./last-ref";
import { Signed } from "./signed";
type SwapUpdate = {
    metagraphId: string,
    source: string,
    swapFromPair: string | null
    swapToPair: string | null
    allowSpendReference: string
    amountIn: number
    amountOutMinimum: number
    amountOutMaximum: number | null
    maxValidGsEpochProgress: number
    parent: string
}

type SwapCalculatedStateAddress = {
    swapHash: string
    sourceAddress: string
    fromToken: TokenInformation
    toToken: TokenInformation
    allowSpendReference: string
    amountIn: number
    amountOutMinimum: number
    amountOutMaximum: number | null
    maxValidGsEpochProgress: number
    poolId?: string
    ordinal: number
    parent: LastRef
}

type SwapUpdateBody = {
    SwapUpdate: SwapUpdate
}

type ConfirmedSwapCalculatedState = {
    value: Record<string, SwapCalculatedStateAddress[]>
}

const createSwapUpdate = async (
    fromPair: string | null,
    toPair: string | null,
    ammMetagraphId: string,
    allowSpendReference: string,
    amountIn: number,
    amountOutMinimum: number,
    account: ReturnType<typeof createAccount>,
    privateKey: string,
    l0Url: string,
    context: string,
): Promise<Signed<SwapUpdateBody>> => {
    log(`Fetching last swap reference for wallet: ${account.address}`, "INFO", context);

    const { data } = await axios.get(
        `${l0Url}/v1/addresses/${account.address}/swaps/last-reference`
    );

    const lastRef = data.data
    log(`Last swap reference for wallet: ${account.address}: ${JSON.stringify(lastRef, null, 2)}`, "INFO", context);

    const body: SwapUpdateBody = {
        SwapUpdate: {
            metagraphId: ammMetagraphId,
            source: account.address,
            swapFromPair: fromPair,
            swapToPair: toPair,
            allowSpendReference,
            amountIn,
            amountOutMinimum,
            amountOutMaximum: null,
            maxValidGsEpochProgress: 1000,
            parent: lastRef,
        }
    };

    const serialized = await serializeBase64(body)
    const signature = await dag4.keyStore.dataSign(
        privateKey,
        serialized
    );

    const publicKey = getPublicKey(account)

    const swapUpdate = {
        value: body,
        proofs: [{ id: publicKey, signature }]
    };

    log(`Signed swap update generated for wallet: ${account.address}: ${JSON.stringify(swapUpdate, null, 2)}`, "INFO", context);

    return swapUpdate;
}

const validateSwapCreated = async (
    ammL0Url: string,
    fromPair: string | null,
    toPair: string | null,
    allowSpendReference: string,
    account: ReturnType<typeof createAccount>,
    logger: (message: string, type?: string, context?: string) => void = log
) => {

    const calculatedState = await getCalculatedState(ammL0Url);
    logger("Pulled calculated state", "INFO", 'AMM')

    const swapCalculatedState = calculatedState?.operations.Swap?.SwapCalculatedState

    const confirmedSwaps = swapCalculatedState?.confirmed.value || {}
    const pendingSwaps = swapCalculatedState?.pending || []
    const failedSwaps = swapCalculatedState?.failed || []

    log(`Looking for confirmed swaps for wallet: ${account.address}`, "INFO", 'AMM')

    logObject(swapCalculatedState)

    const isConfirmedSwap = (confirmedSwaps[account.address] || []).some(
        (swap) =>
            swap.fromToken.identifier === fromPair
            && swap.toToken.identifier === toPair
            && swap.allowSpendReference === allowSpendReference
    );

    const isPendingSwap = pendingSwaps.some(
        (pendingAction) => {
            const updateValue = isPendingAllowSpend(pendingAction)
                ? pendingAction.PendingAllowSpend.update.value
                : pendingAction.PendingSpendAction.update.value;

            return updateValue.swapFromPair === fromPair && updateValue.swapToPair === toPair;
        }
    );

    const isFailedSwap = failedSwaps.some(
        (swap) =>
            swap.update.value.swapFromPair === fromPair && swap.update.value.swapToPair === toPair
    );

    logObject({ isConfirmedSwap, isPendingSwap, isFailedSwap })

    if (isFailedSwap) {
        throwInContext('AMM')("Swap creation failed.");
    } else if (isPendingSwap) {
        throwInContext('AMM')("Swap creation is pending.");
    } else if (isConfirmedSwap) {
        logger("Swap creation validated!", "INFO", 'AMM')
    } else {
        throwInContext('AMM')("Swap not found.");
    }
}

type SwapQuoteRequest = {
    fromTokenId: string | null,
    toTokenId: string | null,
    amount: number,
    slippagePercent: number
}

type SwapQuoteResponse = {
    data: {
        fromTokenId: string | null,
        toTokenId: string | null,
        amount: number,
        slippagePercent: number,
        rate: number,
        priceImpactPercent: number,
        estimatedReceived: number,
        minimumReceived: number
    }
}

const getSwapQuote = async (
    ammL0Url: string,
    request: SwapQuoteRequest
): Promise<SwapQuoteResponse['data']> => {
    const response = await axios.post<SwapQuoteResponse>(`${ammL0Url}/v1/swap/quote`, request);

    return response.data.data;
}

export { createSwapUpdate, validateSwapCreated, getSwapQuote, type SwapQuoteRequest, type SwapQuoteResponse, type SwapUpdate, type SwapCalculatedStateAddress, type ConfirmedSwapCalculatedState }
