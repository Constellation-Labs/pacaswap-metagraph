import { createIndentedLogger, log, Logger } from "./log";

const delay = (ms: number): Promise<void> => new Promise((resolve) => setTimeout(resolve, ms));


type RetryFunction<T> = (logger: Logger) => Promise<T>;

type RetryConfig = {
    maxAttempts?: number;
    delayMs?: number;
}

const defaultRetryConfig: Required<Omit<RetryConfig, 'retryUntil'>> = {
    maxAttempts: 60,
    delayMs: 1000
}

const retry = <T>(context: string, config: Partial<RetryConfig> = defaultRetryConfig) => async (fn: RetryFunction<T>): Promise<T> => {
    const { maxAttempts, delayMs } = { ...defaultRetryConfig, ...config };
    for (let attempt = 1; attempt <= maxAttempts; attempt++) {
        try {
            log(`▶ Attempt ${attempt} of ${maxAttempts}`, "INFO", context);
            const result = await fn(createIndentedLogger(log, 4));
            log(`✓ Completed successfully after ${attempt} attempt(s)`, "SUCCESS", context);
            return result;
        } catch (error) {
            if (attempt === maxAttempts) {
                log(`✗ Failed after ${maxAttempts} attempts`, "ERROR", context);
                throw error;
            }
            const errorMessage = error instanceof Error ? error.message : String(error);
            log(`✗ Attempt ${attempt} failed: ${errorMessage}. Retrying...`, "WARN", context);
            await delay(delayMs);
            log(`▶ Attempt ${attempt + 1} of ${maxAttempts}`, "INFO", context);
        }
    }
    // This will never be reached, but it satisfies the TypeScript compiler
    throw new Error("Unreachable - loop will always either return or throw");
}

export { delay, retry, Logger };