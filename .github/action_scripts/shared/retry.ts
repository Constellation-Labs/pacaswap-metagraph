import { log } from "./log";

const delay = (ms: number): Promise<void> => new Promise((resolve) => setTimeout(resolve, ms));

type Severity = "INFO" | "WARN" | "ERROR" | "EMPTY";
type Logger = (message: string, type?: string | undefined, context?: string | undefined) => void;


const createIndentedLogger = (logger: Logger, indent: number): Logger =>
    (message: string, type?: string, context?: string) => {
        const indentStr = ' '.repeat(indent);
        logger(`${indentStr}${message}`, type, context);
    };

type RetryFunction = (logger: Logger) => Promise<void>;

const retry = (context: string) => async (fn: RetryFunction, maxAttempts: number = 60, delayMs: number = 1000): Promise<void> => {
    log(`\n`, "EMPTY");
    for (let attempt = 1; attempt <= maxAttempts; attempt++) {
        try {
            log(`▶ Attempt ${attempt} of ${maxAttempts}`, "INFO", context);
            await fn(createIndentedLogger(log, 4));
            log(`✓ Completed successfully after ${attempt} attempt(s)`, "INFO", context);
            log(`\n`, "EMPTY");
            return;
        } catch (error) {
            if (attempt === maxAttempts) {
                log(`✗ Failed after ${maxAttempts} attempts`, "ERROR", context);
                log(`\n`, "EMPTY");
                throw error;
            }
            const errorMessage = error instanceof Error ? error.message : String(error);
            log(`✗ Attempt ${attempt} failed: ${errorMessage}. Retrying...`, "WARN", context);
            await delay(delayMs);
            log(`▶ Attempt ${attempt + 1} of ${maxAttempts}`, "INFO", context);
        }
    }
}

export { delay, retry, Logger, Severity };