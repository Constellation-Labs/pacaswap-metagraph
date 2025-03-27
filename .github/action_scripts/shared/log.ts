
// ANSI color codes for terminal output
const colors = {
    reset: "\x1b[0m",
    // Text colors
    red: "\x1b[31m",
    green: "\x1b[32m",
    yellow: "\x1b[33m",
    white: "\x1b[37m",
};

type LogType = "INFO" | "WARN" | "ERROR" | "EMPTY" | "SUCCESS";

// Color mapping for different log types
const typeColors: Record<LogType, string> = {
    "INFO": colors.white,
    "WARN": colors.yellow,
    "ERROR": colors.red,
    "EMPTY": colors.white,
    "SUCCESS": colors.green,
};

type Logger = (message: string, type?: LogType, context?: string, indent?: number) => void;

const log: Logger = (message: string, type: LogType = "INFO", context?: string, indent?: number) => {
    const timestamp = new Date().toISOString();
    const indentStr = indent ? ' '.repeat(indent) : '';

    // Get color for the type
    const typeColor = typeColors[type as keyof typeof typeColors] || colors.white;

    if (context) {
        console.log(
            `[${timestamp}] ${typeColor}[${type}]${colors.reset}${indentStr}[${context}] ${message}`
        );
    } else {
        console.log(
            `[${timestamp}] ${typeColor}[${type}]${colors.reset} ${message}`
        );
    }
};

const logObject = (object: any) => {
    console.log('\n' + JSON.stringify(object, null, 2))
}

const throwInContext = (context: string) => (message: string) => {
    throw new Error(`[${context}] ${message}`);
}

const createIndentedLogger = (logger: Logger, indent: number): Logger =>
    (message: string, type?: LogType, context?: string) => {
        logger(message, type, context, indent);
    };

export { log, logObject, throwInContext, createIndentedLogger, type Logger };
