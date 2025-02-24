
// ANSI color codes for terminal output
const colors = {
    reset: "\x1b[0m",
    // Text colors
    red: "\x1b[31m",
    green: "\x1b[32m",
    yellow: "\x1b[33m",
    white: "\x1b[37m",
};

// Color mapping for different log types
const typeColors = {
    "INFO": colors.white,
    "WARN": colors.yellow,
    "ERROR": colors.red,
    "EMPTY": colors.white,
    "SUCCESS": colors.green,
};

const log = (message: string, type = "INFO", context?: string, indent?: number) => {
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

const logObject = (object: any, context?: string, indent?: number) => {
    console.log('\n' + JSON.stringify(object, null, 2))
}

const throwInContext = (context: string) => (message: string) => {
    throw new Error(`[${context}] ${message}`);
}

export { log, logObject, throwInContext };
