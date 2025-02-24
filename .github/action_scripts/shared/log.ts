const log = (message: string, type = "INFO", context?: string) => {
    const timestamp = new Date().toISOString();
    if (context) {
        console.log(`[${timestamp}] [${type}] [${context}] ${message}`);
    } else {
        console.log(`[${timestamp}] [${type}] ${message}`);
    }
};

const throwInContext = (context: string) => (message: string) => {
    throw new Error(`[${context}] ${message}`);
}

export { log, throwInContext };