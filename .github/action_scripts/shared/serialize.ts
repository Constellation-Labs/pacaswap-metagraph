import { compress } from 'brotli';
import jsSha256 from 'js-sha256';

const removeNulls = (obj: any | null) => {
    const processValue = (value: any) => {
        if (value === null) return undefined;
        if (Array.isArray(value)) {
            return value.map(v => processValue(v)).filter(v => v !== undefined);
        }
        if (typeof value === 'object') {
            return removeNulls(value);
        }
        return value;
    };

    return Object.entries(obj)
        .reduce((acc, [key, value]) => {
            const processed = processValue(value);
            if (processed !== undefined) {
                acc[key] = processed;
            }
            return acc;
        }, {});
};

const sortObjectKeys = (obj: any | null) => {
    if (typeof obj !== 'object' || obj === null) {
        return obj;
    }

    if (Array.isArray(obj)) {
        return obj.map(sortObjectKeys);
    }

    return Object.keys(obj)
        .sort()
        .reduce((acc, key) => {
            acc[key] = sortObjectKeys(obj[key]);
            return acc;
        }, {});
};

const serializeBrotli = async (content, shouldSort = true, shouldRemoveNulls = true) => {
    const compressionLevel = 2;
    const sorted = shouldSort ? sortObjectKeys(content) : content;
    const removedNulls = shouldRemoveNulls ? removeNulls(sorted) : sorted;
    const jsonString = JSON.stringify(removedNulls);
    const encoder = new TextEncoder();
    const utf8Bytes = encoder.encode(jsonString);
    return compress(utf8Bytes, { quality: compressionLevel });
};

const serializeBase64 = async (content, shouldSort = true, shouldRemoveNulls = true) => {
    const sorted = shouldSort ? sortObjectKeys(content) : content;
    const removedNulls = shouldRemoveNulls ? removeNulls(sorted) : sorted;
    const jsonString = JSON.stringify(removedNulls);
    const base64String = Buffer.from(jsonString, 'utf8').toString('base64');
    return base64String;
};

const getHash = (content: any) => jsSha256.sha256(Buffer.from(content, "hex"));

export { serializeBrotli, serializeBase64, getHash };