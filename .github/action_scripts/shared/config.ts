import { z } from 'zod';

const CliArgsSchema = z.object({
    gl0Url: z.string().url("GL0 URL must be a valid URL"),
    ammMl0Url: z.string().url("AMM ML0 URL must be a valid URL"),
    ammCl1Url: z.string().url("AMM CL1 URL must be a valid URL"),
    ammDl1Url: z.string().url("AMM DL1 URL must be a valid URL"),
    metagraphId: z.string().min(1, "Metagraph ID cannot be empty"),
});

const parseSharedArgs = (args) => {
    const [gl0Url, ammMl0Url, ammCl1Url, ammDl1Url, metagraphId] = args;
    return CliArgsSchema.parse({
        gl0Url,
        ammMl0Url,
        ammCl1Url,
        ammDl1Url,
        metagraphId,
    });
};

export { parseSharedArgs, CliArgsSchema };