import { z } from 'zod';

const BaseCliArgsSchema = z.object({
    gl0Url: z.string().url("GL0 URL must be a valid URL"),
    dagCl1Url: z.string().url("DAG CL1 URL must be a valid URL"),
    ammMl0Url: z.string().url("AMM ML0 URL must be a valid URL"),
    ammCl1Url: z.string().url("AMM CL1 URL must be a valid URL"),
    ammDl1Url: z.string().url("AMM DL1 URL must be a valid URL"),
    tokenACl1Url: z.string().url("TokenA CL1 URL must be a valid URL"),
    tokenBCl1Url: z.string().url("TokenB CL1 URL must be a valid URL"),
    tokenAMl0Url: z.string().url("TokenA ML0 URL must be a valid URL"),
    tokenBMl0Url: z.string().url("TokenB ML0 URL must be a valid URL"),
    ammMetagraphId: z.string().min(1, "AMM Metagraph ID cannot be empty"),
    tokenAId: z.string().min(1, "tokenAId cannot be empty"),
    tokenBId: z.string().min(1, "tokenBId cannot be empty"),
});

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

export { parseSharedArgs, CliArgsSchema, BaseCliArgsSchema };