const { z } = require('zod');

const log = (message, type = "INFO") => {
  const timestamp = new Date().toISOString();
  console.log(`[${timestamp}] [${type}] ${message}`);
};

const delay = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

const getPublicKey = (account) => {
  const publicKey = account.publicKey;
  return publicKey.length === 128 ? publicKey.substring(2) : publicKey.substring(2);
};

const CliArgsSchema = z.object({
  gl0Url: z.string().url('GL0 URL must be a valid URL'),
  ammMl0Url: z.string().url('AMM ML0 URL must be a valid URL'),
  ammCl1Url: z.string().url('AMM CL1 URL must be a valid URL'),
  ammDl1Url: z.string().url('AMM DL1 URL must be a valid URL'),
  metagraphId: z.string().min(1, 'Metagraph ID cannot be empty'),
});

const parseSharedArgs = (args) => {
  const [gl0Url, ammMl0Url, ammCl1Url, ammDl1Url, metagraphId] = args;
  return CliArgsSchema.parse({ gl0Url, ammMl0Url, ammCl1Url, ammDl1Url, metagraphId });
};

module.exports = {
  parseSharedArgs,
  log,
  delay,
  getPublicKey
}