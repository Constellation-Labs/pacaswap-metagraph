import { BaseWithCurrencyMetagraphsCliArgsSchema, log } from '../shared';
import liquidityPoolTests from './liquidity-pool'
import stakingTests from './staking'
import withdrawalTests from './withdrawal'

const getBaseConfig = () => {
  const args = process.argv.slice(2);

  const [
    gl0Url,
    dagCl1Url,
    ammMl0Url,
    ammCl1Url,
    ammDl1Url,
    tokenACl1Url,
    tokenBCl1Url,
    tokenAMl0Url,
    tokenBMl0Url,
    ammMetagraphId,
    tokenAId,
    tokenBId,
  ] = args;

  if (args.length < 11) {
    throw new Error(
      "Usage: npx tsx amm-operations/main.ts <gl0-url> <dagcl1-url> <aml0-url> <acl1-url> <adl1-url> <tacl1-url> <tbcl1-url> <taml0-url> <tbml0-url> <amm-metagraph-id> <tokenAId> <tokenBId>"
    );
  }

  return BaseWithCurrencyMetagraphsCliArgsSchema.parse({
    gl0Url,
    dagCl1Url,
    ammMl0Url,
    ammCl1Url,
    ammDl1Url,
    tokenACl1Url,
    tokenBCl1Url,
    tokenAMl0Url,
    tokenBMl0Url,
    ammMetagraphId,
    tokenAId,
    tokenBId
  })
};

const main = async () => {
  const baseConfig = getBaseConfig();

  await liquidityPoolTests(baseConfig);
  await stakingTests(baseConfig);
  await withdrawalTests(baseConfig);
};

main().catch((error) => {
  log(`Error: ${error.message}`, "ERROR");
  process.exit(1);
});
