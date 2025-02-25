import { log } from '../shared';
import { tokenLockTests } from './token-lock'
import { voteAllocationTests } from './vote-allocation'

const getArgs = () => {
  const args = process.argv.slice(2);
  const [
    gl0Url,
    ammMl0Url,
    ammCl1Url,
    ammDl1Url,
    metagraphId
  ] = args;

  if (args.length < 5) {
    throw new Error(
      "Usage: npx tsx amm-operations/main.ts <gl0-url> <aml0-url> <acl1-url> <adl1-url> <amm-metagraph-id>"
    );
  }

  return {
    gl0Url,
    ammMl0Url,
    ammCl1Url,
    ammDl1Url,
    metagraphId
  }
};

const main = async () => {
  const args = getArgs();

  await tokenLockTests(args);
  await voteAllocationTests(args);
};

main().catch((error) => {
  log(`Error: ${error.message}`, "ERROR");
  process.exit(1);
});
