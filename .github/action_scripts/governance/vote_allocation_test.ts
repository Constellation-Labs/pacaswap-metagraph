import { dag4 } from "@stardust-collective/dag4";
import axios from "axios";
import { log, delay, getPublicKey, parseSharedArgs } from "../shared";
import { z } from 'zod';

const AllocationsSchema = z.object({
  key: z.string()
    .min(1, "key cannot be empty"),
  amount: z.number()
    .min(1, "amount must be greater than 0"),
});

const VoteAllocationSchema = z.object({
  privateKey: z.string()
    .min(1, "Private key cannot be empty"),
  allocations: z.array(AllocationsSchema)
    .min(1, "Allocations array cannot be empty"),
});

const CliArgsSchema = z.object({
  voteAllocations: z.string()
    .transform((str, ctx) => {
      try {
        return JSON.parse(str);
      } catch (e) {
        ctx.addIssue({
          code: z.ZodIssueCode.custom,
          message: "Invalid JSON string"
        });
        return z.NEVER;
      }
    })
    .pipe(z.array(VoteAllocationSchema)
      .min(1, "VoteAllocations array cannot be empty")
      .refine(data => data.every(lock =>
        lock.privateKey &&
        lock.allocations
      ), {
        message: "Each vote allocation must include privateKey, and allocations"
      })
    )
});

const createConfig = () => {
  const args = process.argv.slice(2);

  if (args.length < 6) {
    throw new Error(
      "Usage: npx tsx governance/vote_allocation_test.ts <gl0-url> <aml0-url> <acl1-url> <adl1-url> <metagraph-id> <vote-allocations-json>"
    );
  }

  const sharedArgs = parseSharedArgs(args.slice(0, 5));
  const [voteAllocations] = args.slice(5);

  const specificArgs = CliArgsSchema.parse({ voteAllocations });

  return { ...sharedArgs, ...specificArgs };
};

const getSignedVoteAllocation = async (config, { privateKey, publicKey, address, allocations }) => {
  log("Generating signed vote allocations...");
  const { ammMl0Url } = config;

  log(`Fetching last reference for wallet: ${address} ${`${ammMl0Url}/v1/addresses/${address}/vote-info/last-reference`}`);
  const { data: lastRef } = await axios.get(`${ammMl0Url}/v1/addresses/${address}/vote-info/last-reference`);
  const { hash, ordinal } = lastRef;
  const parsedAllocations = allocations.map(allocation => [allocation.key, allocation.amount])

  const body = {
    RewardAllocationVoteUpdate: {
      address,
      allocations: parsedAllocations,
      parent: {
        hash,
        ordinal
      }
    },
  };

  const encodedMessage = Buffer.from(JSON.stringify(body)).toString('base64')
  const signature = await dag4.keyStore.dataSign(
    privateKey,
    encodedMessage
  );

  log(`Signed vote allocation generated for wallet: ${address}`);

  return {
    value: body,
    proofs: [{ id: publicKey, signature }],
  };
};

const sendSignedVoteAllocation = async (config, signedVoteAllocation) => {
  const { ammDl1Url } = config;
  try {
    log("Sending signed vote allocation...");
    await axios.post(`${ammDl1Url}/data`, signedVoteAllocation);
    log("Signed vote allocation sent successfully.");
  } catch (error) {
    log(`Error sending signed vote allocation: ${error.message}`, "ERROR");
    throw error;
  }
};


const validateVoteAllocations = async (config, address, allocations) => {
  const { ammMl0Url } = config;

  const maxAttempts = 60
  for (let attempt = 1; attempt <= maxAttempts; attempt++) {
    try {
      log(`Validating vote allocations (Attempt ${attempt}/${maxAttempts}) for wallet: ${address}`);
      await axios.get(`${ammMl0Url}/v1/addresses/${address}/vote-info`);

      log(`Vote info filled`);
      return
    } catch (error) {
      log(`Error validating vote allocation: ${error.message}`, "WARN");
    }
    await delay(1000);
  }
  throw new Error(`Vote allocation validation failed after ${maxAttempts} attempts.`);
};

const validateAllocationsRewards = async (config) => {
  const { ammMl0Url } = config;

  const maxAttempts = 60
  for (let attempt = 1; attempt <= maxAttempts; attempt++) {
    try {
      log(`Validating vote allocations (Attempt ${attempt}/${maxAttempts})`);
      const { data: lastRewards } = await axios.get(`${ammMl0Url}/v1/governance/allocations/rewards`);

      const someRewardFilled = lastRewards.find(reward => {
        return Object.keys(reward.rewardsInfo).length > 0
      })

      if (someRewardFilled) {
        const nodeValidatorsReward = someRewardFilled.rewardsInfo.NodeValidators
        if (nodeValidatorsReward === 250) {
          log(`Rewards filled correctly`);
          return
        }
        throw new Error(`NodeValidatorsReward invalid weight: ${nodeValidatorsReward}`);
      }

      log(`Rewards not filled yet. Retrying...`);
    } catch (error) {
      log(`Error checking allocation rewards: ${error.message}`, "WARN");
    }
    await delay(1000);
  }
  throw new Error(`Allocation rewards validation failed after ${maxAttempts} attempts.`);
};

const main = async () => {
  const config = createConfig()

  log("Building vote allocations information...");
  const voteAllocationsInfo = config.voteAllocations.map((data) => {
    const account = dag4.createAccount();
    account.loginPrivateKey(data.privateKey);

    return {
      privateKey: data.privateKey,
      publicKey: getPublicKey(account),
      address: account.address,
      allocations: data.allocations,
    };
  });
  log("Vote allocation information built successfully.");

  for (const voteAllocationInfo of voteAllocationsInfo) {
    const { address, allocations } = voteAllocationInfo;

    const signedVoteAllocation = await getSignedVoteAllocation(config, voteAllocationInfo);
    await sendSignedVoteAllocation(config, signedVoteAllocation);

    await validateVoteAllocations(config, address, allocations)
    await delay(10000)
  }

  log(`Waiting 2 minutes to check if the allocations were clear and the rewards filled`)
  await delay(120 * 1000)

  for (const voteAllocationInfo of voteAllocationsInfo) {
    await validateAllocationsRewards(config)
  }

  log("All vote allocations validated successfully.");
};

main().catch((error) => {
  log(`Error: ${error.message}`, "ERROR");
  process.exit(1);
});