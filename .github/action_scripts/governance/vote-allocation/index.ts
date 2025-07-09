import { dag4 } from "@stardust-collective/dag4";
import axios from "axios";
import { delay, getPublicKey, log, retry, getSnapshotRewardForAddress, getSnapshotBalanceForAddress, getGlobalSnapshotCombined, getBalanceForAddress, BaseAmmMetagraphCliArgsSchema, serializeBase64, sendDataUpdate, getAvaialbleRewardsForAddress } from "../../shared";
import { z } from 'zod';

const voteAllocations = [{
    "privateKey": "8971dcc9a07f2db7fa769582139768fd5d73c56501113472977eca6200c679c8",
    "allocations": [{ "key": "NodeValidators", "amount": 10 }]
}
]

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

const createConfig = (argsObject: object) => {
    const argsObj = {
        ...argsObject,
        voteAllocations
    }

    const CliArgsSchema = BaseAmmMetagraphCliArgsSchema.extend({
        voteAllocations: z.array(VoteAllocationSchema)
    });

    return CliArgsSchema.parse(argsObj);
};

const getSignedRewardWithdraw = async (config, { privateKey, publicKey, address }, rewardType, amount) => {
    log("Generating reward withdraw request...");
    const { ammMl0Url } = config;

    log(`Fetching last reference for wallet: ${address} ${`${ammMl0Url}/v1/rewards/${address}/withdrawals/last-reference`}`);
    const { data: lastRef } = await axios.get(`${ammMl0Url}/v1/rewards/${address}/withdrawals/last-reference`);
    const { hash, ordinal } = lastRef.data;

    const body = {
        RewardWithdrawUpdate: {
            metagraphId: config.metagraphId,
            source: address,
            parent: {
                hash,
                ordinal
            },
            rewardType: rewardType,
            amount: amount
        }
    };

    const encodedMessage = await serializeBase64(body)
    const signature = await dag4.keyStore.dataSign(
        privateKey,
        encodedMessage
    );

    const dataUpdate = {
        value: body,
        proofs: [{ id: publicKey, signature }],
    };

    log(`Signed reward withdraw had been generated for: ${address}: ${JSON.stringify(dataUpdate)}`);

    return dataUpdate;
}

const getSignedVoteAllocation = async (config, { privateKey, publicKey, address, allocations }) => {
    log("Generating signed vote allocations...");
    const { ammMl0Url } = config;

    log(`Fetching last reference for wallet: ${address} ${`${ammMl0Url}/v1/addresses/${address}/vote-info/last-reference`}`);
    const { data: lastRef } = await axios.get(`${ammMl0Url}/v1/addresses/${address}/vote-info/last-reference`);
    const { hash, ordinal } = lastRef.data;
    const parsedAllocations = allocations.map(allocation => [allocation.key, allocation.amount])

    const body = {
        RewardAllocationVoteUpdate: {
            allocations: parsedAllocations,
            metagraphId: config.metagraphId,
            parent: {
                hash,
                ordinal
            },
            source: address,
        },
    };

    const encodedMessage = await serializeBase64(body)
    const signature = await dag4.keyStore.dataSign(
        privateKey,
        encodedMessage
    );

    const dataUpdate = {
        value: body,
        proofs: [{ id: publicKey, signature }],
    };

    log(`Signed vote allocation generated for wallet: ${address}: ${JSON.stringify(dataUpdate)}`);

    return dataUpdate;
};

const validateVoteAllocations = async (
    config: ReturnType<typeof createConfig>,
    address: string,
    logger: (message: string, type?: string, context?: string) => void = log
) => {
    const { ammMl0Url } = config;

    try {
        const voteInfo = await axios.get(`${ammMl0Url}/v1/addresses/${address}/vote-info`);
        logger(`Vote info filled for ${address}`);
        return
    } catch (error) {
        logger(`Error validating vote allocation: ${error.message}`, "WARN");
        throw error
    }

};

const validateAllocationsRewards = async ( 
    config: ReturnType<typeof createConfig>,
    address: string,
    expectedTotal: number,
    logger: (message: string, type?: string, context?: string) => void = log
) => {
    const { ammMl0Url } = config;

    const { data: lastRewards } = await axios.get(`${ammMl0Url}/v1/governance/allocations/rewards`);
    console.log(JSON.stringify(lastRewards.data, null, 2));

    const total = lastRewards.data.votes?.[address]?.total;

    logger(`Check frozen governance votes ${total}`);

    if (expectedTotal === 0) {
        if (total === undefined) {
            logger(`Governance votes correctly not filled`);
            return;
        } else {
            throw new Error(`Expected no governance votes, but found: ${total}`);
        }
    }

    if (total !== undefined && Number(total) === expectedTotal) {
        logger(`Governance votes filled correctly`);
        return;
    }

    throw new Error(`Invalid governance votes: expected ${expectedTotal}, got ${total}`);
};

const getExpectedAvaialbleRewardForAddress = async (
  address: string,
  rewardType: string,
  url: string,
  isInvalid: (balance: number) => boolean
) => {
  const balance = await getAvaialbleRewardsForAddress(address, rewardType, url, "");
  
  if (isInvalid(balance)) {
    throw new Error(`Not expected balance condition yet for ${balance}`);
  }

  return balance;
};

const getExpectedBalance = async (address: string, url: string, metagraphId: string, expectedBalance) => {
    const balance = await getSnapshotBalanceForAddress(address, url, "", metagraphId);
    log(`Got balance ${balance}, expecting ${expectedBalance}`)
    if (balance !== expectedBalance) {
        throw new Error(`Avaialble rewards not filled yet`);
    }
};

const voteAllocationTests = async (argsObject: object) => {
    const config = createConfig(argsObject)

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

    for (const voteAllocationInfo of voteAllocationsInfo) {
        const { address } = voteAllocationInfo;
        log(`Initial balance for ${address}`)
        const balance = getBalanceForAddress(address, config.gl0Url, false, "")
    }

    log("Vote allocation information built successfully.");

    for (const voteAllocationInfo of voteAllocationsInfo) {
        const { address } = voteAllocationInfo;

        const signedVoteAllocation = await getSignedVoteAllocation(config, voteAllocationInfo);
        await sendDataUpdate(config.ammDl1Url, signedVoteAllocation);

        await retry(`Validate voting weight for ${address}`, { delayMs: 10000 })(async (logger) => {
            await validateVoteAllocations(config, address, logger)
        })
        await delay(1000)
    }
    await delay(10000)


    for (const voteAllocationInfo of voteAllocationsInfo) {
        const { address } = voteAllocationInfo;
        await retry(`Validate voting weight second time for ${address}`)(async (logger) => {
            await validateVoteAllocations(config, address, logger)
        })

        log(`Governance voting shall appeared in frozen votes for ${address} as month pass`) 
        await retry(`Validate allocations rewards for`, { delayMs: 5000, maxAttempts: 50 })(async (logger) => {
            await validateAllocationsRewards(config, address, 70000 * 1e8, logger)
        })

        log(`Governance voting shall disappeared in frozen votes for ${address} as month pass again`) 
        await retry(`Validate allocations rewards for`, { delayMs: 5000, maxAttempts: 50 })(async (logger) => {
            await validateAllocationsRewards(config, address, 0, logger)
        })
        const govRewardType = "GovernanceVoting"
      
        const initialBalance = await getSnapshotBalanceForAddress(address, config.gl0Url, "", config.metagraphId);
        const initialAvailableRewards = await getAvaialbleRewardsForAddress(address, govRewardType, config.ammMl0Url, "");
        log(`Initial available rewards for ${address} is ${initialAvailableRewards}`) 
      
        const rewards = await getSnapshotRewardForAddress(address, config.gl0Url, "", config.metagraphId);
        if (rewards.length !== 0) {
            throw new Error(`Unexpected rewards for address ${address}: got ${JSON.stringify(rewards)}`);
        }

        
        await retry(`Waiting avaialble rewards for ${address}`, { maxAttempts: 120, delayMs: 1000 })(async (logger) => {
            await getExpectedAvaialbleRewardForAddress(address, govRewardType, config.ammMl0Url, (balance) => balance === 0)
        })

        const avaiableGovReward = await getAvaialbleRewardsForAddress(address, govRewardType, config.ammMl0Url, "")
        log(`Available rewards for ${address} is ${avaiableGovReward}`) 
        const rewardWithdrawRequest = await getSignedRewardWithdraw(config, voteAllocationInfo, govRewardType, avaiableGovReward)
        await sendDataUpdate(config.ammDl1Url, rewardWithdrawRequest);
        await retry(`Waiting withdraw rewards for ${address}`, { maxAttempts: 120, delayMs: 1000 })(async (logger) => {
            await getExpectedAvaialbleRewardForAddress(address, govRewardType, config.ammMl0Url, (balance) => balance >= avaiableGovReward)
        })
        const avaiableGovRewardAfter = await getAvaialbleRewardsForAddress(address, govRewardType, config.ammMl0Url, "")
        log(`Available rewards for ${address} after withdraw is ${avaiableGovRewardAfter}`) 

        const expectedBalance = Number(initialBalance) + Number(avaiableGovReward)

        await retry(`Waiting avaialble rewards for ${address}`, { maxAttempts: 300, delayMs: 1000 })(async (logger) => {
            await getExpectedBalance(address, config.gl0Url, config.metagraphId, expectedBalance)
        })
        const reward = await getSnapshotRewardForAddress(address, config.gl0Url, "", config.metagraphId)
        if (Number(reward) !== Number(avaiableGovReward)) {
            throw new Error(`Unexpected rewards for address ${address}: got ${reward} expect ${avaiableGovReward}`);
        }

        log(`Balance for ${address} shall not change overtime now`) 
        await delay(60000)
        const finalBalance = await getSnapshotBalanceForAddress(address, config.gl0Url, "", config.metagraphId)
        if (finalBalance !== expectedBalance) {
            throw new Error(`Unexpected balance for address ${address}}`);
        }
      }

    log("All vote allocations validated successfully.", "SUCCESS");
};

export { voteAllocationTests }