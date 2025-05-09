import { dag4 } from "@stardust-collective/dag4";
import axios from "axios";
import { delay, getPublicKey, log, retry, getSnapshotRewardForAddress, getSnapshotBalanceForAddress, getGlobalSnapshotCombined, getBalanceForAddress, BaseAmmMetagraphCliArgsSchema, serializeBase64, sendDataUpdate } from "../../shared";
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
    logger: (message: string, type?: string, context?: string) => void = log
) => {
    const { ammMl0Url } = config;

        const { data: lastRewards } = await axios.get(`${ammMl0Url}/v1/governance/allocations/rewards`);

        const someRewardFilled = lastRewards.data.find(reward => {
            return Object.keys(reward.rewardsInfo).length > 0
        })

        logger(`Check reward`);
        if (someRewardFilled) {
            const nodeValidatorsReward = someRewardFilled.rewardsInfo.NodeValidators
            if (nodeValidatorsReward === 25000) {
                logger(`Rewards filled correctly`);
                return
            }
            throw new Error(`NodeValidatorsReward invalid weight: ${nodeValidatorsReward}`);
        }

        throw new Error(`Rewards not filled yet`);
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
    }

    await retry(`Validate allocations rewards for`, { delayMs: 5000, maxAttempts: 50 })(async (logger) => {
        await validateAllocationsRewards(config, logger)
    })

    for (const voteAllocationInfo of voteAllocationsInfo) {
        const { address } = voteAllocationInfo;
      
        let previousBalance = 0;
      
        for (let i = 0; i < 10; i++) {
          const balance = await getSnapshotBalanceForAddress(address, config.gl0Url, "", config.metagraphId);
      
          if (balance < previousBalance) {
            throw new Error(`Balance decreased for address ${address}: ${balance} < ${previousBalance}`);
          }
      
          previousBalance = balance;
      
          const rewards = await getSnapshotRewardForAddress(address, config.gl0Url, "", config.metagraphId);
      
          const expectedRewards = [];
      
          if (rewards.length !== 0) {
            throw new Error(`Unexpected rewards for address ${address}: got ${JSON.stringify(rewards)}`);
          }
      
          await delay(10000);
        }
      }

    log("All vote allocations validated successfully.", "SUCCESS");
};

export { voteAllocationTests }