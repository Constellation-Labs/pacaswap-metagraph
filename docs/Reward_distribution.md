# Reward Distribution Specification

## Table of Contents
1. [Reward Annual amount and Types](#1-reward-annual-amount-and-types)  
2. [General Reward Mechanics](#2-general-reward-mechanics)  
   - [Reward Lifecycle](#reward-lifecycle)  
   - [Liquidity Pool Participation Eligibility](#liquidity-pool-participation-eligibility)  
   - [OnChain Data](#onchain-data)  
3. [Incentive Reward Distribution per reward interval](#3-incentive-reward-distribution-per-reward-interval)  
4. [Node Validator Rewards](#4-node-validator-rewards)  
   - [Snapshot Facilitation](#snapshot-facilitation)  
5. [Vote-Based Rewards](#5-vote-based-rewards)  
   - [Governance Voting](#governance-voting)  
   - [Liquidity Pool Voting](#liquidity-pool-voting)  
   - [Validator from Approved List](#validator-from-approved-list)  
   - [Example](#example)  
6. [DAO Rewards](#6-dao-rewards)  
7. [Governance Rewards](#7-governance-rewards)  
   - [Example](#example-1)  
8. [Known Edge Cases and Inaccuracies](#8-known-edge-cases-and-inaccuracies-for-all-reward-distribution)  

---

### **1\. Reward Annual amount and Types**

There are two categories of rewards:

#### **1.1 Incentive Rewards (65,000,000 tokens annual pool)**

These rewards are distributed to:

* Node Validator Rewards (5% of total incentive rewards) – rewards for validators who facilitate snapshot creation  
* Vote-Based Rewards (75% of total incentive rewards) – rewards for participant in liquidity pools and/or validator, based on governance voting  
* DAO Rewards (20% of total incentive rewards plus rounding remainders)

#### **1.2 Governance Rewards (separate 20,000,000 tokens annual pool)**

Distributed to active voters based on their voting activity during the previous month. Address get reward for *participating*  in governance voting itself

### **2\. General Reward Mechanics**

#### When rewards are distributed

The rewards system is designed to periodically distribute tokens to various parties based on specific criteria.  Rewards are distributed at regular intervals defined by the config parameter `rewards.reward-calculation-interval`, measured in epochs. The current value of that parameter is 60, meaning rewards are distributed every hour. Therefore, an address receives 1/720 of the total monthly reward each hour, assuming the reward amount remains stable throughout the month (as is the case for Governance rewards).
#### Reward Lifecycle

At each distribution point (i.e., every `rewards.reward-calculation-interval` epochs), a fixed amount of incentive rewards and Governance Rewards are calculated and added to the "Reward Buffer" in a calculated state. There is no way to check the status of that Reward Buffer directly.

A portion of the calculated rewards is transferred to the "Available Rewards" bucket in batches during each snapshot cycle, following a FIFO (First-In, First-Out) order. Calculated rewards for the same address and type are summarized and represented as one reward. For example, if Address1 receives a Vote-Based Reward of 100 for being validator, a reward of 200 for participating in Liquidity Pool A, and a reward of 300 for participating in Liquidity Pool B, then a single Vote-Based Reward transaction with a total amount of 600 will be created. The number of rewards moved per snapshot is limited by config entry `rewards.available-rewards-per-snapshot`.  

Users could see available rewards by requesting `${ammMl0Url}/v1/rewards/${address}` endpoint. As a result, a list of available rewards by type is provided. The possible reward types are:

* **NodeValidator** – for being a validator with shares in a liquidity pool, according to the configuration

* **VoteBased** – Based on the result of governance voting

* **Dao** – for being a DAO, as defined in the configuration

* **Governance** – for participating in governance voting itself

Once in the available rewards, users must submit a `RewardWithdrawUpdate` to request a withdrawal. Withdrawals are delayed by `rewards.reward-withdraw-delay` epochs before becoming transformed to RewardTransaction which increase actual balance. 

#### **Liquidity Pool Participation Eligibility** 

*(This section is later referenced in [Node Validator Rewards](#4-node-validator-rewards) and [Vote-Based Rewards](#5-vote-based-rewards))* 

The validator must hold a minimum number of shares in a liquidity pool that matches the required token pair and falls within the specified epoch range, as defined in the configuration file. To meet this condition, it is sufficient to satisfy **any** of the entries in the liquidity pool configuration. (See below for the configuration format.)

##### Liquidity Pool configuration

Liquidity pool eligibility is configured in the parameter `rewards.node-validator-config.liquidity-pools-config`. Each configuration entry may specify:

* `start-epoch` (inclusive)  
* `end-epoch` (inclusive, optional)  
* `minimum-shares`: minimum shares required in eligible LP  
* `token-pairs`: list of eligible token pairs (may include wildcard “\*” which have meaning “any token”)

##### **Example of** Liquidity Pool configuration **Configuration**

`rewards {`  
  `node-validator-config {`  
    `liquidity-pools-config = [`  
      `{`  
        `start-epoch = 1`  
        `end-epoch = 10`  
        `minimum-shares = 100`  
        `token-pairs = [ { token-a = "*", token-b = "*" } ]`  
      `},`  
      `{`  
        `start-epoch = 9`  
        `minimum-shares = 10`  
        `token-pairs = [`  
          `{ token-a = "DAGaa99...", token-b = "DAG5kq5..." },`  
          `{ token-a = "DAG8w8K...", token-b = "DAG" }`  
        `]`  
      `}`  
    `]`  
  `}`  
`}`

**Interpretation**:

This configuration means that, during epochs 1 to 10, a validator must hold at least 100 shares in **any** liquidity pool to be eligible. However, starting from the 9th epoch, an **additional** rule is introduced: holding at least 10 shares in a liquidity pool with one of the following token pairs also qualifies the validator:

* `DAGaa99.../DAG5kq5...`

* `DAG8w8K.../DAG`

At the 9th and 10th epochs, **both** eligibility rules are active. A validator only needs to satisfy **one** of them to be considered eligible for liquidity pool participation. If required we could also use config entry with the same start and end epoch number, for example if we need to set different minimum share amount for another token pair.

#### OnChain data

OnChain data for reward distribution and governance voting result is available. Any onChain data is present only for one snapshot and will be cleared on the next snapshot. Next onChain data is available: 

##### **rewardsUpdate**

rewardsUpdate describes chunks of distributed rewards. Those rewards are not merged by type, so it is possible to figure out how much reward had been received for participating in a particular liquidity pool. It is an array of RewardDistributionChunk values.

Each RewardDistributionChunk has the following structure:

`RewardDistributionChunk(`  
  `receiver: Address,`  
  `rewardType: RewardTypeExtended,`  
  `amount: Amount`  
`)`

* **`receiver`** – the address of the recipient eligible to claim the reward.

* **`rewardType`** – the type of reward being distributed. Possible values:

  * `NodeValidator` – reward for node validator activity.

  * `VoteBasedValidator` – reward based on voting for validators.

  * `VoteBasedLiquidityPool(id: String)` – reward based on participation in a specific liquidity pool; `id` is the identifier of that pool.

  * `Dao` – reward allocated to the DAO.

  * `Governance` – reward for participating in governance.

* **`amount`** – the amount of the reward allocated to the given recipient and reward type


##### **governanceVotingResult**

Contains information about the finished voting round. Data contains next fields:  
 `class GovernanceVotingResult(`  
    `monthlyReference: MonthlyReference,`  
    `votingPowerForAddresses: Map[Address, VotingPower],`  
    `votes: Map[AllocationId, Percentage]`  
  `)`

* **`monthlyReference`** –  month when voting took place 

* **`votingPowerForAddresses`** – Voting power information per address, including information about locked tokens   
    
* **`votes`** –  actual vote information  in form (voted liquidity pool id / node validator) \-\> percentage of votes for that id

##### **processedRewardWithdrawal**

Contains information about acknowledged and processed reward withdraw requests:  
`class ProcessedRewardWithdrawUpdate(`  
`source: Address,`   
`amount: Amount,`   
`hash: Hash)`

* **`source`** –  address which initiate withdraw 

* **`amount`** –  amount of withdrawal tokens  
    
* **`hash`** –  hash of reward withdrawal operation

### **3\. Incentive Reward Distribution per reward interval**

Assuming the epoch duration is 60 seconds, the number of epochs per year is

365 \* 24 \* 60 \* 60 / 60 \= 518400 epochs/year

Then the per-epoch reward is:

65,000,000 / 518400 \= 125.385802469 tokens per epoch

With `reward-calculation-interval = 60`, then:

Reward per distribution \= 125.385802469 \* 60 \= 7523.14814814 tokens

**Incentive Reward Breakdown:**


* Node Validator: 5% of 7,523.148148148148 = 376.1574074074074 tokens
* Vote-Based: 75% of 7,523.148148148148 = 5,642.361111111111 tokens
* DAO: 20% of 7,523.148148148148 = 1,504.6296296296296 tokens

### **4\. Node Validator Rewards**

#### **Key Concepts**

* Receive 5% of incentive rewards  
* Distributed every `rewards.reward-calculation-interval` epochs  
* Rewards is distributed equally (so all eligible validators receive the same amount of reward)   
* Validators shall satisfy two conditions to get reward: they shall participate in [Snapshot facilitation](#snapshot-facilitation) and be eligible according to [Liquidity Pool Participation Eligibility](#liquidity-pool-participation-eligibility)

#### **Snapshot Facilitation** 

The current incremental snapshot, taken when rewards are distributed, must contain the validator's signature. In other words, the validator must have participated in facilitating the snapshot.

#### **Edge Case**

If a validator facilitates snapshots for all but the reward distribution epoch, they do **not** receive the reward.

### **5\. Vote-Based Rewards**

#### **Key Concepts**

* Receive 75% of incentive rewards  
* Distributed every `rewards.reward-calculation-interval` epochs  
* Reward distributed based on frozen governance votes from previous month  
* Governance vote done by `RewardAllocationVoteUpdate`   
* Governance votes are frozen at the end of the “month” (43,200 epochs, if epoch length is 60 seconds)  
* Governance voting is **exclusive** per address. New votes override previous ones.  
* Votes can target **liquidity pools** or **approved validators who are eligible according to [Liquidity Pool Participation Eligibility](#liquidity-pool-participation-eligibility)**.  
* Voting power is allocated as **weights** within each vote  
* Voters allocate all voting power at once, so it is not possible to do partial vote allocation

#### **Governance voting**

Distribution is based on governance voting — that is, rewards are distributed according to how the community decides who should receive them. Such governance voting is valid for one month only, thus address shall vote every month. Each address, using its voting power, can allocate votes to liquidity pools or approved validators to indicate who should receive rewards. It is done by submitting a `RewardAllocationVoteUpdate`. In `RewardAllocationVoteUpdate`, weights are used to define how the entire voting power is distributed. For example, the vote `Address2 -> [(LB, 3), (NodeValidators, 1)]` means that Address2 allocates 75% of its voting power to LB (calculated as 3 / (3 \+ 1)) and 25% to NodeValidators (calculated as 1 / (3 \+ 1)). An address can change its vote during the current "month" by submitting a new `RewardAllocationVoteUpdate`, which completely overrides the previous vote. Voting always allocates **all** of the address's voting power at once — partial allocation is not supported.

At the end of the “month,” all votes are **frozen**, and throughout the **next month**, eligible liquidity pools and validators will receive rewards according to those frozen votes. Note that the term “month” does not refer to a calendar month, but to a fixed number of epochs representing roughly 30 days. For standard configurations, where one epoch is expected to last 60 seconds, a month is considered to be 60 × 24 × 30 \= **43,200** epochs. Thus, vote freezing occurs every 43,200 epochs.

Addresses may vote for two types of entities: **liquidity pools** and **validators** from the approved validator list.

#### **Liquidity pool voting**

User could just use existing Liquidity pool id as string and put it to the `RewardAllocationVoteUpdate`

#### **Validator from approved list**

Users can vote only for all validators at once by using special id string “`NodeValidators”`. Rewards will be distributed across all validators from approved list which are eligible according to [Liquidity Pool Participation Eligibility](#liquidity-pool-participation-eligibility). This list is configured using the `--seedlist` command-line parameter when running the metagraph, in the same way it is configured for Tessellation.

#### **Example**

##### Initial state

Let’s assume we have the following addresses with their associated voting powers:

* **Address1** → 3000  
* **Address2** → 4000  
* **Address3** → 6000  
* **Address4** → 10,000

We also have:

* Approved validators:

  * **VA** → Address5  
  * **VB** → Address6

* Liquidity pools: **LA** and **LB**

**Liquidity Pool LA:**

* Address1 → 200 shares  
* Address3 → 1200 shares  
* Address5 → 600 shares

**Liquidity Pool LB:**

* Address5 → 2000 shares  
* Address2 → 3000 shares  
* Address4 → 5000 shares

Assume that submitted Governance votes During the “Month” are:

* Address4 → `[(LB, 200), (NodeValidators, 100)]`  
* Address2 → `[(LB, 3), (NodeValidators, 1)]`  
* Address3 → `[(LA, 1), (NodeValidators, 2)]`  
* Address4 → `[(LA, 200), (NodeValidators, 100)]`

##### Build allocation map

Step 1: Calculate effective Votes 

* Address2 → `[(LB, 3), (NodeValidators, 1)]`  
* Address3 → `[(LA, 1), (NodeValidators, 2)]`  
* Address4 → `[(LA, 200), (NodeValidators, 100)]`

The first vote from Address4 is ignored because a second vote was submitted within the same “month.” Therefore only the latest vote is counted.

Step 2: Calculate Total Voting Power

We only consider addresses that submitted valid votes:

* Address2: 4000  
* Address3: 6000  
* Address4: 10,000

Total voted power \= 4000 \+ 6000 \+ 10,000 \= 20,000

Step 3: Relative Voting Power per Address

* Address2 → 4000 / 20,000 \= **20%**  
* Address3 → 6000 / 20,000 \= **30%**  
* Address4 → 10,000 / 20,000 \= **50%**

Step 4: Calculate Allocation Based on Vote Weights

Address2 → `[(LB, 3), (NodeValidators, 1)]`

* Total weight \= 3 \+ 1 \= 4  
* LB: 3 / 4 \= **75%** of 20% \= **15%** absolute  
* NodeValidators: 1 / 4 \= **25%** of 20% \= **5%** absolute

Address3 → `[(LA, 1), (NodeValidators, 2)]`

* Total weight \= 1 \+ 2 \= 3  
* LA: 1 / 3 ≈ **33.33%** of 30% ≈  **9.9999%** absolute  
* NodeValidators: 2 / 3 ≈ **66.67%** of 30% ≈ **19.99999%** absolute

Address4 → `[(LA, 200), (NodeValidators, 100)]`

* Total weight \= 200 \+ 100 \= 300  
* LA: 200 / 300 ≈ **66.67%** of 50% ≈ **33.33%** absolute  
* NodeValidators: 100 / 300 ≈ **33.33%** of 50% ≈ **16.67%** absolute

Step 5: Final Calculated Distributions

* **Address2** → LB: 15%, NodeValidators: 5%  
* **Address3** → LA: 9.999%, NodeValidators: 19.999%  
* **Address4** → LA: 33.33%, NodeValidators: 16.67%

Step 6: Aggregate Reward Distribution

* **LB** → **15%**  
* **NodeValidators**→ 5% (from Address2) \+ 16.67% (from Address4) \+ 19.999% (from Address3)  \= **41.666%**  
* **LA** →  9.999% (from Address3) \+ 33.33% (from Address4) \= **43.33%**


Thus, at every `rewards.reward-calculation-interval`, we distribute Vote-Based Rewards using the following allocation map:

* LB → 15%  
* LA → 43.3333%  
* NodeValidators → 41.666%

##### Calculate reward distribution

For liquidity pools, rewards are distributed proportionally to the shareholders based on their shares in the pool. For example, if an address holds half of the total shares in a liquidity pool, that address will receive half of the rewards allocated to that pool. Because the share distribution can change during the “month” (due to new participants or changes in shares), the liquidity pool reward distribution is dynamic and reflects the current share proportions at the time of distribution.

In the case of validators, reward equally distributes across all validators. The entire validator portion of the reward is assigned directly to the validator’s address.

All Vote-Based Rewards calculated for the same address—whether from validators or multiple liquidity pools—are summed up and represented as a single Vote-Based Reward transaction. For instance, if `address1` receives a Vote-Based Reward of 100, plus 200 for participating in liquidity pool A, and 300 for liquidity pool B, a single Vote-Based Reward transaction of 600 will be created.

For example, let’s assume we have allocation map from previous example, and we have the liquidity pools with the following shares:

**Liquidity Pool LA:**

* Address1 → 200 shares  
* Address3 → 1200 shares  
* Address5 → 600 shares

**Liquidity Pool LB:**

* Address5 → 2000 shares  
* Address2 → 3000 shares  
* Address4 → 5000 shares

Then:

Step 1: Calculate allocated tokens  
 The reward per `rewards.reward-calculation-interval` is:  
65,000,000 divided by 518,400 (number of epochs in a year), multiplied by 0.75 (75% of rewards go to vote-based distribution), multiplied by 60 (reward interval) = **5,642.361111111111** tokens to distribute.

Step2: Calculate distribution reward 

The rewards allocation map from the previous example is:

* LB → 15%  
* LA → 43.3333%  
* NodeValidators → 41.666%; because we have 2 validators in total every validator receive 20,833% (41.666% / 2\)

Thus, the rewards are:

1. Approved validator VA (Address5): 5,642.361111111111 × 20.833% = **1,175.4910648145833**
2. Approved validator VB (Address6): 5,642.361111111111 × 20.833% = **1,175.4910648145833**
3. Shareholders for LA:

   5,642.361111111111 × 43.3333% = **2,445.023148148148**

   Shareholders receive rewards proportional to their shares. Total shares for LA: 200 + 1,200 + 600 = 2,000       
    
Proportions:  
* Address1 → 200 / 2,000 \= 10%  
* Address3 → 1,200 / 2,000 \= 60%  
* Address5 → 600 / 2,000 \= 30%  

  Rewards:

* Address1 → 244.5023148148148

* Address3 → 1,467.013888888889

* Address5 → 733.507 (rounded)



4. Shareholders for LB:

   5,642.361111111111 × 15% = **846.3541666666666**

   Shareholders receive rewards proportional to their shares. Total shares for LB: 2,000 + 3,000 + 5,000 = 10,000

Proportions:
* Address5 → 2,000 / 10,000 = 20%
* Address2 → 3,000 / 10,000 = 30%
* Address4 → 5,000 / 10,000 = 50%



Rewards:

* Address5 → 169.2708333333333
* Address2 → 253.90625
* Address4 → 423.1770833333333



Step3: Aggregate distribution reward
So, the final Vote-Based Reward distribution is (assume that validators have addresses VA (Address5) and VB (Address6)):

* Address1 → 244.5023148148148 (Liquidity Pool LA reward)
* Address2 → 253.90625 (Liquidity Pool LB reward)
* Address3 → 1,467.013888888889 (Liquidity Pool LA reward)
* Address4 → 423.1770833333333 (Liquidity Pool LB reward)
* Address5 → 1,175.4910648145833 (Validator VA reward) + 733.507 (Liquidity Pool LA reward) + 169.2708333333333 (Liquidity Pool LB reward) = **2,078.2688981489166**
* Address6 → 1,175.4910648145833 (Validator VB reward)

The total Vote-Based Rewards sum up as expected to approximately **5,642.361111111111** tokens.

*Note: The token amount precision is up to 8 decimal places; smaller fractions are rounded down.*

### 6\. DAO Rewards

#### **Key Concepts**

* Receive 20% of incentive rewards \+ any remainder from rounding operations from other incentive rewards distribution  
* Distributed every `rewards.reward-calculation-interval` epochs  
* Recipient Address: Configurable under `rewards.dao-address`

### 7\. Governance Rewards

#### **Key Concepts**

* 20,000,000 DEX tokens are allocated annually for Governance Rewards  
* Distributed every `rewards.reward-calculation-interval` epochs.   
* Addresses receive rewards for participating in governance voting. The content or outcome of the vote does not affect the reward amount — simply participating in the voting process is sufficient to earn a governance reward.  
* Rewards are based on frozen voting allocations (which are used for [Vote-Based Reward distribution](#5-vote-based-rewards)) as of the end of the previous “month” (43,200 epochs, if epoch length is 60 seconds) and distributed proportionally to the voting power of addresses  
* A user must vote by `RewardAllocationVoteUpdate` every month to continue receiving Governance Rewards


#### Example

Step 1: Calculating voting power shares:  
Available voting powers:  
`Address1: 10,000 votes`  
`Address2: 3,000 votes`  
`Address3: 7,000 votes`

Only Address2 and Address3 voted in last month → Total active votes \= 10,000

* Address2 share \= 30%  
* Address3 share \= 70%

Step 2: Calculating Governance Rewards distribution per address:

3858 tokens will be distributed among addresses that had votes frozen in the last month, proportionally. Use voting power shares: Address2 share \= 30%; Address3 share \= 70% 

Rewards will be calculated by using formula::

`(governancePool / epochProgress1Year) * voting power share * rewards.reward-calculation-interval`

So using that formula give:

Address2 –\>  `(20,000,000 / 518,400) * 0.3 = 11.57407407407407 = 11.57407407 (token amount is rounded down after 8th digit) * 60 = 694.4444442 tokens distributed`

Address3 –\> `(20,000,000 / 518,400) * 0.7 = 38.58024691358025 = 38.58024691 (token amount is rounded down after 8th digit) * 60 = 2314.8148146 tokens distributed`

When splitting Governance Rewards, rounding may leave a small leftover amount (the *remainder*). Instead of discarding it:

* The system selects one of the top voters (those with the highest voting power) **at random**, using a deterministic seed.

* This selected "lucky" voter receives the leftover amount in addition to their normal share.

### **8\. Known Edge Cases and Inaccuracies for all reward distribution**

All known issues described below may arise only if the configured value of reward-calculation-interval (currently set to 60) is modified. With the current setting, which is aligned with both monthly and yearly durations, no issues are expected. 

#### **Never Reaching the "Final Year Epoch" in Governance distribution**

Because distributions only occur every `rewards.reward-calculation-interval` epochs, we might never land exactly on `epochProgress1Year` — the final epoch in the year. This could leave some small reward governance portion undistributed.

#### **Misalignment Between Epoch Interval and Months**

Because we distribute rewards every `rewards.reward-calculation-interval` and if that value is not aligned with the “month” duration (which is 43200 epochs), then we could calculate rewards a bit incorrectly. For example distributed Governance Rewards in next example is distributed a bit incorrectly

Example:

* Assume `rewards.reward-calculation-interval = 10`, and a month has **15 epochs** in that example (not 43200 as it shall be in normal case, we use 15 to shows inconsistency)

* Voting for Address A:

  * January (Month 1): Entitled to **150 tokens**  
  * February (Month 2): Entitled to **300 tokens**

Actual distribution:

`10th epoch: based on Month 1: (150 / 15) * 10 = 100 tokens`  
`20th epoch: based on Month 2: (300 / 15) * 10 = 200 tokens`  
`30th epoch: still based on Month 2: (300 / 15) * 10 = 200 tokens`

**Total given to Address A:** 500 tokens  
 **Expected**: 150 (Month 1\) \+ 300 (Month 2\) \= 450 tokens  
 **Over-distribution** of 50 tokens due to overlapping month boundary and fixed epoch interval.

Similar issue could be present for vote based rewards as well: we distribute for previous epochs by using current configuration without respecting “months” boundaries. In case of big “month” length and small `rewards.reward-calculation-interval` that error is small but present.