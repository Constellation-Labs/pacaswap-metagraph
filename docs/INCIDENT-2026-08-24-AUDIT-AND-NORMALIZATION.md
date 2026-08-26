# PacaSwap Incident — Audit and Normalization Plan

**Status:** ready for execution · **Audit date:** 2026-08-25 · **Incident date:** 2026-08-24

Every figure in this document was derived from mainnet global snapshots and the block explorer during this audit, not carried over from earlier notes. Reproduction commands are in Appendix D.

---

## 1. What happened, in one page

At currency snapshot **731261** (global **6814499**), four fee transactions of exactly **2^62** each — `4,611,686,018,427,387,904` — were issued from `DAG5Yno9tMKHLe1G6J5QSbiqRicWV2HRKunDtFuR`, an address holding **zero** PACA. All four were accepted. **184,467,440,737.10 PACA** was created out of nothing.

The bug was in Tessellation, not in PacaSwap. `acceptFeeTxs` unwrapped the balance map from `Balance` to raw `Long`, applied every fee transaction in a fold, and checked the result only once at the end via `NonNegLong.from`. Both the debit and the credit wrapped silently.

```
start   0
tx 1   -4,611,686,018,427,387,904
tx 2   -9,223,372,036,854,775,808   ← Long.MinValue
tx 3   +4,611,686,018,427,387,904   ← wrapped positive
tx 4    0                            ← back to start
```

`4 × 2^62 = 2^64` exactly, so the debits cancel to zero. The final state was `{source: 0, four destinations: 2^62 each}` — every entry a valid `NonNegLong`. Nothing rejected it, and the source wallet looks clean to this day.

**A corrected end-state guard would not have caught this.** The end state is entirely valid. The check has to happen per transaction. Fixed in `tessellation#1571` / `#1572`, shipped in **v3.5.26**.

The mint was then sold into the PACA/DAG pool. The price collapsed from **4.2133** to **22,282** PACA/DAG — a factor of **5,289** — and **11,946,797.68 DAG** was drained from the pool.

Three DAG figures in this document are distinct and must not be interchanged.

| Figure | Value | Measured from |
|---|---|---|
| Attacker's net DAG proceeds | 12,122,329.77 | the paired swap ledger |
| **PACA/DAG pool's net DAG loss** | **11,946,797.68** | pre-attack reserve minus the reserve now |
| Net DAG that left the metagraph *address* | 11,798,857.58 | the address balance |

Only the middle figure describes the pool, and only it drives the injection in §6.

The pool lost less than the attacker took because non-attacker buyers were paying DAG *into* the pool across the same window — 179,113.93 by the swap ledger. That does not close the 175,532.09 gap between the first two rows to the unit; the remaining 3,581.84 is the swap ledger and the reserve snapshot disagreeing at the edges of the window, and it is carried here as a residual rather than attributed. The address figure is lower again because the address backs all four pools and the other three were net receivers of 147,940.10 over the window.

---

## 2. Timeline

| Event | Currency ordinal | Global ordinal | Time (UTC) |
|---|---|---|---|
| Mint — 4 fee transactions | 731261 | 6814499 | 15:56:24 |
| First legitimate purchase | — | 6814844 | ~16:35 |
| Attacker extraction begins | — | 6814575 | ~16:05 |
| Last legitimate purchase | — | 6815462 | ~19:44 |
| Metagraph stopped | 731646 | ~6815495 | ~19:50 |

Window: **385 currency snapshots**. Of those, **60 global ordinals carried metagraph spend activity**, holding **132 spend-transaction legs**. Legs pair into swaps — one leg is what the user paid, the other what the pool returned — giving **62 paired swaps**, of which **44 are against the PACA/DAG pool**. The remaining **8 legs** do not pair cleanly, because several ordinals carry more than one action; they are listed in `docs/data/swaps.json` marked `note`, and none of them affects a deduction.

Every figure in this document is regenerated and checked by `scripts/verify_remediation.py`, which reads the raw captures in `docs/data/`. It exits non-zero if any published number fails to reproduce.

---

## 3. Affected wallets

### 3.1 Attacker-controlled — 5 addresses, no entitlement

| Address | PACA held now | Role |
|---|---|---|
| `DAG1kEmLAgnCVBURHrL4AMsfn9TZdk4QCYQ8tUu3` | 46,116,860,184.27 | mint recipient, **untouched at exactly 2^62** |
| `DAG7ZjENTP4T36PPSp3skJdTHtQbcuLfpEaAFWdn` | 46,116,860,184.27 | mint recipient, **untouched at exactly 2^62** |
| `DAG4w5mUqNNxQNS4hgdpx3E8FGgiu2UCRsJxHwhX` | 45,730,238,413.57 | mint recipient, partly drained |
| `DAG8uqhyGtFABWSS5KeVB2ia1R4vXop5AeijXeoU` | 41,116,860,184.27 | mint recipient, partly drained |
| `DAG5Yno9tMKHLe1G6J5QSbiqRicWV2HRKunDtFuR` | 0.00 | signed all four fee transactions; balance wrapped to zero |

Everything these five hold is phantom. They are zeroed in full.

The four mint transaction hashes are in Appendix C.

### 3.2 Innocent buyers — 10 addresses, entitled to keep fair value

**These people paid real DAG.** They bought PACA during the window at whatever price the corrupted pool was quoting. They are victims of the attack, not participants in it.

Together they paid **175,181.66 DAG** and received **1,377,628,385.56 PACA**.

| Address | DAG paid, net | PACA received, net of anything sold back | **Keeps** | Removed | Received ÷ fair |
|---|---|---|---|---|---|
| `DAG6zZakMJrrf25FSvPZAi8QA9wVDdmvFkPvTbKu` | 90,000.00 | 680,159,799.76 | **371,605.05** | 679,788,194.70 | 1,830x |
| `DAG3sGFqKZ974eoCQeZN3jyhsVakPaEZ9usvvCw7` | 20,000.00 | 263,319,195.36 | **81,867.01** | 263,237,328.35 | 3,216x |
| `DAG7iLJFTAF1sESqM95TJ3W41ibFN29kMYEBiPzb` | 13,000.00 | 159,895,324.85 | **54,210.06** | 159,841,114.79 | 2,950x |
| `DAG6pvRsWjTzmPSGgNZUu6MGgsQDNQHdnzNGNhzf` | 8,777.54 | 146,947,445.94 | **36,800.43** | 146,910,645.52 | 3,993x |
| `DAG4jWvjPdpvUqbpUXUobcQG6Js7XGfzZzhvxFmS` | 13,003.46 | 92,723,024.35 | **53,577.31** | 92,669,447.04 | 1,731x |
| `DAG8Eyr6SGvLorNU4rQspeUXZLZi3wt84CwbV1Ep` | 7,000.00 | 20,000,000.00 | **29,387.55** | 19,970,612.46 | 681x |
| `DAG4kfRPpcPSh4cMn8ZgdMuTEfdu3yz4veZFrv3L` | 2,000.00 | 9,807,129.05 | **8,302.51** | 9,798,826.54 | 1,181x |
| `DAG5434oVLFRRTqVSsTv4Y1qvyoMBkb4Tey21YuZ` | 11,350.66 | 2,970,570.79 | **46,450.70** | 2,924,120.09 | 64x |
| `DAG7uHRz6stwzsEnSHB2w1VxVHsCq7PDuDhTbjNP` | 10,000.00 | 1,039,590.35 | **41,853.89** | 997,736.46 | 25x |
| `DAG1DD2bM1hpFyWwa8UNgh3wMLGAe5JDSwpoUS9M` | 50.00 | 766,305.10 | **204.77** | 766,100.32 | 3,742x |

**`DAG3sGF…` is the case Duc raised.** Two swaps of 10,000 DAG at global ordinals 6815350 and 6815439. The community report of "20k DAG, 260 million SWAP" matches exactly.

### 3.3 Downstream transferees — 2 addresses, no entitlement

Received phantom PACA by transfer, paid nothing for it.

| Address | PACA | Note |
|---|---|---|
| `DAG4fVZch1qTY2ccA5eHkxe2RMTFsnNDU6Zu6mUU` | 11,000,000.00 | received by transfer, paid nothing |
| `DAG0xNaGuUfhorbRKqjsaDt6BP8eqWW3ZMRo1nRp` | 5,435,084.90 | received by transfer, paid nothing |

### 3.4 Traded but hold nothing — 4 addresses, no action

Four addresses traded during the window but hold **zero PACA** today — they bought and sold back, or forwarded everything on. They are in no deduction list because there is nothing to deduct.

| Address | Net DAG | PACA balance now |
|---|---|---|
| `DAG3r7cxaymfB7W2xrcKSgnf8UJvVymGnUrN6HEs` | +1,722.76 | 0.00 |
| `DAG6nuE7gfyDe8KMe45CBeJkwGsDpkxHehQVemPa` | +1,495.16 | 0.00 |
| `DAG2zpTAnvSZyhDUtdjXVYPn4oZwEWRFVuRGoyp7` | −310.00 | 0.00 (forwarded to `DAG0xNaGu…`, which is in the plan) |
| `DAG16dGe2UzXPoHoQxBDm3qa4jRNiT4AgToPwTMU` | −6,840.20 | 0.00 (sold back at ordinal 6815495) |

### 3.5 The pool itself

The PACA/DAG pool entry in the calculated state at currency ordinal 731646 holds **3,603,483,140.82469011 PACA** against a pre-attack reserve of 51,120,803.29, and **186,466.23956291 DAG** against a pre-attack 12,133,263.92. The phantom the attacker sold sits on the PACA side; the DAG side is what he carried out.

Both figures are read from the pool, not from `DAG7X5idd4aLfp4XC6WQdG1eDfR3LGPVEwtUUB2W`. That address is shared by all four pools, so its balance does not describe this one — the distinction is what §6 turns on.

The address balance is a separate, also-needed number: it held **3,603,518,762.19858122 PACA** when the data was captured, 35,621.37 above the pool's reserve. §5 deducts against the *balance*, not the reserve — the pool row there removes 3,553,123,518.84858115, leaving 50,395,243.34506729 on the address.

The deduction leaves **50,395,243.35000000 PACA** on the address, and §6 writes exactly that as the pool's PACA reserve — **1:1, to the unit, nothing stranded**.

The rounding is absorbed by the reserve rather than by the deduction, and that direction is forced. Tessellation **v3.5.28 is already released** with this deduction in `adjustments.json`, and `validateRequiredAdjustments` compares `Amount`s exactly — a metagraph emitting any other value stalls at this ordinal with `"not authorized"`. The deduction is therefore frozen, and the reserve is fitted to it. The cost is that the written reserve sits 0.00493271 PACA above the raw replay figure of 50,395,243.34506729. The replay is a counterfactual estimate, not a measured quantity, and half a cent of PACA is far inside its own precision — while a mismatch against a shipped release is not.

---

## 4. The other three pools are clean — do not touch them

PacaSwap runs four pools, all **TOKEN/DAG**, sharing one metagraph address. That makes the flows look tangled at first. Every swap in all four during the window was checked:

| Pool | Swaps | Price range in window | Verdict |
|---|---|---|---|
| PACA / DAG | 44 | 4.31 → 22,282 PACA/DAG | **corrupted, 5,170×** |
| `DAG0S16WD…` / DAG | 8 | 0.0075 → 0.0081 | stable |
| `DAG7Ghth1…` / DAG | 6 | 28.35 → 29.13 | stable |
| `DAG0CyySf…` / DAG | 4 | 6.46 → 6.51 | stable |

Phantom PACA never entered the other three — they are TOKEN/DAG pairs, so PACA cannot be deposited into them. Attackers who converted phantom to DAG and then bought other tokens paid those pools **full market price**. Those pools received real DAG for real tokens.

**No remediation is required for the other three pools, and none should be applied.** Applying one would take value from uninvolved users.

---

## 5. What changed from the original remediation plan

The original plan computed each holder's deduction as **the full net PACA received from the pool**, protecting only pre-attack holdings. It gave **no credit for DAG paid during the window**.

Under that plan, `DAG3sGF…` loses all 263,319,195.36 PACA *and* the 20,000 DAG they paid. Same for the other nine.

The corrected plan replays the **26 legitimate purchases made by these 10 addresses** against the **pre-attack constant-product curve** — 51,120,803.29 PACA / 12,133,263.92 DAG, 0.3% fee — in ordinal order, with all attacker trades excluded. Each buyer keeps exactly what that replay gives them.

| | PACA removed |
|---|---|
| Original plan (nominal) | 189,413,866,545.09 |
| **Corrected plan (nominal)** | **189,413,903,467.12** |
| Actual removed after saturation | **184,027,281,696.42** |
| Difference | **+36,922.04** |

**Per-address comparison**

| Address | Kind | Original plan | Entitlement | **Corrected** |
|---|---|---|---|---|
| `DAG1kEmLAgnCVB…` | Attacker | 46,116,860,184.27 | 0.00 | **46,116,860,184.27** |
| `DAG7ZjENTP4T36…` | Attacker | 46,116,860,184.27 | 0.00 | **46,116,860,184.27** |
| `DAG4w5mUqNNxQN…` | Attacker | 46,116,860,184.27 | 0.00 | **45,730,238,413.57** |
| `DAG8uqhyGtFABW…` | Attacker | 46,116,860,184.27 | 0.00 | **41,116,860,184.27** |
| `DAG7X5idd4aLfp…` | Pool | 3,552,362,337.53 | 0.00 | **3,553,123,518.85** |
| `DAG6zZakMJrrf2…` | Buyer | 680,159,799.76 | 371,605.05 | **679,788,194.70** |
| `DAG3sGFqKZ974e…` | Buyer | 263,319,195.36 | 81,867.01 | **263,237,328.35** |
| `DAG7iLJFTAF1sE…` | Buyer | 159,895,324.85 | 54,210.06 | **159,841,114.79** |
| `DAG6pvRsWjTzmP…` | Buyer | 146,947,445.94 | 36,800.43 | **146,910,645.52** |
| `DAG4jWvjPdpvUq…` | Buyer | 92,723,024.35 | 53,577.31 | **92,669,447.04** |
| `DAG8Eyr6SGvLor…` | Buyer | 20,000,000.00 | 29,387.55 | **19,970,612.46** |
| `DAG4fVZch1qTY2…` | Downstream | 11,000,000.00 | 0.00 | **11,000,000.00** |
| `DAG4kfRPpcPSh4…` | Buyer | 9,807,129.05 | 8,302.51 | **9,798,826.54** |
| `DAG0xNaGuUfhor…` | Downstream | 5,435,084.90 | 0.00 | **5,435,084.90** |
| `DAG5434oVLFRRT…` | Buyer | 2,970,570.79 | 46,450.70 | **2,924,120.09** |
| `DAG7uHRz6stwzs…` | Buyer | 1,039,590.35 | 41,853.89 | **997,736.46** |
| `DAG1DD2bM1hpFy…` | Buyer | 766,305.10 | 204.77 | **766,100.32** |

The corrected total is **higher**, not lower. That is the reconciliation working:

```
  +761,181.31   more removed from the pool reserve
  −724,259.28   retained by the 10 addresses that still hold PACA
  ─────────────
   +36,922.04   trading fees the pool legitimately earned on those trades
```

Supply balances to the unit. Nothing is being given away.

---

## 6. Pool normalization

| | PACA | DAG | Price (PACA/DAG) |
|---|---|---|---|
| Pre-attack | 51,120,803.29 | 12,133,263.92 | 4.2133 |
| Now | 3,603,483,140.82 | 186,466.24 | 19,325 |
| Original plan target | 51,120,803.29 | 186,466.24 | **274.1** ❌ |
| **Corrected target** | **50,395,243.35** | **12,308,553.85** | **4.0943** ✅ |

The original target restores PACA but not the drained DAG, leaving the pool at **274 PACA/DAG — still 65× below pre-attack**. That is not a healthy pool; the first arbitrageur to touch it takes the difference.

The corrected target is the **counterfactual**: where the pool would be had the attack never happened but those 26 purchases had. It is slightly below pre-attack PACA (buyers took some out) and slightly above pre-attack DAG (buyers paid some in) — which is exactly right.

**DAG injection required: 12,122,087.60812058** (12,308,553.84768349 minus the 186,466.23956291 still there). The 12M treasury injection does **not** cover this — it falls short by 122,087.61 DAG.

The `Now` row above is read from the pool's calculated state at currency ordinal 731646 / global 6815497: `tokenB.amount = 18646623956291`. An earlier revision of this document put it at 334,406.34, computed as the pre-attack reserve minus the net DAG that left the *address*. That conflated all four pools and understated the injection by 147,940.10 DAG.

`k` must be recomputed as `5039524335000000 × 1230855384768349 = 6202925664405883123272915000000`. `SwapCalculations` prices off `k` directly rather than deriving it from reserves, so a stale `k` keeps quoting the attack price no matter what the reserves say.

---

## 7. Normalization walkthrough — REMOVED, do not reconstruct from memory

The step-by-step that stood here has been removed because it was wrong, and a wrong runbook is
worse than none: it stopped at ordinal 731647 and stated that three places move together and
"there is no fourth". That is no longer true.

**The deployment now touches five hardcoded ordinals, not three.** Two of them did not exist when
that walkthrough was written:

| Ordinal | What fires | Emits balance artifacts |
|---|---|---|
| 731647 | The 17 deductions and `updated-pools-13.json` — the remediation in §6 | Yes |
| **731648** | `updated-pools-14.json` — sets `totalShares` to the sum of `addressShares` in all four pools, so the LPs stop collectively claiming more than 100% of a pool | No |
| **731649** | A `SpendAction` sweeping 71,706,224.58112005 UP out of the custody address, which has no private key and therefore cannot be swept any other way | Yes |

All three are fail-closed: a resource problem halts the snapshot rather than skipping the state
change it carries.

**The fragility worth knowing before restart.** Those ordinals are absolute literals. The
metagraph stopped at 731646, so the first snapshot it produces on restart is 731647 and they line
up. If that assumption breaks — a rollback, an unexpected snapshot, anything that makes the first
new ordinal something other than 731647 — then 731648 and 731649 are already in the past and
**will never fire**. No halt, no error: the normalization and the sweep simply do not happen, and
the only symptom is the inconsistencies in §6 and the share ledger persisting.

The operational procedure is maintained outside this document. What remains here is the incident
record: what happened, who was affected, why each figure is what it is, and how to reproduce it.
That part is still accurate and is the reason to keep the file.

## 8. On locking the untouched wallets

Two wallets still hold exactly 2^62 each, untouched: `DAG7ZjENTP4T36…` and `DAG1kEmLAgnCVB…`.

**Locking them is weaker than the deduction and unnecessary if L1 stays down through the remediation snapshot** — the deduction at 731647 zeroes them outright, which is strictly stronger than a lock.

Locking becomes **essential** only if the metagraph must restart *before* the remediation ordinal fires. In that case those wallets are live and spendable, and a lock is the only thing standing between them and the pool.

Decision rule: **if L1 stays down through ordinal 731647, do not bother locking. If L1 must come up first, locking is mandatory.**

---

## 9. Supply reconciliation — the proof this is correct

The strongest check available: does the phantom fully account for itself?

```
minted  (4 × 2^62)                    184,467,440,737.10 PACA
removed by this remediation           184,027,281,696.42 PACA
still held in token locks                 440,195,962.71 PACA
                                      ──────────────────
removed + locked                      184,467,477,659.13 PACA
minus minted                                  +36,922.04 PACA
```

The residual is **+36,922.04 PACA**. It is not zero and is not claimed to be: it is the trading fee
the pool retained on the 26 legitimate purchases, which stays with the pool as real supply rather
than being deducted. Every unit of the mint is accounted for, and the only excess is fee income the
pool genuinely earned.

**Phantom PACA in circulation after this applies: zero.** Everything not removed is locked.

## 10. Token locks — scheduled, not outstanding

**440,195,962.71 PACA** of phantom sits in **7 active token locks across 6 addresses**. A
`BalanceAdjustment` only touches the balances map and cannot reach locked value, so it cannot be
taken now.

| Address | Locked PACA | Unlocks in |
|---|---|---|
| `DAG7uHRz6stwzsEnSHB2w1VxVHsCq7PDuDhTbjNP` | 150,000,000.00 | ~179 days |
| `DAG6zZakMJrrf25FSvPZAi8QA9wVDdmvFkPvTbKu` | 200,000,000.00 | ~719 days |
| `DAG5434oVLFRRTqVSsTv4Y1qvyoMBkb4Tey21YuZ` | 60,000,000.00 | ~719 days |
| `DAG4fVZch1qTY2ccA5eHkxe2RMTFsnNDU6Zu6mUU` | 10,000,000.00 | ~719 days |
| `DAG4kfRPpcPSh4cMn8ZgdMuTEfdu3yz4veZFrv3L` | 9,808,105.56 | ~719 days |
| `DAG8Eyr6SGvLorNU4rQspeUXZLZi3wt84CwbV1Ep` | 9,000,000.00 + 1,387,857.15 | ~719 days |

**None of this affects the restart.** The earliest release is roughly six months away and the pool is
healthy without it.

This used to be unschedulable. `convertToAdjustmentEntries` ended in `.toMap` keyed by `currencyId`,
so a currency held exactly one live block and the newest silently retired the rest — meaning a second
block could not be added at all. **That is fixed in `#1575` / `#1576`:** blocks are grouped per
currency and the acceptance path selects the one matching the ordinal being produced. All four
Pacaswap blocks (109991, 145000, 472325, 731647) are now live simultaneously, which also closes the
silent replay divergence on the three older ordinals.

**Procedure when the locks release**, roughly 6 and 24 months out:

1. Confirm the released balances on-chain for the addresses above.
2. Append a new block to `adjustments.json` and a matching resource on the metagraph side, at an
   ordinal shortly after the release.
3. Merge both, deploy tessellation to GL0 first, then the metagraph.

The mechanism is in place and tested; only the ordinal and the amounts need filling in at the time.

## 11. What remains outside this plan

**The extracted DAG — 12,122,329.77157270.** Moved to exchange wallets on the global layer. Nothing in this plan reaches it; recovery is an exchange-cooperation matter. Addresses for that conversation:

```
DAG6cgAhAYiYgyFbB8QokzQ816trwRHPpw28Datj     5,380,000.00 DAG
DAG7154cniS1zgt4qmF64mjMYDnbk95UwssNacN4     1,000,000.00 DAG
DAG56ixyxAxTk6oPRexm5nwbUsBWTCfuPGAsMc6Q     1,008,000.00 DAG
DAG8dDPF5RJwMhWHWkxcXhLy8KzkaM8VYVu66JZ2       261,000.00 DAG
DAG733wEioNzKWMrrCBvMrAPsrgGJGun6P8obbD5       174,300.00 DAG
DAG5Yno9tMKHLe1G6J5QSbiqRicWV2HRKunDtFuR     4,298,982.11 DAG   ← also the fee-tx signer
```

`DAG5Yno9…` is the strongest pivot for attribution: it signed the four fee transactions, seeded the drainer wallets with their initial 10 DAG, and received 4.3M DAG back at the end.

That is the only item left outside this plan, and it is a recovery matter rather than a technical one.

---

## Appendix A — Per-buyer transaction evidence

Every purchase, from global-snapshot `spendActions`.

| Buyer | Global ordinal | Paid | Received | Rate PACA/DAG |
|---|---|---|---|---|
| `DAG6zZakMJrrf2…` | 6815254 | 10,000.00 DAG | 111,531,115.76 PACA | 11,153.1 |
| `DAG6zZakMJrrf2…` | 6815270 | 10,000.00 DAG | 102,841,651.67 PACA | 10,284.2 |
| `DAG6zZakMJrrf2…` | 6815283 | 10,000.00 DAG | 95,130,048.92 PACA | 9,513.0 |
| `DAG6zZakMJrrf2…` | 6815297 | 10,000.00 DAG | 86,917,999.01 PACA | 8,691.8 |
| `DAG6zZakMJrrf2…` | 6815312 | 10,000.00 DAG | 80,897,753.01 PACA | 8,089.8 |
| `DAG6zZakMJrrf2…` | 6815326 | 10,000.00 DAG | 72,885,041.43 PACA | 7,288.5 |
| `DAG6zZakMJrrf2…` | 6815343 | 10,000.00 DAG | 68,239,573.39 PACA | 6,824.0 |
| `DAG6zZakMJrrf2…` | 6815375 | 10,000.00 DAG | 58,217,441.54 PACA | 5,821.7 |
| `DAG6zZakMJrrf2…` | 6815433 | 5,000.00 DAG | 108,267,925.75 PACA | 21,653.6 |
| `DAG6zZakMJrrf2…` | 6815462 | 5,000.00 DAG | 95,231,249.29 PACA | 19,046.2 |
| `DAG3sGFqKZ974e…` | 6815350 | 10,000.00 DAG | 64,024,715.96 PACA | 6,402.5 |
| `DAG3sGFqKZ974e…` | 6815439 | 10,000.00 DAG | 199,294,479.40 PACA | 19,929.4 |
| `DAG7iLJFTAF1sE…` | 6815243 | 13,000.00 DAG | 159,895,324.85 PACA | 12,299.6 |
| `DAG6pvRsWjTzmP…` | 6815190 | 8,777.54 DAG | 146,947,445.94 PACA | 16,741.3 |
| `DAG4jWvjPdpvUq…` | 6815305 | 5,000.00 DAG | 41,162,715.84 PACA | 8,232.5 |
| `DAG4jWvjPdpvUq…` | 6815349 | 8,003.46 DAG | 51,560,308.51 PACA | 6,442.3 |
| `DAG8Eyr6SGvLor…` | 6814844 | 5,000.00 DAG | 15,898,172.12 PACA | 3,179.6 |
| `DAG8Eyr6SGvLor…` | 6814965 | 2,000.00 DAG | 35,489,685.03 PACA | 17,744.8 |
| `DAG4kfRPpcPSh4…` | 6815279 | 2,000.00 DAG | 19,615,234.61 PACA | 9,807.6 |
| `DAG5434oVLFRRT…` | 6815382 | 11,350.66 DAG | 62,970,570.79 PACA | 5,547.7 |
| `DAG7uHRz6stwzs…` | 6815217 | 10,000.00 DAG | 151,039,590.35 PACA | 15,104.0 |
| `DAG1DD2bM1hpFy…` | 6815348 | 10.00 DAG | 66,066.04 PACA | 6,606.6 |
| `DAG1DD2bM1hpFy…` | 6815360 | 10.00 DAG | 58,994.45 PACA | 5,899.4 |
| `DAG1DD2bM1hpFy…` | 6815407 | 10.00 DAG | 222,825.48 PACA | 22,282.5 |
| `DAG1DD2bM1hpFy…` | 6815420 | 10.00 DAG | 222,799.79 PACA | 22,280.0 |
| `DAG1DD2bM1hpFy…` | 6815453 | 10.00 DAG | 195,619.34 PACA | 19,561.9 |

The rate column is the audit trail. Across the 26 purchases it runs from **3,179.6** to **22,282.5** PACA per DAG. A healthy pool quotes **4.21**, so even the cheapest of these was paying roughly 755x the fair rate, and the worst 5,289x. Nobody buying here could have been getting a fair price.

## Appendix B — Attacker extraction

| Address | Global ordinal | PACA sold | DAG received |
|---|---|---|---|
| `DAG4w5mUqNNxQN…` | 6814575 | 1,000,000.00 | 232,092.82 |
| `DAG4w5mUqNNxQN…` | 6814621 | 10,000,000.00 | 1,910,063.45 |
| `DAG4w5mUqNNxQN…` | 6814635 | 30,000,000.00 | 3,243,936.43 |
| `DAG4w5mUqNNxQN…` | 6814653 | 300,000,000.00 | 5,146,574.46 |
| `DAG4w5mUqNNxQN…` | 6814667 | 1,000,000,000.00 | 1,146,304.95 |
| `DAG4w5mUqNNxQN…` | 6814722 | *(bought 954,378,229.30)* | −1,000,000.00 |
| `DAG8uqhyGtFABW…` | 6814815 | 1,000,000,000.00 | 1,008,476.04 |
| `DAG8uqhyGtFABW…` | 6814851 | 1,000,000,000.00 | 182,331.94 |
| `DAG8uqhyGtFABW…` | 6814878 | 1,000,000,000.00 | 78,226.35 |
| `DAG8uqhyGtFABW…` | 6815378 | 1,000,000,000.00 | 114,240.66 |
| `DAG8uqhyGtFABW…` | 6815401 | 1,000,000,000.00 | 60,082.68 |

Net: `DAG4w5mUqNNxQN…` **10,678,972.11 DAG**, `DAG8uqhyGtFABW…` **1,443,357.66 DAG**. Total **12,122,329.77 DAG**.

## Appendix C — Mint transactions

All four in currency snapshot **731261** / global **6814499**, source `DAG5Yno9tMKHLe1G6J5QSbiqRicWV2HRKunDtFuR`, amount `4,611,686,018,427,387,904` each, `parent` **null**.

| Destination | Fee-transaction hash |
|---|---|
| `DAG8uqhyGtFABWSS5KeVB2ia1R4vXop5AeijXeoU` | `d50e8ee719b37f425b0e52d83fd8196f40460fe5b487071321b8deb8339ab131` |
| `DAG4w5mUqNNxQNS4hgdpx3E8FGgiu2UCRsJxHwhX` | `c1db013c80c0ea526a7774034b471b8d5b6f071bf697df656dfa3b85a786be00` |
| `DAG7ZjENTP4T36PPSp3skJdTHtQbcuLfpEaAFWdn` | `be02914f5df9580640d8541538a3be1c00ca9fb5b6b0870d83d59a8e99bfc7b0` |
| `DAG1kEmLAgnCVBURHrL4AMsfn9TZdk4QCYQ8tUu3` | `3b6284b71a7b0f709af4455e504c06f8df05cefbc83be062ec2c552a7d293ff5` |

## Appendix D — Reproducing this audit

```bash
M=DAG7X5idd4aLfp4XC6WQdG1eDfR3LGPVEwtUUB2W
BE=https://be-mainnet.constellationnetwork.io
LB=https://l0-lb-mainnet.constellationnetwork.io

# 1. the mint — four fee transactions, one snapshot, source holding zero
curl -s "$BE/currency/$M/addresses/DAG5Yno9tMKHLe1G6J5QSbiqRicWV2HRKunDtFuR/fee-transactions"

# 2. every swap leg in the window (60 ordinals carry activity)
curl -s "$BE/addresses/$M/spend-transactions?limit=2000"      # DAG legs
curl -s "$BE/currency/$M/addresses/$M/spend-transactions?limit=2000"   # PACA legs

# 3. authoritative leg data, all currencies, per ordinal
curl -s -H 'Accept: application/json' "$LB/global-snapshots/<ORDINAL>" \
  | jq ".value.spendActions[\"$M\"]"

# 4. current balances
curl -s "$BE/currency/$M/addresses/<ADDR>/balance"
```

The load balancer returns **HTTP 403** under aggressive parallel fetching. Sixty sequential requests spaced ~1s completes cleanly.

Raw data backing every figure here is committed in `docs/data/` and checked by `scripts/verify_remediation.py`: `legs_full.json` (132 legs across 60 ordinals), `swaps.json` (70 reconstructed swaps), `replay.json` (curve replay), `corrected_plan.json` (final per-address figures).
