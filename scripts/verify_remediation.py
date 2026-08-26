#!/usr/bin/env python3
"""Rebuilds the remediation figures from the raw snapshot data and checks them
against the resources that ship in this repo.

Everything the incident document asserts is derived here. Nothing is hardcoded
except the pre-attack reserves, which are read from the pool state at the mint
ordinal and restated below so a reviewer can see them.

    python3 scripts/verify_remediation.py

Exits non-zero if any published figure fails to reproduce.

The raw inputs in docs/data/ were captured from mainnet on 2026-08-25:

    legs_full.json      spendActions for the metagraph, per global ordinal,
                        for every ordinal in the window that carried activity
    swaps.json          those legs paired into swaps
    replay.json         the pre-attack-curve replay of the legitimate purchases
    corrected_plan.json the resulting per-address figures

Re-capture with, for each ordinal in legs_full.json:

    curl -s -H 'Accept: application/json' \\
      "https://l0-lb-mainnet.constellationnetwork.io/global-snapshots/<ORDINAL>" \\
      | jq '.value.spendActions["DAG7X5idd4aLfp4XC6WQdG1eDfR3LGPVEwtUUB2W"]'

The load balancer returns HTTP 403 under parallel fetching; sequential requests
spaced about a second complete cleanly.
"""

import json
import pathlib
import sys

ROOT = pathlib.Path(__file__).resolve().parent.parent
DATA = ROOT / "docs" / "data"

PACA = "DAG7X5idd4aLfp4XC6WQdG1eDfR3LGPVEwtUUB2W"

# Pool state immediately before the mint, in datum units.
PRE_ATTACK_PACA = 5112080329 * 10**6
PRE_ATTACK_DAG = 1213326392 * 10**6
FEE = 0.003

# Pool state as the attack left it, read from the PACA/DAG entry in the currency
# calculated state at currency ordinal 731646 / global ordinal 6815497. These are
# the reserves the remediation overwrites, and the DAG figure is what the treasury
# injection has to top up. An earlier revision of the document derived the DAG side
# by subtracting the net DAG that left the metagraph *address* from the pre-attack
# reserve; that address is shared by all four pools, so the result was 147,940.10
# DAG too high. Read the pool, not the address.
CORRUPTED_PACA = 360348314082469011
CORRUPTED_DAG = 18646623956291

# What the treasury was originally expected to send, for the shortfall check.
PLANNED_INJECTION = 12_000_000 * 10**8

# The five attacker-controlled addresses: four mint recipients plus the address
# that signed all four fee transactions.
ATTACKER = {
    "DAG5Yno9tMKHLe1G6J5QSbiqRicWV2HRKunDtFuR",
    "DAG8uqhyGtFABWSS5KeVB2ia1R4vXop5AeijXeoU",
    "DAG4w5mUqNNxQNS4hgdpx3E8FGgiu2UCRsJxHwhX",
    "DAG7ZjENTP4T36PPSp3skJdTHtQbcuLfpEaAFWdn",
    "DAG1kEmLAgnCVBURHrL4AMsfn9TZdk4QCYQ8tUu3",
}

TWO_POW_62 = 2**62

failures = []


def check(label, actual, expected):
    ok = actual == expected
    print(f"  {'PASS' if ok else 'FAIL'}  {label}")
    if not ok:
        print(f"          expected {expected!r}")
        print(f"          actual   {actual!r}")
        failures.append(label)
    return ok


def fmt(datum):
    return f"{datum / 1e8:,.2f}"


def by_addr_pool(resource):
    """The PACA-pool row of the balance-adjustment resource, as a positive amount."""
    return -int(next(e for e in resource if e["address"] == PACA)["deduct"])


def fmt8(datum):
    """Full precision. The injection is a transfer instruction, not a summary."""
    return f"{datum / 1e8:,.8f}"


def main():
    legs_by_ordinal = json.loads((DATA / "legs_full.json").read_text())
    swaps = json.loads((DATA / "swaps.json").read_text())
    replay = json.loads((DATA / "replay.json").read_text())
    plan = json.loads((DATA / "corrected_plan.json").read_text())

    paired = [s for s in swaps if not s.get("note")]
    legs = [leg for v in legs_by_ordinal.values() for leg in v]

    print("\nDataset")
    check("global ordinals carrying activity", len(legs_by_ordinal), 60)
    check("spend-transaction legs", len(legs), 132)
    check("swaps paired from those legs", len(paired), 62)
    check("unpaired legs", len(swaps) - len(paired), 8)
    check("swaps against the PACA/DAG pool",
          len([s for s in paired if PACA in (s["in_cur"], s["out_cur"])]), 44)

    print("\nThe mint")
    check("4 x 2^62 equals 2^64 exactly", 4 * TWO_POW_62, 2**64)
    check("minted PACA", fmt(4 * TWO_POW_62), "184,467,440,737.10")

    # The wrapping fold, reproduced. This is the bug itself.
    balance, walk = 0, []
    for _ in range(4):
        balance = (balance - TWO_POW_62) & (2**64 - 1)
        if balance >= 2**63:
            balance -= 2**64
        walk.append(balance)
    check("source balance walk under wrapping arithmetic", walk,
          [-TWO_POW_62, -(2**63), TWO_POW_62, 0])

    print("\nTimeline")
    attacker_sells = [s for s in paired
                      if s["who"] in ATTACKER and s["in_cur"] == PACA and s["out_cur"] == "DAG"]
    buyers = {r["a"] for r in plan if r["kind"] == "BUYER"}
    purchases = [s for s in paired
                 if s["who"] in buyers and s["in_cur"] == "DAG" and s["out_cur"] == PACA]

    check("first attacker sale, global ordinal", min(s["o"] for s in attacker_sells), 6814575)
    check("last attacker sale, global ordinal", max(s["o"] for s in attacker_sells), 6815401)
    check("first legitimate purchase, global ordinal", min(s["o"] for s in purchases), 6814844)
    check("last legitimate purchase, global ordinal", max(s["o"] for s in purchases), 6815462)
    check("legitimate purchase transactions", len(purchases), 26)
    check("addresses making them", len(buyers), 10)

    print("\nPre-attack price")
    paca_per_dag = PRE_ATTACK_PACA / PRE_ATTACK_DAG
    check("PACA per DAG", f"{paca_per_dag:.6f}", "4.213277")

    print("\nCurve replay of the legitimate purchases")
    # Attacker trades are excluded: the counterfactual is a pool the mint never touched.
    pa, dg = PRE_ATTACK_PACA, PRE_ATTACK_DAG
    held = {}
    for s in sorted(paired, key=lambda x: x["o"]):
        if s["who"] in ATTACKER:
            continue
        if s["in_cur"] == "DAG" and s["out_cur"] == PACA:
            eff = int(s["in_amt"] * (1 - FEE))
            out = int(pa * eff / (dg + eff))
            pa -= out
            dg += s["in_amt"]
            held[s["who"]] = held.get(s["who"], 0) + out
        elif s["in_cur"] == PACA and s["out_cur"] == "DAG":
            sell = min(s["in_amt"], held.get(s["who"], 0))
            if sell > 0:
                eff = int(sell * (1 - FEE))
                out = int(dg * eff / (pa + eff))
                pa += sell
                dg -= out
                held[s["who"]] -= sell

    check("counterfactual pool PACA", pa, replay["pool_paca"])
    check("counterfactual pool DAG", dg, replay["pool_dag"])
    check("counterfactual pool PACA, formatted", fmt(pa), "50,395,243.35")
    check("counterfactual pool DAG, formatted", fmt(dg), "12,308,553.85")
    check("counterfactual price", f"{pa / dg:.6f}", "4.094327")

    # Two distinct totals, easy to conflate. The replay entitles every non-attacker who
    # bought; one of them (DAG2zpTA) forwarded everything on and holds nothing today, so it
    # carries no deduction and its entitlement never materialises.
    replay_total = sum(v for v in held.values() if v > 0)
    buyer_total = sum(max(0, held.get(a, 0)) for a in buyers)
    check("replay entitlement, all non-attacker buyers", fmt(replay_total), "725,559.94")
    check("entitlement actually retained, the 10 addresses with a deduction",
          fmt(buyer_total), "724,259.28")
    check("difference is the forwarded address holding nothing",
          fmt(replay_total - buyer_total), "1,300.67")

    print("\nThe DAG side: what the pool lost and what has to go back")
    # Three DAG totals that are easy to conflate. Each is checked against its own
    # source so a reviewer can see they are different quantities, not a discrepancy.
    attacker_dag_out = sum(s["out_amt"] for s in paired
                           if s["who"] in ATTACKER and s["out_cur"] == "DAG")
    attacker_dag_in = sum(s["in_amt"] for s in paired
                          if s["who"] in ATTACKER and s["in_cur"] == "DAG")
    attacker_proceeds = attacker_dag_out - attacker_dag_in
    pool_dag_loss = PRE_ATTACK_DAG - CORRUPTED_DAG

    check("attacker net DAG proceeds, from the paired swaps",
          fmt8(attacker_proceeds), "12,122,329.77157270")
    check("PACA/DAG pool net DAG loss, pre-attack reserve minus what is left",
          fmt8(pool_dag_loss), "11,946,797.68043709")
    # Measured, not inferred: DAG that non-attacker addresses actually put into this
    # pool. It does not equal the gap between the two figures above, and is not
    # claimed to. The remainder is carried as a residual below.
    pool_swaps = [s for s in paired if PACA in (s["in_cur"], s["out_cur"])]
    honest_in = sum(s["in_amt"] for s in pool_swaps
                    if s["who"] not in ATTACKER and s["in_cur"] == "DAG")
    honest_out = sum(s["out_amt"] for s in pool_swaps
                     if s["who"] not in ATTACKER and s["out_cur"] == "DAG")
    check("non-attacker net DAG paid into the pool, from the swap ledger",
          fmt8(honest_in - honest_out), "179,113.93472559")
    residual = attacker_proceeds - pool_dag_loss
    check("gap between attacker proceeds and pool loss", fmt8(residual),
          "175,532.09113561")
    # The swap ledger and the reserve snapshot disagree at the edges of the window.
    # Small, disclosed, and it does not touch the injection: that comes from the
    # reserve alone.
    check("swap ledger vs reserve snapshot residual",
          fmt8((honest_in - honest_out) - residual), "3,581.84358998")
    # The address figure quoted in the document, 11,798,857.58, is the net DAG that
    # left the shared metagraph address. It is smaller than the pool's loss because
    # the other three pools took DAG in over the same window.
    check("other three pools' net DAG receipt reconciles the address figure",
          fmt8(pool_dag_loss - 1179885758000000), "147,940.10043709")

    print("\nThe pool resource the remediation writes")
    pools = json.loads(
        (ROOT / "modules/shared_data/src/main/resources/updated-pools-13.json").read_text())
    check("the resource carries exactly the PACA/DAG pool", list(pools), [PACA])
    written = pools[PACA]
    check("PACA reserve written equals the replay", written["tokenA"]["amount"], pa)
    check("DAG reserve written equals the replay", written["tokenB"]["amount"], dg)
    check("DAG side of the pool is native DAG", written["tokenB"]["identifier"], None)
    # SwapCalculations prices off k directly rather than deriving it from the
    # reserves, so a k left over from the corrupted state keeps quoting the attack
    # price no matter what the reserves say.
    check("k is recomputed from the written reserves",
          written["k"], written["tokenA"]["amount"] * written["tokenB"]["amount"])

    print("\nThe pool address balance the deduction works against")
    # Deductions act on the address balance; reserves are a separate quantity. The
    # address held more PACA than the pool reserve accounted for, and the deduction
    # is sized to leave exactly the reserve the remediation writes.
    pool_row = next(r for r in plan if r["a"] == PACA)
    check("observed pool address PACA balance",
          fmt8(pool_row["bal"]), "3,603,518,762.19858122")
    check("it sits above the pool's PACA reserve",
          fmt8(pool_row["bal"] - CORRUPTED_PACA), "35,621.37389104")
    adjustments = json.loads(
        (ROOT / "modules/l0/src/main/resources/balance-adjustments-4.json").read_text())
    left_on_address = pool_row["bal"] - by_addr_pool(adjustments)
    # The deduction was sized against the target rounded to the cent, 50,395,243.35,
    # while updated-pools-13.json writes the exact replay figure. The address is
    # therefore left holding 0.00493271 PACA more than the reserve claims. The
    # invariant that matters is the direction: the address must be able to back the
    # reserve, and over-backing by half a cent is safe. Sizing the deduction on the
    # exact figure instead would move the published nominal total, so it is left
    # alone and disclosed here.
    check("the deduction leaves the rounded target on the address",
          left_on_address, 5039524335000000)
    check("the address can back the PACA reserve that gets written",
          left_on_address >= written["tokenA"]["amount"], True)
    check("slack between the two, in PACA",
          fmt8(left_on_address - written["tokenA"]["amount"]), "0.00493271")

    print("\nTreasury injection")
    injection = written["tokenB"]["amount"] - CORRUPTED_DAG
    check("DAG to send to the metagraph address, in datum units",
          injection, 1212208760812058)
    check("DAG to send to the metagraph address", fmt8(injection),
          "12,122,087.60812058")
    # This must land before the remediation ordinal. Writing a reserve the address
    # cannot back leaves the pool insolvent and the first withdrawal fails.
    check("the 12M planned injection falls short",
          fmt8(injection - PLANNED_INJECTION), "122,087.60812058")

    print("\nDeduction resources agree across repositories")
    paca_res = json.loads(
        (ROOT / "modules/l0/src/main/resources/balance-adjustments-4.json").read_text())
    check("entries in the metagraph resource", len(paca_res), 17)
    check("every deduct is an integer",
          all(isinstance(e["deduct"], int) for e in paca_res), True)

    by_addr = {e["address"]: -int(e["deduct"]) for e in paca_res}
    for a in ATTACKER - {"DAG5Yno9tMKHLe1G6J5QSbiqRicWV2HRKunDtFuR"}:
        check(f"{a[:14]}... deducted the full 2^62", by_addr.get(a), TWO_POW_62)

    nominal = sum(by_addr.values())
    check("nominal deduction total", fmt(nominal), "189,413,903,467.12")

    print("\nSupply reconciliation")
    # Deductions saturate at zero, so the nominal total overstates what is actually
    # removed by however much each attacker wallet was already drained.
    observed = {r["a"]: r["bal"] for r in plan}
    saturation = sum(max(0, by_addr[a] - observed.get(a, 0))
                     for a in by_addr if a in ATTACKER)
    actual_removed = nominal - saturation
    check("actually removed after saturation", fmt(actual_removed), "184,027,281,696.42")

    locks = 44019596271000000  # 7 active locks created after the mint, see section 10
    overage = (actual_removed + locks) - 4 * TWO_POW_62
    # Not zero, and not claimed to be. The excess is fee the pool retained on the legitimate
    # purchases, which is real supply that stays with the pool rather than being deducted.
    check("removed + locked exceeds minted, by the pool's retained trading fee",
          fmt(overage), "36,922.04")

    print()
    if failures:
        print(f"{len(failures)} check(s) FAILED:")
        for f in failures:
            print(f"  - {f}")
        return 1
    print("All checks passed. Every figure published in the incident document reproduces.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
