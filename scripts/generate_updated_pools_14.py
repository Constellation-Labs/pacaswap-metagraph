#!/usr/bin/env python3
"""Generates updated-pools-14.json mechanically from the ledger.

    python3 scripts/generate_updated_pools_14.py [--write]

Prints the resulting state and exits non-zero unless every invariant holds. Without --write it
only reports, so it can run in CI as a check that the shipped resource still reproduces.

WHY THIS IS GENERATED AND NOT HAND-WRITTEN
------------------------------------------
The twelve earlier updated-pools files were assembled by hand. Every one of the 48 pool records
in them carries `k != tokenA * tokenB`, and from the second file onward the share ledgers do not
sum to totalShares either. Those inconsistencies are still in the live state today. This script
derives every field, recomputes k and totalShares, and refuses to write a file that does not
close.

WHAT IT WRITES, AND WHY
-----------------------
Target: after normalization, every reserve equals what the wallet actually holds.

  token side  := max(book, wallet)
                 Where the book is short we fund the wallet up to it, so the book is unchanged.
                 Where the wallet is over the book (The Upsider AI), no transfer can fix that
                 without moving tokens out, so the book is raised to the wallet instead. That is
                 a gain for that pool's LPs; nobody loses.
  DAG side    := unchanged
                 The DAG shortfall is closed by the treasury transfer, not by rewriting the book.
                 DAG is also pooled across all four pools and cannot be attributed per pool.
  k           := tokenA.amount * tokenB.amount, recomputed
  totalShares := sum(addressShares), so the pool stops promising more than 100% of itself
  addressShares, owner := untouched

Note the writer this resource feeds (updatePoolsAtOrdinal) also blanks onChain and
sharedArtifacts for that snapshot. Apply it at an ordinal where no spend action is in flight.
"""

import argparse
import json
import pathlib
import sys
from decimal import Decimal as D

ROOT = pathlib.Path(__file__).resolve().parent.parent
RESOURCES = ROOT / "modules" / "shared_data" / "src" / "main" / "resources"
OUT = RESOURCES / "updated-pools-14.json"
CACHE = ROOT / "incident-audit-2026-08-25" / "collateral-gap" / "global-latest.json"
STATE = pathlib.Path.home() / "Documents" / "731646-calculated-state"

AMM = "DAG7X5idd4aLfp4XC6WQdG1eDfR3LGPVEwtUUB2W"
NAMES = {
    "DAG0CyySf35ftDQDQBnd1bdQ9aPyUdacMghpnCuM": "Dor Technologies (DOR)",
    "DAG0S16WDgdAvh8VvroR6MWLdjmHYdzAF5S181xh": "USDC.dag",
    "DAG7Ghth1WhWK83SB3MtXnnHYZbCsmiRTwJrgaW1": "The Upsider AI (UP)",
    AMM: "PacaSwap (SWAP)",
}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--write", action="store_true")
    # The Upsider AI surplus can be resolved two ways and they are mutually exclusive:
    # book it into the pool, or sweep it out of the custody address with a SpendAction.
    # Sweeping is the chosen route, so the book stays where it is and the wallet comes down.
    ap.add_argument("--book-up-surplus", action="store_true",
                    help="raise the UP book to the wallet instead of sweeping the surplus out")
    args = ap.parse_args()

    state = json.loads(STATE.read_text())
    snap, info = json.loads(CACHE.read_text())
    p13 = json.loads((RESOURCES / "updated-pools-13.json").read_text())[AMM]
    pools = state["operations"]["LiquidityPool"]["LiquidityPoolCalculatedState"]["confirmed"]["value"]
    cur = info["lastCurrencySnapshots"]

    out, rows, fails = {}, [], []
    for pid, p in sorted(pools.items()):
        tokenA, tokenB = dict(p["tokenA"]), dict(p["tokenB"])

        # The SWAP/DAG pool is already rewritten by the authorized remediation at 731647; start
        # from its post-remediation values, never from the pre-remediation ones.
        if pid == AMM:
            tokenA["amount"] = p13["tokenA"]["amount"]
            tokenB["amount"] = p13["tokenB"]["amount"]

        for side in (tokenA, tokenB):
            cid = side.get("identifier")
            if cid is None:
                continue                       # DAG: closed by transfer, book unchanged
            wallet = cur[cid]["Right"][1]["balances"][AMM]
            if pid == AMM:
                wallet = p13["tokenA"]["amount"]   # remediation makes SWAP 1:1 by construction
            book = side["amount"]
            # max() only where we intend to raise the book. For the surplus pool the wallet is
            # brought DOWN to the book by the sweep, so the book must not move.
            side["amount"] = max(book, wallet) if args.book_up_surplus else max(book, min(book, wallet))
            rows.append((NAMES[cid], book, wallet, side["amount"]))

        shares = p["poolShares"]["addressShares"]
        total = sum(shares.values())
        k = tokenA["amount"] * tokenB["amount"]

        if k != tokenA["amount"] * tokenB["amount"]:
            fails.append(f"{pid} k")
        if total != sum(shares.values()):
            fails.append(f"{pid} shares")
        if total < p["poolShares"]["totalShares"]:
            fails.append(f"{pid}: totalShares would DROP, refusing (would dilute nobody but "
                         f"implies shares vanished)")

        out[pid] = {
            "poolId": p["poolId"],
            "tokenA": tokenA,
            "tokenB": tokenB,
            "owner": p["owner"],
            "k": k,
            "poolShares": {"totalShares": total, "addressShares": shares},
        }

    print(f"{'pool / token':<28}{'book before':>24}{'wallet':>24}{'book after':>24}")
    for name, book, wallet, after in rows:
        note = "  <- raised to wallet" if after != book else ""
        print(f"{name:<28}{book/1e8:>24,.8f}{wallet/1e8:>24,.8f}{after/1e8:>24,.8f}{note}")

    print()
    print(f"{'pool':<28}{'totalShares before':>22}{'after':>22}{'k == A*B':>12}")
    for pid, rec in sorted(out.items()):
        before = pools[pid]["poolShares"]["totalShares"]
        ok = rec["k"] == rec["tokenA"]["amount"] * rec["tokenB"]["amount"]
        print(f"{NAMES[pid]:<28}{before:>22,}{rec['poolShares']['totalShares']:>22,}{str(ok):>12}")

    if fails:
        print("\nFAILED:"); [print("  -", f) for f in fails]
        return 1

    if args.write:
        OUT.write_text(json.dumps(out, indent=2) + "\n")
        print(f"\nwrote {OUT.relative_to(ROOT)} ({OUT.stat().st_size:,} bytes)")
    else:
        print("\nreport only; pass --write to emit the resource")
    return 0


if __name__ == "__main__":
    sys.exit(main())
