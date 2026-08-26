#!/usr/bin/env python3
"""Alerts when the AMM's book stops matching its wallet.

    METAGRAPH_L0_URL=https://<ml0-host> python3 scripts/monitor_collateral.py

Exits non-zero when any ledger is short by more than the accepted baseline, which is what makes
a scheduled CI run fail and notify. Read-only: it queries two public APIs and writes nothing.

WHY THIS EXISTS
---------------
The metagraph checks the same invariant internally every snapshot, but it only writes to the node
log, and a log nobody reads is not an alert. This runs outside the node so a mismatch reaches
people.

WHAT IT COMPARES
----------------
  pool reserves   GET {METAGRAPH_L0_URL}/liquidity-pools     (calculated state, the book)
  wallet          the public block explorer                  (what is actually held on chain)

DAG is summed across all four pools because it is fungible and shared by one custody address, so
only the aggregate is meaningful. Each token belongs to exactly one pool, so those are exact per
pool. Never derive one from the other.

THE BASELINE
------------
`scripts/collateral-baseline.json` records the shortfall we currently accept, per ledger. While
the known gap is open, alerting on any shortfall would fire on every run and become noise; the
useful signal is the gap GROWING. Once the treasury top-up lands and every ledger reads 1:1, set
every baseline to 0 and this becomes a strict check.

IN-FLIGHT VALUE
---------------
A reserve is credited when the SpendAction is generated; the wallet only moves when the global
layer settles it. Between those two moments the book legitimately runs ahead. The public pool
endpoint does not expose pending spend actions, so a single sample can show a false shortfall
during active trading. `--confirm-after` re-samples and only reports a breach that persists,
which is what distinguishes a real divergence from a swap in flight.
"""

import argparse
import json
import os
import pathlib
import sys
import time
import urllib.request

ROOT = pathlib.Path(__file__).resolve().parent.parent
BASELINE = ROOT / "scripts" / "collateral-baseline.json"

AMM = "DAG7X5idd4aLfp4XC6WQdG1eDfR3LGPVEwtUUB2W"
BE = os.environ.get("BLOCK_EXPLORER_URL", "https://be-mainnet.constellationnetwork.io")

NAMES = {
    "DAG": "DAG",
    "DAG0CyySf35ftDQDQBnd1bdQ9aPyUdacMghpnCuM": "Dor Technologies (DOR)",
    "DAG0S16WDgdAvh8VvroR6MWLdjmHYdzAF5S181xh": "USDC.dag",
    "DAG7Ghth1WhWK83SB3MtXnnHYZbCsmiRTwJrgaW1": "The Upsider AI (UP)",
    AMM: "PacaSwap (SWAP)",
}


def get(url, timeout=30):
    req = urllib.request.Request(url, headers={"Accept": "application/json"})
    with urllib.request.urlopen(req, timeout=timeout) as r:
        return json.loads(r.read().decode())


def unwrap(payload):
    """The routes wrap lists as {data, meta} and singles as {data}."""
    return payload.get("data", payload) if isinstance(payload, dict) else payload


def read_reserves(pools_url=None, pools_file=None):
    """Total reserve per ledger, from the book. Key `DAG` is the native side."""
    payload = json.loads(pathlib.Path(pools_file).read_text()) if pools_file else get(pools_url)
    pools = unwrap(payload)
    if not isinstance(pools, list) or not pools:
        raise SystemExit(f"expected a non-empty list of pools, got: {str(payload)[:200]}")

    reserves = {}
    for p in pools:
        for side in ("tokenA", "tokenB"):
            t = p[side]
            ident = t.get("id") or t.get("identifier")   # native DAG carries no identifier
            key = "DAG" if ident in (None, "", "null") else ident
            reserves[key] = reserves.get(key, 0) + int(t["amount"])
    return reserves


def read_wallet(ledgers):
    """What the custody address actually holds, per ledger."""
    held = {}
    for led in ledgers:
        url = (
            f"{BE}/addresses/{AMM}/balance" if led == "DAG"
            else f"{BE}/currency/{led}/addresses/{AMM}/balance"
        )
        held[led] = int(get(url)["data"]["balance"])
        time.sleep(1)   # the load balancer rejects parallel bursts
    return held


def sample(args):
    reserves = read_reserves(args.pools_url, args.pools_file)
    wallet = read_wallet(sorted(reserves))
    return {led: reserves[led] - wallet[led] for led in reserves}, reserves, wallet


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--pools-url", default=(os.environ.get("METAGRAPH_L0_URL", "").rstrip("/") + "/liquidity-pools")
                    if os.environ.get("METAGRAPH_L0_URL") else None)
    ap.add_argument("--pools-file", help="read the book from a file instead (testing, or while stopped)")
    ap.add_argument("--confirm-after", type=int, default=0,
                    help="seconds to wait before re-sampling; only a breach seen twice is reported")
    args = ap.parse_args()

    if not args.pools_url and not args.pools_file:
        raise SystemExit("set METAGRAPH_L0_URL, or pass --pools-url / --pools-file")

    baseline = json.loads(BASELINE.read_text()) if BASELINE.exists() else {}
    accepted = baseline.get("accepted_shortfall", {})

    shortfall, reserves, wallet = sample(args)

    breaches = {l: s for l, s in shortfall.items() if s > int(accepted.get(l, 0))}
    if breaches and args.confirm_after:
        print(f"possible breach on {sorted(breaches)}; re-sampling in {args.confirm_after}s "
              "to rule out value in flight", flush=True)
        time.sleep(args.confirm_after)
        shortfall2, reserves, wallet = sample(args)
        breaches = {l: s for l, s in shortfall2.items()
                    if s > int(accepted.get(l, 0)) and l in breaches}
        shortfall = shortfall2

    print(f"{'ledger':<26}{'book':>22}{'wallet':>22}{'shortfall':>20}{'accepted':>20}")
    for led in sorted(shortfall, key=lambda l: NAMES.get(l, l)):
        acc = int(accepted.get(led, 0))
        flag = "  BREACH" if shortfall[led] > acc else ""
        print(f"{NAMES.get(led, led):<26}{reserves[led]/1e8:>22,.8f}{wallet[led]/1e8:>22,.8f}"
              f"{shortfall[led]/1e8:>20,.8f}{acc/1e8:>20,.8f}{flag}")

    if breaches:
        print()
        for led, s in sorted(breaches.items()):
            over = s - int(accepted.get(led, 0))
            print(f"BREACH {NAMES.get(led, led)}: the book promises {over/1e8:,.8f} more than "
                  f"accepted. Reserve {reserves[led]}, wallet {wallet[led]}, shortfall {s}.")
        print("\nThe book has moved further from the wallet than the accepted baseline. "
              "Investigate before allowing further withdrawals.")
        return 1

    print("\nevery ledger is within the accepted baseline")
    return 0


if __name__ == "__main__":
    sys.exit(main())
