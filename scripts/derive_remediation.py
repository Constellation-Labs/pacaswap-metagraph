#!/usr/bin/env python3
"""Derive the FeeTransaction-bug remediation inputs from mainnet snapshot data.

The mint at metagraph ordinal 731261 credited four wallets 2^62 PACA each out of
nothing. This walks every currency snapshot from the mint to the freeze, follows
the phantom through swaps, token locks and plain transfers, and writes the static
resources the remediation needs:

  balance-adjustments-4.json   deductions emitted by Main.customArtifacts
  adjustments.json.fragment    the matching tessellation block, must agree exactly
  updated-pools-13.json        pre-attack pool reserves for OneTimeFixesHandler

Value only ever rests in three places: the global-layer balances map, the
global-layer activeTokenLocks map, and the pool reserves in the calculated state.
Swaps, stakes, locks and transfers are edges between those, so following the edges
out of the mint is enough to place every unit of the phantom.

Usage:
  derive_remediation.py fetch  --out DIR [--gl0 URL]
  derive_remediation.py derive --snapshots DIR [--emit DIR]
  derive_remediation.py verify --tessellation-resource PATH
"""
import argparse
import brotli
import collections
import json
import os
import sys
import urllib.request
from concurrent.futures import ThreadPoolExecutor

PACA = "DAG7X5idd4aLfp4XC6WQdG1eDfR3LGPVEwtUUB2W"
MINT_ORDINAL = 731261
FREEZE_ORDINAL = 731646
# The snapshot the mint landed in, referenced by every deduction so the artifact
# carries its own provenance.
MINT_SNAPSHOT_HASH = "0200028940b285045a40b3f2176b3dcd33f2a94821d24ecfa12ab6db1103e358"
# Pre-attack reserves, recovered by inverting the pricing info on the attacker's
# first swap. Asserted against the derived flow in check_reserves().
PRE_ATTACK_PACA = 5112080329000000
PRE_ATTACK_DAG = 1213326392000000
# Hashes of the four minting fee transactions. Snapshots carry the signed
# transaction but not its hash, so these come from hashing them separately and the
# derivation asserts the destination set matches what it found on chain.
FEE_TX_HASHES = {
    "DAG8uqhyGtFABWSS5KeVB2ia1R4vXop5AeijXeoU": "d50e8ee719b37f425b0e52d83fd8196f40460fe5b487071321b8deb8339ab131",
    "DAG4w5mUqNNxQNS4hgdpx3E8FGgiu2UCRsJxHwhX": "c1db013c80c0ea526a7774034b471b8d5b6f071bf697df656dfa3b85a786be00",
    "DAG7ZjENTP4T36PPSp3skJdTHtQbcuLfpEaAFWdn": "be02914f5df9580640d8541538a3be1c00ca9fb5b6b0870d83d59a8e99bfc7b0",
    "DAG1kEmLAgnCVBURHrL4AMsfn9TZdk4QCYQ8tUu3": "3b6284b71a7b0f709af4455e504c06f8df05cefbc83be062ec2c552a7d293ff5",
}


def fetch(out, gl0, first_gl, last_gl):
    os.makedirs(out, exist_ok=True)

    def grab(n):
        for _ in range(4):
            try:
                with urllib.request.urlopen(f"{gl0}/global-snapshots/{n}", timeout=30) as r:
                    gs = json.load(r)
                break
            except Exception:
                gs = None
        if gs is None:
            return 0, 1
        binaries = gs.get("value", gs).get("stateChannelSnapshots", {}).get(PACA, [])
        saved = 0
        for b in binaries:
            cs = json.loads(brotli.decompress(bytes(b["value"]["content"])))
            cs["__global__"] = n
            with open(f"{out}/cs-{cs['value']['ordinal']}.json", "w") as fh:
                json.dump(cs, fh)
            saved += 1
        return saved, 0

    with ThreadPoolExecutor(5) as pool:
        results = list(pool.map(grab, range(first_gl, last_gl + 1)))
    saved = sum(s for s, _ in results)
    errors = sum(e for _, e in results)
    print(f"{last_gl - first_gl + 1} globals, {saved} currency snapshots, {errors} errors")
    if errors:
        sys.exit("refusing to derive from an incomplete fetch")


def load(snapshots):
    out = []
    for name in sorted(os.listdir(snapshots)):
        if not name.startswith("cs-"):
            continue
        doc = json.load(open(f"{snapshots}/{name}"))
        out.append(doc)
    out.sort(key=lambda d: d["value"]["ordinal"])
    return out


def one(d):
    return next(iter(d)) if isinstance(d, dict) and len(d) == 1 else d


def walk(docs):
    """Collect every value-moving event, keyed by the three resting places."""
    mints = []          # (address, amount, feeTxHash)
    pool_paca = 0       # signed PACA flow into the pool address
    pool_dag = 0
    gained = collections.Counter()   # PACA received from the pool, net of PACA sent in
    locked = collections.Counter()   # PACA moved into activeTokenLocks
    lock_refs = []                   # (ordinal, address, amount, unlockEpoch)
    transfers = []                   # (source, destination, amount) plain L1 edges

    for doc in docs:
        v = doc["value"]
        ordinal = v["ordinal"]
        if ordinal < MINT_ORDINAL or ordinal > FREEZE_ORDINAL:
            continue

        for ft in v.get("feeTransactions") or []:
            f = ft["value"]
            mints.append((f["destination"], f["amount"]))

        for a in v.get("artifacts") or []:
            tag = one(a)
            if tag != "SpendAction":
                continue
            for st in a[tag]["spendTransactions"]:
                cur, amt, src, dst = st.get("currencyId"), st["amount"], st["source"], st["destination"]
                if cur == PACA:
                    if src == PACA:
                        gained[dst] += amt
                        pool_paca -= amt
                    if dst == PACA:
                        gained[src] -= amt
                        pool_paca += amt
                elif cur is None:
                    if src == PACA:
                        pool_dag -= amt
                    if dst == PACA:
                        pool_dag += amt

        for blk in v.get("tokenLockBlocks") or []:
            for tl in blk.get("value", blk).get("tokenLocks", []):
                t = tl.get("value", tl)
                if t.get("currencyId") != PACA:
                    continue
                locked[t["source"]] += t["amount"]
                lock_refs.append((ordinal, t["source"], t["amount"], t.get("unlockEpoch")))

        for blk in v.get("blocks") or []:
            for tx in blk.get("block", blk).get("value", blk).get("transactions", []):
                t = tx.get("value", tx)
                transfers.append((t["source"], t["destination"], t["amount"]))

    return dict(mints=mints, pool_paca=pool_paca, pool_dag=pool_dag, gained=gained,
                locked=locked, lock_refs=lock_refs, transfers=transfers)


def derive(flow, balances):
    """Split the phantom into what is deductible now and what is stuck in locks.

    A holder's phantom sits in up to three places: liquid balance, its own token
    locks, and whatever it forwarded to someone else. Only the liquid part is
    reachable by BalanceAdjustment, so the deduction is
      phantom - locked - forwarded
    and the forwarded part is chased to whoever holds it now. Anything above that
    is the holder's own pre-attack PACA and is left alone.
    """
    minted = sum(a for _, a in flow["mints"])
    mint_wallets = sorted({addr for addr, _ in flow["mints"]})
    escaped = minted - sum(balances.get(a, 0) for a in mint_wallets)

    from_pool = {a: v for a, v in flow["gained"].items()
                 if v > 0 and a not in mint_wallets and a != PACA}
    held = dict(from_pool)

    # Follow plain transfers out of tainted addresses in ordinal order. A transfer
    # between two addresses that never took PACA out of the pool carries no
    # phantom and must not drag its recipient into the deduction set.
    forwarded = collections.Counter()
    for src, dst, amount in flow["transfers"]:
        if src not in held:
            continue
        moved = min(amount, held[src] - forwarded[src] - flow["locked"][src])
        if moved <= 0:
            continue
        forwarded[src] += moved
        held[dst] = held.get(dst, 0) + moved

    deductions, clamped = {}, 0
    for a, phantom in held.items():
        amount = phantom - forwarded[a] - flow["locked"][a]
        if amount <= 0:
            continue
        available = balances.get(a, 0)
        if amount > available:
            clamped += amount - available
            amount = available
        if amount > 0:
            deductions[a] = amount

    stuck = sum(flow["locked"][a] for a in held)
    return dict(minted=minted, mint_wallets=mint_wallets, escaped=escaped,
                from_pool=from_pool, held=held, deductions=deductions, stuck=stuck,
                clamped=clamped, pool_surplus=flow["pool_paca"])


def check(flow, d, balances):
    """The derivation is only trustworthy if the phantom accounts for itself."""
    fails = []
    if len(flow["mints"]) != 4:
        fails.append(f"expected 4 fee transactions, saw {len(flow['mints'])}")
    if len({a for _, a in flow["mints"]}) != 1:
        fails.append("fee transactions are not all the same amount")
    if {a for a, _ in flow["mints"]} != set(FEE_TX_HASHES):
        fails.append("mint destinations do not match the recorded fee-transaction hashes")

    placed = d["pool_surplus"] + sum(d["from_pool"].values())
    if placed != d["escaped"]:
        fails.append(f"phantom does not close: escaped {d['escaped']} but placed {placed}")

    accounted = sum(d["deductions"].values()) + d["stuck"] + d["clamped"]
    if accounted != sum(d["from_pool"].values()):
        fails.append(f"third-party split does not close: {accounted} "
                     f"!= {sum(d['from_pool'].values())}")

    # Every lock has to belong to an address the phantom actually reached, or the
    # transfer chase missed a hop.
    for _, addr, _, _ in flow["lock_refs"]:
        if addr not in d["held"]:
            fails.append(f"token lock at {addr} is not reachable from the mint")

    # After deducting the surplus the metagraph address has to still cover the
    # reserve being written, or LP withdrawals outrun the balance backing them.
    left = balances.get(PACA, 0) - d["pool_surplus"]
    if left < PRE_ATTACK_PACA:
        fails.append(f"metagraph address would be left at {left}, short of the "
                     f"{PRE_ATTACK_PACA} reserve being written")
    return fails


def emit(flow, d, balances, out):
    os.makedirs(out, exist_ok=True)
    per_mint = flow["mints"][0][1]

    entries = []
    for a in d["mint_wallets"]:
        entries.append(dict(address=a, reason="FeeTransactionBugDeduction",
                            reference=[FEE_TX_HASHES[a], MINT_SNAPSHOT_HASH], deduct=-per_mint))
    entries.append(dict(address=PACA, reason="FeeTransactionBugDeduction",
                        reference=[MINT_SNAPSHOT_HASH], deduct=-d["pool_surplus"]))
    for a in sorted(d["deductions"], key=lambda a: -d["deductions"][a]):
        entries.append(dict(address=a, reason="FeeTransactionBugDeduction",
                            reference=[MINT_SNAPSHOT_HASH], deduct=-d["deductions"][a]))

    with open(f"{out}/balance-adjustments-4.json", "w") as fh:
        json.dump(entries, fh, indent=2)
        fh.write("\n")

    # Tessellation parses the same shape, takes math.abs of deduct and matches on
    # address plus Amount only, so it gets the entries verbatim minus the reason.
    tess = [dict(address=e["address"], reason=e["reason"], deduct=e["deduct"],
                 reference=e["reference"]) for e in entries]
    with open(f"{out}/adjustments.json.fragment", "w") as fh:
        json.dump(dict(currencyId=PACA, snapshotOrdinal=735000, adjustments=tess), fh, indent=2)
        fh.write("\n")

    with open(f"{out}/updated-pools-13.json", "w") as fh:
        json.dump({PACA: {
            "poolId": PACA,
            "tokenA": {"identifier": PACA, "amount": PRE_ATTACK_PACA},
            "tokenB": {"identifier": None, "amount": balances["__pool_dag__"]},
            "k": PRE_ATTACK_PACA * balances["__pool_dag__"],
        }}, fh, indent=2)
        fh.write("\n")

    return entries


def verify(metagraph_path, tessellation_path):
    """Cross-check the two shipped resources.

    The metagraph emits the artifacts and tessellation demands them. Both sides match on
    address plus Amount, and a mismatch either way stalls the metagraph at the fix ordinal
    rather than failing anything earlier, so the files have to be compared before release.
    """
    mg = json.load(open(metagraph_path))
    blocks = json.load(open(tessellation_path))
    paca_at = [i for i, b in enumerate(blocks) if b["currencyId"] == PACA]
    if not paca_at:
        sys.exit(f"no Pacaswap block in {tessellation_path}")
    # loadAndCreateAdjustmentEntries collects with .toMap, so only the last block for a
    # currency is ever live. An earlier 735000 block would be silently dead.
    tess = blocks[paca_at[-1]]

    def key(entries):
        return sorted((e["address"], abs(e["deduct"]) if e.get("deduct") else -e["increase"])
                      for e in entries)

    fails = []
    if tess["snapshotOrdinal"] != 735000:
        fails.append(f"the live Pacaswap block is at ordinal {tess['snapshotOrdinal']}, not 735000")
    if key(mg) != key(tess["adjustments"]):
        fails.append("address/amount sets differ between the two resources")
    if len({e["address"] for e in mg}) != len(mg):
        fails.append("the metagraph resource repeats an address, which would deduct twice")

    for f in fails:
        print(f"FAIL {f}")
    if fails:
        sys.exit(1)
    print(f"{len(mg)} adjustments agree across both resources, "
          f"{sum(abs(e['deduct']) for e in mg) / 10 ** 8:,.2f} PACA nominal")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("mode", choices=["fetch", "derive", "verify"])
    ap.add_argument("--out")
    ap.add_argument("--snapshots")
    ap.add_argument("--emit")
    ap.add_argument("--metagraph-resource", default="modules/l0/src/main/resources/balance-adjustments-4.json")
    ap.add_argument("--tessellation-resource", help="path to tessellation's adjustments.json")
    ap.add_argument("--balances", help="address -> PACA balance at the freeze")
    ap.add_argument("--pool-dag", type=int, help="DAG the metagraph still holds for the pool")
    ap.add_argument("--gl0", default=os.environ.get("GL0_URL"),
                    help="global L0 base URL, defaults to $GL0_URL")
    ap.add_argument("--first-gl", type=int, default=6814490)
    ap.add_argument("--last-gl", type=int, default=6815790)
    a = ap.parse_args()

    if a.mode == "fetch":
        if not a.gl0:
            sys.exit("set --gl0 or $GL0_URL to a global L0 base URL")
        return fetch(a.out, a.gl0, a.first_gl, a.last_gl)
    if a.mode == "verify":
        return verify(a.metagraph_resource, a.tessellation_resource)

    flow = walk(load(a.snapshots))
    balances = json.load(open(a.balances))
    balances["__pool_dag__"] = a.pool_dag
    d = derive(flow, balances)

    fails = check(flow, d, balances)
    scale = 10 ** 8
    print(f"minted            {d['minted'] / scale:>22,.2f} PACA across {len(d['mint_wallets'])} wallets")
    print(f"still in wallets  {sum(balances.get(x, 0) for x in d['mint_wallets']) / scale:>22,.2f}")
    print(f"escaped           {d['escaped'] / scale:>22,.2f}")
    print(f"  in pool reserve {d['pool_surplus'] / scale:>22,.2f}")
    print(f"  third-party     {sum(d['from_pool'].values()) / scale:>22,.2f} "
          f"across {len(d['from_pool'])} addresses")
    print(f"    deductible    {sum(d['deductions'].values()) / scale:>22,.2f} "
          f"across {len(d['deductions'])} addresses")
    print(f"    in locks      {d['stuck'] / scale:>22,.2f} across {len(flow['lock_refs'])} locks")
    print(f"    pre-attack    {d['clamped'] / scale:>22,.2f} left with holders (balance below phantom)")
    print(f"metagraph buffer  "
          f"{(balances.get(PACA, 0) - d['pool_surplus'] - PRE_ATTACK_PACA) / scale:>22,.2f} "
          f"over the reserve being written")
    print()
    for ordinal, addr, amount, epoch in flow["lock_refs"]:
        print(f"  lock ord {ordinal} {addr} {amount / scale:>18,.2f} unlockEpoch {epoch}")
    print()
    if fails:
        for f in fails:
            print(f"FAIL {f}")
        sys.exit(1)
    print("all invariants hold")

    if a.emit:
        entries = emit(flow, d, balances, a.emit)
        print(f"\nwrote {len(entries)} deductions to {a.emit}")


if __name__ == "__main__":
    main()
