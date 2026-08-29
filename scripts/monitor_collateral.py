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



# ---------------------------------------------------------------------------- alerting

# Slack renders a coloured bar down the left of an attachment. It is the strongest visual signal
# available and the only one that reads at a glance from a phone notification, so severity is
# carried there rather than in an emoji alone.
COLOUR_BREACH = "#A93226"   # deep red, a real divergence
COLOUR_DRILL = "#E67E22"    # amber, a test - must never be mistaken for an incident
COLOUR_OK = "#1F6B5B"       # green, a breach that has cleared
COLOUR_BLIND = "#8E44AD"    # purple, the check could not run - a different problem from a breach

# A breach does not fix itself, so an hourly cron would post the same alert 24 times a day and
# the channel would be muted within one. Alert on the TRANSITION instead: once when it starts,
# once when it clears, and a quiet reminder every REMIND_EVERY runs while it persists.
#
# Tied to the cron interval: this is a number of RUNS, not of hours, so it has to move whenever
# the schedule does or the reminder silently drifts. It was 16 at */15.
REMIND_EVERY = 4    # at hourly that is roughly four hours, as before


def slack_payload(breaches, reserves, wallet, shortfall, accepted, run_url=None, drill=False):
    """Built from the measured values, never from scraped console text."""
    if drill:
        title = ":test_tube: Collateral monitor drill (not an incident)"
        colour = COLOUR_DRILL
        lead = ("*This is a test.* Nothing is wrong and no reading was taken. "
                "It exists to prove this alert reaches you before it has to.")
    else:
        title = ":rotating_light: PacaSwap collateral breach"
        colour = COLOUR_BREACH
        n = len(breaches)
        lead = (f"*{n} ledger{'s' if n != 1 else ''} short beyond the accepted baseline.* "
                "The pools claim more than the custody address holds, so withdrawals can fail "
                "and the first out is paid before the last. Investigate before allowing more.")

    # Two columns of ledger -> overage. Slack allows ten fields; more than that gets summarised.
    fields = []
    for led in sorted(breaches, key=lambda l: -breaches[l])[:8]:
        over = breaches[led] - int(accepted.get(led, 0))
        fields.append({"type": "mrkdwn",
                       "text": f"*{NAMES.get(led, led)}*\n`{over / 1e8:,.8f}` over"})
    if len(breaches) > 8:
        fields.append({"type": "mrkdwn", "text": f"*+{len(breaches) - 8} more*\nsee the run"})

    table = [f"{'ledger':<26}{'book':>22}{'wallet':>22}{'short':>20}"]
    for led in sorted(shortfall, key=lambda l: NAMES.get(l, l)):
        flag = "  <-- BREACH" if led in breaches else ""
        table.append(f"{NAMES.get(led, led):<26}{reserves[led] / 1e8:>22,.8f}"
                     f"{wallet[led] / 1e8:>22,.8f}{shortfall[led] / 1e8:>20,.8f}{flag}")

    blocks = [
        {"type": "header", "text": {"type": "plain_text", "text": title, "emoji": True}},
        {"type": "section", "text": {"type": "mrkdwn", "text": lead}},
    ]
    if fields:
        blocks.append({"type": "section", "fields": fields})
    blocks += [
        {"type": "divider"},
        {"type": "section", "text": {"type": "mrkdwn", "text": "```" + "\n".join(table) + "```"}},
        {"type": "context", "elements": [{"type": "mrkdwn", "text":
            "DAG is summed across all four pools because it is shared by one custody address. "
            "Each token belongs to one pool, so those are exact."}]},
    ]
    if run_url:
        blocks.append({"type": "actions", "elements": [
            {"type": "button", "text": {"type": "plain_text", "text": "View the run", "emoji": True},
             "url": run_url, "style": "danger" if not drill else "primary"}]})

    return {"text": title, "attachments": [{"color": colour, "blocks": blocks}]}


def recovery_payload(shortfall, accepted, run_url=None):
    """Told as loudly as the breach was. A channel that only ever receives bad news teaches
    people that silence means nothing, when silence should mean resolved."""
    table = [f"{'ledger':<26}{'shortfall':>20}{'accepted':>20}"]
    for led in sorted(shortfall, key=lambda l: NAMES.get(l, l)):
        table.append(f"{NAMES.get(led, led):<26}{shortfall[led] / 1e8:>20,.8f}"
                     f"{int(accepted.get(led, 0)) / 1e8:>20,.8f}")
    return {"text": ":white_check_mark: PacaSwap collateral recovered", "attachments": [{"color": COLOUR_OK, "blocks": [
        {"type": "header", "text": {"type": "plain_text", "text": ":white_check_mark: Collateral recovered", "emoji": True}},
        {"type": "section", "text": {"type": "mrkdwn", "text":
            "*Every ledger is back within the accepted baseline.* The book and the wallet agree "
            "again. No action needed."}},
        {"type": "section", "text": {"type": "mrkdwn", "text": "```" + "\n".join(table) + "```"}},
    ] + ([{"type": "actions", "elements": [
        {"type": "button", "text": {"type": "plain_text", "text": "View the run"}, "url": run_url}]}]
         if run_url else [])}]}


def unreadable_payload(detail, run_url=None):
    """The monitor could not read the book at all.

    Deliberately not styled as a collateral breach, because it is not one and confusing the two
    would be worse than either alone: this says nothing about whether the book matches the wallet.
    It says the check did not happen. That is its own incident - the metagraph may be down, the
    host unreachable, or METAGRAPH_L0_URL wrong - and it has to reach a person, because a monitor
    that goes quiet exactly when the thing it watches breaks is worse than no monitor at all.
    """
    return {"text": ":mag: PacaSwap collateral monitor could not read the book",
            "attachments": [{"color": COLOUR_BLIND, "blocks": [
                {"type": "header", "text": {"type": "plain_text",
                    "text": ":mag: Collateral monitor is blind", "emoji": True}},
                {"type": "section", "text": {"type": "mrkdwn", "text":
                    "*The check did not run.* This is not a collateral breach and says nothing "
                    "either way about the book - it means the monitor could not read it. The "
                    "metagraph may be down, the host unreachable, or the URL wrong."}},
                {"type": "section", "text": {"type": "mrkdwn", "text": "```" + detail + "```"}},
            ] + ([{"type": "actions", "elements": [
                {"type": "button", "text": {"type": "plain_text", "text": "View the run"},
                 "url": run_url, "style": "danger"}]}] if run_url else [])}]}


def post_slack(webhook, payload):
    req = urllib.request.Request(webhook, data=json.dumps(payload).encode(),
                                 headers={"Content-Type": "application/json"})
    with urllib.request.urlopen(req, timeout=30) as r:
        return r.status, r.read().decode()[:200]


def get(url, timeout=30):
    req = urllib.request.Request(url, headers={"Accept": "application/json"})
    try:
        with urllib.request.urlopen(req, timeout=timeout) as r:
            return json.loads(r.read().decode())
    except urllib.error.HTTPError as e:
        if e.code == 404 and url.endswith("/liquidity-pools"):
            # The base URL either includes the API version prefix or it does not, and getting it
            # wrong looks identical to the metagraph being down. Say which it is.
            base = url[: -len("/liquidity-pools")]
            alt = base[: -len("/v1")] if base.endswith("/v1") else base + "/v1"
            raise SystemExit(
                f"404 from {url}\n"
                f"METAGRAPH_L0_URL is probably wrong. Try {alt} instead.\n"
                f"The script appends /liquidity-pools, so set the API base and nothing more."
            )
        raise SystemExit(f"HTTP {e.code} from {url}: {e.reason}")
    except urllib.error.URLError as e:
        raise SystemExit(
            f"cannot reach {url}: {e.reason}\n"
            "If the metagraph is stopped this is expected. If it is running, check that the host "
            "is reachable from here and that its TLS certificate is valid for that name."
        )


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
    ap.add_argument("--slack-webhook", default=os.environ.get("SLACK_WEBHOOK_URL"),
                    help="post the alert here on breach. Defaults to $SLACK_WEBHOOK_URL.")
    ap.add_argument("--run-url", default=os.environ.get("RUN_URL"),
                    help="link included in the alert, normally the CI run")
    ap.add_argument("--previous-state", choices=["success", "failure", "unknown"], default="unknown",
                    help="conclusion of the previous run, so the alert fires on the transition "
                         "rather than every 15 minutes for as long as the breach lasts")
    ap.add_argument("--run-number", type=int, default=0,
                    help="used to send a quiet reminder every REMIND_EVERY runs while a breach persists")
    ap.add_argument("--simulate-breach", action="store_true",
                    help="fabricate a breach and exit non-zero, to prove the alerting path works "
                         "without waiting for a real one. Touches nothing and reads nothing.")
    args = ap.parse_args()

    if args.simulate_breach:
        # A drill. An alert nobody has ever seen fire is an alert nobody can trust.
        print("SIMULATED BREACH - this is a drill, no real reading was taken\n")
        print(f"{'ledger':<26}{'book':>22}{'wallet':>22}{'shortfall':>20}{'accepted':>20}")
        print(f"{'DAG':<26}{21585973.95064077:>22,.8f}{19944845.99137282:>22,.8f}"
              f"{1641127.95926795:>20,.8f}{0.0:>20,.8f}  BREACH")
        print("\nBREACH DAG: the book promises 1,641,127.95926795 more than accepted.")
        print("\nThis is a SIMULATION triggered by --simulate-breach. Nothing is wrong.")
        if args.slack_webhook:
            demo = {"DAG": 164112795926795}
            st, body = post_slack(args.slack_webhook, slack_payload(
                breaches=demo,
                reserves={"DAG": 2158597395064077},
                wallet={"DAG": 1994484599137282},
                shortfall=demo,
                accepted={"DAG": 0},
                run_url=args.run_url,
                drill=True))
            print(f"slack: {st} {body}")
        return 1

    if not args.pools_url and not args.pools_file:
        raise SystemExit("set METAGRAPH_L0_URL, or pass --pools-url / --pools-file")

    baseline = json.loads(BASELINE.read_text()) if BASELINE.exists() else {}
    accepted = baseline.get("accepted_shortfall", {})

    # A read that cannot happen used to exit here through SystemExit, before any Slack call: the
    # job went red and nobody was told. That is the failure mode this whole monitor exists to
    # remove, one level up. Catch it and alert, with the same transition suppression as a breach
    # so a long outage does not post every 15 minutes.
    try:
        shortfall, reserves, wallet = sample(args)
    except SystemExit as e:
        detail = str(e)
        print(detail)
        ongoing = args.previous_state == "failure"
        remind = ongoing and args.run_number and args.run_number % REMIND_EVERY == 0
        if not args.slack_webhook:
            print("slack: no webhook configured, nobody was told")
        elif ongoing and not remind:
            print("slack: suppressed, already reported as unreadable and not yet cleared")
        else:
            st, body = post_slack(args.slack_webhook, unreadable_payload(detail, args.run_url))
            print(f"slack: {st} {body} (unreadable)" + (" (periodic reminder)" if remind else ""))
        return 1

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
        ongoing = args.previous_state == "failure"
        remind = ongoing and args.run_number and args.run_number % REMIND_EVERY == 0
        if args.slack_webhook and (not ongoing or remind):
            st, body = post_slack(args.slack_webhook, slack_payload(
                breaches, reserves, wallet, shortfall, accepted, args.run_url))
            print(f"slack: {st} {body}" + (" (periodic reminder)" if remind else ""))
        elif ongoing:
            print("slack: suppressed, this breach was already reported and has not cleared")
        return 1

    print("\nevery ledger is within the accepted baseline")
    if args.slack_webhook and args.previous_state == "failure":
        st, body = post_slack(args.slack_webhook, recovery_payload(shortfall, accepted, args.run_url))
        print(f"slack: {st} {body} (recovered)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
