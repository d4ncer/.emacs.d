#!/bin/sh
# Alternate agent-shell streaming benchmark variants in fresh Emacs processes.
#
# Usage: test/bench-agent-shell-stream.sh RUNS OUT.jsonl VARIANT...
#        test/bench-agent-shell-stream.sh summary OUT.jsonl
# A VARIANT is SCENARIO[:OFF[,OFF...]][@LOAD], for example preload:chat or
# panel@el.  Each round runs every variant once, so drift favours none.
# BENCH_CHUNKS, BENCH_DELAY_MS and BENCH_PRELOAD_BYTES pass through.
set -eu
root=$(cd "$(dirname "$0")/.." && pwd)

summary() {
  python3 - "$1" <<'EOF'
import json, statistics, sys

rows = {}
for line in open(sys.argv[1]):
    result = json.loads(line)
    key = (result["variant"])
    rows.setdefault(key, []).append(result)

def cell(values):
    values = [v for v in values if v is not None]
    if not values:
        return "-"
    return "%.1f (%.1f-%.1f)" % (statistics.median(values), min(values),
                                 max(values))

columns = [
    ("runs", None),
    ("lead KB", lambda r: r["lead-bytes-before"] / 1000),
    ("turn wall ms", lambda r: max(r["wall-ms"])),
    ("chunk ms p50", lambda r: r["chunk"]["median"]),
    ("chunk ms p95", lambda r: r["chunk"]["p95"]),
    ("max stall ms", lambda r: r["stall-max-ms"]),
    ("stalls>50ms", lambda r: r["stalls-over-50ms"]),
    ("CPU share %", lambda r: 100 * r["cpu-share"]),
    ("relabel ms total", lambda r: r["relabel"]["total"]),
    ("GC n", lambda r: r["gc-count"]),
    ("GC ms", lambda r: r["gc-ms"]),
]
print("| variant | " + " | ".join(name for name, _ in columns) + " |")
print("|---" * (len(columns) + 1) + "|")
for key, results in rows.items():
    cells = []
    for name, get in columns:
        cells.append(str(len(results)) if get is None
                     else cell([get(r) for r in results]))
    print("| %s | %s |" % (key, " | ".join(cells)))
bad = [(r["variant"], r["checks"]) for rs in rows.values() for r in rs
       if r["checks"]["chunks-seen"] != r["checks"]["chunks-expected"]
       or r["checks"]["ends-rendered"] != r["shells"]
       or r["checks"]["new-errors"]]
print("\nfailed checks: %d" % len(bad))
for variant, checks in bad:
    print(variant, checks)
EOF
}

if [ "$1" = summary ]; then
  summary "$2"
  exit 0
fi

runs=$1 out=$2
shift 2
for round in $(seq "$runs"); do
  for variant in "$@"; do
    load=elc
    case $variant in *@*) load=${variant#*@} ;; esac
    spec=${variant%@*}
    scenario=${spec%%:*}
    off=
    case $spec in *:*) off=${spec#*:} ;; esac
    line=$(BENCH_SCENARIO=$scenario BENCH_OFF=$off BENCH_LOAD=$load \
             emacs -Q --batch -l "$root/test/bench-agent-shell-stream.el" \
             -f +bench-run 2>/dev/null | grep '^BENCH-RESULT ' || true)
    if [ -z "$line" ]; then
      echo "round $round $variant: no result" >&2
      continue
    fi
    printf '%s\n' "${line#BENCH-RESULT }" |
      python3 -c 'import json,sys; r=json.load(sys.stdin); r["variant"]=sys.argv[1]; r["round"]=int(sys.argv[2]); print(json.dumps(r))' \
        "$variant" "$round" >> "$out"
    echo "round $round $variant done" >&2
  done
done
summary "$out"
