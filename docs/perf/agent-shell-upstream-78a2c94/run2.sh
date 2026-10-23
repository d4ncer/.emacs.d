#!/bin/sh
# Usage: run2.sh RUNS OUT VARIANT...  VARIANT is NAME=SCENARIO:OFF:PACKAGES:EXTRA[,EXTRA]
set -eu
runs=$1 out=$2; shift 2
for round in $(seq "$runs"); do
  for v in "$@"; do
    name=${v%%=*}; rest=${v#*=}
    IFS=: read scenario off packages extra <<E2
$rest
E2
    loads="-l /tmp/as-upstream-bench/normcheck.el"
    for e in $(echo "$extra" | tr , ' '); do loads="$loads -l $e"; done
    line=$(BENCH_SCENARIO=$scenario BENCH_OFF=$off BENCH_LOAD=eln BENCH_PACKAGES=$packages \
      emacs -Q --batch -l /Users/raghuk/.emacs.d/test/bench-agent-shell-stream.el $loads \
      -f +bench-run 2>/dev/null | grep '^BENCH-RESULT ' || true)
    [ -z "$line" ] && { echo "round $round $name: no result" >&2; continue; }
    printf '%s\n' "${line#BENCH-RESULT }" | python3 -c 'import json,sys; r=json.load(sys.stdin); r["variant"]=sys.argv[1]; r["round"]=int(sys.argv[2]); print(json.dumps(r))' "$name" "$round" >> "$out"
    echo "round $round $name done" >&2
  done
done
echo ALLDONE >&2
