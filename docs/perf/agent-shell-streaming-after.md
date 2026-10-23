# Agent shell streaming after the local workarounds

Follow-up to `agent-shell-streaming-baseline.md`. `lisp/+agent-shell-perf.el`
works around three upstream agent-shell costs that grow with every streamed
event. `modules/mod-ai.el` turns on `+agent-shell-perf-mode` once agent-shell
loads. Upstream HEAD had the same code on 2026-10-23, so each workaround's
docstring names the function it covers and says when to delete it.

Measured on 2026-10-23 on the same 12-core Apple Silicon Mac as the baseline,
load average 5.4 to 6.2 from other work the whole time. Every row is the median
of 5 runs, range in brackets, with the two sides alternating within each round
in fresh batch Emacs processes. All runs use 15 ms pacing. Rows without
"interpreted" ran native code (see Caveats). `BENCH_OFF=perf`
turns the whole mode off, and `perf-NAME` removes one entry of
`+agent-shell-perf--advice`. Raw JSONL rows and the CPU profile are in
`agent-shell-streaming-after/`.

## Result

| variant | CPU ms | CPU share % | relabel ms | relabels | stalls > 50 ms | max stall ms | GC n | GC ms |
|---|---|---|---|---|---|---|---|---|
| mixed, upstream | 6353 (6103-6425) | 58 (57-59) | 4312 (3925-4360) | 1728 | 6 (4-7) | 60 (57-62) | 7 | 278 (271-291) |
| mixed, workarounds | 3304 (3138-3434) | 31 (29-32) | 359 (323-419) | 65 | 6 (5-7) | 69 (65-100) | 3 | 157 (147-168) |
| preload, upstream | 5162 (5129-5167) | 48 (47-48) | 3991 (3914-4057) | 583 | 4 (2-4) | 65 (60-68) | 3 | 134 (131-137) |
| preload, workarounds | 2043 (1852-2105) | 19 (17-20) | 343 (327-361) | 21 | 7 (5-9) | 69 (62-74) | 2 | 96 (82-99) |
| mixed interpreted, upstream | 10647 (10633-10687) | 97 (97-98) | 7532 (7341-7668) | 1140 | 29 (29-30) | 94 (90-128) | 21 | 1266 (1246-1279) |
| mixed interpreted, workarounds | 4677 (4448-4741) | 44 (41-44) | 544 (510-587) | 64 | 13 (11-14) | 129 (96-171) | 7 | 449 (433-457) |

Rows are from `final.jsonl`. CPU per turn falls by about half in every
scenario. The interpreted `mixed` run, which matches the live session, drops
from one saturated core to 44% and has half as many timer gaps over 50 ms.
Its longest single gap grew from 94 to 129 ms (not explained).
In the native runs the stall counts did not improve: they sit near the
50 ms threshold on both sides and moved in both directions between rounds.

Every run passed the harness's work checks: every chunk arrived, every shell
rendered its end marker, the transcript grew, and no new errors were logged.

## Each workaround

Each step was measured against the previous steps with the new piece toggled
off, 5 alternating rounds per side.

| step | scenario | before CPU ms | after CPU ms | share % | paired rounds lower |
|---|---|---|---|---|---|
| 1. Deferred relabel | mixed | 6976 (6945-7032) | 3183 (2941-3875) | 65 → 30 | 5 of 5 |
| 1. Deferred relabel | preload | 5439 (5381-5673) | 2337 (1337-2732) | 50 → 22 | 5 of 5 |
| 2. Batched transcript | mixed | 4180 (3423-4389) | 3522 (2657-3568) | 39 → 33 | 5 of 5 |
| 2. Batched transcript | preload | 2811 (2454-2843) | 2110 (1845-2119) | 26 → 20 | 5 of 5 |
| 3. Cache directory | mixed | 3479 (3447-3525) | 3360 (3172-3401) | 33 → 31 | 5 of 5 |
| 3. Cache directory | preload | 2052 (2004-2102) | 2062 (2011-2066) | 19 → 19 | 2 of 5 |

1. **Deferred relabel** (`relabel.jsonl`). Upstream
   `agent-shell-chat--schedule-relabel` relabels the whole buffer on the next
   tick after every event. The workaround keeps that for `input-submitted`,
   `turn-complete`, `prompt-ready`, `session-restored`, `permission-request`,
   `permission-response` and `shell-maker-finish-output-hook`. Every other
   event schedules one relabel 0.5 s later. In `mixed`, relabels per turn went
   from 1729 to 66 and relabel time from 4996 to 241 ms. GC went from 7
   collections and 275 ms to 4 and 167 ms.
2. **Batched transcript** (`transcript.jsonl`). Upstream
   `agent-shell--append-transcript` stats the file and appends to it once per
   chunk. The workaround collects appends per shell and writes them in order
   one second after the first, at turn end, on buffer kill, at exit and before
   `agent-shell-open-transcript`. Transcript sizes are the same with and
   without it: 49338 bytes per `preload` turn, and 145651 or 145882 bytes per
   `mixed` turn on both sides.
3. **Cache directory** (`cache-dir.jsonl`). Upstream `agent-shell-cache-dir`
   calls `make-directory` on every markdown render. The workaround reuses a
   result for 5 s. The gain shows only in `mixed`, about 120 ms per turn. In
   `preload` it is inside the noise.

## Reverted

Rechecking the transcript's existence at most every 2 s, without batching,
did not move `mixed` (3946 against 4009 ms). `preload` was 2453 against 2618
ms, lower in 5 of 5 rounds but inside both ranges. Batching replaced it, since
batched writes check the file once per write anyway. Rows are in
`transcript-stat-reverted.jsonl`.

## Labels match upstream

The harness now records a SHA-1 of each shell's text and of its chat label
overlays (positions and all properties) at turn end. It also relabels once
more through upstream and checks that no label changes. Across all 110
runs here, with and without the workarounds and native or
interpreted, each scenario produced one text digest and one label digest,
and every run was already settled.

`test/agent-shell-perf-acp-test.el` checks the same over real ACP shells.
One test compares text and label overlays from the same turn with the mode
off and on. Another checks that the first agent label appears within the
relabel delay of the first chunk and well before turn end. Making every
relabel deferred fails the first test. A 5 s delay fails the second.

## What limits it now

One unreported profiled `mixed` run with every workaround on took 3047
samples (`agent-shell-streaming-after/profile-mixed.txt`).

| share | path |
|---|---|
| 55% | `agent-shell--update-fragment` per chunk |
| 34% | of which `agent-shell--render-markdown`, re-rendering the fragment's markdown, including code highlighting through `python-mode` |
| 10% | of which `text-property-search-*` in `agent-shell-ui-update-fragment` and `agent-shell-ui--group-header-range` |
| 7% | `agent-shell-chat--prompt-runs` in the deferred relabels |
| 4% | Automatic GC |
| 1% | `write-region` for the batched transcript |

Before the workarounds, relabeling took 51% and transcript writes 20%. Markdown
re-rendering per chunk is now the limiter. It is upstream work in
`agent-shell-markdown` and was not changed here.

## Caveats

- The baseline's caveats still apply. Batch Emacs has no redisplay, and core
  scheduling moves per-chunk numbers.
- `BENCH_LOAD=elc` runs here report `subr` for agent-shell functions in the
  harness's `code-types` check, so they ran native code from the eln cache.
  The baseline's `elc` runs report `byte-code-function`; the cache filled in
  between. Both sides of every comparison here ran the same code. Upstream
  `preload` here (5162 ms) is close to the baseline's native `preload`
  (5252 ms), not its byte-compiled one (5947 ms).
- The deferred relabel shows a new response label up to 0.5 s after its
  first chunk. A transcript reader sees streamed text up to 1 s late, but
  complete at turn end.
