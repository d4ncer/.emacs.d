# Agent shell streaming baseline

Baseline for the report that Emacs stops responding to typing while several
agent shells stream. Measured on 2026-10-23 with GNU Emacs 32.0.50 on a
12-core Apple Silicon Mac, load average 4.5 to 5.9 from other work the whole
time. Every number is the median of 5 runs per variant, with the range in
brackets. Variants alternate within each round in fresh batch Emacs processes.

Rerun with `test/bench-agent-shell-stream.sh`. The raw JSONL rows and the CPU
profile are in `agent-shell-streaming-baseline/`.

## What the harness runs

`test/bench-agent-shell-stream.el` starts real agent shells against
`test/fixtures/agent-shell-projects-agent.py` over ACP. It loads the compiled
`elpaca/builds` packages (`BENCH_LOAD=elc`) and the `lisp/` sources, the
same way production loads them. It turns on what `modules/mod-ai.el` turns
on: `agent-shell-chat-mode` (on by default upstream), transcripts at the
default path, `agent-shell-notify-mode` (its send function is stubbed so no
desktop notifications fire), `+agent-shell-pstack-guard-mode`,
`+agent-shell-pstack-panel-mode`, `+agent-shell-mermaid-mode`, Evil with
shells starting in insert state, and `gc-cons-threshold` of 16 MB from
`mod-core.el`. The math renderer stays off. The live session has not loaded it,
and it is not in `agent-shell-mode-hook` there.

The measured turn sends 600 `agent_message_chunk` notifications of 8 to 72
characters each. The text is markdown with headings, lists, links, two code
fences and a table, about 24 KB per turn. The turn also has 5 thought chunks
and 8 `tool_call` / `tool_call_update` pairs with 3 KB of output each. The
fixture sleeps `BENCH_DELAY_MS` between chunks, 15 ms by default, which is
about 50 chunks a second per shell.

Scenarios:

- **fresh.** One pstack lead shell (`+agent-shell-pstack-mode` on, so the
  drive watches it).
- **preload.** The same lead, first grown to 2.45 MB by replaying unmeasured
  turns through the same ACP path. Each replay turn has 300 chunks, 30 tool
  calls with 6 KB output and 20 thought chunks, at no delay. That matches the
  live 2.8 MB transcript, which is mostly tool output (415 tool calls, 109
  agent messages, 9 prompts).
- **panel.** The lead plus two read-only panel members started with
  `+agent-shell-drive-start :read-only t` and watched by the drive. All
  three stream at once.
- **mixed.** `panel` with the lead preloaded to 2.45 MB. This is the closest
  to the live session.

Toggles, applied one at a time to `mixed`. `chat` sets
`agent-shell-chat-mode-enabled` to nil. `transcript` sets
`agent-shell-transcript-file-path-function` to nil. `drive` skips pstack mode
and the member watches. `render` leaves `+agent-shell-mermaid-mode` off.

Metrics:

- **turn wall ms.** Time from submit to the last shell's `turn-complete`.
- **chunk ms.** Time spent inside `agent-shell--on-notification` for each
  `agent_message_chunk`.
- **max stall ms.** The longest gap between runs of a 10 ms repeating timer
  during the turn. This stands in for "cannot type".
- **CPU share.** The process's CPU time divided by elapsed time. Near 100%
  means Emacs is saturated and input waits behind it.
- **relabel ms.** Time spent in `agent-shell-chat--relabel-buffer`.
- **GC.** The deltas in `gcs-done` and `gc-elapsed`.

Each run checks its own work. It checks that every chunk reached the handler
(`chunks-seen` against `chunks-expected`), that each shell rendered the
`BENCH-END` marker, that the transcript grew only when it was on, that the
drive saw turn text only when it was on, that chat labels applied only when on,
and that no new errors were logged. All 105 runs passed every check.

## Results at 15 ms pacing (unsaturated)

| variant | lead KB | turn wall ms | chunk ms p50 | chunk ms p95 | max stall ms | stalls > 50 ms | CPU share % | CPU ms | relabel ms | GC n | GC ms |
|---|---|---|---|---|---|---|---|---|---|---|---|
| fresh | 0.2 | 10405 (10369-10551) | 2.2 (1.8-2.8) | 5.2 (4.3-8.1) | 61 (58-69) | 1 (1-5) | 23.5 (18.8-28.3) | 2469 (2005-2970) | 751 (598-998) | 4 | 127 (95-165) |
| preload | 2452 | 10743 (10506-10786) | 1.0 (0.8-1.1) | 1.9 (1.7-2.1) | 59 (54-65) | 3 (2-5) | 50.1 (47.6-52.5) | 5445 (5187-5704) | 4394 (3979-4490) | 4 | 161 (156-166) |
| panel | 0.2 | 10536 (10389-10609) | 1.5 (0.9-1.5) | 3.1 (2.6-4.0) | 54 (44-68) | 1 (0-2) | 44.3 (35.9-46.3) | 4713 (3850-4927) | 1530 (1263-1644) | 8 | 224 (201-281) |
| mixed | 2452 | 10795 (10762-11072) | 0.7 (0.6-0.8) | 1.4 (1.3-1.5) | 62 (58-65) | 4 (3-5) | 65.3 (56.3-69.2) | 7122 (6290-7576) | 4966 (4299-5352) | 6 | 254 (251-280) |
| mixed, chat off | 2452 | 10564 (10512-10686) | 1.3 (1.1-1.7) | 3.1 (3.0-4.5) | 64 (56-130) | 6 (2-8) | 32.1 (31.6-40.0) | 3446 (3377-4254) | 0 | 3 | 131 (128-192) |
| mixed, transcript off | 2452 | 10759 (10717-10840) | 0.4 (0.4-0.4) | 1.1 (1.1-1.1) | 58 (56-62) | 4 (3-4) | 61.1 (58.8-64.2) | 6612 (6390-7003) | 5076 (5007-5377) | 6 | 245 (236-251) |
| mixed, drive off | 2452 | 10772 (10723-10793) | 0.7 (0.7-0.8) | 1.4 (1.4-1.5) | 62 (60-69) | 3 (2-4) | 62.9 (61.8-68.6) | 6816 (6716-7484) | 4723 (4678-5327) | 6 | 256 (252-268) |
| mixed, render off | 2452 | 10772 (10763-10834) | 0.7 (0.7-0.8) | 1.4 (1.4-1.5) | 62 (61-68) | 4 (3-6) | 63.6 (62.8-69.1) | 6952 (6826-7557) | 4846 (4729-5308) | 6 | 262 (256-270) |

Turn wall time is set by the fixture's pacing (600 × 15 ms is 9 s), so it
cannot separate variants. CPU ms and CPU share do.

Relabel cost per call depends on buffer size. It is 0.7 to 1.1 ms on a fresh
shell and 7.2 ms (median) on the 2.45 MB lead. It runs once per chunk
event: 587 relabels for 601 chunks in `preload`.

### Interpreted, byte-compiled and native code

The live Emacs loaded agent-shell before the rebuild. `(type-of (symbol-function
'agent-shell--on-notification))` returns `interpreted-function` there, and so
does `agent-shell-chat--relabel`. `BENCH_LOAD=el` reproduces that state.
`BENCH_LOAD=eln` reads agent-shell, acp, shell-maker, markdown-mode and Evil
native code from a scratch eln cache. Native code is what a restarted
session runs once JIT compilation finishes. `lisp/` stays interpreted in every
mode, the same as in production.

| variant | CPU share % | CPU ms | relabel p50 ms | relabels | max stall ms | stalls > 50 ms | GC n | GC ms |
|---|---|---|---|---|---|---|---|---|
| preload, interpreted | 85.8 (82.9-86.5) | 9362 (9123-9453) | 15.2 | 455 | 119 (91-137) | 23 (22-24) | 16 | 1003 (980-1025) |
| preload, byte-compiled | 54.8 (49.7-55.6) | 5947 (5408-6034) | 8.0 | 583 | 58 (55-62) | 3 (2-4) | 4 | 167 (155-170) |
| preload, native | 48.2 (46.8-48.4) | 5252 (5070-5298) | 6.7 | 588 | 66 (63-68) | 3 (2-3) | 3 | 144 (136-174) |
| mixed, interpreted | 97.4 (97.3-97.8) | 10617 (10613-10661) | 1.5 (all shells) | 1173 | 90 (89-95) | 30 (29-31) | 21 | 1234 (1228-1248) |
| mixed, byte-compiled | 63.4 (63.2-63.9) | 6931 (6900-6962) | 0.7 (all shells) | 1734 | 71 (52-78) | 3 (1-5) | 7 | 260 (254-310) |

The interpreted `mixed` run, which matches the live session, holds one core
at 97% for the whole turn. It also has 30 timer gaps over 50 ms.

## Results at 5 ms pacing (saturated)

At 200 chunks a second per shell, every variant except chat off saturates
(91 to 96% CPU). Saturated runs coalesce relabels and flatten the
differences. For example, interpreted `preload` gets through only 137
relabels against 461 compiled. Use these rows to see the saturation, not to
rank fixes.

| variant | CPU share % | CPU ms | relabels | relabel share of CPU |
|---|---|---|---|---|
| mixed | 95.1 (94.8-95.6) | 3695 | 866 | 55% |
| mixed, chat off | 51.5 (50.9-52.9) | 2009 | 0 | 0% |
| mixed, transcript off | 95.6 (95.4-95.8) | 3729 | 980 | 65% |
| mixed, drive off | 95.3 (93.9-95.6) | 3704 | 889 | 56% |
| mixed, render off | 95.1 (94.9-95.5) | 3713 | 814 | 54% |
| preload, interpreted | 93.6 (91.6-94.3) | 3666 | 137 | 65% |
| preload, byte-compiled | 94.6 (94.0-95.2) | 3685 | 461 | 76% |
| preload, native | 91.1 (89.6-92.7) | 3504 | 490 | 73% |

## Profile of `mixed`, byte-compiled, 15 ms pacing

One unreported run with `profiler-start 'cpu` took 4396 samples. The full
report is `agent-shell-streaming-baseline/profile-mixed.txt`.

| share | path | source |
|---|---|---|
| 51% | `agent-shell-chat--relabel-buffer` from a zero-delay timer | `agent-shell-chat-mode.el:969-987`, scheduled for every event by the subscription at `:1040-1042` |
| 46% | `agent-shell-chat--label-prompts` → `agent-shell-chat--prompt-runs` | `agent-shell-chat-mode.el:474-490` walks every `font-lock-face` change from `point-min` |
| 27% | `next-single-property-change` (leaf) inside `prompt-runs` | `agent-shell-chat-mode.el:484` |
| 4% | `agent-shell-chat--label-responses` (`re-search-forward` for markers) | `agent-shell-chat-mode.el:820` |
| 20% | `agent-shell--append-transcript` per chunk | `agent-shell.el:3103-3112`, `:10932-10936` |
| 10% | `write-region` (append per chunk) | `agent-shell.el:10936` |
| 9% | `file-exists-p` in `agent-shell--ensure-transcript-file` per chunk | `agent-shell.el:10860` |
| 10% | `agent-shell--update-fragment` | `agent-shell.el:5064` |
| 4% | `agent-shell-cache-dir` → `make-directory` on every markdown render | `agent-shell.el:375`, `:3796` |
| 3% | Automatic GC | |
| below 0.5% | `+agent-shell-drive--parse`, mermaid render hook, notify | `lisp/` |

## Caveats

- **Core type changes the per-chunk numbers.** At low load macOS keeps a batch
  process on efficiency cores. One flood run (`BENCH_DELAY_MS=0`, fresh, two
  runs) gives a chunk p50 of 0.5 ms. The 15 ms pacing gives 2.5 ms. Forcing
  efficiency cores with `taskpolicy -c background` gives 1.9 ms. So chunk ms
  falls as CPU share rises across variants (fresh 2.2, mixed 0.7), and
  chunk ms cannot be compared across variants with different load. CPU ms
  within a run, CPU share, and the relabel share are the robust comparisons.
  The GUI Emacs probably runs at a higher QoS on performance cores (not
  measured), so its absolute costs may be lower than these.
- **Batch Emacs has no redisplay.** Fontification and redisplay of a visible
  shell are missing from every number here. The production cost can only be
  higher than this.
- **Not exercised.** The agent-shell-projects 1 s inbox poll. The live
  session has not loaded `+agent-shell-projects`, and no poll timer is in its
  `timer-list`. The math renderer was not exercised either, for the reason
  given above.
