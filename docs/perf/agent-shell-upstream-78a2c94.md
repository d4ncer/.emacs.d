# Agent shell workarounds on upstream 78a2c94

Follow-up to `agent-shell-streaming-after.md`. Upstream agent-shell 78a2c94
("Find prompts by walking field boundaries #905") is origin HEAD on
2026-10-24. It is 191 commits past the installed 868850f and needs newer
dependencies (shell-maker 0.98.2, acp 0.17.1; installed are 0.97.3 and
0.15.2). This note asks which pieces of `lisp/+agent-shell-perf.el` still
pay for themselves on top of it.

Measured on 2026-10-24 on the same 12-core Apple Silicon Mac, load average
3.5 to 7.3 from other work. Every row is the median of 5 runs, range in
brackets, with all variants of a batch alternating within each round in fresh
batch Emacs processes, 15 ms pacing. All runs loaded native code
(`BENCH_LOAD=eln`). The upstream side was byte- and native-compiled from
agent-shell 78a2c94, shell-maker e089f07 and acp.el a0cb90d into a scratch
directory and loaded with `BENCH_PACKAGES`. Raw rows, the profile and the
patch are in `agent-shell-upstream-78a2c94/`.

## Result

`main.jsonl`. A is production today, B is upstream alone, C is upstream with
every workaround, and the "all but" rows turn one workaround off on upstream.

| variant | CPU ms | CPU share % | relabel ms | relabels | stalls > 50 ms | GC n | GC ms |
|---|---|---|---|---|---|---|---|
| mixed, A 868850f + all | 2059 (1496-2391) | 19 (14-22) | 204 (154-333) | 66 | 5 (0-6) | 3 | 138 (119-171) |
| mixed, B 78a2c94 alone | 3910 (3305-4622) | 36 (30-42) | 2271 (1901-2701) | 2099 | 1 (0-2) | 5 | 191 (168-235) |
| mixed, C 78a2c94 + all | 2234 (1558-2932) | 20 (14-27) | 878 (586-1161) | 566 | 2 (0-3) | 3 | 131 (97-161) |
| mixed, all but relabel | 3250 (2878-3842) | 30 (26-35) | 2178 (1943-2698) | 2132 | 1 (0-4) | 5 | 198 (163-245) |
| mixed, all but transcript | 2878 (2183-2960) | 26 (20-27) | 943 (639-994) | 566 | 0 (0-3) | 4 | 132 (111-171) |
| mixed, all but cache-dir | 1912 (1620-2643) | 18 (14-24) | 782 (614-1060) | 561 | 1 (0-3) | 3 | 113 (99-150) |
| preload, A 868850f + all | 1406 (1016-1705) | 13 (9-16) | 198 (134-261) | 22 | 1 (0-4) | 1 | 41 (37-80) |
| preload, B 78a2c94 alone | 2796 (2376-3274) | 26 (22-30) | 1877 (1578-2255) | 679 | 2 (0-2) | 3 | 106 (96-124) |
| preload, C 78a2c94 + all | 1425 (1114-1847) | 13 (10-17) | 703 (544-928) | 187 | 1 (0-3) | 1 | 39 (35-58) |
| preload, all but relabel | 2747 (2566-3146) | 25 (23-29) | 2071 (1880-2406) | 681 | 2 (1-3) | 3 | 135 (94-138) |
| preload, all but transcript | 1657 (1558-2454) | 15 (15-22) | 655 (637-1051) | 182 | 1 (0-2) | 2 | 71 (37-73) |
| preload, all but cache-dir | 1453 (1141-2362) | 13 (11-22) | 730 (518-1274) | 184 | 0 (0-3) | 1 | 58 (34-72) |

Upstream alone (B) costs 3910 ms per `mixed` turn against 6353 ms for
868850f alone in the previous note. One relabel now takes about 1.1 ms
instead of 2.5 ms. Upstream still relabels on every event, so B uses about
twice the CPU of production.

## The relabel workaround half-misses on 78a2c94

C relabels 566 times per `mixed` turn where A relabels 66 times. Upstream
996c2af added `agent-shell-chat--schedule-relabel` to
`agent-shell-section-functions`, which runs after every
`agent-shell--update-fragment` with a range alist. That alist has no
`:event`, so `+agent-shell-perf--throttle-relabel` treats the call like
`shell-maker-finish-output-hook` and relabels on the next tick. The fix
treats only a call with no arguments as a boundary, so a section range
defers like a streaming event (`relabel-fix.patch`, two lines plus the
docstring). E below is C with that fix.

`fix.jsonl`, same method:

| variant | CPU ms | CPU share % | relabel ms | relabels | GC n |
|---|---|---|---|---|---|
| mixed, B 78a2c94 alone | 4185 (3699-4458) | 38 (34-40) | 2365 (2139-2672) | 2105 | 6 |
| mixed, C 78a2c94 + all | 2150 (2006-2888) | 20 (18-27) | 906 (844-1177) | 561 | 3 |
| mixed, E 78a2c94 + all, relabel fixed | 1629 (1353-2356) | 15 (12-22) | 127 (83-192) | 66 | 3 |
| mixed, E without transcript | 2336 (1736-2916) | 21 (16-27) | 137 (92-151) | 67 | 3 |
| mixed, E without cache-dir | 1484 (1143-1757) | 14 (10-16) | 116 (80-133) | 67 | 3 |
| preload, B 78a2c94 alone | 3115 (2420-3612) | 28 (22-33) | 2051 (1569-2375) | 682 | 4 |
| preload, C 78a2c94 + all | 1523 (1264-1707) | 14 (11-16) | 803 (555-851) | 189 | 1 |
| preload, E 78a2c94 + all, relabel fixed | 877 (728-979) | 8 (7-9) | 92 (65-106) | 22 | 2 |
| preload, E without transcript | 1150 (879-1219) | 11 (8-11) | 94 (62-101) | 22 | 1 |
| preload, E without cache-dir | 838 (731-1466) | 8 (7-14) | 78 (69-171) | 22 | 2 |

`final.jsonl` puts production next to the candidates. F is E without the
cache-dir workaround.

| variant | CPU ms | CPU share % | relabels |
|---|---|---|---|
| mixed, A production | 1737 (1571-2018) | 16 (14-18) | 67 |
| mixed, E | 1214 (1182-2021) | 11 (11-19) | 66 |
| mixed, F | 1593 (1123-2065) | 15 (10-19) | 66 |
| preload, A production | 1322 (1254-1574) | 12 (12-14) | 22 |
| preload, E | 1129 (787-1225) | 10 (7-11) | 22 |
| preload, F | 818 (695-957) | 7 (6-9) | 22 |

## Each workaround on 78a2c94

Paired per round, CPU ms with the piece off minus with it on.

| workaround | batch | mixed | preload |
|---|---|---|---|
| relabel, as shipped | main | +898 (678..2284), 5 of 5 | +1299 (957..1633), 5 of 5 |
| relabel fix on top | fix (C minus E) | +532 (385..654), 5 of 5 | +544 (399..979), 5 of 5 |
| transcript | main | +46 (1..1401), 5 of 5 | +468 (-103..608), 4 of 5 |
| transcript | fix | +708 (-620..1284), 4 of 5 | +171 (22..342), 5 of 5 |
| cache-dir | main | -236 (-856..233), 2 of 5 | -59 (-232..515), 2 of 5 |
| cache-dir | fix | -148 (-1078..129), 1 of 5 | -6 (-146..517), 2 of 5 |
| cache-dir | final (F minus E) | +132 (-229..411), 3 of 5 | -172 (-335..13), 1 of 5 |

- **Relabel** still carries most of the win, and needs the fix to do so.
- **Transcript** helps in 18 of 20 paired rounds. The size swings with the
  load on the machine, but it is never clearly zero.
- **Cache-dir** is noise. Its sign flips between batches, and turning it off
  costs more in only 11 of 30 paired rounds.

Stall counts sit near the 50 ms threshold for every variant, so they do not
separate anything. GC drops from 5 or 6 collections to 3 with the relabel
workaround on.

## Correctness

- Every run passed the work checks: all chunks arrived, every shell rendered
  its end marker, the transcript grew, no new errors were logged, and one more
  upstream relabel changed no label.
- `code-types`, `package-files` and `native-files` confirm each side ran what
  it claims. 78a2c94 rows load `/tmp/asup/*/*.elc` and run native code from
  the scratch eln directory, including `agent-shell-prompt-ranges`, which
  868850f lacks.
- On 78a2c94 the raw text digests differ between runs of one variant, with
  or without workarounds. In one diffed pair the only difference was one
  extra blank line after a streamed code block, probably from the new
  deferred markdown render (not traced). `normcheck.el` adds digests with blank-line runs
  collapsed. With it, every variant of a scenario has the same label digest,
  and the normalized text digests come from one shared pool that B, with no
  workarounds, also produces. The workarounds do not change what is rendered.
  Transcript bytes match A, except one `mixed` run at 145883 bytes, a size
  the previous note also saw on 868850f.
- `test/agent-shell-perf-acp-test.el` against 78a2c94 (`acp-upstream.txt`)
  passes 4 of 5 with both the shipped and the fixed file. The failing test
  is `transcript-recreated-after-deletion`. Upstream 07cb582 writes YAML
  frontmatter before `# Agent Shell Transcript`, so the test's
  `string-prefix-p` is stale. The recreated file holds the second turn and
  not the first, as intended.

## Signatures

Every function in `+agent-shell-perf--advice` still exists on 78a2c94 with
the same arguments. `agent-shell-chat--schedule-relabel`,
`agent-shell--emit-event`, `agent-shell-open-transcript` and
`agent-shell-cache-dir` have identical bodies. `agent-shell--append-transcript`
now disables the transcript on a write error instead of logging each one,
which batching passes through. `agent-shell-chat--relabel-timer` and
`agent-shell-chat--labeled` are unchanged. The one conflict is the new
`agent-shell-section-functions` caller described above.

## What limits it now

One unreported profiled `mixed` run of E (`profile-mixed-E.txt`, 997
samples, 295 idle and 71 in the harness's label checks after the turn).
Shares are of the remaining 631.

| share | path |
|---|---|
| 22% | `agent-shell--render-markdown`, mostly from the deferred render timer, including `python-mode` code highlighting |
| 20% | `agent-shell-ui-update-fragment`, of which `text-property-search-*` in `agent-shell-ui--group-header-range` |
| 15% | `+agent-shell-drive--parse`, `concat` per chunk in our drive watcher, interpreted in this harness |
| 10% | Automatic GC |
| 6% | `agent-shell-chat--relabel` in the deferred relabels |

Markdown rendering is still the largest piece and is upstream work. The drive
watcher's `concat` is local and is the next local target.

## Recommendation

Update agent-shell to 78a2c94 or later, shell-maker to e089f07 or later, and
acp to 0.17.1 or later. In the same change, apply
`relabel-fix.patch`, delete the cache-dir workaround, update the ACP test's
transcript header assertion, and keep the relabel and transcript workarounds.
Bumping without the patch lands near production today (A is 234 ms lower in
`mixed`, 5 of 5 rounds, and 274 ms higher in `preload`, 3 of 5), and leaves
about 530 ms per turn unrecovered.
