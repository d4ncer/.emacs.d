# Emacs 32 configuration fixes

Implementation follows the eight audit findings. Each step has a focused check
before the complete configuration gate.

| Step | Implementation | Verification |
| --- | --- | --- |
| 1 | Respect buffer kill vetoes before deleting files | Core ERT fixtures |
| 2 | Repair deferred integration activation and explicit hook names | Activation ERT and startup fixtures |
| 3 | Cache Dired and Vulpea model line data outside redisplay | Cache invalidation, remote probe and render ERT |
| 4 | Activate modes after package loading; defer Org until its own entry points | First input, source and Org file checks |
| 5 | Replace unused incremental loader with owned, retryable call triggers | Trigger lifecycle and failure ERT |
| 6 | Adopt native Emacs 32 tree-sitter installation/remapping and editable Grep | Native syntax, Nix grammar and exported search edit checks |
| 7 | Walk face inheritance lists with cycle protection | Core ERT fixtures |
| 8 | Own profiler hooks/timers and distinguish wall time from GC counters | Profiler lifecycle and file benchmark ERT |

The finder and agent project packages retain forwarding files at their old
paths. Existing sessions cache those locations for child worker processes.
`test/config-relocation-test.el` exercises both finder workers and the headless
agent worker through those entry points.

Run `emacs -Q --batch -l test/run-config.el` for the combined local gate.

## Verification completed

Verified on 2026-10-22 with Emacs 32.0.50, source revision
`1b85386156dd21647e31dcca4f7e81b8253ff69f`.

- Complete local gate: 108 ERT tests, all passing; syntax, startup and strict
  byte compilation gates passed.
- Disposable restore: all 92 locked package builds and restored startup passed;
  the complete gate passed again with 108 ERT tests.
- Separate macOS GUI: first completion, editor and Org activation, Dired counts
  and mode line rendering passed.
- Nix: the pinned grammar compiled and nix-ts-mode fontification passed in a
  temporary grammar directory.
- Native Python syntax benchmark: 300 functions, 0.102s first activation and
  0.204s total for 20 subsequent activations. This isolated measurement does not
  predict complete file visits with language servers and personal hooks.

The existing server was inspected without reloading its configuration. Restart
Emacs to activate the complete implementation. The old finder worker paths were
also tested independently with all 21 finder tests passing.
