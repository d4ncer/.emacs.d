# Emacs 32 configuration architecture

| Step | Implementation | Verification |
| --- | --- | --- |
| 1 | Record Emacs build identity, Elpaca recipes/commits and grammar recipes | Locked recipe resolution and disposable package rebuild |
| 2 | Use one verification command with fresh processes and disposable fixtures | Syntax, ERT, startup, worker and compilation gates |
| 3 | Establish local paths, macros and declaration conventions in explicit bootstrap | Cold startup and helper compilation |
| 4 | Separate local finder and agent packages; own hooks, timers, workers and teardown | Existing workflow tests, lifecycle tests and headless capture |
| 5 | Isolate Expert and eglot-tempel workarounds as removable adapters | Protocol scope, lifecycle and multiline snippet fixtures |
| 6 | Document module order, activation policy, dependencies and maintenance | README, MODULES.md and DEPENDENCIES.md cross checked against source |

The restored configuration must initialize Elpaca's use-package integration
before evaluating feature declarations. General is a synchronous bootstrap
dependency because modules use its declaration keywords.

`test/restore-config.el` copies this configuration, obtains the locked revisions
from local Git objects, rebuilds the packages and runs the complete gate. On
macOS it also starts a separate GUI Emacs for completion, source and Org
activation, Dired counts and model line rendering. The private configuration and
shell credential refresh are excluded from this disposable test.

## Verification completed

Verified on 2026-10-22 with the Emacs build recorded in `emacs-build.eld`.

- All 92 locked packages rebuilt in a disposable configuration directory.
- Restored startup passed, including synchronous use-package and General
  initialization and checks against eagerly loading Org, Org Agenda and Vulpea.
- The full local and restored gates each passed all 108 ERT tests, startup
  checks, headless capture, worker checks and strict byte compilation.
- A separate macOS GUI passed completion, source and Org activation, Dired
  counts and mode line rendering checks, then exited successfully.
- Compatibility forwarding paths passed real subprocess checks for both finder
  workers and the headless agent worker.

The restore uses local clone transport to reuse available objects from partial
repositories. It exercises package source builds and activation; it does not
rebuild Emacs itself or reinstall every existing grammar binary. Expert adapter
tests use protocol fixtures, so a real deployed server remains the removal gate
for those workarounds. See `docs/DEPENDENCIES.md` for maintenance instructions.
