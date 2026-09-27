# Agent-shell Code Review Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Review the active changeset against discovered requirements in a right sidebar, then answer questions, send selected fixes, and rerun review.

**Architecture:** Small standalone Elisp files capture the Git snapshot, resolve requirements, and validate reviewer output. A coordinator owns review sessions and state; a dedicated mode renders the sidebar and invokes actions. The package exposes optional hooks for notifications without depending on that package.

**Tech Stack:** Emacs Lisp, agent-shell public events and insertion API, Git CLI, ERT, `special-mode`.

**Spec:** [Agent-shell code review](../specs/2026-09-27-agent-shell-review-design.md)

## Global Constraints

- Ship `lisp/agent-shell-review.el` and focused helper files as a standalone package. Public symbols use `agent-shell-review-`; private symbols use `agent-shell-review--` or a helper's `agent-shell-review-<area>--` prefix.
- Depend on agent-shell and built-in Emacs libraries, never on personal `+` helpers or agent-review.
- Give new `.el` files a lexical-binding header, package metadata,
  `Commentary`, `Code`, `provide`, and footer sections.
- `C-u` selects a spec file; an agent-shell origin always wins over other project shells.
- Start each review pass in a fresh agent session; answers to questions continue that pass in the same session.
- Keep `modules/mod-ai.el` for package wiring and `modules/mod-keybindings.el` for the `SPC l r` leader binding.
- Run Elisp checks through `emacsclient`, never `emacs`. Before tests, use
  `emacsclient -a '' --eval '(emacs-pid)'` to connect or start a daemon.
- Implement the notifications plan first for Task 6 integration; review core
  must still load and work when that optional package is absent.
- Use the repository's `[scope] Imperative summary` commit subjects.

## Review Focus

These inputs need explicit tests in the owning tasks:

1. A feature branch pushed to its same-named upstream still reviews its commits against the integration branch (Task 1).
2. An untracked path with spaces and a binary change appears in the snapshot without corrupting the diff (Task 1).
3. An absolute or `~/.codex` spec path outside the project wins over a project candidate; ambiguous candidates prompt (Task 2).
4. A malformed response gets one reformat request, then a visible failure without a false clear result (Tasks 3–4).
5. A killed implementation shell causes selected fixes plus clarified criteria to reach a new shell (Task 5).

---

## File map

- `lisp/agent-shell-review-git.el`: base selection, diff and untracked capture, fingerprint.
- `lisp/agent-shell-review-spec.el`: transcript adapter, spec candidates, conversation fallback.
- `lisp/agent-shell-review-protocol.el`: prompt and strict result parser.
- `lisp/agent-shell-review.el`: public DWIM entry, run state, agent session/event handling, action commands.
- `lisp/agent-shell-review-ui.el`: sidebar mode, rendering, item navigation, marks, display policy.
- `test/agent-shell-review-*-test.el`: one ERT file for each helper and one for coordinator/UI behavior.
- `modules/mod-ai.el`, `modules/mod-keybindings.el`: local activation, optional notifications, leader binding.

Task 4 defines coordinator state and actions without a UI dependency. Task 5
loads the UI after those definitions; the UI calls them without requiring the
coordinator back.

Use this nonexiting ERT pattern with a running server, changing the test file and
regexp per task. It reloads source and returns `"PASS"` only when all matching
tests pass:

```sh
emacsclient --eval '(progn (load-file (expand-file-name "lisp/agent-shell-review-git.el" user-emacs-directory)) (load-file (expand-file-name "test/agent-shell-review-git-test.el" user-emacs-directory)) (let ((s (ert-run-tests-batch "^agent-shell-review-git-test-"))) (if (zerop (ert-stats-completed-unexpected s)) "PASS" (error "ERT failed"))))'
```

### Task 1: Capture a complete Git snapshot

**Files:** Create `lisp/agent-shell-review-git.el`; create `test/agent-shell-review-git-test.el`.

**Interfaces:** `agent-shell-review-git-snapshot (root &optional base-ref)`
returns `(:root ROOT :base REF :diff TEXT :fingerprint SHA1)` or signals
`user-error`. `agent-shell-review-git-current-fingerprint (snapshot)`
recomputes the fingerprint for stale detection.
`agent-shell-review-git-max-bytes` defaults to 524288.

- [ ] **Step 1: Write failing ERT tests** named
  `agent-shell-review-git-test-pushed-branch`,
  `agent-shell-review-git-test-working-tree`, and
  `agent-shell-review-git-test-untracked-binary-size`. Use disposable Git
  repositories with `main`, `feature`, and `origin/feature`; assert:

```elisp
(should (string-match-p "committed-feature" (plist-get snapshot :diff)))
(should (string-match-p "new file with spaces" (plist-get snapshot :diff)))
(should-error (agent-shell-review-git-snapshot empty-root) :type 'user-error)
```

  Include staged/unstaged hunks, binary metadata, oversize assertions, and a
  fingerprint change after editing a reviewed file.
- [ ] **Step 2: Run the Task 1 ERT command above**; expect missing source or function failure.
- [ ] **Step 3: Implement** the interface. Base precedence: explicit ref; on
  local `main`/`master`, its same-named tracking remote; otherwise remote
  default branch, then a sole `main`/`master`; prompt when ambiguous. Compare
  from merge base to working tree, add nonignored untracked files, and enforce
  the byte cap without truncation. Distinguish `git diff` exit 1 from a real
  Git error for untracked files.
- [ ] **Step 4: Run the Task 1 ERT command**; expect `"PASS"`.
- [ ] **Step 5: Check `check-parens`** on the new source through
  `emacsclient`; expect no error.

```sh
emacsclient --eval '(with-temp-buffer (insert-file-contents (expand-file-name "lisp/agent-shell-review-git.el" user-emacs-directory)) (emacs-lisp-mode) (check-parens) "PASS")'
```
- [ ] **Step 6: Commit** with `[agent-shell-review] Capture review changesets`.

### Task 2: Discover the requirements source

**Files:** Create `lisp/agent-shell-review-spec.el`; create `test/agent-shell-review-spec-test.el`.

**Interfaces:** `agent-shell-review-spec-resolve (root implementation-shell
&optional explicit-file)` returns `(:kind file|conversation :source PATH|BUFFER
:text TEXT)` or signals `user-error`. `agent-shell-review-spec-transcript-text
(shell)` is the isolated adapter for agent-shell transcript storage.
`agent-shell-review-spec-search-roots` is customizable; candidate search has
maximum depth 5 and source text has a 262144-byte cap.

- [ ] **Step 1: Write failing ERT tests** named
  `agent-shell-review-spec-test-referenced-external-file`,
  `agent-shell-review-spec-test-ambiguous-candidates`, and
  `agent-shell-review-spec-test-conversation-fallback`. Use temporary roots
  and stubs for transcript text and selection; assert:

```elisp
(should (eq (plist-get result :kind) 'file))
(should (equal (plist-get result :source) external-spec))
(should (= selection-count 1))
(should (eq (plist-get fallback :kind) 'conversation))
```

  Also assert explicit-file precedence and that missing or oversized sources
  report an error rather than returning truncated text.
- [ ] **Step 2: Run ERT** with
  `emacsclient --eval '(progn (load-file (expand-file-name "lisp/agent-shell-review-spec.el" user-emacs-directory)) (load-file (expand-file-name "test/agent-shell-review-spec-test.el" user-emacs-directory)) (ert-run-tests-batch "^agent-shell-review-spec-test-"))'`;
  expect missing source or function failure.
- [ ] **Step 3: Implement** resolution. Read `agent-shell--transcript-file`
  only in this adapter when bound and readable; otherwise use the shell
  buffer when it contains a usable conversation. Search referenced paths
  first, then project spec directories and configured `~/.codex`/`~/.claude`
  roots while skipping caches. Prompt only for ambiguous credible candidates.
- [ ] **Step 4: Run the Task 2 ERT command**; expect zero unexpected ERT
  results.
- [ ] **Step 5: Commit** with `[agent-shell-review] Discover review requirements`.

### Task 3: Define and validate the reviewer protocol

**Files:** Create `lisp/agent-shell-review-protocol.el`; create `test/agent-shell-review-protocol-test.el`.

**Interfaces:** `agent-shell-review-protocol-prompt (snapshot requirements
clarifications)` returns the review-only prompt string.
`agent-shell-review-protocol-parse (text root)` returns
`(:kind questions|findings|clear :items ITEMS)` or signals a parse error.
Each finding has `:priority`, `:file`, `:line`, `:title`, `:evidence`,
`:requirement`, and `:suggestion`.

- [ ] **Step 1: Write failing ERT tests** named
  `agent-shell-review-protocol-test-questions`, `...-priorities`,
  `...-clear`, and `...-malformed`. Include:

```elisp
(should (eq (plist-get (agent-shell-review-protocol-parse questions-json root) :kind) 'questions))
(should (equal (plist-get (car (plist-get findings :items)) :priority) "P1"))
(should-error (agent-shell-review-protocol-parse unsafe-path-json root))
(should (string-match-p "ask" (agent-shell-review-protocol-prompt snapshot source answers)))
```

  Cover plain/fenced JSON, `P0`–`P3`, positive lines, missing evidence,
  mixed questions/findings, snapshot, requirements, clarifications, and no-edit
  instruction.
- [ ] **Step 2: Run ERT** with
  `emacsclient --eval '(progn (load-file (expand-file-name "lisp/agent-shell-review-protocol.el" user-emacs-directory)) (load-file (expand-file-name "test/agent-shell-review-protocol-test.el" user-emacs-directory)) (ert-run-tests-batch "^agent-shell-review-protocol-test-"))'`;
  expect missing source or function failure.
- [ ] **Step 3: Implement** the two interfaces with `json-parse-string` and a single documented response schema. Preserve the raw response in the reviewer shell only.
- [ ] **Step 4: Run the Task 3 ERT command**; expect zero unexpected ERT
  results.
- [ ] **Step 5: Commit** with `[agent-shell-review] Validate reviewer results`.

### Task 4: Drive fresh reviewer sessions

**Files:** Create `lisp/agent-shell-review.el`; create `test/agent-shell-review-test.el`.

**Interfaces:** `agent-shell-review (&optional arg)` is the interactive entry
(`C-u` reads a file). A `cl-defstruct agent-shell-review--run` holds project,
implementation shell, reviewer shell, snapshot, requirements, clarifications,
status, streamed text, and one repair-attempt flag.
`agent-shell-review-status-change-hook` receives `(run status)`.
`agent-shell-review-reviewer-shell-p (buffer)` and
`agent-shell-review-sidebar-for-shell (buffer)` support optional notifications.

- [ ] **Step 1: Write failing ERT tests** named
  `agent-shell-review-test-origin-shell`, `...-fresh-session`,
  `...-questions-continue`, and `...-parse-repair`. Stub shell selection,
  session start, insertion, and event subscriptions; include:

```elisp
(should (eq (agent-shell-review--run-implementation-shell run) origin-shell))
(should (eq (agent-shell-review--run-reviewer-shell run) fresh-shell))
(should (= repair-requests 1))
(should (eq (agent-shell-review--run-status run) 'error))
```

  Also assert source-buffer ambiguity prompts, `C-u` selects the specified
  file, the initial prompt waits for readiness, questions continue the same
  session, status hooks fire once per transition, read-only mode is selected
  when advertised, and non-`end_turn` never clears.
- [ ] **Step 2: Run ERT** with
  `emacsclient --eval '(progn (load-file (expand-file-name "lisp/agent-shell-review.el" user-emacs-directory)) (load-file (expand-file-name "test/agent-shell-review-test.el" user-emacs-directory)) (ert-run-tests-batch "^agent-shell-review-test-"))'`;
  expect missing source or function failure.
- [ ] **Step 3: Implement** the run state and event handler using
  `agent-message-chunk`, `turn-complete`, `error`, and `clean-up`. Isolate
  `agent-shell--start :no-focus t :new-session t` in one adapter because the
  public starter focuses the shell. Use public `agent-shell-insert :submit t
  :no-focus t` after prompt readiness. Query advertised session modes and
  select read-only when available; otherwise retain the no-edit prompt.
- [ ] **Step 4: Run the Task 4 ERT command**; expect zero unexpected ERT
  results.
- [ ] **Step 5: Commit** with `[agent-shell-review] Run independent reviews`.

### Task 5: Render and act in the sidebar

**Files:** Create `lisp/agent-shell-review-ui.el`; modify `lisp/agent-shell-review.el`; modify `test/agent-shell-review-test.el`.

**Interfaces:** `agent-shell-review-mode` derives from `special-mode`.
`agent-shell-review-ui-show (run)` displays the right side window without
selecting it. `agent-shell-review-ui-render (run)` displays progress, questions,
findings, clear, stale, or error. Commands: `agent-shell-review-answer`,
`agent-shell-review-submit-answers`, `agent-shell-review-mark`,
`agent-shell-review-send-marked`, `agent-shell-review-rerun`, and
`agent-shell-review-open-reviewer`.

- [ ] **Step 1: Write failing ERT tests** named
  `agent-shell-review-test-sidebar-width`, `...-clarifications-rerun`,
  `...-replacement-fixer`, and `...-status-rendering`. On a sufficiently
  wide test frame, assert:

```elisp
(should (<= (window-width sidebar) (/ (frame-width) 3)))
(should (eq (selected-window) source-window))
(should (equal (agent-shell-review--run-clarifications new-run) answers))
(should (string-match-p "clarified criterion" replacement-prompt))
```

  Also assert unanswered questions block submission and marking alone sends
  nothing. The header/mode line shows progress and result states, a changed
  fingerprint marks old findings stale, and a parse failure shows a short
  error plus reviewer-shell action without copying raw output.
- [ ] **Step 2: Run the Task 5 ERT command**; expect failure.
- [ ] **Step 3: Implement** compact rows and expandable details. Bind `n/p`
  navigation, `RET` source visit, `TAB` details, `a` answer, `C-c C-c` submit
  answers, `m/u` marks, `S` send, `g` rerun, `v` reviewer shell, and `q` hide.
  Render immediately in `display-buffer-in-side-window` on the right with
  width capped at one third; retain old findings as stale during rerun.
- [ ] **Step 4: Run the Task 4 ERT command** with the new Task 5 tests;
  expect zero unexpected results.
- [ ] **Step 5: Manually check** a narrow and wide frame, question round,
  marked fix request, and rerun.
- [ ] **Step 6: Commit** with `[agent-shell-review] Add interactive review sidebar`.

### Task 6: Wire packages into this configuration

**Files:** Modify `modules/mod-ai.el`; modify `modules/mod-keybindings.el`.

**Interfaces:** `SPC l r` calls `agent-shell-review`; `mod-ai.el` configures
optional review status alerts through `agent-shell-notify-send`, suppresses
generic review-shell completion, and includes the review sidebar in related
focus buffers.

- [ ] **Step 1: Add deferred `use-package agent-shell-review :ensure nil`
  setup** and optional notify glue in `mod-ai.el`; add the leader binding in
  `mod-keybindings.el`. Keep the standalone review package free of personal
  helpers.
- [ ] **Step 2: Run all four review ERT files and the notification ERT file
  through `emacsclient`**; expect zero unexpected results.

```sh
emacsclient --eval '(progn (dolist (f (quote ("lisp/agent-shell-notify.el" "lisp/agent-shell-review-git.el" "lisp/agent-shell-review-spec.el" "lisp/agent-shell-review-protocol.el" "lisp/agent-shell-review.el" "lisp/agent-shell-review-ui.el" "test/agent-shell-review-git-test.el" "test/agent-shell-review-spec-test.el" "test/agent-shell-review-protocol-test.el" "test/agent-shell-review-test.el" "test/agent-shell-notify-test.el"))) (load-file (expand-file-name f user-emacs-directory))) (let ((a (ert-run-tests-batch "^agent-shell-review-")) (b (ert-run-tests-batch "^agent-shell-notify-"))) (if (and (zerop (ert-stats-completed-unexpected a)) (zerop (ert-stats-completed-unexpected b))) "PASS" (error "ERT failed"))))'
```

- [ ] **Step 3: Check parens and byte-compile** changed Elisp through
  `emacsclient`; expect no errors or warnings and keep generated `.elc` files
  out of the commit.

```sh
emacsclient --eval '(progn (dolist (f (quote ("lisp/agent-shell-review-git.el" "lisp/agent-shell-review-spec.el" "lisp/agent-shell-review-protocol.el" "lisp/agent-shell-review.el" "lisp/agent-shell-review-ui.el"))) (with-temp-buffer (insert-file-contents (expand-file-name f user-emacs-directory)) (emacs-lisp-mode) (check-parens)) (byte-compile-file (expand-file-name f user-emacs-directory))) "PASS")'
```
- [ ] **Step 4: Manually verify** `SPC l r` from an agent shell and a source
  buffer. With notify enabled, confirm one specialized alert per review
  result; with it disabled, confirm the sidebar still works. Verify the
  leader binding with `lookup-key`.
- [ ] **Step 5: Commit** with `[ai] Enable agent shell review workflow`.
