# Agent-shell Notifications Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Show a macOS alert when an agent-shell agent finishes, needs approval, or fails.

**Architecture:** A global minor mode subscribes to each agent-shell buffer and maps ACP events to alert messages. Delivery, focus suppression, and review-specific overrides are public extension points, so the package remains usable on its own.

**Tech Stack:** Emacs Lisp, agent-shell event API, ERT, macOS `osascript`.

**Spec:** [Agent-shell system notifications](../specs/2026-09-27-agent-shell-notifications-design.md)

## Global Constraints

- Create `lisp/agent-shell-notify.el` as a standalone package; prefix public symbols `agent-shell-notify-` and internal symbols `agent-shell-notify--`.
- Depend only on agent-shell and built-in Emacs libraries; do not use personal `+` helpers or require the review package.
- Give new `.el` files a lexical-binding header, package metadata,
  `Commentary`, `Code`, `provide`, and footer sections.
- Keep `modules/mod-ai.el` to activation and preferences; no new `init.el` module.
- Run Elisp checks through `emacsclient`, never `emacs`. Before tests, use
  `emacsclient -a '' --eval '(emacs-pid)'` to connect or start a daemon.
- Use the repository's `[scope] Imperative summary` commit subjects.

## Review Focus

These conditions need explicit tests in the owning tasks:

1. Enabling the mode after a shell exists attaches once; re-enabling does not duplicate alerts (Task 1).
2. Killing a shell or disabling the mode removes subscriptions (Task 1).
3. A selected shell suppresses alerts only while its frame has focus (Task 2).
4. An error arriving after completion replaces the completion alert for that turn (Task 2).
5. Agent text with quotes or newlines remains data in the `osascript` invocation (Task 2).

---

## File map

- `lisp/agent-shell-notify.el`: mode, event mapping, alert policy, and sender.
- `test/agent-shell-notify-test.el`: focused ERT tests with stubbed agent-shell subscriptions and sender.
- `modules/mod-ai.el`: deferred activation after agent-shell loads.

For each ERT run below, use a running Emacs server and this command after
loading the package and test file in that server:

```sh
emacsclient --eval '(progn (load-file (expand-file-name "lisp/agent-shell-notify.el" user-emacs-directory)) (load-file (expand-file-name "test/agent-shell-notify-test.el" user-emacs-directory)) (let ((s (ert-run-tests-batch "^agent-shell-notify-test-"))) (if (zerop (ert-stats-completed-unexpected s)) "PASS" (error "ERT failed"))))'
```

### Task 1: Subscribe and classify agent events

**Files:** Create `lisp/agent-shell-notify.el`; create `test/agent-shell-notify-test.el`.

**Interfaces:**
- `agent-shell-notify--classify (event)` returns nil or a plist with `:kind` and `:body`.
- Global `agent-shell-notify-mode` attaches to `agent-shell-buffers` and `agent-shell-mode-hook`; subscriptions are stored per shell and removed on cleanup/disable.
- `agent-shell-notify--handle (shell-buffer event)` consumes the classification and calls the delivery interface defined in Task 2.

- [ ] **Step 1: Write failing ERT tests** named `agent-shell-notify-test-classify` and `agent-shell-notify-test-subscription-lifecycle`. Include these assertions, plus `max_tokens`/error classification and fake subscribe/unsubscribe counts:

```elisp
(should (eq (plist-get (agent-shell-notify--classify '((:event . permission-request))) :kind) 'permission))
(should (eq (plist-get (agent-shell-notify--classify '((:event . turn-complete) (:data . ((:stop-reason . "end_turn"))))) :kind) 'ready))
(should (= subscribe-count 1))
(should (= unsubscribe-count 1))
```

- [ ] **Step 2: Run the ERT command above**; expect `file-missing`
  because the package has not been created.
- [ ] **Step 3: Implement** the named interfaces in `lisp/agent-shell-notify.el`. Use one subscription per buffer and avoid reading private agent-shell state.
- [ ] **Step 4: Run the same ERT command**; expect `"PASS"`.
- [ ] **Step 5: Commit** the package and tests with `[agent-shell-notify] Classify agent events`.

### Task 2: Deliver alerts with focus and duplicate policy

**Files:** Modify `lisp/agent-shell-notify.el`; modify `test/agent-shell-notify-test.el`.

**Interfaces:**
- `agent-shell-notify-send (title body &optional relevant-buffers)` applies the focus rule and calls `agent-shell-notify-send-function`.
- `agent-shell-notify-send-function` defaults to `agent-shell-notify--macos-send (title body)`; tests bind a fake sender.
- `agent-shell-notify-suppress-event-function (shell-buffer event)` defaults to nil and lets an optional integration suppress generic events.
- `agent-shell-notify-related-buffers-function (shell-buffer)` returns buffers whose selection suppresses the alert; by default it returns the shell.

- [ ] **Step 1: Write failing ERT tests** named
  `agent-shell-notify-test-focus-and-dedupe` and
  `agent-shell-notify-test-osascript-arguments`. Bind a fake sender and
  stub frame focus; include these assertions and cases for an error replacing
  pending completion, a review-shell event suppressed by the optional
  predicate, a selected related sidebar, and missing `osascript`:

```elisp
(should (null sent))              ; focused, selected shell
(should (= (length sent) 1))      ; unfocused or unselected shell
(should (equal (car (last argv)) "say \"hi\"\nnext"))
(should-not (string-match-p "say hi" fixed-script))
```
- [ ] **Step 2: Run the ERT command above**; expect the new tests to fail.
- [ ] **Step 3: Implement** the four interfaces and the nonblocking `osascript` sender. Use a fixed AppleScript source with `on run argv`; pass title/body as separate process arguments. Delay a completion alert briefly (250 ms) so an error from the same turn can cancel and replace it.
- [ ] **Step 4: Add deferred activation** in `modules/mod-ai.el` with `use-package agent-shell-notify :ensure nil :after agent-shell`. Do not require the review package.
- [ ] **Step 5: Run the ERT command**; expect `"PASS"`.
- [ ] **Step 6: Check syntax and byte-compile** with `emacsclient`
  (`check-parens` on the source and `byte-compile-file` on changed Elisp);
  expect no errors or warnings. Keep generated `.elc` files out of the commit.

```sh
emacsclient --eval '(progn (with-temp-buffer (insert-file-contents (expand-file-name "lisp/agent-shell-notify.el" user-emacs-directory)) (emacs-lisp-mode) (check-parens)) (byte-compile-file (expand-file-name "lisp/agent-shell-notify.el" user-emacs-directory)))'
```
- [ ] **Step 7: Manually verify** permission, completion, and error alerts in
  a disposable shell, including the focused-shell suppression rule.
- [ ] **Step 8: Commit** with `[agent-shell-notify] Deliver system alerts`.
