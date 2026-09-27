# Direct ACP Review Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Run reviews through a dedicated ACP client, then send selected fixes to the exact Emacs implementation session that requested the review.

**Architecture:** A transport helper owns the ACP process and exposes a small event interface to the existing review coordinator. The coordinator and sidebar retain review state without depending on agent-shell; an optional Emacs adapter captures and sends to the original agent-shell buffer.

**Tech Stack:** Emacs Lisp 30.1+, local `acp.el`, `agent-shell` only in the optional adapter, Evil, ERT.

**Spec:** [Direct ACP reviewer with main-session handoff](../specs/2026-09-28-direct-acp-review-design.md)

## Global Constraints

- Keep `agent-shell-review`, `SPC l r`, the existing Git snapshot/protocol schema, and the right sidebar.
- The core review files must not require or call agent-shell; the adapter alone may do so.
- A review must create no reviewer `agent-shell-mode` buffer and must never create a replacement implementation shell.
- Default to `codex-acp` with login authentication; keep command, arguments, environment, and optional model independently configurable.
- Select an advertised read-only mode before the first prompt; fail visibly if none exists or selection fails.
- Advertise no ACP write capability, reject write requests, decline permission requests, and restrict ACP reads to the project or explicit requirements file.
- Keep the 524288-byte diff cap and 262144-byte requirements cap; reject excess input without truncation.
- Keep sidebar Evil normal state, normal Doom modeline, no inline key hints, and stable point during redraws.
- Preserve the existing uncommitted sidebar UX edits in `lisp/agent-shell-review-ui.el`, `modules/mod-ai.el`, and `test/agent-shell-review-test.el`; integrate them during migration.

## Review Focus

1. ACP read request traverses a symlink outside the project: deny it unless it is the explicitly selected requirements file (Task 2 test).
2. An old ACP callback fires after a rerun or buffer kill: do not alter the active sidebar or issue a new prompt (Task 3 test).
3. A prompt response lacks `end_turn` or an agent chunk has non-text content: report an error or ignore the chunk, never infer a clear review (Tasks 2 and 3 tests).
4. The captured implementation buffer is busy, unready, or dead when `S` is pressed: retain marks and prompt for retry or copy (Tasks 4 and 5 tests).
5. No spec file or usable origin context exists: ask for a file or entered multiline criteria before starting ACP (Task 1 test).

---

## File Structure

- Modify `lisp/agent-shell-review-spec.el`: discover requirements from plain origin text and prompt for a missing source.
- Create `lisp/agent-shell-review-acp.el`: own the reviewer ACP client, request policy, session lifecycle, and retained diagnostics.
- Modify `lisp/agent-shell-review.el`: coordinate snapshots, ACP events, questions, results, reruns, cancellation, and origin callback data.
- Modify `lisp/agent-shell-review-ui.el`: display and act on coordinator state, send marked fixes through a callback, show diagnostics, and cancel on buffer kill.
- Create `lisp/agent-shell-review-agent-shell.el`: optional provider for origin context and exact-buffer fix delivery.
- Modify `modules/mod-ai.el`: install the provider, Evil bindings, and notification callback without reviewer-shell hooks.
- Modify `test/agent-shell-review-spec-test.el` and `test/agent-shell-review-test.el`; create `test/agent-shell-review-acp-test.el` and `test/agent-shell-review-agent-shell-test.el`.
- Leave `lisp/agent-shell-review-git.el`, `lisp/agent-shell-review-protocol.el`, and the leader binding in `modules/mod-keybindings.el` intact unless a focused test exposes a defect.

## Task 1: Requirements from plain context

**Files:** Modify `lisp/agent-shell-review-spec.el`; test `test/agent-shell-review-spec-test.el`.

**Interfaces:** Produce `(agent-shell-review-spec-resolve root origin-text &optional explicit-file) -> plist` with `:kind`, `:source`, `:text`. `origin-text` is a string or nil, never a shell buffer. A prompted text source has `:kind 'entered` and `:source "entered requirements"`; file and conversation sources retain their existing kinds. The function signals `user-error` on cancellation, unreadable files, or input above 262144 bytes.

- [ ] **Step 1: Write failing ERT tests.** Update the four existing spec tests to pass transcript text directly; assert explicit file precedence, referenced external spec selection, search ambiguity, user-section extraction, and byte cap. Add `agent-shell-review-spec-test-prompted-source`: with no candidates/context, stub source choice and text entry, then assert the entered plist. Test file choice and cancelled/blank text.
- [ ] **Step 2: Run red.** `emacs -Q --batch -L lisp --eval '(dolist (d (directory-files "elpaca/builds" t "^[^.].*")) (when (file-directory-p d) (add-to-list (quote load-path) d)))' -l test/agent-shell-review-spec-test.el --eval '(ert-run-tests-batch-and-exit "^agent-shell-review-spec-test-")'`; expect failures from the old shell argument and missing prompt.
- [ ] **Step 3: Implement the new signature.** Remove `agent-shell-review-spec-transcript-text` and all agent-shell references. Keep file discovery order; choose file or text when discovery fails, using minibuffer text input that accepts pasted newlines. Read explicit/referenced files in full within the cap and preserve source provenance.
- [ ] **Step 4: Run the same focused command; expect all spec tests to pass.**
- [ ] **Step 5: Commit** `lisp/agent-shell-review-spec.el` and `test/agent-shell-review-spec-test.el` with `[agent-shell-review] Decouple requirements from agent-shell`.

## Task 2: Direct ACP transport and request policy

**Files:** Create `lisp/agent-shell-review-acp.el`; test `test/agent-shell-review-acp-test.el`.

**Interfaces:** `(agent-shell-review-acp-create root requirements-file on-event) -> transport` constructs an inert transport; `(agent-shell-review-acp-start transport) -> transport` begins requests after the coordinator stores it; `(agent-shell-review-acp-send transport prompt) -> non-nil request token`; `(agent-shell-review-acp-close transport) -> nil`; `(agent-shell-review-acp-diagnostics transport) -> string`. `requirements-file` is an absolute file path or nil. `on-event` receives a plist with `:type` in `ready`, `chunk`, `complete`, `error`; `chunk` carries `:text`, `complete` carries `:stop-reason`, and `error` carries `:message`. `ready` means `session/new`, optional model choice, and read-only selection all succeeded. Define independent `defcustom`s for command list (default `("codex-acp")`), login environment (default `("OPENAI_API_KEY=" "DEFAULT_AUTH_REQUEST={\"methodId\":\"chat-gpt\"}")`), and optional model ID (default nil).

- [ ] **Step 1: Write failing fake-ACP tests.** Assert `initialize` advertises `writeTextFile: false`, then `session/new` with the project root, optional advertised model selection, `session/set_mode` for the advertised read-only ID, `ready`, and only then `session/prompt` with one text content block. Assert a missing or rejected read-only mode emits `error` before a prompt. Assert permitted in-project reads, explicit external requirements-file reads, denial of symlink escapes and all writes, and cancelled `session/request_permission` responses. Assert irrelevant-session notifications and non-text chunks produce no `chunk`; shutdown is idempotent and diagnostics survive it. Make one fake callback synchronous to prove `create`/`start` ordering.
- [ ] **Step 2: Run red.** `emacs -Q --batch -L lisp --eval '(dolist (d (directory-files "elpaca/builds" t "^[^.].*")) (when (file-directory-p d) (add-to-list (quote load-path) d)))' -l test/agent-shell-review-acp-test.el --eval '(ert-run-tests-batch-and-exit "^agent-shell-review-acp-test-")'`; expect the missing package/interface failure.
- [ ] **Step 3: Implement the transport.** Use `acp-make-client`, the three ACP subscriptions, `acp-send-request`, and `acp-shutdown`. Use `acp-make-initialize-request`, `acp-make-session-new-request`, `acp-make-session-set-mode-request`, and `acp-make-session-prompt-request` with a one-element text-content-block list. Select mode/model only from advertised IDs (`session/set_config_option` for advertised model config, or `session/set_model` for advertised legacy models); fail unsupported optional model selection. Use `file-truename` for read boundaries and `acp-send-response` for every incoming request. Capture raw messages and errors before shutdown. Never report `ready` after failure/close.
- [ ] **Step 4: Run the same focused command; expect all ACP transport tests to pass.**
- [ ] **Step 5: Commit** the new transport and test with `[agent-shell-review] Add direct ACP review transport`.

## Task 3: Move review orchestration to ACP

**Files:** Modify `lisp/agent-shell-review.el`; test `test/agent-shell-review-test.el`.

**Interfaces:** Consume the Task 1 resolver and Task 2 transport. Define `(defcustom agent-shell-review-origin-provider nil ...)`, called as `(funcall provider root)` in the invoking buffer; provider result is nil or a plist `(:context STRING :send FUNCTION :buffer BUFFER)`. `:send` has signature `(lambda (prompt) ...)` and returns non-nil insertion details or signals `user-error`. Keep `(agent-shell-review--begin root origin &optional spec-file clarifications stale-result) -> run`, where `origin` is that plist, and define `(agent-shell-review--cancel run) -> nil`. Run fields include `transport`, `origin`, `send-fixes`, `requirements`, `raw-answer`, `result`, and `sidebar-buffer`; remove reviewer-shell/agent-shell subscription fields. Preserve the public interactive command.

- [ ] **Step 1: Write failing coordinator tests.** Replace shell-event mocks with Task 2 transport fakes and remove the obsolete preferred-agent-config test. Assert origin provider runs from the public command in either binding context, requirements are resolved before `agent-shell-review-acp-create`, the same ACP session receives question answers and one parse-repair prompt, and findings/clear/error close the client while questions keep it. Assert non-`end_turn` completion is an error; a failed request or process exit never becomes clear. Assert rerun carries answers and stale findings, closes the old transport first, starts a new one, and ignores late old events. Assert killing the active sidebar cancels, while hiding it does not.
- [ ] **Step 2: Run red.** Use the Task 1/2 load-path command with `-l test/agent-shell-review-test.el --eval '(ert-run-tests-batch-and-exit "^agent-shell-review-test-\\(origin\\|fresh\\|questions\\|parse\\|clarifications\\|rerun\\|cancel\\|stale-callback\\)")'`; expect old shell behavior to fail.
- [ ] **Step 3: Implement the coordinator.** Remove `require 'agent-shell`, `agent-shell-review--start-shell`, replacement-shell code, and reviewer-shell lookup APIs. Resolve plain origin text, create prompt, store the new inert ACP transport on the run, then start it and submit only after `ready`. Make each callback check both the run's active identity and transport before mutating status. Clear raw answer before answer/repair turns; close on terminal result; leave questions alive. Keep status-change hook and sidebar presentation.
- [ ] **Step 4: Run the focused coordinator tests; expect pass.**
- [ ] **Step 5: Commit** coordinator and its migrated tests with `[agent-shell-review] Coordinate direct ACP review passes`.

## Task 4: Sidebar handoff and diagnostics

**Files:** Modify `lisp/agent-shell-review-ui.el`; test `test/agent-shell-review-test.el`.

**Interfaces:** Consume run `:send-fixes` callback and `transport` from Task 3. Keep `S`, `g`, `v`, and other mode keys; `v` opens a read-only `*Agent Review Diagnostics: PROJECT*` buffer from retained transport diagnostics and raw answer. `S` assembles one prompt and invokes `:send-fixes` only if supplied; without it, open a copyable `*Agent Review Fixes: PROJECT*` buffer. Preserve marks and assembled prompt on callback error, busy/unready/dead origin, and copy path.

- [ ] **Step 1: Write failing UI tests.** Replace `agent-shell-review-test-replacement-fixer` with assertions that the callback receives one prompt containing all marked findings, original requirements, and clarified criteria, and no new shell is started. Assert callback failure retains marks and copyable prompt; no callback opens the prompt buffer. Assert `v` shows raw answer and ACP errors without an `agent-shell-mode` buffer. Assert `TAB`, mark/unmark, and status redraw preserve selected item and Evil normal state; modeline is inherited and no key hints appear. Assert kill-hook calls coordinator cancellation but `q`/hide does not.
- [ ] **Step 2: Run red.** Use the Task 3 focused command with selector `"^agent-shell-review-test-\\(handoff\\|diagnostic\\|evil\\|render\\|no-inline\\|navigation\\|sidebar\\|kill\\)"`; expect obsolete shell access and handoff tests to fail.
- [ ] **Step 3: Implement UI changes.** Use the callback and copy path; retain prompt in the run or copy buffer before attempting delivery. Render diagnostics using the Task 2 accessor. Add kill-buffer cancellation and guard it against a sidebar reused by a new run. Keep the existing uncommitted Evil/modeline/point fixes and revise their tests only for changed interfaces.
- [ ] **Step 4: Run the focused UI tests; expect pass.**
- [ ] **Step 5: Commit** UI and tests with `[agent-shell-review] Keep review handoff in the sidebar`.

## Task 5: Optional Emacs adapter and whole-workflow verification

**Files:** Create `lisp/agent-shell-review-agent-shell.el`, `test/agent-shell-review-agent-shell-test.el`; modify `modules/mod-ai.el`, `test/agent-shell-review-test.el`.

**Interfaces:** `(agent-shell-review-agent-shell-origin root) -> origin plist or nil` implements Task 3's provider; from `agent-shell-mode` it captures the exact current buffer, from a source buffer it offers matching project shells, and with none it returns nil. `(agent-shell-review-agent-shell-send buffer prompt) -> insertion details` calls `agent-shell-insert :text prompt :submit t :no-focus t :shell-buffer buffer` only if the captured buffer lives, has `agent-shell-session-id`, and is not `shell-maker-busy`. `mod-ai.el` installs this provider and a review notification callback; notifications suppress alerts while the sidebar is focused.

- [ ] **Step 1: Write failing adapter tests.** Assert exact origin wins over other matching shells; source-buffer ambiguity prompts for a choice; no shell yields nil. Assert transcript text comes from the captured buffer, and closed/busy/unready buffers signal `user-error` without `agent-shell-insert`. Assert successful insertion uses the exact captured buffer and returns details. Add an integration test that starts a fake review from an implementation shell and checks `agent-shell-buffers`/`agent-shell-switch-buffer` contains no reviewer entry and the callback still targets that original shell.
- [ ] **Step 2: Run red.** `emacs -Q --batch -L lisp --eval '(dolist (d (directory-files "elpaca/builds" t "^[^.].*")) (when (file-directory-p d) (add-to-list (quote load-path) d)))' -l test/agent-shell-review-agent-shell-test.el --eval '(ert-run-tests-batch-and-exit "^agent-shell-review-agent-shell-test-")'`; expect the adapter interface to be absent.
- [ ] **Step 3: Implement adapter and module wiring.** Move transcript access and shell choice out of the core; capture a buffer in the `:send` closure and never search again during delivery. Check readiness immediately before insertion and require its non-nil return. In `mod-ai.el`, remove reviewer-shell notification suppression/related-buffer hooks, install provider after loading review, retain Evil normal bindings, and make review notifications independent of reviewer shell identity.
- [ ] **Step 4: Run adapter and all review tests.** Run the focused command above, then `emacs -Q --batch -L lisp --eval '(dolist (d (directory-files "elpaca/builds" t "^[^.].*")) (when (file-directory-p d) (add-to-list (quote load-path) d)))' --eval '(dolist (f (directory-files "test" t "-test\\.el$")) (load f nil t))' -f ert-run-tests-batch-and-exit`; expect all tests to pass. Byte-compile changed Elisp in batch with the same load path; expect no errors.
- [ ] **Step 5: Check a real `codex-acp` handshake in running Emacs.** Start from an implementation agent, confirm `initialize`, `session/new`, and read-only mode selection precede the first prompt; answer a question if offered; verify `S` reaches that same implementation buffer, `v` shows diagnostics, and no review buffer enters `agent-shell-switch-buffer`. If the agent advertises no usable read-only mode, leave the review in a visible error state and report this compatibility limit before claiming completion.
- [ ] **Step 6: Commit** adapter, module wiring, and tests with `[agent-shell-review] Return fixes to the originating session`.

## Final Review

- [ ] Compare the branch against the spec section by section, including the existing UX tweaks and the no-shell guarantee.
- [ ] Run the complete ERT suite, syntax check and focused byte compilation, then inspect `git diff --check` and `git status --short`.
- [ ] Request the execution method's required independent review; address concrete findings and rerun only the affected checks.
