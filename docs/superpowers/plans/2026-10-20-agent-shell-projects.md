# Agent shell projects v1 implementation plan

Status: All six milestones implemented on `feat/agent-shell-projects`.
Each milestone passed its focused gate before the next milestone began.

**Proposal:** [Agent shell projects v1](../specs/2026-10-20-agent-shell-projects-design.md).

## Repository integration

Add the feature under `lisp/` and activate it through an Elpaca-integrated
`use-package` declaration in `modules/mod-ai.el`. Preserve deferred loading;
the extension must not pull agent-shell, Magit, or Embark into early startup.
Use existing installed Emacs facilities and public agent-shell interfaces.

The working namespace is `+agent-shell-projects-`. Check it against existing
helpers before introducing symbols. New Elisp files follow the repository's
lexical-binding, Commentary, Code, provide, and footer conventions. Use General
for user-facing bindings and local maps for generated artefact and sidebar
content. Exact keys and the number of implementation files remain flexible.

Keep these responsibilities distinct as the code grows:

| Responsibility | Contents |
| --- | --- |
| Project state | IDs, artefact references, agent associations, snapshots, annotations, checkpoints, persistence |
| UI | Home, sidebar, artefact views, modeline, history navigation, Embark targets, project transient |
| Agent integration | Subscriptions, selection, context builders, operation transport, feedback and outcome requests |
| Code review | Git capture, comparisons, summary, walkthrough, Magit-style annotated diff |

Tests belong under `test/`. Introduce tests for consequential persistence,
mapping, concurrency, and integration behavior rather than duplicating simple
rendering code in assertions.

## Milestone 1 Document project and saved versions

- [x] Define project, artefact, snapshot, checkpoint, and agent association
  records with stable IDs and a schema version.
- [x] Implement the configurable local store, immutable content snapshots, and
  atomic manifest updates. Recover existing state after reopening Emacs.
- [x] Enrol Markdown files, Org files, and Org subtrees. Establish the subtree
  identity policy and handle rename or move without losing identity.
- [x] Build the minimal home: project name, linked authoring shell and status,
  artefact names, and one binary attention indicator per artefact.
- [x] Open an artefact and its sidebar. Show version information in the
  modeline. Add previous, next, and current history navigation.
- [x] Provide direct editing through the ordinary source buffer and preserve
  the captured version for review. Settle unsaved-edit handoff behavior here.

**Demonstration:** Create a named project from an existing Markdown document,
edit and capture another version, browse both, then reopen the project with its
history intact. Repeat for an Org subtree in a file containing other headings.

**Verification:** Focused persistence and content deduplication tests, subtree
identity checks, and a manual native-mode editing/history check. Viewing a
version must not alter source or acknowledge review activity.

## Milestone 2 Annotations and contextual actions

- [x] Add annotation creation from a selected region, replies, open/resolved
  state, submission metadata, and attachment health.
- [x] Preserve the original snapshot, selected text, and surrounding context.
  Map straightforward edits and identifiable moves. Expose uncertain or
  deleted anchors for reassessment and manual reattachment.
- [x] Implement sidebar threads in passage order, selection highlighting,
  composition, and a resolved-thread filter.
- [x] Add Embark target finders and action maps for artefacts, selections,
  annotations, and shell references. Retain existing generic Embark behavior
  outside the extension's buffers and selections.
- [x] Add user-only resolution and reopening, with application validation that
  rejects those operations from agent integration.
- [x] Compute attention from new relevant activity. Opening a thread and
  resolving a thread must not be confused with reviewing a whole revision.

**Demonstration:** Comment on a passage, revise and move it, inspect its original
excerpt, reply, resolve and reopen it, and reattach a deliberately ambiguous
target through Embark.

**Verification:** ERT covers insertion before a target, edits at selection
boundaries, moves, duplicate quotations, deletion, restart, and the independence
of attachment and resolution. Check the contextual targets manually in Emacs.

## Milestone 3 Authoring agent and feedback loop

- [x] Support choosing an existing shell or creating a fresh authoring shell.
  Store a collection of associations with one feedback recipient for v1.
- [x] Subscribe to relevant agent-shell events; update status and reported
  progress live while preserving UI position. Clean up subscriptions on shell
  death, reassociation, and mode disable.
- [x] Define the structured operation API for agent annotations, replies,
  progress, explanation links, and validation records. Settle the transport
  behind that API. Correlate operations and make retries idempotent.
- [x] Implement replaceable context builders and prompt delivery using public
  agent-shell facilities. Treat busy, queued, failed, and disconnected shells
  explicitly without duplicating a batch on retry.
- [x] Record a feedback batch against the versions actually reviewed and the
  original comment snapshots, even when current source has advanced. Advance
  acknowledgement only after submission succeeds. Preserve newer activity and
  leave unopened artefacts at their previous acknowledgement.
- [x] Add mark-reviewed for a pass with no comments. Carry unresolved older
  annotations through subsequent checkpoints.
- [x] Handle responses that explain, disagree, or ask a question as well as
  those identifying an edit. Source edits do not resolve annotations.

**Demonstration:** Submit several comments, let the agent revise and raise its
own question, inspect responses, and complete another round while an older
comment remains open. Include direct user edits in the evolving artefact.

**Verification:** ERT covers batch identity, queued or failed submission,
deduplicated operations, event subscription lifecycle, user-only resolution,
and new activity arriving after checkpoint capture.

## Milestone 4 Side questions and conversation extraction

- [x] Add a configurable preferred side agent or prompt-for-selection policy.
- [x] Automatically prime a fresh shell with context from the replaceable
  builder: question, original selection and snapshot, proposal, and relevant
  project decisions.
- [x] Add explicit Add outcome as comment. Request a draft, correlate the
  response with that conversation and selection, allow editing, and attach it
  only through the user's capture action.
- [x] Support multiple captures during one side conversation and preserve the
  original target if the artefact changes while the discussion is running.
- [x] Keep captured outcomes in the feedback batch until submission.
- [x] Extract selected conversation prose into Markdown, Org, or a heading in
  an existing Org file and enrol the saved destination.

**Demonstration:** Ask a question from a Markdown selection, capture two edited
outcomes from one discussion, and submit them together with a direct comment.
Extract another response into an existing Org file and review that subtree.

**Verification:** Test deterministic context inputs, default versus prompted
agent selection, interruption, late responses from another shell, capture
against an older snapshot, and repeated capture without duplicate annotations.

## Milestone 5 Git workspace and annotated diff

- [x] Discover the workspace through Git, supporting both ordinary checkouts
  and linked worktrees. Define new-project and imported-work base selection
  with an explicit override.
- [x] Capture the full changeset since that base: committed, staged, unstaged,
  and new non-ignored files. Retain rename, deletion, and binary-change
  metadata. Include changes independent of author identity.
- [x] Store changed file contents and necessary base contents without copying
  the repository or changing its index, HEAD, or working files during capture.
  Recheck moving source contents before finalising a checkpoint.
- [x] Add comparisons against the project base and the last reviewed
  checkpoint. All views use the same capture manifest.
- [x] Build expandable file and hunk sections with annotations on either diff
  side, including deleted text. Keep sidebar and Embark behavior consistent
  with document review.
- [x] Visit and edit the corresponding current source, showing intervening
  changes where a historical location is uncertain.

**Demonstration:** Review a branch with committed and dirty changes, new files,
and a deletion. Edit a source passage, complete a feedback round, and compare
against the last checkpoint. Repeat inside a linked worktree.

**Verification:** Use small disposable Git fixtures for workspace variants and
change types. Check that capture leaves Git state unchanged. Validate old/new
diff anchors and that excluded ignored files do not enter the changeset.

## Milestone 6 Code explanations and connected views

- [x] Define structured summary and walkthrough records tied to captured
  workspace and proposal snapshots.
- [x] Cover outcome, success criteria, constraints and priorities, alternatives,
  tradeoffs, relationships among changes, validation, and remaining gaps.
  Separate recorded decisions from reconstructed intent.
- [x] Add Review code to the project transient and implement summary,
  walkthrough, and diff cycling with position retention and back navigation.
- [x] Link summary points to walkthrough entries, proposal passages, related
  hunks across files, and validation evidence. Allow the whole diff to be
  restored after focusing on one behavior.
- [x] Let the author maintain explanations as the proposal and implementation
  evolve. Handle failed or stale generation without displaying explanations
  as though they apply to different source contents.
- [x] Support importing existing work and correcting its reconstructed proposal
  through the normal annotation loop.

**Demonstration:** Start from an existing code workspace, inspect and correct
its proposal, follow a behavior through all three views, ask about a decision,
submit feedback, then review updated explanations and code together.

**Verification:** Test cross-view links against immutable snapshot IDs,
stale-generation handling, navigation state, cross-file hunk references, and
annotation round trips. Manually inspect readability and keyboard flow.

## Final acceptance and documentation

- [x] Run the document and code scenarios in the proposal, including an open
  annotation from an older version and direct user edits.
- [x] Verify durable reopening after Emacs restart and shell reconnection.
- [x] Check display behavior with the existing side-window configuration and
  Evil bindings; keep contextual maps local and discoverable.
- [x] Check changed Elisp syntax, focused byte compilation, and the relevant
  ERT suites. Use emacsclient when a server is running. Avoid unrelated tests
  once the remaining behavior is sufficiently verified.
- [x] Document entry commands, the project transient, Embark actions, history
  navigation, configuration, context builders, and the agent operation API.
- [x] Confirm that the home remains names plus a binary attention icon, that
  versions stay inside artefacts, and that only the user resolves comments.

## Verification record

| Gate | Result |
| --- | --- |
| Milestone 1 | 3 ERT tests and strict byte compilation passed |
| Milestone 2 | 7 ERT tests and strict byte compilation passed |
| Milestone 3 | 11 ERT tests and strict byte compilation passed |
| Milestone 4 | 15 ERT tests and strict byte compilation passed |
| Milestone 5 | 17 ERT tests and strict byte compilation passed |
| Milestone 6 | 21 ERT tests and strict byte compilation passed |
| Final integration | 26/26 ERT tests; syntax and strict byte compilation; cold-start and live Emacs checks passed |

The final suite adds a real local ACP process fixture for feedback delivery,
source revision, structured replies, a fresh side agent and editable outcome
capture. Further regression checks cover transactional rollback, unsaved source
conflicts, native Org subtree annotations, and direct code editing. Git fixtures
exercise ordinary checkouts and linked worktrees, including binary and symlink
changes. The startup test evaluates the actual use-package declaration and
checks deferred activation and General/Evil bindings. A live emacsclient check
verifies home, sidebar, annotation overlays, Embark targets and Evil keys using
disposable project data and the normal UI change hooks.

Model-provider behavior is not part of the deterministic acceptance gate; the
ACP fixture uses the installed agent-shell implementation without network access.
The [usage guide](../../AGENT-SHELL-PROJECTS.md) documents the complete workflow,
configuration and operation API.

## First usable slice

Milestones 1 and 2 form the smallest useful document workflow: a named project,
Markdown or Org artefacts, a minimal home, saved versions, native editing,
anchored annotations, a sidebar, and Embark actions. Milestone 3 closes the
authoring loop. Milestone 4 adds focused exploration. Milestones 5 and 6 deliver
the agreed code review experience. Code review remains part of v1 even though
the document workflow is delivered first.
