# Emacs configuration architecture

This configuration requires Emacs 32 or newer. Feature modules choose packages,
settings, and bindings. Local libraries implement reusable behavior and own the
resources they install.

## Bootstrap and dependency order

`early-init.el` loads `lisp/+bootstrap.el` and calls `+bootstrap-setup`. Bootstrap
establishes local library paths, the optional site library directory, core
macros, and use-package conventions. `init.el` can invoke the same idempotent
setup when loaded directly.

Elpaca is bootstrapped at a recorded commit. Its lock menu is configured before
package declarations. General initializes before modules that install leader
bindings. The core module establishes shell environment inheritance before
later features can launch external tools. The optional keychain refresh follows
shell environment initialization.

A helper library declares its own dependencies. Modules using core macros
require `+corelib` for evaluation and compilation. The local leader macro lives
in `+keybindings`, allowing its consumers to compile without installing the
entire global leader map.

## Active feature modules

The authoritative order is the sequence of requires in `init.el`.

| Module | Ownership |
| --- | --- |
| mod-keybindings | General, global leader map and personal commands |
| mod-core | Core macros, environment, lifecycle hooks and editing defaults |
| mod-evil | Evil, collection bindings, surround, undo and region selection |
| mod-completion | Vertico, Orderless, Consult, Embark, Corfu, Cape and history |
| mod-ui | Batppuccin, fonts and frame appearance, ligatures, Pulsar and mode lines |
| mod-windows | Display rules, side windows and layout policy |
| mod-editor | Reverting, structural editing, file history, spelling and highlighting |
| mod-nav | Avy, custom actions and Ace Window |
| mod-project | Projectile and the local asynchronous file finder |
| mod-git | Magit, Transient, remote links and revision browsing |
| mod-lsp | Native tree-sitter policy, grammar pins, Eglot, Flymake and snippets |
| mod-format | Apheleia and formatter selection |
| mod-debug | Elisp debugger bindings and profiling commands |
| mod-dired | Dired, editable listings and cached directory mode line |
| mod-eshell | Eshell and custom shell commands |
| mod-ai | gptel, agent-shell, notifications, diagram rendering and project bindings |
| mod-languages | Major modes, language server selection and sibling rules |
| mod-org | Org, agenda, Vulpea, journal, sidebar and citation integration |
| mod-misc | Folding, parentheses, mode line visibility and breadcrumbs |
| mod-docs | Help, Helpful integration, Eldoc, Info and man pages |
| mod-text | Tempel templates, file templates, PDF Tools and optional Excali |
| mod-core-extras | File IO, remote files, search, native Grep Edit and system utilities |

## Activation contracts

- `use-package-always-defer` is enabled. Commands, mode hooks, file associations,
  and `:after-call` provide activation triggers.
- `use-package-hook-name-suffix` is nil: hook declarations use complete names.
- `:after` specifies ordering. Integrations that should load when their parent
  loads also use `:demand t`.
- Timed background activation uses standard `:defer N`. Recentf uses this path.
- `+use-package` implements only `:after-call`. Its registry tracks hook and
  function triggers separately from timed loading. Failed loads remain
  retryable; successful loads remove every trigger.
- `+run-hook-once` shares one completion state across its trigger hooks. It
  removes dispatchers after success and releases its recursion guard on error.
- First input enables completion. First file or buffer use enables deferred
  editor services. Opening a source file does not activate Org or Vulpea.
- Org files and commands activate Org. Agenda and note commands activate their
  required integrations. The personal `+life` library is loaded with Vulpea.
- Agent-shell notifications, Mermaid and the pstack guard activate with
  agent-shell. Math rendering waits for its backend and uses
  `agent-shell-mode-hook`. See `docs/AGENT-SHELL-PSTACK.md` for pstack.

## Native tree-sitter and search

`mod-lsp` owns `treesit-enabled-modes`, installation confirmation, font-lock
level, and the grammar recipes in `treesit-grammars.eld`. Native query caching
is used directly. Third-party Nix dispatch ensures its grammar is available
before entering nix-ts-mode and falls back to conf-unix-mode if installation is
declined. `mod-languages` contains only the additional third-party remappings.

Grep buffers and Embark exports use native Grep Edit. In normal state, `i` or
`e` enters editing. Changes update source buffers immediately; `C-c C-c` saves
the changed source buffers and finishes. Use ordinary source-buffer undo to
revert edits. There is no separate wgrep transaction or wgrep dependency.

## Local library boundaries

### Agent project package

`lisp/agent-shell-projects/` contains:

- `core`: records, immutable content, persistence and source tracking.
- `lifecycle`: owned registrations, timers, setup and teardown.
- `capture`, `jobs`, and `worker`: workspace enrolment and background services.
- Main entry, annotations and menu: document and contextual UI.
- Agent integration: inbox transport and request delivery. Starting, sending
  to and watching shells goes through `lisp/+agent-shell-drive.el`.
- Code and review: captured diffs, explanations and connected navigation.

Core state and workspace capture load without Magit, Org, agent-shell, or the
review UI. Workers resolve their libraries relative to their own directory.

Public entry points include `+agent-shell-projects`, `+agent-shell-projects-menu`,
`+agent-shell-projects-open`, `+agent-shell-projects-enrol-code`, and
`+agent-shell-projects-capture-code`.

`+agent-shell-projects-setup` installs registered integrations idempotently.
`+agent-shell-projects-mode` additionally runs subscriptions and inbox polling.
`+agent-shell-projects-teardown` removes owned hooks and Embark registrations,
restores previous contextual map entries, detaches subscriptions, stops workers,
cleans temporary requests, and cancels owned timers. Persistent project content
and source buffers are retained. Setup restores source tracking for enrolled
buffers.

### Project file package

`lisp/project-files/` contains the picker, isolated index worker and Python
search worker. Small or remote projects retain Projectile's finder; large local
projects use searchable manifests and bounded result sets.

`+project-files-setup` enables integrations through `+project-files-mode`.
`+project-files-teardown` removes advice and hooks and stops active workers.
`+project-find-file`, `+project-find-file-fast`, and `+project-files-refresh` are
its interactive entry points.

### Mode line data

`+modeline-data` caches Dired entry counts, local free space, and Vulpea sidebar
data. Listing changes, note assignments, and database updates schedule refreshes.
Rendering reads cached values. Remote free-space probes are omitted. Buffer death
and mode disable cancel pending work and remove local tracking hooks.

### Measurement and compatibility

`+profile` owns temporary profiling hooks and recordings. Its benchmark refuses
already visited files and retains a fixture modified during measurement. It
reports wall time, collection count and collection time separately without
forcing garbage collection.

`lisp/compat/` contains the Expert and eglot-tempel adapters. Each has an explicit
mode that installs and removes its advice. See [dependency management](DEPENDENCIES.md)
for upstream removal criteria.

Other `lisp/+*.el` files are small editing, theme, window, Org, or UI helpers.
The old `lisp/rk-*` directories are historical helpers and are not added by
bootstrap or loaded by active modules.

## Verification

Run `emacs -Q --batch -l test/run-config.el`. Integration gates use the real
bootstrap conventions and fresh processes. Subsystem runners also compile into
temporary directories with warnings treated as errors. No compiled files are
written beside configuration sources.

The suite covers cancellation, face inheritance, loader retries, first-use
activation, cached rendering, source saving, compatibility adapters, locked
recipes, headless capture and resource teardown. Slow Git fixtures check that
editor input can continue while workers run.

Use `test/restore-config.el` for a separate rebuild and GUI gate. Performance
measurements have their own command; wall-clock comparisons are recorded as
observations rather than timing assertions in ERT.
