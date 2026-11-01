# AGENTS.md

Guidance for changing this Emacs configuration.

## Repository map

- `early-init.el` handles early startup. `init.el` requires Emacs 32 or newer,
  bootstraps Elpaca with `use-package`, and loads modules in order.
- `modules/` contains feature configuration. Check `init.el` for the active
  modules and their load order. `lisp/` contains local helper libraries and the
  `agent-shell-projects/`, `project-files/`, and `compat/` library directories.
  `+bootstrap.el` owns their load paths and declaration conventions.
- `modules/mod-keybindings.el` defines the main `SPC` leader (`C-SPC` in insert
  state). Feature-specific bindings may live in their module.
- `templates/` contains file templates. `docs/MODULES.md` is a longer
  architecture overview; verify details against `init.el` and the modules.
- `docs/UI-GUIDELINES.md` governs every screen this configuration draws
  (views, mode-line and buffer annotations, inline renderings). Read it
  before changing one.
- `docs/PSTACK-REMOTE-PROTOCOL.md` is the normative protocol for running
  pstack arenas and delegates on a remote host. Read its "Adding a
  backend" section before adding a host kind: kinds supply transport
  only, and launching and containment still assume systemd.

## Making changes

- Edit the module that owns the behavior. Add a module to `init.el` only when
  introducing a distinct feature area.
- Preserve deferred loading. `modules/mod-core.el` lists packages guarded
  against eager loading in `+expensive-packages`.
- Use Elpaca-integrated `use-package` for package configuration. Use
  `:after-call`, `:commands`, and hooks when they suit the loading behavior;
  follow the surrounding module's patterns.
- Use General for leader and mode bindings, with `:wk` labels where they aid
  discovery. Inside a `use-package` form, bind keys with its `:general`
  keyword, never `define-key` in `:config`. Direct keymap edits are
  appropriate for local, generated, or text-property keymaps.
- Apply a config change to the running Emacs too, and check the live
  values with plain reads, not `let`. Reloading a library keeps every bound
  `defcustom` and `defvar`: afterwards reset only options whose default
  expression changed and whose live value still equals the old default,
  never global minor modes or values `~/private` sets. `defvar-keymap` also
  survives a reload, so after changing bindings rebuild the keymap,
  re-evaluate the module's `use-package` form and check `key-binding` in the
  real buffer and Evil state.
- Before building on data an agent sends over ACP, confirm the agent sends
  it: run `test/agent-shell-probe.el` against the real adapter and read what
  reached Emacs. A match in a shell's rendered text is not evidence.
- Edit Elisp with `lisp/+elisp-tools.el`, whose Commentary lists its
  commands, never a script that finds forms by string position or
  reindents a region. After any other edit, byte-compile the file;
  `check-parens` passes a paren that closes the wrong form.
- The gate fails when reindenting any tracked Elisp moves a line; `indent
  --fix FILE...` rewrites them. Plain batch `emacs -Q` indents differently
  from the configuration, and with tabs, which the gate also rejects.
- Verify the smallest relevant behavior after editing. Check syntax for changed
  Elisp, then run focused byte compilation or ERT tests when they add value.
  Use `emacsclient` for live Emacs inspection when a server is running.
  Run `emacs -Q --batch -l test/run-config.el` for the complete local gate.
  See `docs/DEPENDENCIES.md` for package updates and the fresh install check.

## Elisp style

- Give new `.el` files a lexical-binding header and the usual `Commentary`,
  `Code`, `provide`, and footer sections.
- Prefix custom functions and variables with `+`; follow existing names when
  extending an established helper library.
- Prefer concise docstrings to explanatory comments. Use spaces and aim for
  80-column lines where practical.

## Commits

Use the repository's `[scope] Imperative summary` subject style. Keep the
subject concise, add a short body for multiple changes, and omit co-author
footers.

Push only when the complete local gate exits 0; take its status from
`rc=$?` straight after it, never from grepping its output. A known flaky
test still needs a rerun that exits 0. After a verified push of a feature
branch, fast-forward `main` to it and push `main` too, without asking; if
`main` has diverged, rebase the branch on it first and say so.

## Public repository

This repository is public. Nothing specific to the owner's employer may
land in it: no work repository names, paths or sizes, remote workspace or
template names, internal hosts, URLs or tool names, inventories of a work
machine, or results of experiments on work infrastructure, in code, tests,
fixtures, docs or commit messages. Work-specific config, profiles and notes
go in `~/private/` (`+private-config-directory`); public code uses neutral
examples such as `~/src/big-repo`. The gate checks a private denylist from
`~/private/` when present. Tell any agent you delegate to the same rule.
