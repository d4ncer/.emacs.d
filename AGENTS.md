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
  discovery. Direct keymap edits are appropriate for local, generated, or
  text-property keymaps.
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
