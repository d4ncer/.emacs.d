# My Emacs configuration

An Emacs 32 configuration with Evil bindings, native tree-sitter syntax support,
Eglot, and an Org knowledge and task workflow.

The main components are:

- Vertico, Orderless, Consult, and Embark for navigation and contextual actions.
- Corfu, Cape, Tempel, and eglot-tempel for in-buffer completion and snippets.
- Apheleia for asynchronous formatting; Magit for Git.
- Batppuccin themes, Doom Modeline, and Pulsar for presentation.
- Org, Vulpea, Vulpea Journal, Vulpea UI, and Citar for notes and references.
- gptel and agent-shell, with local project authoring and annotated review tools.
- Elpaca with recorded package revisions and native grammar source pins.

## Requirements and setup

Use Emacs 32 or newer. The verified development build is recorded in
[emacs-build.eld](emacs-build.eld). Git is required for Elpaca; ripgrep and Python 3
are required for the asynchronous finder for large projects.

Clone this repository as `~/.emacs.d`, then start Emacs. Elpaca installs the
recorded packages on first startup. Tree-sitter asks before installing a missing
grammar. Language servers and formatters are separate system tools.

Personal integrations use `~/life/elisp/` and an optional `~/private/` configuration.
Override `+site-lisp-directory` or `+private-config-directory` before bootstrap to
change these locations; nil disables the private configuration. The personal
agenda workflow requires the external `+life` library.

See [dependency setup and updates](docs/DEPENDENCIES.md) for native build
requirements, grammar management, and restore checks.

## Navigation

`SPC` is the leader in normal and motion state; `C-SPC` is the insert-state
leader. `,` is the mode-specific local leader. Which-key shows available keys.

Useful starting points include `SPC pf` for project files, `SPC /` for project
search, `SPC gs` for Magit, and `SPC aP` for agent projects. See the
[agent project guide](docs/AGENT-SHELL-PROJECTS.md).

## Verification

```sh
emacs -Q --batch -l test/run-config.el
```

This runs syntax checks, focused compilation, activation checks, and all local
ERT suites in fresh processes. Mermaid integration checks need `mmdc` and an
environment where its local Chromium renderer can launch.

```sh
emacs -Q --batch -L test -l test/restore-config.el
```

The opt-in fresh install check needs network access. It copies the
configuration, clones the latest package sources into a temporary directory,
builds them, and runs the same gates. On macOS it also opens a disposable GUI Emacs for completion and mode line
checks, then closes it.

[MODULES.md](docs/MODULES.md) describes ownership, dependencies, and activation.
The [performance changes](docs/superpowers/plans/2026-10-22-config-performance.md)
and [architecture changes](docs/superpowers/plans/2026-10-22-config-architecture.md)
record the implementation and verification results.

## Credits

The configuration incorporates helpers adapted from Doom Emacs and
[Chris Barrett's configuration](https://github.com/chrisbarrett/.emacs.d).
