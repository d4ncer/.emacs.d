# Dependency setup and maintenance

The configuration uses Emacs 32 native facilities and Elpaca. Packages track
their latest upstream revisions; nothing pins them. When an update breaks
something, fix the configuration forward. Two files record other inputs:

| File | Contents |
| --- | --- |
| emacs-build.eld | Verified Emacs source and Homebrew formula identities, build options and runtime libraries |
| treesit-grammars.eld | Grammar source commits and source subdirectories |

`elpaca-bootstrap.el` clones Elpaca's default branch on a fresh install. After
that, Elpaca updates itself like any other package.

## System prerequisites

Install Git, ripgrep, and Python 3. A C/C++ compiler is needed to install native
grammars; macOS Command Line Tools provide one. GNU ls (`gls`, from coreutils) is
used for Dired's configured listing options on macOS.

Language servers and formatters remain separate executables. The configuration
selects TypeScript or Deno by project markers and Expert for Elixir. envrc
provides project environments. exec-path-from-shell inherits the login shell's
PATH early on macOS; keychain refresh runs only if its executable is present.

Optional native features have additional prerequisites:

- PDF Tools needs its epdfinfo build dependencies, including Poppler.
- Excali needs Canvas and dynamic-module support plus make, pkg-config, Cairo,
  Pango, zlib, librsvg, gdk-pixbuf, and webp. Its module is built by Elpaca.
- Mermaid rendering needs Node and Mermaid CLI (`mmdc`), including its Chromium
  installation. Its integration tests launch that renderer locally.
- Spell-fu uses the configured Aspell dictionary.
- Math rendering uses the latex-to-svg backend and its system tools.

Emacs major version does not imply that Canvas, modules, tree-sitter, or every
image type was compiled in. Optional feature checks remain in the configuration.

## Package updates

1. Check the package source directories for local changes. Preserve or resolve
   those changes explicitly before merging upstream.
2. Update through Elpaca, for example `M-x elpaca-update-all`.
3. Run `emacs -Q --batch -l scripts/verify-packages.el`. It lists every package
   build whose bytecode is missing or older than its source, compiles those
   files, and exits non-zero if any fail.
4. Restart Emacs, exercise the affected workflow, and run
   `emacs -Q --batch -l test/run-config.el`.

Elpaca compiles a package only when it builds it. It treats an existing build
directory as complete and never compiles it again, so .elc files deleted from
`elpaca/builds` stay missing. Run the verify script instead of deleting
bytecode. Each of its compilers sees every build directory, so a package that
requires a library its `Package-Requires` header omits still compiles. It
recompiles files whose source changed after their last compile, but not the
unchanged dependents of an updated macro library. Use `M-x elpaca-rebuild` for
those, or to rebuild any one package from scratch.

## Emacs and grammar updates

For a new Emacs development snapshot, record its source revision or precise
package-manager build identity. The version string alone is insufficient.
The current Homebrew source identity comes from `INSTALL_RECEIPT.json` in the
installed Cellar directory; its formula revision and used options are recorded
in `emacs-build.eld`.

Compare the new build's grammar recipes against `treesit-grammars.eld`. Update
pins deliberately, then verify fontification, indentation, and syntax navigation
for affected languages. The configured Nix source pin was compiled and exercised
against nix-ts-mode in a temporary grammar directory.

Grammar binaries are built locally into `tree-sitter/`, which Git ignores.
Opening a file whose grammar is missing offers to build it from its pin in
`treesit-grammars.eld`. Reinstall a grammar when validating a changed source
pin. No grammar download occurs without installation confirmation.

## Verification commands

```sh
emacs -Q --batch -l scripts/verify-packages.el
emacs -Q --batch -l test/run-config.el
emacs -Q --batch -L test -l test/restore-config.el
emacs -Q --batch -l test/benchmark-config.el
```

The first command compiles stale package bytecode, as described above. An
optional argument names another builds directory. The second runs local tests
with installed dependencies. Mermaid tests need an environment that permits
Chromium to launch.

The third command is an opt-in fresh install check and needs network access.
It copies the configuration to a disposable directory, clones every package
from upstream, builds them, and runs the same tests against that directory. On
macOS it also starts and closes a separate GUI Emacs to check first completion,
source and Org activation, Dired counts, and mode line rendering. Shell
environment refresh and optional private configuration are disabled only
inside this check. The user's running server is not reloaded. Native build
dependencies must be installed.

## Compatibility adapter removal

| Adapter | Reason and upstream | Removal gate |
| --- | --- | --- |
| +expert-compat | Expert 0.1.5/0.1.6 exit validation, incremental sync and project toolchain discovery; github.com/expert-lsp/expert, toolchain settings from PR 682 | With advice disabled, verify EOF typing and completion, a clean shutdown, and the project's Elixir/Erlang executables against the deployed server. Retain each workaround until its reproducer passes. |
| +eglot-tempel-compat | Literal indentation from the eglot-tempel 0.8.2 converter; github.com/fejfighter/eglot-tempel | The multiline snippet insertion check must pass without the adapter, including a representative Elixir snippet. |

Each adapter can be disabled independently and removes its advice and hooks.
Their tests verify scope, indentation and lifecycle; a simulated protocol fixture
cannot establish that a new server release fixed its underlying bug.
