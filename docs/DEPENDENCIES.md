# Dependency setup and maintenance

The configuration uses Emacs 32 native facilities and Elpaca. Three files record
its reproducible inputs:

| File | Contents |
| --- | --- |
| emacs-build.eld | Verified Emacs source and Homebrew formula identities, build options and runtime libraries |
| elpaca-lock.eld | Active package recipes and Git commit hashes |
| treesit-grammars.eld | Grammar source commits and source subdirectories |

The Elpaca bootstrap commit is also pinned in `elpaca-bootstrap.el`. Keep that
pin aligned with Elpaca's entry in the package lock.

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

1. Restart Emacs with this configuration so Elpaca's active orders match it.
2. Check the package source directories for local changes. Preserve or resolve
   those changes explicitly; commit hashes do not capture uncommitted patches.
3. Update the selected packages through Elpaca. Exercise the affected workflow
   and run `emacs -Q --batch -l test/run-config.el`.
4. Run `M-x elpaca-write-lock-file` and select this repository's
   `elpaca-lock.eld`. Inspect the recipe and commit changes.
5. If Elpaca changed, update its bootstrap pin to the same recorded commit.
6. Run the locked recipe check and isolated restore before committing the update.

Generating a lock file records the installed state. Loading the configuration
with a different lock does not reset an existing checkout automatically. To
restore an older state, restore its lock and bootstrap pin together and build in
a fresh directory, or explicitly rebuild the affected packages at those refs.

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

Existing grammar binaries are retained. The recipe file controls future
installations; it does not establish the provenance of an older installed
binary. Reinstall a grammar when validating a changed source pin. No grammar
download occurs without installation confirmation during ordinary editing.

## Verification commands

```sh
emacs -Q --batch -l test/run-config.el
emacs -Q --batch -L test -l test/restore-config.el
emacs -Q --batch -l test/benchmark-config.el
```

The first command runs local tests with installed dependencies. Mermaid tests
need an environment that permits Chromium to launch.

The restore command copies the configuration to a disposable directory, clones
package repositories from local Git objects, checks out recorded commits and
rebuilds packages. It runs the same tests against that restored directory. On
macOS it also starts and closes a separate GUI Emacs to check first completion,
source and Org activation, Dired counts, and mode line rendering. Shell environment
refresh and optional private configuration are disabled only inside this test.
The user's running server is not reloaded.

The offline restore uses local clone transport so partial repositories can reuse
their available Git objects without attempting to serve missing historical trees.
It requires the recorded objects to exist in local repositories;
a machine without those objects must fetch the recorded revisions first. Native
build dependencies must also be installed. Elpaca may need its archive metadata
for undeclared dependencies; the active lock includes transitive packages.

## Compatibility adapter removal

| Adapter | Reason and upstream | Removal gate |
| --- | --- | --- |
| +expert-compat | Expert 0.1.5/0.1.6 exit validation, incremental sync and project toolchain discovery; github.com/expert-lsp/expert, toolchain settings from PR 682 | With advice disabled, verify EOF typing and completion, a clean shutdown, and the project's Elixir/Erlang executables against the deployed server. Retain each workaround until its reproducer passes. |
| +eglot-tempel-compat | Literal indentation from the pinned eglot-tempel 0.8.2 converter; github.com/fejfighter/eglot-tempel | The multiline snippet insertion check must pass without the adapter, including a representative Elixir snippet. |

Each adapter can be disabled independently and removes its advice and hooks.
Their tests verify scope, indentation and lifecycle; a simulated protocol fixture
cannot establish that a new server release fixed its underlying bug.
