# My Emacs configuration

I'm sensing... an Emacs 32 config. Evil keys, native tree-sitter, Eglot, and an
Org brain that never forgets. Just like me.

## The vision

- Vertico, Orderless, Consult, Embark: find anything. I already know where it is.
- Corfu, Cape, Tempel, eglot-tempel: completion so good it finishes your sentences.
- Apheleia formats, Magit commits. Magit is the Gus of Git. Reliable. Fancy.
- Batppuccin, Doom Modeline, Pulsar: latte by day, mocha by night. It follows macOS.
- Org, Vulpea, Vulpea Journal, Vulpea UI, Citar: notes and references. Case files.
- gptel and agent-shell, with project authoring, annotated review and pstack panels.
- Elpaca tracks latest upstream. Grammars too. Living on the edge, son.

## Setup

- Emacs 32 or newer. The blessed build lives in [emacs-build.eld](emacs-build.eld).
- Need Git for Elpaca. Need ripgrep and Python 3 for the big-project finder.
- Clone to `~/.emacs.d` and start Emacs. Elpaca installs everything first run.
- Missing grammar? Tree-sitter asks first. Manners.
- Language servers and formatters are BYO. I'm psychic, not a package manager.
- Personal bits live in `~/life/elisp/` and an optional `~/private/`.
- Set `+site-lisp-directory` or `+private-config-directory` before bootstrap to
  move them. `nil` turns off the private config.
- The agenda workflow needs the external `+life` library. Don't ask.
- Native builds, grammars and update steps:
  [dependency setup and updates](docs/DEPENDENCIES.md).

## Getting around

- `SPC` leads in normal and motion state. `C-SPC` in insert state.
- `,` is the local leader. Which-key reads your mind so I don't have to.
- `SPC pf` project files. `SPC /` project search. `SPC gs` Magit.
- `SPC aP` agent projects. Partner's handbook:
  [agent project guide](docs/AGENT-SHELL-PROJECTS.md).

## Proving it

```sh
emacs -Q --batch -l test/run-config.el
```

- Syntax, compilation, activation and every ERT suite, each in a fresh process.
- Mermaid checks want `mmdc` and a Chromium that's allowed to launch.

```sh
emacs -Q --batch -L test -l test/restore-config.el
```

- Opt-in fresh install. Needs network. Clones latest sources somewhere
  temporary, builds them, runs the same gates.
- On macOS it also pops a throwaway GUI Emacs to check completion and the mode
  line, then closes it. You saw nothing.

## Case files

- [MODULES.md](docs/MODULES.md): who owns what, who needs whom, when it wakes up.
- Package updates: run `scripts/verify-packages.el` after, or things get slow.

## Credits

- Helpers borrowed from Doom Emacs and
  [Chris Barrett's configuration](https://github.com/chrisbarrett/.emacs.d).
- I've heard it both ways.
