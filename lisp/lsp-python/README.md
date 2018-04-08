[![MELPA](https://melpa.org/packages/lsp-python-badge.svg)](https://melpa.org/#/lsp-python)

Python support for lsp-mode using [python-language-server](https://github.com/palantir/python-language-server).

## Installation

Install [`lsp-mode`](https://github.com/emacs-lsp/lsp-mode) first, and either clone
this repository, or install from MELPA. Add the following to your `.emacs`:

```emacs-lisp
(require 'lsp-mode)
(require 'lsp-python)
(add-hook 'python-mode-hook #'lsp-python-enable)
```
