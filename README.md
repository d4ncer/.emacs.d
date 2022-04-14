# My .emacs setup

![Screenshot of elisp & TSX buffers](https://raw.githubusercontent.com/d4ncer/.emacs.d/master/screenshots/main-new.png)

## Main tenets

* Prefer basic semantic highlighting via grayscale & font weight; **no** syntax highlighting.
  * I use Nicolas Rougier's [`nano`](https://github.com/rougier/nano-emacs) as my theme base.
* I use `evil`, but my bindings are a chaotic mix of both `vim` & `emacs` philosophies
* Favour LSP to power an IDE-like experience wherever possible (I use `lsp-mode`), ~except for TypeScript, where I use [`tide`](https://github.com/ananthakumaran/tide/)~
* `org-mode` for building out a second brain (via `org-roam`), task management (via a GTD-esque workflow within `org-roam`), and literate development (via `org-babel`).
* `straight.el` for package management

## Credits

* Heavily ~~copied~~ borrowed & adapted from [Chris Barrett](https://github.com/chrisbarrett)'s
monster of a [setup](https://github.com/chrisbarrett/.emacs.d).
