;;; +eglot-tempel-compat.el --- Multiline snippet indentation -*- lexical-binding: t; -*-

;;; Commentary:
;; Adapt literal LSP indentation in the pinned eglot-tempel converter.
;; Remove after https://github.com/fejfighter/eglot-tempel provides equivalent
;; indentation and the multiline expansion regression passes without advice.

;;; Code:

(require 'eglot-tempel)
(require 'subr-x)

(defun +eglot-tempel-reindent (template)
  "Make eglot-tempel TEMPLATE reindent multi-line LSP snippets.
LSP snippets embed their own literal newlines and leading whitespace,
which tempel inserts verbatim (see `tempel--element').  The baked-in
indentation does not adapt to the column where the snippet is expanded,
so every line after the first lands at the wrong indent.  For each line
after the first, emit a newline, the whitespace-stripped text, then a
`>' element so `indent-according-to-mode' reindents once the line's real
content is present — the same behaviour yasnippet provides via
`yas-indent-line'.  Indenting after the text (rather than via `n>',
which indents the still-empty line) is what lets tree-sitter modes align
closing delimiters such as Elixir's `end'."
  (mapcan
   (lambda (elt)
     (if (and (stringp elt) (string-search "\n" elt))
         (let ((lines (split-string elt "\n")))
           (cons (car lines)
                 (mapcan (lambda (line) (list 'n (string-trim-left line) '>))
                         (cdr lines))))
       (list elt)))
   template))

(define-minor-mode +eglot-tempel-compat-mode
  "Reindent multiline LSP snippets during conversion to Tempel."
  :global t :group 'eglot
  (if +eglot-tempel-compat-mode
      (advice-add 'eglot-tempel--convert :filter-return #'+eglot-tempel-reindent)
    (advice-remove 'eglot-tempel--convert #'+eglot-tempel-reindent)))

(provide '+eglot-tempel-compat)
;;; +eglot-tempel-compat.el ends here
