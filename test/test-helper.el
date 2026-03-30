;;; test-helper.el --- Test helpers for +life.el tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Sets up load paths, loads vulpea-note for real struct/meta/predicate
;; support, stubs DB/IO functions so tests run without a live database.

;;; Code:

(require 'cl-lib)
(require 'ert)

;;; Load paths — pull from elpaca repos/builds

(let ((emacs-dir (or (getenv "EMACS_DIR")
                     (expand-file-name "~/.emacs.d"))))
  (dolist (dir (list
                (expand-file-name "elpaca/builds/dash" emacs-dir)
                (expand-file-name "elpaca/builds/s" emacs-dir)
                (expand-file-name "elpaca/builds/org" emacs-dir)
                (expand-file-name "elpaca/repos/vulpea" emacs-dir)
                (expand-file-name "lisp" emacs-dir)
                (expand-file-name "test" emacs-dir)))
    (add-to-list 'load-path dir)))

;;; Org variables needed by vulpea-note-meta-get-list

(require 'ol)  ; defines org-link-bracket-re

;;; Load real vulpea-note (struct, meta-get, tagged-p)

(require 'vulpea-note)

;;; Stub DB/IO functions that +life.el transitively needs

(unless (fboundp 'vulpea-create)
  (defun vulpea-create (_title &rest _args) nil))

(unless (fboundp 'vulpea-visit)
  (defun vulpea-visit (_note &optional _other-window) nil))

(unless (fboundp 'vulpea-select)
  (cl-defun vulpea-select (_prompt &key filter-fn require-match initial-prompt expand-aliases)
    (ignore filter-fn require-match initial-prompt expand-aliases)
    (make-vulpea-note)))

(unless (fboundp 'vulpea-select-from)
  (cl-defun vulpea-select-from (_prompt _notes &key require-match initial-prompt expand-aliases)
    (ignore require-match initial-prompt expand-aliases)
    (make-vulpea-note)))

(unless (fboundp 'vulpea-db-query-by-tags-every)
  (defun vulpea-db-query-by-tags-every (_tags) nil))

(unless (fboundp 'vulpea-db-query-by-links-some)
  (defun vulpea-db-query-by-links-some (_dest-ids &optional _link-type) nil))

(unless (fboundp 'vulpea-db-get-by-id)
  (defun vulpea-db-get-by-id (_id) nil))

(unless (fboundp 'vulpea-db-query-by-ids)
  (defun vulpea-db-query-by-ids (_ids) nil))

(unless (fboundp 'vulpea-meta-get)
  (defun vulpea-meta-get (_note-or-id _prop &optional _type) nil))

(unless (fboundp 'vulpea-buffer-meta-get)
  (defun vulpea-buffer-meta-get (_prop _type &optional _bound) nil))

(unless (fboundp 'vulpea-journal-today)
  (defun vulpea-journal-today () nil))

(unless (fboundp 'vulpea-db-autosync-mode)
  (defun vulpea-db-autosync-mode (&optional _arg) nil))

;; Provide features so require calls in +life.el succeed
(provide 'vulpea)
(provide 'vulpea-db)
(provide 'vulpea-select)

;;; Test utilities

(defun +life-test/make-note (&rest args)
  "Create a `vulpea-note' for testing.
ARGS are passed to `make-vulpea-note'."
  (apply #'make-vulpea-note args))

(provide 'test-helper)
;;; test-helper.el ends here
