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

(unless (fboundp 'vulpea-buffer-prop-get)
  (defun vulpea-buffer-prop-get (_name) nil))

(unless (fboundp 'vulpea-buffer-meta-get-list)
  (defun vulpea-buffer-meta-get-list (_prop &optional _type) nil))

;;; Org stubs needed by +life agenda functions

(unless (boundp 'org-default-notes-file)
  (defvar org-default-notes-file nil))

(unless (boundp 'org-agenda-files)
  (defvar org-agenda-files nil))

(unless (boundp 'org-agenda-archives-mode)
  (defvar org-agenda-archives-mode nil))

(unless (fboundp 'org-read-agenda-file-list)
  (defun org-read-agenda-file-list () nil))

(unless (fboundp 'org-add-archive-files)
  (defun org-add-archive-files (files) files))

(unless (fboundp 'org-get-category)
  (defun org-get-category (&optional _pos _force-refresh) "???"))

(unless (fboundp 'org-find-exact-headline-in-buffer)
  (defun org-find-exact-headline-in-buffer (_heading &optional _buffer _pos-only) nil))

(unless (fboundp 'org-refile)
  (defun org-refile (&optional _arg _default-buffer _rfloc _msg) nil))

(unless (fboundp 'org-agenda-refile)
  (defun org-agenda-refile (&optional _goto _rfloc _no-update) nil))

(unless (fboundp 'org-tags-view)
  (defun org-tags-view (&optional _todo-only _match) nil))

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
