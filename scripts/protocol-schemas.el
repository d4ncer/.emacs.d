;;; protocol-schemas.el --- Render the remote protocol's record schemas -*- lexical-binding: t; -*-

;;; Commentary:
;; Run after changing a record kind or a host capability:
;;
;;   emacs -Q --batch -l scripts/protocol-schemas.el -f +protocol-schemas-batch
;;
;; docs/PSTACK-REMOTE-PROTOCOL.md holds sections between marker comments,
;; <!-- BEGIN GENERATED NAME --> and <!-- END GENERATED NAME -->.  This
;; renders each from the code: the field types, one section per record
;; kind in `+agent-shell-arena-remote--kinds' with its fields, types and
;; presence, and the host capabilities in
;; `+agent-shell-arena-remote-capability-registry'.  The batch command
;; rewrites those sections in place; the gate fails when they differ.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defconst +protocol-schemas-root
  (file-name-directory
   (directory-file-name (file-name-directory (or load-file-name
                                                 buffer-file-name))))
  "This configuration's top level.")

(let ((builds (expand-file-name "elpaca/builds" +protocol-schemas-root)))
  (when (file-directory-p builds)
    (dolist (directory (directory-files builds t "\\`[^.]"))
      (when (file-directory-p directory) (add-to-list 'load-path directory))))
  (dolist (directory '("lisp/compat" "lisp"))
    (add-to-list 'load-path (expand-file-name directory +protocol-schemas-root))))
(require '+agent-shell-arena-remote)

(defconst +protocol-schemas-file
  (expand-file-name "docs/PSTACK-REMOTE-PROTOCOL.md" +protocol-schemas-root)
  "The protocol spec whose generated sections this renders.")

(defconst +protocol-schemas-types
  `((text "string" "Any JSON string.")
    (id "id" ,(format "A string matching the Emacs regexp `` %s ``, safe \
in a path and a ref."
                      +agent-shell-arena-git--name-regexp))
    (sha "sha" "A full commit id: 40 or 64 lowercase hex digits.")
    (agent "agent" ,(format "An agent identifier matching `` %s ``."
                            +agent-shell-arena-remote--agent-regexp))
    (host-kind "host kind" "A kind registered in \
`+agent-shell-arena-remote-host-kinds`, by default one of \
`coder`, `ssh` and `local`.")
    (failure-kind "failure kind"
                  ,(format "A provider failure kind, one of %s; a reader \
takes an unknown one as `provider_error`."
                           (mapconcat (lambda (entry) (format "`%s`" (car entry)))
                                      +agent-shell-drive-failure-kinds ", ")))
    (natnum "whole number" "A JSON integer of 0 or more.")
    (number "number" "Any JSON number.")
    (boolean "boolean" "`true` or `false`.")
    (profile "profile" "A repository profile as plain data, resolved on \
the laptop; its keys are the profile's own, such as `shared` and `env`."))
  "Each scalar field type: (TYPE LABEL DEFINITION).")

(defun +protocol-schemas--type (type)
  "Return field TYPE as the spec's tables write it."
  (pcase type
    (`(opt ,inner) (+protocol-schemas--type inner))
    (`(enum . ,symbols)
     (concat "one of " (mapconcat (lambda (symbol) (format "`\"%s\"`" symbol))
                                  symbols ", ")))
    ('(strings) "list of strings")
    (`(list ,kind) (format "list of [`%s`](#%s)" kind kind))
    (`(record ,kind) (format "[`%s`](#%s)" kind kind))
    (_ (or (nth 1 (assq type +protocol-schemas-types))
           (error "Unknown field type %S" type)))))

(defun +protocol-schemas--presence (type)
  "Return what a reader makes of a field of TYPE that is absent."
  (pcase type
    (`(opt ,_) "optional, absent or null")
    ('boolean "absent is false")
    ((or '(strings) `(list ,_)) "absent is empty")
    (_ "required")))

(defun +protocol-schemas--file-line (kind)
  "Return the line saying how KIND is stored, or nil when it is nested."
  (when-let* ((top (assq kind +agent-shell-arena-remote--top-kinds)))
    (format "A file of its own: `\"version\"` from 1 to %d%s.\n\n"
            (or (nth 2 top) +agent-shell-arena-remote-wire-version)
            (if (nth 1 top) (format " and `\"kind\": \"%s\"`" kind)
              ", and no `\"kind\"`"))))

(defun +protocol-schemas-render-kind (kind)
  "Return the spec's section for record KIND."
  (pcase-let ((`(,_ ,_ ,_ . ,fields) (+agent-shell-arena-remote--kind kind)))
    (concat
     (format "#### `%s`\n\n" kind)
     (+protocol-schemas--file-line kind)
     "| Key | Type | Presence |\n|---|---|---|\n"
     (mapconcat (pcase-lambda (`(,_ ,key ,type))
                  (format "| `%s` | %s | %s |\n" key
                          (+protocol-schemas--type type)
                          (+protocol-schemas--presence type)))
                fields "")
     "\n")))

(defun +protocol-schemas-render-types ()
  "Return the spec's section defining the scalar field types."
  (concat "| Type | Meaning |\n|---|---|\n"
          (mapconcat (pcase-lambda (`(,_ ,label ,definition))
                       (format "| %s | %s |\n" label definition))
                     +protocol-schemas-types "")
          "\n"))

(defun +protocol-schemas-render-capabilities ()
  "Return the spec's section listing the host capabilities."
  (concat "| Capability | Proved when | To set it up |\n|---|---|---|\n"
          (mapconcat (pcase-lambda (`(,_ ,key . ,properties))
                       (format "| `%s` | %s | %s |\n" key
                               (plist-get properties :proof)
                               (plist-get properties :setup)))
                     +agent-shell-arena-remote-capability-registry "")
          "\n"))

(defun +protocol-schemas-sections ()
  "Return each generated section as (NAME . TEXT), in the spec's order."
  (append
   (list (cons "types" (+protocol-schemas-render-types)))
   (mapcar (lambda (entry)
             (cons (format "record %s" (car entry))
                   (+protocol-schemas-render-kind (car entry))))
           +agent-shell-arena-remote--kinds)
   (list (cons "capabilities" (+protocol-schemas-render-capabilities)))))

(defconst +protocol-schemas--marker
  "^<!-- \\(BEGIN\\|END\\) GENERATED \\([^>]*[^ >]\\) -->$"
  "A line opening or closing a generated section.")

(defun +protocol-schemas-read (text)
  "Return the generated sections in TEXT as (NAME . CONTENTS), in order.
Signal when a marker has no partner."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let (sections)
      (while (re-search-forward +protocol-schemas--marker nil t)
        (let ((name (match-string 2)))
          (unless (equal (match-string 1) "BEGIN")
            (error "END GENERATED %s has no BEGIN" name))
          (let ((start (1+ (line-end-position))))
            (unless (re-search-forward
                     (format "^<!-- END GENERATED %s -->$" (regexp-quote name))
                     nil t)
              (error "BEGIN GENERATED %s has no END" name))
            (push (cons name (buffer-substring-no-properties
                              start (line-beginning-position)))
                  sections))))
      (nreverse sections))))

(defun +protocol-schemas-drift (text)
  "Return the names of the generated sections TEXT holds out of date.
That counts one TEXT lacks, one the code no longer generates, and one
whose contents differ from what the generator renders now."
  (let ((written (+protocol-schemas-read text))
        (wanted (+protocol-schemas-sections)))
    (append
     (seq-keep (pcase-lambda (`(,name . ,contents))
                 (unless (equal contents (cdr (assoc name written))) name))
               wanted)
     (seq-keep (lambda (section)
                 (unless (assoc (car section) wanted) (car section)))
               written))))

(defun +protocol-schemas-update (text)
  "Return TEXT with each generated section it holds rendered afresh.
Signal when TEXT lacks a section the code generates."
  (let ((sections (+protocol-schemas-sections)))
    (when-let* ((missing (seq-remove (lambda (section)
                                       (assoc (car section)
                                              (+protocol-schemas-read text)))
                                     sections)))
      (error "The spec has no section for %s; add its markers"
             (mapconcat #'car missing ", ")))
    (with-temp-buffer
      (insert text)
      (pcase-dolist (`(,name . ,contents) sections)
        (goto-char (point-min))
        (re-search-forward (format "^<!-- BEGIN GENERATED %s -->\n"
                                   (regexp-quote name)))
        (let ((start (point)))
          (re-search-forward (format "^<!-- END GENERATED %s -->$"
                                     (regexp-quote name)))
          (delete-region start (line-beginning-position))
          (goto-char start)
          (insert contents)))
      (buffer-string))))

(defun +protocol-schemas--contents (file)
  "Return FILE's text."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix)) (insert-file-contents file))
    (buffer-string)))

(defun +protocol-schemas-batch ()
  "Rewrite the generated sections of the protocol spec, then exit."
  (let* ((text (+protocol-schemas--contents +protocol-schemas-file))
         (updated (+protocol-schemas-update text)))
    (if (equal text updated)
        (message "%s is up to date" +protocol-schemas-file)
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region updated nil +protocol-schemas-file nil 'silent))
      (message "Rewrote %s" (string-join (+protocol-schemas-drift text) ", ")))
    (kill-emacs 0)))

(provide 'protocol-schemas)
;;; protocol-schemas.el ends here
