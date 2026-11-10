;;; +elisp-tools.el --- Batch Elisp indentation and form editing -*- lexical-binding: t; -*-

;;; Commentary:
;; Tools any agent can run from a shell, without a live Emacs:
;;
;;   emacs -Q --batch -l lisp/+elisp-tools.el -f +elisp-tools-batch COMMAND ...
;;
;;   indent [--fix] [FILE...]   Reindent FILEs, or all tracked Elisp; exit 1
;;                              listing every line that moves, or with --fix
;;                              rewrite them.
;;   show FILE NAME             Print the top-level form named NAME.
;;   replace FILE NAME SOURCE   Replace that form with the one in SOURCE.
;;   insert FILE WHERE SOURCE   Insert SOURCE's form; WHERE is --end (before
;;                              `provide'), --after=NAME or --before=NAME.
;;   delete FILE NAME           Delete that form.
;;
;; SOURCE is a file, or - for standard input, holding exactly one form.
;; NAME is the symbol a form defines, its second element, as in
;; (defun NAME ...), (use-package NAME ...) or (put 'NAME ...).  When
;; several forms share it, prefix the head, cl-defmethod:NAME, or suffix
;; the line the form starts on, NAME@120.
;;
;; An edit reindents only the form it writes, byte-compiles the file and
;; prints the warnings about that form.  It exits 0 when there are none,
;; 2 when there are, and 1 when the edit or compilation fails.
;;
;; Indentation matches the live configuration: the `calculate-lisp-indent'
;; patch from `+lisp-mode-patch', every package build on `load-path', and the
;; indent declarations of every macro this repository defines.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'bytecomp)

(defconst +elisp-tools-root
  (file-name-directory
   (directory-file-name (file-name-directory (or load-file-name
                                                 buffer-file-name))))
  "This configuration's top level.")

(defvar +elisp-tools--ready nil
  "Non-nil once `+elisp-tools-setup' has prepared indentation.")

(defun +elisp-tools-files ()
  "Return the absolute names of the tracked Elisp files."
  (let ((default-directory +elisp-tools-root))
    (with-temp-buffer
      (unless (eql 0 (call-process "git" nil '(t nil) nil
                                   "ls-files" "--" "*.el"))
        (error "git ls-files failed in %s" default-directory))
      (mapcar #'expand-file-name (split-string (buffer-string) "\n" t)))))

(defun +elisp-tools--forms (file)
  "Return the top-level forms FILE reads to, ignoring a trailing error."
  (with-temp-buffer
    (insert-file-contents file)
    (let (forms)
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (error nil))
      (nreverse forms))))

(defun +elisp-tools--declare (form)
  "Apply FORM's indent declaration or indent property, if it has one."
  (pcase form
    (`(,(or 'defmacro 'cl-defmacro 'defun 'cl-defun 'defsubst 'define-inline)
       ,(and name (pred symbolp)) ,_ . ,body)
     (dolist (item body)
       (when (eq (car-safe item) 'declare)
         (pcase (assq 'indent (cdr item))
           (`(indent ,spec) (function-put name 'lisp-indent-function spec))))))
    (`(,(or 'put 'function-put) ',(and name (pred symbolp))
       'lisp-indent-function ,(and spec (or (pred integerp) (pred symbolp))))
     (function-put name 'lisp-indent-function spec))))

(defun +elisp-tools--walk (form function)
  "Call FUNCTION on FORM and on every list nested in it."
  (when (consp form)
    (funcall function form)
    (while (consp form)
      (+elisp-tools--walk (car form) function)
      (setq form (cdr form)))))

(defun +elisp-tools--module-p (feature)
  "Return non-nil when loading FEATURE would configure this Emacs."
  (when-let* ((file (locate-library (symbol-name feature))))
    (or (string-prefix-p (expand-file-name "modules/" +elisp-tools-root)
                         (expand-file-name file))
        (string-prefix-p "run-" (symbol-name feature))
        (string-prefix-p "restore-" (symbol-name feature)))))

(defun +elisp-tools-setup (&optional files)
  "Prepare indentation as the live configuration does it.
Read every tracked file's indent declarations, and load the libraries
FILES require and the definitions of the autoloaded functions they call,
so each package's own declarations apply too."
  (unless +elisp-tools--ready
    (setq load-prefer-newer t)
    (let ((builds (expand-file-name "elpaca/builds" +elisp-tools-root)))
      (when (file-directory-p builds)
        (dolist (directory (directory-files builds t "\\`[^.]"))
          (when (file-directory-p directory)
            (add-to-list 'load-path directory t)))
        (dolist (file (file-expand-wildcards
                       (expand-file-name "*/*-autoloads.el" builds)))
          (with-demoted-errors "Autoloads: %S" (load file nil t)))))
    (dolist (directory '("test" "lisp/compat" "lisp/project-files" "lisp"))
      (add-to-list 'load-path (expand-file-name directory +elisp-tools-root)))
    (require '+lisp-mode-patch)
    (let ((all (mapcar #'+elisp-tools--forms
                       (delete-dups (append (mapcar #'expand-file-name files)
                                            (+elisp-tools-files)))))
          features heads)
      (dolist (file (or files (+elisp-tools-files)))
        (dolist (form (+elisp-tools--forms file))
          (+elisp-tools--walk
           form (lambda (form)
                  (pcase form
                    (`(require ',(and feature (pred symbolp)) . ,_)
                     (push feature features))
                    (`(use-package ,(and feature (pred symbolp)) . ,_)
                     (push feature features))
                    (`(,(and head (pred symbolp)) . ,_) (push head heads)))))))
      (let ((inhibit-message t) (message-log-max nil))
        (dolist (feature (delete-dups features))
          (unless (or (featurep feature) (+elisp-tools--module-p feature))
            (ignore-errors (require feature nil t))))
        (dolist (head (delete-dups heads))
          (when (and (autoloadp (symbol-function head))
                     (not (function-get head 'lisp-indent-function)))
            (ignore-errors (autoload-do-load (symbol-function head) head)))))
      ;; Last, so this repository's declarations win over a loaded copy.
      (dolist (forms all)
        (dolist (form forms) (+elisp-tools--walk form #'+elisp-tools--declare))))
    (setq +elisp-tools--ready t)))

(defun +elisp-tools--mode ()
  "Prepare the current buffer for indenting Elisp."
  (delay-mode-hooks (emacs-lisp-mode))
  (setq-local indent-tabs-mode nil))

(defun +elisp-tools--moved (before after)
  "Return the 1-based numbers of lines that differ between BEFORE and AFTER."
  (cl-loop for old in (split-string before "\n")
           for new in (split-string after "\n")
           for line from 1
           unless (equal old new) collect line))

(defun +elisp-tools-indent-file (file &optional fix)
  "Return the lines of FILE that reindenting moves.
With FIX, rewrite FILE when any move, after checking it reads the same."
  (let ((path (expand-file-name file)))
    (with-temp-buffer
      (insert-file-contents path)
      (+elisp-tools--mode)
      (let ((before (buffer-string)))
        (indent-region (point-min) (point-max))
        (let ((moved (+elisp-tools--moved before (buffer-string))))
          (when (and fix moved)
            (unless (equal (+elisp-tools--read-all before)
                           (+elisp-tools--read-all (buffer-string)))
              (error "%s: reindenting changed what it reads" file))
            (let ((coding-system-for-write 'utf-8-unix))
              (write-region nil nil path nil 'silent)))
          moved)))))

(defun +elisp-tools--read-all (text)
  "Return every form TEXT reads to."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let (forms)
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil))
      (nreverse forms))))

(defun +elisp-tools-indent (files &optional fix)
  "Reindent FILES; return (FILE . MOVED-LINES) for each that moves.
With FIX, rewrite those files."
  (+elisp-tools-setup)
  (cl-loop for file in files
           for moved = (+elisp-tools-indent-file file fix)
           when moved collect (cons file moved)))

;;;; Forms

(defun +elisp-tools--name (form)
  "Return the symbol FORM names, its second element, or nil."
  (pcase (car-safe (cdr-safe form))
    ((and name (pred symbolp) (guard name)) name)
    (`(quote ,(and name (pred symbolp))) name)))

(defun +elisp-tools--lead (start)
  "Return where the comment lines directly above START begin, or START."
  (save-excursion
    (goto-char start)
    (beginning-of-line)
    (let ((lead start))
      (while (and (zerop (forward-line -1))
                  (looking-at-p "[ \t]*;"))
        (setq lead (point)))
      lead)))

(defun +elisp-tools--top-level ()
  "Return (HEAD NAME START END LEAD) for each top-level form in the buffer.
LEAD is where the comment lines directly above the form begin."
  (save-excursion
    (goto-char (point-min))
    (let (forms)
      (condition-case nil
          (while t
            (forward-comment (buffer-size))
            (let* ((start (point)) (form (read (current-buffer))))
              (push (list (car-safe form) (+elisp-tools--name form)
                          start (point) (+elisp-tools--lead start))
                    forms)))
        (end-of-file nil))
      (nreverse forms))))

(defun +elisp-tools--find (name)
  "Return the one top-level (HEAD NAME START END LEAD) that NAME selects.
NAME is a symbol name, HEAD:NAME to choose among forms sharing it by
their head, or NAME@LINE by the line where the form starts."
  (pcase-let* ((`(,head ,symbol ,line)
                (progn
                  (unless (string-match
                           "\\`\\(?:\\([^:]+\\):\\)?\\(.+?\\)\\(?:@\\([0-9]+\\)\\)?\\'"
                           name)
                    (error "Bad form name %s" name))
                  (list
                   (and (match-string 1 name) (intern (match-string 1 name)))
                   (intern (match-string 2 name))
                   (and (match-string 3 name)
                        (string-to-number (match-string 3 name))))))
               (matches (seq-filter
                         (lambda (entry)
                           (and (eq (nth 1 entry) symbol)
                                (or (null head) (eq (car entry) head))
                                (or (null line)
                                    (= line (line-number-at-pos (nth 2 entry))))))
                         (+elisp-tools--top-level))))
    (pcase matches
      (`(,entry) entry)
      ('nil (error "No top-level form named %s" name))
      (_ (error "%d forms named %s, at lines %s; add HEAD: or @LINE"
                (length matches) name
                (mapconcat (lambda (entry)
                             (format "%d (%s)" (line-number-at-pos (nth 2 entry))
                                     (car entry)))
                           matches ", "))))))

(defun +elisp-tools--source (source)
  "Return SOURCE's text, from a file or from standard input for -.
Signal an error unless it holds exactly one form."
  (let ((text (string-trim
               (with-temp-buffer
                 (insert-file-contents
                  (if (equal source "-") "/dev/stdin" source))
                 (buffer-string)))))
    (unless (= 1 (length (condition-case err (+elisp-tools--read-all text)
                           (error (error "%s does not read: %S" source err)))))
      (error "%s must hold exactly one form" source))
    text))

(defun +elisp-tools--write (start end text &optional before after)
  "Replace START to END with BEFORE, TEXT and AFTER.
Reindent only TEXT and return its line range."
  (goto-char start)
  (delete-region start end)
  (insert (or before ""))
  (let ((beginning (point-marker)))
    (insert text)
    (indent-region beginning (point))
    (prog1 (cons (line-number-at-pos beginning) (line-number-at-pos))
      (insert (or after ""))
      (set-marker beginning nil))))

(defun +elisp-tools-edit (file function)
  "Call FUNCTION in a buffer visiting FILE's text, then save it.
FUNCTION returns the edited line range, which this returns."
  (+elisp-tools-setup (list file))
  (let ((path (expand-file-name file)))
    (with-temp-buffer
      (insert-file-contents path)
      (+elisp-tools--mode)
      (prog1 (funcall function)
        (let ((coding-system-for-write 'utf-8-unix))
          (write-region nil nil path nil 'silent))))))

(defun +elisp-tools-show (file name)
  "Return the text of FILE's top-level form NAME."
  (with-temp-buffer
    (insert-file-contents file)
    (+elisp-tools--mode)
    (pcase-let ((`(,_ ,_ ,start ,end) (+elisp-tools--find name)))
      (buffer-substring-no-properties start end))))

(defun +elisp-tools-replace (file name text)
  "Replace FILE's top-level form NAME with TEXT; return its line range."
  (+elisp-tools-edit
   file (lambda ()
          (pcase-let ((`(,_ ,_ ,start ,end) (+elisp-tools--find name)))
            (+elisp-tools--write start end text)))))

(defun +elisp-tools-insert (file where text)
  "Insert TEXT into FILE as a top-level form; return its line range.
WHERE is (after . NAME), (before . NAME), or nil for before `provide'."
  (+elisp-tools-edit
   file (lambda ()
          (let ((position
                 (pcase where
                   (`(after . ,name) (nth 3 (+elisp-tools--find name)))
                   (`(before . ,name) (nth 4 (+elisp-tools--find name)))
                   (_ (or (nth 4 (seq-find (lambda (entry)
                                             (eq (car entry) 'provide))
                                           (reverse (+elisp-tools--top-level))))
                          (point-max))))))
            (if (eq (car where) 'after)
                (+elisp-tools--write position position text "\n\n")
              (+elisp-tools--write position position text nil "\n\n"))))))

(defun +elisp-tools-delete (file name)
  "Delete FILE's top-level form NAME, its comment and the blank line after."
  (+elisp-tools-edit
   file (lambda ()
          (pcase-let ((`(,_ ,_ ,_ ,end ,start) (+elisp-tools--find name)))
            (goto-char end)
            (skip-chars-forward " \t")
            (when (eolp) (forward-line 1))
            (when (looking-at-p "[ \t]*$") (forward-line 1))
            (delete-region start (point))
            (let ((line (line-number-at-pos start))) (cons line line))))))

(defun +elisp-tools-compile (file)
  "Byte-compile FILE to a temporary place; return (STATUS . WARNINGS).
STATUS is error, warning or ok."
  (let* ((temporary (make-temp-file "elisp-tools-" t))
         (byte-compile-dest-file-function
          (lambda (source)
            (expand-file-name (concat (file-name-base source) ".elc")
                              temporary)))
         (byte-compile-log-buffer (generate-new-buffer " *elisp-tools log*"))
         (inhibit-message t)
         (compiled nil))
    (unwind-protect
        (progn
          (setq compiled (ignore-errors (byte-compile-file
                                         (expand-file-name file))))
          (let ((log (with-current-buffer byte-compile-log-buffer
                       (string-trim (buffer-string)))))
            (cons (cond ((not compiled) 'error)
                        ((string-match-p "Warning" log) 'warning)
                        (t 'ok))
                  log)))
      (kill-buffer byte-compile-log-buffer)
      (delete-directory temporary t))))

;;;; Command line

(defun +elisp-tools--usage ()
  "Signal an error describing the batch commands."
  (error "Usage: indent [--fix] [FILE...] | show FILE NAME | \
replace FILE NAME SOURCE | insert FILE --end|--after=NAME|--before=NAME \
SOURCE | delete FILE NAME"))

(defun +elisp-tools--entries (log)
  "Return compilation LOG's warnings, each with its continuation lines."
  (let (entries)
    (dolist (line (split-string log "\n" t))
      (cond ((string-match-p "\\`\\(?:In \\|\\s-*\\'\\)" line))
            ((and entries (string-match-p "\\`\\s-" line))
             (setcar entries (concat (car entries) "\n" line)))
            (t (push line entries))))
    (nreverse entries)))

(defun +elisp-tools--message (entry)
  "Return warning ENTRY without its file and position."
  (replace-regexp-in-string "\\`[^ ]*?:\\(?:[0-9]+:[0-9]+:\\)? *" "" entry))

(defun +elisp-tools--warnings (log range text &optional before)
  "Split compilation LOG's new warnings into (EDITED . OTHERS).
A warning is new unless BEFORE, the log before the edit, has its
message.  It is EDITED when it falls in the line RANGE, or has no line
and names a symbol that the edited TEXT mentions."
  (let ((old (mapcar #'+elisp-tools--message (+elisp-tools--entries (or before ""))))
        edited others)
    (dolist (entry (+elisp-tools--entries log))
      (cond ((member (+elisp-tools--message entry) old))
            ((cond ((string-match ":\\([0-9]+\\):[0-9]+: " entry)
                    (<= (car range) (string-to-number (match-string 1 entry))
                        (cdr range)))
                   ((string-match "\u2018\\([^\u2019]+\\)\u2019" entry)
                    (string-match-p (concat "\\_<" (regexp-quote (match-string 1 entry))
                                            "\\_>")
                                    text))
                   (t t))
             (push entry edited))
            (t (push entry others))))
    (cons (nreverse edited) (nreverse others))))

(defun +elisp-tools--report-edit (file range before)
  "Print the edited RANGE of FILE, compile it, and exit with its status.
Only new warnings about the edited form count, against BEFORE, the
compilation log before the edit; the other new ones are tallied."
  (princ (format "%s:%d-%d: edited\n" file (car range) (cdr range)))
  (pcase-let* ((`(,status . ,log) (+elisp-tools-compile file))
               (text (with-temp-buffer
                       (insert-file-contents file)
                       (goto-char (point-min))
                       (forward-line (1- (car range)))
                       (buffer-substring (point) (progn (forward-line
                                                         (- (cdr range)
                                                            (car range) -1))
                                                        (point)))))
               (`(,edited . ,others)
                (+elisp-tools--warnings log range text before)))
    (dolist (entry edited) (princ (concat entry "\n")))
    (when others
      (princ (format "%d new warnings elsewhere in the file\n"
                     (length others))))
    (princ (format "Byte compilation: %s\n"
                   (cond ((eq status 'error) "error")
                         (edited "new warnings in the edited form")
                         (t "ok"))))
    (kill-emacs (cond ((eq status 'error) 1) (edited 2) (t 0)))))

(defun +elisp-tools--edit-command (file edit)
  "Compile FILE, call EDIT for the edited line range, then report on it."
  (+elisp-tools-setup (list file))
  (let ((before (cdr (+elisp-tools-compile file))))
    (+elisp-tools--report-edit file (funcall edit) before)))

(defun +elisp-tools-batch ()
  "Run the command in `command-line-args-left', then exit."
  (let ((args command-line-args-left))
    (setq command-line-args-left nil)
    (condition-case err
        (pcase args
          (`("indent" . ,rest)
           (let* ((fix (member "--fix" rest))
                  (default-directory (if (remove "--fix" rest)
                                         default-directory
                                       +elisp-tools-root))
                  (files (or (remove "--fix" rest) (+elisp-tools-files)))
                  (moved (+elisp-tools-indent files fix)))
             (dolist (entry moved)
               (princ (format "%s:%s: %s\n"
                              (file-relative-name (car entry))
                              (mapconcat #'number-to-string
                                         (seq-take (cdr entry) 8) ",")
                              (if fix "reindented" "moves when reindented"))))
             (princ (format "Indentation: %d of %d files %s\n" (length moved)
                            (length files) (if fix "reindented" "move")))
             (kill-emacs (if (and moved (not fix)) 1 0))))
          (`("show" ,file ,name)
           (princ (concat (+elisp-tools-show file name) "\n"))
           (kill-emacs 0))
          (`("replace" ,file ,name ,source)
           (let ((text (+elisp-tools--source source)))
             (+elisp-tools--edit-command
              file (lambda () (+elisp-tools-replace file name text)))))
          (`("insert" ,file ,where ,source)
           (let ((where (cond ((equal where "--end") nil)
                              ((string-prefix-p "--after=" where)
                               (cons 'after (substring where 8)))
                              ((string-prefix-p "--before=" where)
                               (cons 'before (substring where 9)))
                              (t (+elisp-tools--usage))))
                 (text (+elisp-tools--source source)))
             (+elisp-tools--edit-command
              file (lambda () (+elisp-tools-insert file where text)))))
          (`("delete" ,file ,name)
           (+elisp-tools--edit-command
            file (lambda () (+elisp-tools-delete file name))))
          (_ (+elisp-tools--usage)))
      (error (princ (format "%s\n" (error-message-string err)))
             (kill-emacs 1)))))

(provide '+elisp-tools)
;;; +elisp-tools.el ends here
