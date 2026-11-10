;;; +lisp-mode-patch.el --- el-patch indentation for Lisp -*- lexical-binding: t; -*-

;;; Commentary:
;; Loaded with the first Lisp buffer, and by +elisp-tools for batch
;; indentation.

;;; Code:

(require 'cl-lib)
(require 'el-patch)
(require 'lisp-mode)

(defvar calculate-lisp-indent-last-sexp)

(el-patch-defun calculate-lisp-indent (&optional parse-start)
  (el-patch-concat
    "Return appropriate indentation for current line as Lisp code.
In usual case returns an integer: the column to indent to.
If the value is nil, that means don't change the indentation
because the line starts inside a string.

PARSE-START may be a buffer position to start parsing from, or a
parse state as returned by calling `parse-partial-sexp' up to the
beginning of the current line.

The value can also be a list of the form (COLUMN CONTAINING-SEXP-START).
This means that following lines at the same level of indentation
should not necessarily be indented the same as this line.
Then COLUMN is the column to indent to, and CONTAINING-SEXP-START
is the buffer position of the start of the containing expression."
    (el-patch-add
      "\n\nAlso align plist keywords and indent quoted lists as data, as
Doom Emacs does."))
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
          state
          ;; setting this to a number inhibits calling hook
          (desired-indent nil)
          (retry t)
          whitespace-after-open-paren
          calculate-lisp-indent-last-sexp containing-sexp)
      (cond ((or (markerp parse-start) (integerp parse-start))
             (goto-char parse-start))
            ((null parse-start) (beginning-of-defun))
            (t (setq state parse-start)))
      (unless state
        ;; Find outermost containing sexp
        (while (< (point) indent-point)
          (setq state (parse-partial-sexp (point) indent-point 0))))
      ;; Find innermost containing sexp
      (while (and retry
                  state
                  (> (elt state 0) 0))
        (setq retry nil)
        (setq calculate-lisp-indent-last-sexp (elt state 2))
        (setq containing-sexp (elt state 1))
        ;; Position following last unclosed open.
        (goto-char (1+ containing-sexp))
        ;; Is there a complete sexp since then?
        (if (and calculate-lisp-indent-last-sexp
                 (> calculate-lisp-indent-last-sexp (point)))
            ;; Yes, but is there a containing sexp after that?
            (let ((peek (parse-partial-sexp calculate-lisp-indent-last-sexp
                                            indent-point 0)))
              (if (setq retry (car (cdr peek))) (setq state peek)))))
      (if retry
          nil
        ;; Innermost containing sexp found
        (goto-char (1+ containing-sexp))
        (setq whitespace-after-open-paren (looking-at (rx whitespace)))
        (if (not calculate-lisp-indent-last-sexp)
            ;; indent-point immediately follows open paren.
            ;; Don't call hook.
            (setq desired-indent (current-column))
          ;; Find the start of first element of containing sexp.
          (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
          (cond ((looking-at "\\s(")
                 ;; First element of containing sexp is a list.
                 ;; Indent under that list.
                 )
                ((> (save-excursion (forward-line 1) (point))
                    calculate-lisp-indent-last-sexp)
                 ;; This is the first line to start within the containing sexp.
                 ;; It's almost certainly a function call.
                 (if (or (= (point) calculate-lisp-indent-last-sexp)
                         whitespace-after-open-paren
                         (el-patch-add
                           ;; Align keywords in plists if each newline begins with
                           ;; a keyword. This is useful for "unquoted plist
                           ;; function" macros, like `map!' and `defhydra'.
                           (when-let* ((first (elt state 1))
                                       (char (char-after (1+ first))))
                             (and (eq char ?:)
                                  (ignore-errors
                                    (or (save-excursion
                                          (goto-char first)
                                          ;; FIXME Can we avoid `syntax-ppss'?
                                          (when-let* ((parse-sexp-ignore-comments t)
                                                      (end (scan-lists (point) 1 0))
                                                      (depth (ppss-depth (syntax-ppss))))
                                            (and (re-search-forward "^\\s-*:" end t)
                                                 (= (ppss-depth (syntax-ppss))
                                                    (1+ depth)))))
                                        (save-excursion
                                          (cl-loop for pos in (reverse (elt state 9))
                                                   unless (memq (char-after (1+ pos)) '(?: ?\())
                                                   do (goto-char (1+ pos))
                                                   for fn = (read (current-buffer))
                                                   if (symbolp fn)
                                                   return (function-get fn 'indent-plists-as-data)))))))

                           ;; Check for quotes or backquotes around.
                           (let ((positions (elt state 9))
                                 (quotep 0))
                             (while positions
                               (let ((point (pop positions)))
                                 (or (when-let* ((char (char-before point)))
                                       (cond
                                        ((eq char ?\())
                                        ((memq char '(?\' ?\`))
                                         (or (save-excursion
                                               (goto-char (1+ point))
                                               (skip-chars-forward "( ")
                                               (when-let* ((fn (ignore-errors (read (current-buffer)))))
                                                 (if (and (symbolp fn)
                                                          (fboundp fn)
                                                          ;; Only special forms and
                                                          ;; macros have special
                                                          ;; indent needs.
                                                          (not (functionp fn)))
                                                     (setq quotep 0))))
                                             (cl-incf quotep)))
                                        ((memq char '(?, ?@))
                                         (setq quotep 0))))
                                     ;; If the spelled out `quote' or `backquote'
                                     ;; are used, let's assume
                                     (save-excursion
                                       (goto-char (1+ point))
                                       (and (looking-at-p "\\(\\(?:back\\)?quote\\)[\t\n\f\s]+(")
                                            (cl-incf quotep 2)))
                                     (setq quotep (max 0 (1- quotep))))))
                             (> quotep 0))))
                     ;; Containing sexp has nothing before this line
                     ;; except the first element, or the first element is
                     ;; preceded by whitespace.  Indent under that element.
                     nil
                   ;; Skip the first element, find start of second (the first
                   ;; argument of the function call) and indent under.
                   (progn (forward-sexp 1)
                          (parse-partial-sexp (point)
                                              calculate-lisp-indent-last-sexp
                                              0 t)))
                 (backward-prefix-chars))
                (t
                 ;; Indent beneath first sexp on same line as
                 ;; `calculate-lisp-indent-last-sexp'.  Again, it's
                 ;; almost certainly a function call.
                 (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point) calculate-lisp-indent-last-sexp
                                     0 t)
                 (backward-prefix-chars)))))
      ;; Point is at the point to indent under unless we are inside a string.
      ;; Call indentation hook except when overridden by lisp-indent-offset
      ;; or if the desired indentation has already been computed.
      (let ((normal-indent (current-column)))
        (cond ((elt state 3)
               ;; Inside a string, don't change indentation.
               nil)
              ((and (integerp lisp-indent-offset) containing-sexp)
               ;; Indent by constant offset
               (goto-char containing-sexp)
               (+ (current-column) lisp-indent-offset))
              ;; in this case calculate-lisp-indent-last-sexp is not nil
              (calculate-lisp-indent-last-sexp
               (or
                ;; try to align the parameters of a known function
                (and lisp-indent-function
                     (not retry)
                     (funcall lisp-indent-function indent-point state))
                ;; If the function has no special alignment
                ;; or it does not apply to this argument,
                ;; try to align a constant-symbol under the last
                ;; preceding constant symbol, if there is such one of
                ;; the last 2 preceding symbols, in the previous
                ;; uncommented line.
                (and (save-excursion
                       (goto-char indent-point)
                       (skip-chars-forward " \t")
                       (looking-at ":"))
                     ;; The last sexp may not be at the indentation
                     ;; where it begins, so find that one, instead.
                     (save-excursion
                       (goto-char calculate-lisp-indent-last-sexp)
                       ;; Handle prefix characters and whitespace
                       ;; following an open paren.  (Bug#1012)
                       (backward-prefix-chars)
                       (while (not (save-excursion
                                     (skip-chars-backward " \t")
                                     (or (= (point) (line-beginning-position))
                                         (and containing-sexp
                                              (= (point) (1+ containing-sexp))))))
                         (forward-sexp -1)
                         (backward-prefix-chars))
                       (setq calculate-lisp-indent-last-sexp (point)))
                     (> calculate-lisp-indent-last-sexp
                        (save-excursion
                          (goto-char (1+ containing-sexp))
                          (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
                          (point)))
                     (let ((parse-sexp-ignore-comments t)
                           indent)
                       (goto-char calculate-lisp-indent-last-sexp)
                       (or (and (looking-at ":")
                                (setq indent (current-column)))
                           (and (< (line-beginning-position)
                                   (prog2 (backward-sexp) (point)))
                                (looking-at ":")
                                (setq indent (current-column))))
                       indent))
                ;; another symbols or constants not preceded by a constant
                ;; as defined above.
                normal-indent))
              ;; in this case calculate-lisp-indent-last-sexp is nil
              (desired-indent)
              (t
               normal-indent))))))

(provide '+lisp-mode-patch)
;;; +lisp-mode-patch.el ends here
