;;; elisp-tools-test.el --- Batch indentation and form editing -*- lexical-binding: t; -*-

;;; Commentary:
;; Lint, reindent and edit scratch Elisp files by top-level form name.

;;; Code:

(require 'ert)
(require '+elisp-tools)

(defmacro +elisp-tools-test--with-file (text &rest body)
  "Run BODY with `file' a scratch Elisp file holding TEXT."
  (declare (indent 1))
  `(let* ((directory (make-temp-file "elisp-tools-test-" t))
          (file (expand-file-name "scratch.el" directory))
          (+elisp-tools--ready t))
     (unwind-protect
         (progn
           (write-region ,text nil file nil 'silent)
           ,@body)
       (delete-directory directory t))))

(defun +elisp-tools-test--contents (file)
  "Return FILE's text."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defconst +elisp-tools-test--text
  ";;; scratch.el --- Scratch -*- lexical-binding: t; -*-

;; Explains one.
(defun +scratch-one ()
  \"One.\"
   1)

(cl-defmethod +scratch-two ((_ integer)) 2)

(cl-defmethod +scratch-two ((_ string)) \"2\")

(provide 'scratch)
;;; scratch.el ends here
"
  "A file whose first form is misindented and whose second name repeats.")

(ert-deftest +elisp-tools-test-indent-reports-and-fixes ()
  "The lint names the lines that move; fixing moves only those."
  (+elisp-tools-test--with-file +elisp-tools-test--text
    (should (equal '(6) (+elisp-tools-indent-file file)))
    (should (equal +elisp-tools-test--text (+elisp-tools-test--contents file)))
    (should (equal '(6) (+elisp-tools-indent-file file t)))
    (should (string-search "\"One.\"\n  1)"
                           (+elisp-tools-test--contents file)))
    (should-not (+elisp-tools-indent-file file))))

(ert-deftest +elisp-tools-test-declarations ()
  "Indent declarations and properties in any file reach the indenter."
  (let ((name (make-symbol "+elisp-tools-test-macro")))
    (+elisp-tools--walk `(eval-and-compile
                           (defmacro ,name (&rest body)
                             "Doc." (declare (indent 0)) body))
                        #'+elisp-tools--declare)
    (should (eql 0 (function-get name 'lisp-indent-function)))
    (+elisp-tools--declare `(put ',name 'lisp-indent-function 2))
    (should (eql 2 (function-get name 'lisp-indent-function)))))

(ert-deftest +elisp-tools-test-show-and-ambiguity ()
  "A name selects one form; a shared name needs its head."
  (+elisp-tools-test--with-file +elisp-tools-test--text
    (should (equal "(defun +scratch-one ()\n  \"One.\"\n   1)"
                   (+elisp-tools-show file "+scratch-one")))
    (should (equal "(provide 'scratch)" (+elisp-tools-show file "scratch")))
    (should-error (+elisp-tools-show file "+scratch-none"))
    (let ((err (should-error (+elisp-tools-show file "+scratch-two"))))
      (should (string-match-p "2 forms .* lines 8 (cl-defmethod), 10"
                              (cadr err))))
    (should (string-search "string" (+elisp-tools-show file "+scratch-two@10")))
    (should (string-search "provide"
                           (+elisp-tools-show file "provide:scratch@12")))
    (should-error (+elisp-tools-show file "+scratch-two@9"))
    (should-error (+elisp-tools-show file "defun:+scratch-two"))))

(ert-deftest +elisp-tools-test-replace-reindents-only-the-form ()
  "Replacing a form reindents it and leaves the rest as it was."
  (+elisp-tools-test--with-file
      (replace-regexp-in-string "(provide" "(defvar +scratch-v\n     1)\n\n(provide"
                                +elisp-tools-test--text)
    (should (equal '(12 . 14)
                   (+elisp-tools-replace
                    file "+scratch-v"
                    "(defvar +scratch-v\n2\n\"Two.\")")))
    (let ((text (+elisp-tools-test--contents file)))
      (should (string-search "(defvar +scratch-v\n  2\n  \"Two.\")\n\n(provide"
                             text))
      (should (string-search "\"One.\"\n   1)" text)))))

(ert-deftest +elisp-tools-test-insert-and-delete ()
  "Insertion lands before provide or beside a named form; deletion
takes the form and the blank line after it."
  (+elisp-tools-test--with-file +elisp-tools-test--text
    (should (equal '(12 . 13)
                   (+elisp-tools-insert file nil "(defun +scratch-end ()\n1)")))
    (should (string-search "\"2\")\n\n(defun +scratch-end ()\n  1)\n\n(provide"
                           (+elisp-tools-test--contents file)))
    (should (equal '(3 . 3)
                   (+elisp-tools-insert file '(before . "+scratch-one")
                                        "(defvar +scratch-first nil)")))
    (should (string-search "\n\n(defvar +scratch-first nil)\n\n;; Explains one.\
\n(defun +scratch-one"
                           (+elisp-tools-test--contents file)))
    (+elisp-tools-insert file '(after . "+scratch-one") "(defvar +scratch-after t)")
    (should (string-search "   1)\n\n(defvar +scratch-after t)\n\n(cl-defmethod"
                           (+elisp-tools-test--contents file)))
    (+elisp-tools-delete file "+scratch-after")
    (+elisp-tools-delete file "+scratch-end")
    (+elisp-tools-delete file "+scratch-first")
    (should (equal +elisp-tools-test--text (+elisp-tools-test--contents file)))
    (+elisp-tools-delete file "+scratch-one")
    (should-not (string-search "Explains" (+elisp-tools-test--contents file)))))

(ert-deftest +elisp-tools-test-source-holds-one-form ()
  "A source must read as exactly one form."
  (+elisp-tools-test--with-file "(a)\n(b)\n"
    (should-error (+elisp-tools--source file))
    (write-region "(a" nil file nil 'silent)
    (should-error (+elisp-tools--source file))
    (write-region "\n(a b)\n" nil file nil 'silent)
    (should (equal "(a b)" (+elisp-tools--source file)))))

(ert-deftest +elisp-tools-test-compile-status ()
  "Compilation reports ok, warning or error with the log."
  (+elisp-tools-test--with-file +elisp-tools-test--text
    (should (eq 'ok (car (+elisp-tools-compile file))))
    (+elisp-tools-insert file nil "(defun +scratch-warn () (+scratch-missing))")
    (pcase-let ((`(,status . ,log) (+elisp-tools-compile file)))
      (should (eq 'warning status))
      (should (string-search "+scratch-missing" log)))
    (should-not (directory-files directory nil "\\.elc\\'"))))

(ert-deftest +elisp-tools-test-warnings-about-the-edit ()
  "Only new warnings count, split by whether they concern the edit."
  (let ((before "\nIn toplevel form:\nf.el:3:1: Warning: old one\n")
        (after (concat "\nIn toplevel form:\nf.el:9:1: Warning: old one\n"
                       "f.el:5:2: Warning: in range\n  continued\n"
                       "f.el:30:2: Warning: out of range\n"
                       "\nIn end of data:\n"
                       "f.el: Warning: the function ‘+mine’ is not known\n"
                       "f.el: Warning: the function ‘+theirs’ is not known\n")))
    (should (equal '(("f.el:5:2: Warning: in range\n  continued"
                      "f.el: Warning: the function ‘+mine’ is not known")
                     "f.el:30:2: Warning: out of range"
                     "f.el: Warning: the function ‘+theirs’ is not known")
                   (+elisp-tools--warnings after '(4 . 6) "(+mine)" before)))))

(ert-deftest +elisp-tools-test-command-line ()
  "The batch command edits from standard input and exits by status."
  (+elisp-tools-test--with-file +elisp-tools-test--text
    (let ((run (lambda (input &rest args)
                 (with-temp-buffer
                   (let ((status
                          (if input
                              (progn
                                (insert input)
                                (apply #'call-process-region
                                       (point-min) (point-max)
                                       (expand-file-name invocation-name
                                                         invocation-directory)
                                       t t nil "-Q" "--batch" "-l"
                                       (locate-library "+elisp-tools")
                                       "-f" "+elisp-tools-batch" args))
                            (apply #'call-process
                                   (expand-file-name invocation-name
                                                     invocation-directory)
                                   nil t nil "-Q" "--batch" "-l"
                                   (locate-library "+elisp-tools")
                                   "-f" "+elisp-tools-batch" args))))
                     (cons status (buffer-string)))))))
      (should (eql 1 (car (funcall run nil "indent" file))))
      (should (eql 0 (car (funcall run nil "indent" "--fix" file))))
      (should (eql 0 (car (funcall run nil "indent" file))))
      (pcase-let ((`(,status . ,output)
                   (funcall run "(defun +scratch-one ()\n1)"
                            "replace" file "+scratch-one" "-")))
        (should (eql 0 status))
        (should (string-search "scratch.el:4-5: edited" output))
        (should (string-search "Byte compilation: ok" output)))
      (should (eql 2 (car (funcall run "(defun +scratch-w () (+scratch-gone))"
                                   "insert" file "--end" "-"))))
      (should (eql 1 (car (funcall run nil "show" file "+scratch-two")))))))

(provide 'elisp-tools-test)
;;; elisp-tools-test.el ends here
