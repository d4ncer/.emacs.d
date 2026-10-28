;;; agent-shell-pstack-reload-test.el --- Live reload tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise reloads against disposable libraries, without a running Emacs server.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'bytecomp)
(require '+agent-shell-pstack-reload)

(defmacro +agent-shell-pstack-reload-test--with-libraries (&rest body)
  "Run BODY with temporary libraries under `dir' and isolated load state."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "pstack-reload-" t))
          (load-path (cons dir load-path))
          (load-history (copy-tree load-history))
          (minor-mode-list (copy-sequence minor-mode-list))
          (global-minor-modes (copy-sequence global-minor-modes))
          (+agent-shell-pstack-reload--libraries
           '(+reload-fixture-first +reload-fixture-second)))
     (unwind-protect
         (progn ,@body)
       (let (symbols)
         (mapatoms (lambda (symbol)
                     (when (string-prefix-p "+reload-fixture-"
                                            (symbol-name symbol))
                       (push symbol symbols))))
         (dolist (symbol symbols)
           (set 'features (delq symbol (symbol-value 'features)))
           (makunbound symbol)
           (fmakunbound symbol)
           (setplist symbol nil)))
       (delete-directory dir t))))

(defun +agent-shell-pstack-reload-test--write (directory feature &rest forms)
  "Write a temporary library providing FEATURE in DIRECTORY, with FORMS."
  (let ((file (expand-file-name (concat (symbol-name feature) ".el") directory)))
    (with-temp-file file
      (insert ";;; -*- lexical-binding: t; -*-\n")
      (prin1 '(defgroup +reload-fixture-options nil "Reload test options."
                :group 'applications)
             (current-buffer))
      (insert "\n")
      (dolist (form (append forms (list (list 'provide (list 'quote feature)))))
        (prin1 form (current-buffer))
        (insert "\n")))
    file))

(defun +agent-shell-pstack-reload-test--option (directory default)
  "Write the first library with an option whose DEFAULT is an expression."
  (+agent-shell-pstack-reload-test--write
   directory '+reload-fixture-first
   '(defvar +reload-fixture-sets nil)
   `(defcustom +reload-fixture-option ,default
      "A reloadable option."
      :type 'sexp :group '+reload-fixture-options
      :set (lambda (symbol value)
             (push value +reload-fixture-sets)
             (set-default symbol value)))))

(ert-deftest +agent-shell-pstack-reload-test-changed-default ()
  "Adopt a changed default, including a list that gained an element."
  (+agent-shell-pstack-reload-test--with-libraries
    (+agent-shell-pstack-reload-test--option dir ''(first))
    (require '+reload-fixture-first)
    (set '+reload-fixture-sets nil)
    (+agent-shell-pstack-reload-test--option dir ''(first second))
    (let ((summary (+agent-shell-pstack-reload)))
      (should (equal (symbol-value '+reload-fixture-option) '(first second)))
      (should (equal (symbol-value '+reload-fixture-sets)
                     '((first second) (first))))
      (should (string-match-p
               (regexp-quote "reloaded 1 libraries (+reload-fixture-first)")
               summary))
      (should (string-match-p
               (regexp-quote "options reset: +reload-fixture-option;") summary))
      (should (string-match-p "schema guard dropped 0 pstack panels" summary)))))

(ert-deftest +agent-shell-pstack-reload-test-user-value ()
  "Keep a plain setf override even without Customize metadata."
  (+agent-shell-pstack-reload-test--with-libraries
    (+agent-shell-pstack-reload-test--option dir ''(first))
    (require '+reload-fixture-first)
    (setf (symbol-value '+reload-fixture-option) '(private))
    (set '+reload-fixture-sets nil)
    (should-not (get '+reload-fixture-option 'saved-value))
    (should-not (get '+reload-fixture-option 'customized-value))
    (+agent-shell-pstack-reload-test--option dir ''(first second))
    (+agent-shell-pstack-reload)
    (should (equal (symbol-value '+reload-fixture-option) '(private)))
    (should (equal (symbol-value '+reload-fixture-sets) '((private))))))

(ert-deftest +agent-shell-pstack-reload-test-equal-default-values ()
  "Compare evaluated values even when the standard expressions differ."
  (+agent-shell-pstack-reload-test--with-libraries
    (+agent-shell-pstack-reload-test--option dir '(list 'first))
    (require '+reload-fixture-first)
    (set '+reload-fixture-sets nil)
    (+agent-shell-pstack-reload-test--option dir '(append '(first) nil))
    (+agent-shell-pstack-reload)
    (should (equal (symbol-value '+reload-fixture-option) '(first)))
    (should (equal (symbol-value '+reload-fixture-sets) '((first))))))

(ert-deftest +agent-shell-pstack-reload-test-global-mode ()
  "Never reevaluate a minor mode, even when its default changes to nil."
  (+agent-shell-pstack-reload-test--with-libraries
    (let ((mode-body
           '(if +reload-fixture-mode nil (cl-incf +reload-fixture-offs))))
      (+agent-shell-pstack-reload-test--write
       dir '+reload-fixture-first
       '(defvar +reload-fixture-offs 0)
       `(define-minor-mode +reload-fixture-mode "Fixture mode."
          :global t :init-value t ,mode-body))
      (require '+reload-fixture-first)
      (funcall '+reload-fixture-mode 1)
      (+agent-shell-pstack-reload-test--write
       dir '+reload-fixture-first
       `(define-minor-mode +reload-fixture-mode "Fixture mode."
          :global t :init-value nil ,mode-body))
      (+agent-shell-pstack-reload)
      (should (symbol-value '+reload-fixture-mode))
      (should (memq '+reload-fixture-mode global-minor-modes))
      (should (zerop (symbol-value '+reload-fixture-offs))))))

(ert-deftest +agent-shell-pstack-reload-test-restores-global-mode ()
  "Restore an enabled global mode if the loaded code cleared its variable."
  (+agent-shell-pstack-reload-test--with-libraries
    (let ((mode '(define-minor-mode +reload-fixture-mode "Fixture mode."
                   :global t
                   (unless +reload-fixture-mode
                     (error "The off branch must never run")))))
      (+agent-shell-pstack-reload-test--write dir '+reload-fixture-first mode)
      (require '+reload-fixture-first)
      (funcall '+reload-fixture-mode 1)
      (+agent-shell-pstack-reload-test--write
       dir '+reload-fixture-first mode '(setq +reload-fixture-mode nil))
      (+agent-shell-pstack-reload)
      (should (symbol-value '+reload-fixture-mode)))))

(ert-deftest +agent-shell-pstack-reload-test-unloaded-library ()
  "Leave an unused library unloaded even when it is available on load-path."
  (+agent-shell-pstack-reload-test--with-libraries
    (+agent-shell-pstack-reload-test--write dir '+reload-fixture-first)
    (+agent-shell-pstack-reload-test--write
     dir '+reload-fixture-second '(error "An unloaded library was loaded"))
    (require '+reload-fixture-first)
    (+agent-shell-pstack-reload)
    (should-not (featurep '+reload-fixture-second))))

(ert-deftest +agent-shell-pstack-reload-test-default-error ()
  "Skip an option whose old default cannot be evaluated."
  (+agent-shell-pstack-reload-test--with-libraries
    (+agent-shell-pstack-reload-test--option
     dir '(if (boundp '+reload-fixture-fail) (error "Unavailable") 1))
    (require '+reload-fixture-first)
    (set '+reload-fixture-fail t)
    (set '+reload-fixture-sets nil)
    (+agent-shell-pstack-reload-test--option dir 2)
    (+agent-shell-pstack-reload)
    (should (= (symbol-value '+reload-fixture-option) 1))
    (should (equal (symbol-value '+reload-fixture-sets) '(1)))))

(ert-deftest +agent-shell-pstack-reload-test-order-and-summary ()
  "Reload dependencies first and fold the schema guard message into the summary."
  (+agent-shell-pstack-reload-test--with-libraries
    (+agent-shell-pstack-reload-test--write
     dir '+reload-fixture-first '(defvar +reload-fixture-revision 1))
    (+agent-shell-pstack-reload-test--write dir '+reload-fixture-second)
    (require '+reload-fixture-first)
    (require '+reload-fixture-second)
    (+agent-shell-pstack-reload-test--write
     dir '+reload-fixture-first '(setq +reload-fixture-revision 2))
    (+agent-shell-pstack-reload-test--write
     dir '+reload-fixture-second
     '(unless (= +reload-fixture-revision 2) (error "Dependency loaded too late"))
     '(message "Dropped %d pstack panels made before the panel code changed" 3))
    (let ((original-message (symbol-function 'message)) messages)
      (cl-letf (((symbol-function 'message)
                 (lambda (&rest args)
                   (push (apply #'format-message args) messages)
                   (apply original-message args))))
        (+agent-shell-pstack-reload))
      (should (= (length messages) 1))
      (should (string-match-p
               (regexp-quote
                "reloaded 2 libraries (+reload-fixture-first, +reload-fixture-second)")
               (car messages)))
      (should (string-match-p "options reset: none" (car messages)))
      (should (string-match-p "schema guard dropped 3 pstack panels"
                              (car messages))))))

(ert-deftest +agent-shell-pstack-reload-test-new-default-error ()
  "Skip an option whose new default cannot be evaluated."
  (+agent-shell-pstack-reload-test--with-libraries
    (+agent-shell-pstack-reload-test--option dir 1)
    (require '+reload-fixture-first)
    (set '+reload-fixture-sets nil)
    (+agent-shell-pstack-reload-test--option dir '(error "Unavailable"))
    (+agent-shell-pstack-reload)
    (should (= (symbol-value '+reload-fixture-option) 1))
    (should (equal (symbol-value '+reload-fixture-sets) '(1)))))

(ert-deftest +agent-shell-pstack-reload-test-newer-source ()
  "Prefer edited source over old bytecode, even with load-prefer-newer nil."
  (+agent-shell-pstack-reload-test--with-libraries
    (let* ((source (+agent-shell-pstack-reload-test--option dir 1))
           (bytecode (concat source "c"))
           (load-prefer-newer nil))
      (should (byte-compile-file source))
      (require '+reload-fixture-first)
      (+agent-shell-pstack-reload-test--option dir 2)
      (set-file-times bytecode (time-subtract (current-time) 60))
      (+agent-shell-pstack-reload)
      (should (= (symbol-value '+reload-fixture-option) 2)))))

(provide 'agent-shell-pstack-reload-test)
;;; agent-shell-pstack-reload-test.el ends here
