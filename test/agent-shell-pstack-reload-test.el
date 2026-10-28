;;; agent-shell-pstack-reload-test.el --- Live reload tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise reloads against disposable libraries, without a running Emacs server.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'bytecomp)
(require '+agent-shell-pstack-reload)
(require '+agent-shell-arena-remote)
(require 'agent-shell-pstack-panel-test)

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
      (should (string-match-p "carried over 0 pstack panels\\'" summary)))))

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
  "Reload dependencies first and end with a single summary."
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
     '(unless (= +reload-fixture-revision 2) (error "Dependency loaded too late")))
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
      (should (string-match-p "carried over 0 pstack panels"
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

;;;; Carrying panels

(defmacro +agent-shell-pstack-reload-test--with-copies (&rest body)
  "Run BODY with the arena-git and panel libraries loaded from copies.
BODY sees `dir', which holds the copies, and an empty remote table.  The
original libraries are loaded again afterwards."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "pstack-reload-copies-" t))
          (features '(+agent-shell-arena-git +agent-shell-pstack-panel))
          (originals (mapcar (lambda (feature)
                               (locate-library
                                (concat (symbol-name feature) ".el")))
                             features))
          (load-history (copy-tree load-history))
          (+agent-shell-pstack-reload--libraries features)
          (+agent-shell-arena-remote--arenas (make-hash-table :test #'equal)))
     (unwind-protect
         (progn
           (dolist (original originals)
             (let ((copy (expand-file-name (file-name-nondirectory original)
                                           dir)))
               (copy-file original copy)
               (load copy nil t t)))
           ,@body)
       (let ((+agent-shell-pstack-reload--carrying t))
         (dolist (original originals)
           (load original nil t t)))
       (delete-directory dir t))))

(defun +agent-shell-pstack-reload-test--edit (directory feature from to)
  "Replace FROM with TO in FEATURE's copy in DIRECTORY."
  (let ((file (expand-file-name (concat (symbol-name feature) ".el")
                                directory)))
    (with-temp-file file
      (insert-file-contents file)
      (should (search-forward from nil t))
      (replace-match to t t))))

(defun +agent-shell-pstack-reload-test--attach (panel)
  "Give PANEL a delegate manifest and a remote record; return them."
  (let* ((id (+agent-shell-pstack-panel-id panel))
         (role (+agent-shell-arena-git--role :name "a" :worktree "/w"))
         (delegate (+agent-shell-arena-git--delegate :isolation 'worktree))
         (manifest (+agent-shell-arena-git--manifest
                    :id id :base "0123abcd" :roles (list role) :delegate delegate))
         (remote (+agent-shell-arena-remote--make :id id :panel id
                                                  :phase 'remote)))
    (setf (+agent-shell-pstack-panel-arena panel) manifest
          (+agent-shell-pstack-panel-remote panel) remote)
    (puthash id remote +agent-shell-arena-remote--arenas)
    (list manifest role delegate remote)))

(ert-deftest +agent-shell-pstack-reload-test-carries-running-panel ()
  "Carry a running panel over slots added to the panel and manifest structs."
  (+agent-shell-pstack-reload-test--with-copies
    (+agent-shell-pstack-panel-test--with
      (lead-event :kind 'turn-complete :session "s1"
                  :text (+agent-shell-pstack-panel-test--message
                         +agent-shell-pstack-panel-test--one))
      (pcase-let* ((panel (car (+agent-shell-pstack-panel-list)))
                   (id (+agent-shell-pstack-panel-id panel))
                   (members (+agent-shell-pstack-panel-members panel))
                   (member (car members))
                   (`(,manifest ,role ,delegate ,remote)
                    (+agent-shell-pstack-reload-test--attach panel)))
        (+agent-shell-pstack-reload-test--edit
         dir '+agent-shell-pstack-panel "delegate-options identity runner)"
         "delegate-options identity runner (carried 'fresh))")
        (+agent-shell-pstack-reload-test--edit
         dir '+agent-shell-arena-git "base roles sparse delegate)"
         "base roles sparse delegate (extra 7))")
        (let ((summary (+agent-shell-pstack-reload))
              (new (gethash id +agent-shell-pstack-panel--panels)))
          (should (string-match-p
                   "reloaded 2 libraries .*; carried over 1 pstack panel\\'"
                   summary))
          (should-not (eq new panel))
          (should (+agent-shell-pstack-panel--current-p new))
          (should (eq 'fresh (cl-struct-slot-value
                              '+agent-shell-pstack-panel 'carried new)))
          (should (eq 'running (+agent-shell-pstack-panel-phase new)))
          (should (equal "b" (+agent-shell-pstack-panel-brief new)))
          (should (eq lead (+agent-shell-pstack-panel-lead new)))
          (should (eq members (+agent-shell-pstack-panel-members new)))
          (should (eq member (car (+agent-shell-pstack-panel-members new))))
          (let ((arena (+agent-shell-pstack-panel-arena new)))
            (should-not (eq manifest arena))
            (should (= 7 (cl-struct-slot-value
                          '+agent-shell-arena-git-manifest 'extra arena)))
            (should (equal id (+agent-shell-arena-git-manifest-id arena)))
            (should (eq role (car (+agent-shell-arena-git-manifest-roles
                                   arena))))
            (should (eq delegate (+agent-shell-arena-git-manifest-delegate
                                  arena))))
          (should (eq remote (+agent-shell-pstack-panel-remote new)))
          (should (eq remote (gethash id +agent-shell-arena-remote--arenas)))
          (should (equal id (+agent-shell-arena-remote-panel remote)))
          (member-event "a" :kind 'init-finished)
          (member-event "a" :kind 'turn-complete :text "Still works")
          (should (eq 'finished (+agent-shell-pstack-panel-phase new)))
          (should (eq lead (caar sent)))
          (should (string-match-p "Still works" (cdar sent))))))))

(ert-deftest +agent-shell-pstack-reload-test-refuses-lost-slot ()
  "Load nothing when the new code removes a slot a running panel uses.
A finished panel loses the slot instead of blocking the reload."
  (+agent-shell-pstack-reload-test--with-copies
    (+agent-shell-pstack-panel-test--with
      (lead-event :kind 'turn-complete :session "s1"
                  :text (+agent-shell-pstack-panel-test--message
                         +agent-shell-pstack-panel-test--one))
      (let* ((panel (car (+agent-shell-pstack-panel-list)))
             (id (+agent-shell-pstack-panel-id panel))
             (finished (copy-sequence panel)))
        (+agent-shell-pstack-reload-test--attach panel)
        (setf (+agent-shell-pstack-panel-id finished) "finished"
              (+agent-shell-pstack-panel-phase finished) 'finished)
        (puthash "finished" finished +agent-shell-pstack-panel--panels)
        (+agent-shell-pstack-reload-test--edit
         dir '+agent-shell-arena-git "(defconst +agent-shell-arena-git--identity"
         "(defvar +reload-fixture-loaded t)\n\
(defconst +agent-shell-arena-git--identity")
        (+agent-shell-pstack-reload-test--edit
         dir '+agent-shell-arena-git "id directory git-dir toplevel prefix base"
         "id directory git-dir toplevel prefix")
        (let ((err (should-error (+agent-shell-pstack-reload)
                                 :type 'user-error)))
          (should (equal (cadr err)
                         (format "pstack: reloaded nothing; the new code \
cannot carry running panel %s (+agent-shell-arena-git-manifest.base); \
retry after it finishes" id))))
        (should-not (boundp '+reload-fixture-loaded))
        (should (memq 'base (mapcar #'car (cl-struct-slot-info
                                            '+agent-shell-arena-git-manifest))))
        (should (eq panel (gethash id +agent-shell-pstack-panel--panels)))
        (+agent-shell-pstack-panel--finish panel)
        (+agent-shell-pstack-reload)
        (should (boundp '+reload-fixture-loaded))
        (makunbound '+reload-fixture-loaded)
        (should-not (memq 'base (mapcar #'car
                                        (cl-struct-slot-info
                                         '+agent-shell-arena-git-manifest))))
        (should (equal id (+agent-shell-arena-git-manifest-id
                           (+agent-shell-pstack-panel-arena
                            (gethash id +agent-shell-pstack-panel--panels)))))
        (should (eq 'finished (+agent-shell-pstack-panel-phase
                               (gethash "finished"
                                        +agent-shell-pstack-panel--panels))))))))

(provide 'agent-shell-pstack-reload-test)
;;; agent-shell-pstack-reload-test.el ends here
