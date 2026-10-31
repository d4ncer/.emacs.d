;;; agent-shell-plans-test.el --- Live plan tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Drive the installed notification advice with synthetic ACP messages.
;; Render real Magit sections without starting an adapter or session.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require '+agent-shell-plans-view)

(declare-function agent-shell--on-notification "agent-shell")

(defmacro +agent-shell-plans-test--with (&rest body)
  "Run BODY with isolated shells, plans and an advised notification stub."
  (declare (indent 0))
  `(let* ((+agent-shell-plans--plans (make-hash-table :test #'eq))
          (+agent-shell-plans--serial 0)
          (+agent-shell-plans--timer nil)
          (+agent-shell-plans--buffer " *agent plans test*")
          (+agent-shell-plans-change-hook
           '(+agent-shell-plans--schedule-refresh))
          (+agent-shell-plans--refresh-delay 0.02)
          (shell (generate-new-buffer " *plan shell*"))
          (other (generate-new-buffer " *other plan shell*")))
     (unwind-protect
         (save-window-excursion
           (dolist (buffer (list shell other))
             (with-current-buffer buffer
               (setq-local major-mode 'agent-shell-mode)))
           (cl-letf (((symbol-function 'agent-shell--on-notification) #'ignore))
             (advice-add 'agent-shell--on-notification :after
                         #'+agent-shell-plans--note-notification)
             ,@body))
       (when-let* ((view (get-buffer +agent-shell-plans--buffer)))
         (kill-buffer view))
       (dolist (buffer (list shell other))
         (when (buffer-live-p buffer) (kill-buffer buffer)))
       (when (timerp +agent-shell-plans--timer)
         (cancel-timer +agent-shell-plans--timer)))))

(defun +agent-shell-plans-test--notify (shell entries &optional kind)
  "Send SHELL a synthetic notification with ENTRIES and update KIND."
  (agent-shell--on-notification
   :state (list (cons :buffer shell))
   :acp-notification
   `((params . ((update . ((sessionUpdate . ,(or kind "plan"))
                           (entries . ,entries))))))))

(defun +agent-shell-plans-test--entry (status text &optional codex)
  "Make an entry with STATUS and TEXT, using step when CODEX is non-nil."
  (list (cons 'status status) (cons (if codex 'step 'content) text)))

(defun +agent-shell-plans-test--card (shell)
  "Move to and return SHELL's card."
  (let ((section (seq-find (lambda (section)
                             (eq (oref section value) shell))
                           (oref magit-root-section children))))
    (should section)
    (goto-char (oref section start))
    section))

(ert-deftest +agent-shell-plans-test-record-and-replace ()
  (+agent-shell-plans-test--with
    (let* ((changes 0)
           (+agent-shell-plans-change-hook (list (lambda () (cl-incf changes))))
           (entries (list (+agent-shell-plans-test--entry "pending" "Read"))))
      (+agent-shell-plans-test--notify shell entries "agent_message_chunk")
      (should (= 0 changes))
      (should (= 0 (hash-table-count +agent-shell-plans--plans)))
      (+agent-shell-plans-test--notify shell entries)
      (should (= 1 changes))
      (should (equal entries (plist-get (gethash shell +agent-shell-plans--plans)
                                      :entries)))
      (setcdr (assq 'content (car entries)) "Mutated")
      (should (equal "Read" (map-elt (car (plist-get
                                         (gethash shell +agent-shell-plans--plans)
                                         :entries)) 'content)))
      (+agent-shell-plans-test--notify
       shell (vector (+agent-shell-plans-test--entry "in_progress" "Write" t)))
      (should (= 2 changes))
      (should (equal "Write" (+agent-shell-plans--text
                              (car (plist-get
                                    (gethash shell +agent-shell-plans--plans)
                                    :entries)))))
      (+agent-shell-plans-test--notify
       other (list (+agent-shell-plans-test--entry "completed" "Other")))
      (should (= 2 (hash-table-count +agent-shell-plans--plans)))
      (should (equal "Write" (+agent-shell-plans--text
                              (car (plist-get
                                    (gethash shell +agent-shell-plans--plans)
                                    :entries))))))))

(ert-deftest +agent-shell-plans-test-forget-killed-and-empty ()
  (+agent-shell-plans-test--with
    (+agent-shell-plans-test--notify
     shell (list (+agent-shell-plans-test--entry "pending" "Read")))
    (let* ((changes 0)
           (+agent-shell-plans-change-hook (list (lambda () (cl-incf changes)))))
      (kill-buffer shell)
      (should (= 1 changes))
      (should-not (gethash shell +agent-shell-plans--plans))
      (+agent-shell-plans-test--notify
       shell (list (+agent-shell-plans-test--entry "pending" "Too late")))
      (should (= 1 changes))
      (should (= 0 (hash-table-count +agent-shell-plans--plans))))
    (+agent-shell-plans-test--notify
     other (list (+agent-shell-plans-test--entry "completed" "Done")))
    (+agent-shell-plans-test--notify other [])
    (should-not (gethash other +agent-shell-plans--plans))))

(ert-deftest +agent-shell-plans-test-debounce ()
  (+agent-shell-plans-test--with
    (+agent-shell-plans)
    (let ((redraws 0)
          (entries (list (+agent-shell-plans-test--entry "pending" "Read"))))
      (cl-letf (((symbol-function '+agent-shell-plans--render)
                 (lambda () (cl-incf redraws))))
        (dotimes (_ 8) (+agent-shell-plans-test--notify shell entries))
        (should (= 0 redraws))
        (let ((deadline (+ (float-time) 1)))
          (while (and +agent-shell-plans--timer (< (float-time) deadline))
            (accept-process-output nil 0.01)))
        (should (= 1 redraws))
        (should-not +agent-shell-plans--timer)))))

(ert-deftest +agent-shell-plans-test-render-fold-expand-and-visit ()
  (+agent-shell-plans-test--with
    (let ((+agent-shell-plans--preview-entries 2))
      (+agent-shell-plans-test--notify
       shell (vector (+agent-shell-plans-test--entry "completed" "Read")
                     (+agent-shell-plans-test--entry "in_progress" "Write\ncode" t)
                     (+agent-shell-plans-test--entry "pending" "Test")
                     (+agent-shell-plans-test--entry "pending" "Review")))
      (+agent-shell-plans)
      (should (string-match-p "\\`agent plans · 1 working, 1 without a plan$" (buffer-string)))
      (should (string-match-p "● WORKING.*1/4 · Write code" (buffer-string)))
      (should (= 1 (how-many "^Actions$" (point-min) (point-max))))
      (let ((card (+agent-shell-plans-test--card shell)))
        (should (oref card hidden))
        (magit-section-toggle card)
        (should-not (oref card hidden))
        (should (string-match-p "    ✓ Read" (buffer-string)))
        (let ((more (car (oref card children))))
          (should (oref more hidden))
          (goto-char (oref more start))
          (should (looking-at "    … 2 more"))
          (should (invisible-p (oref more content)))
          (magit-section-toggle more)
          (should-not (invisible-p (oref more content)))
          (goto-char (oref more content))
          (should (looking-at "    ◌ Test"))
          (let (visited)
            (cl-letf (((symbol-function 'pop-to-buffer)
                       (lambda (buffer &rest _) (setq visited buffer))))
              (call-interactively (key-binding (kbd "RET"))))
            (should (eq visited shell))))))))

(ert-deftest +agent-shell-plans-test-order-and-states ()
  (+agent-shell-plans-test--with
    (+agent-shell-plans-test--notify
     shell (list (+agent-shell-plans-test--entry "in_progress" "Working")))
    (+agent-shell-plans-test--notify
     other (list (+agent-shell-plans-test--entry "completed" "Done")))
    (should (equal (list shell other) (+agent-shell-plans--sorted)))
    (+agent-shell-plans-test--notify
     other (list (+agent-shell-plans-test--entry "in_progress" "New work")))
    (should (equal (list other shell) (+agent-shell-plans--sorted)))
    (+agent-shell-plans-test--notify
     other (list (+agent-shell-plans-test--entry "pending" "Waiting")))
    (+agent-shell-plans)
    (should (string-match-p "◌ PENDING.*0/1" (buffer-string)))
    (+agent-shell-plans-test--notify
     shell (list (+agent-shell-plans-test--entry "completed" "Finished")))
    (+agent-shell-plans--render)
    (should (string-match-p "✓ DONE.*1/1" (buffer-string)))
    (should-not (string-match-p "1/1 · Finished" (buffer-string)))))

(ert-deftest +agent-shell-plans-test-preserve-place-folds-and-return ()
  (+agent-shell-plans-test--with
    (let ((+agent-shell-plans--preview-entries 1))
      (+agent-shell-plans-test--notify
       shell (list (+agent-shell-plans-test--entry "in_progress" "First")
                   (+agent-shell-plans-test--entry "pending" "Second")))
      (+agent-shell-plans)
      (let* ((view (current-buffer))
             (card (+agent-shell-plans-test--card shell))
             (more (car (oref card children))))
        (magit-section-toggle card)
        (magit-section-toggle more)
        (goto-char (oref more content))
        (move-to-column 10)
        (+agent-shell-plans-test--notify
         other (list (+agent-shell-plans-test--entry "in_progress" "Newer")))
        (+agent-shell-plans--render)
        (should (eq shell (+agent-shell-plans--shell)))
        (should (= 10 (current-column)))
        (should (equal "    ◌ Second\n" (thing-at-point 'line t)))
        (let ((section (magit-current-section)))
          (should-not (oref section hidden))
          (should-not (oref (oref section parent) hidden)))
        (switch-to-buffer shell)
        (+agent-shell-plans)
        (should (eq view (current-buffer)))
        (should (eq shell (+agent-shell-plans--shell)))
        (should (= 10 (current-column)))))))

(ert-deftest +agent-shell-plans-test-recorder-does-not-load-view ()
  (with-temp-buffer
    (should
     (zerop
      (call-process
       (expand-file-name invocation-name invocation-directory) nil t nil
       "-Q" "--batch" "-L"
       (file-name-directory (locate-library "+agent-shell-plans"))
       "-l" "+agent-shell-plans"
       "--eval"
       "(progn (when (or (featurep 'magit-section)
                         (featurep '+agent-shell-plans-view))
                 (error \"View loaded with recorder\"))
          (unless (advice-member-p #'+agent-shell-plans--note-notification
                                   'agent-shell--on-notification)
            (error \"Recorder advice missing\")))")))))

(ert-deftest +agent-shell-plans-test-leaves-other-buffers-alone ()
  "Leaving the view in a window never redraws the buffer shown next."
  (+agent-shell-plans-test--with
    (save-window-excursion
      (+agent-shell-plans)
      (let ((window (selected-window))
            (other (generate-new-buffer " *plans-other*")))
        (unwind-protect
            (progn
              (with-current-buffer other (insert "a transcript"))
              (set-window-buffer window other)
              (+agent-shell-plans--shown window)
              (with-current-buffer other
                (should-error (+agent-shell-plans--render))
                (should (equal "a transcript" (buffer-string)))))
          (kill-buffer other))))))

(provide 'agent-shell-plans-test)
;;; agent-shell-plans-test.el ends here
