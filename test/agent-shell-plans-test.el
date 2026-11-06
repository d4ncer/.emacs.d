;;; agent-shell-plans-test.el --- Live plan tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Drive the installed notification advice with synthetic ACP messages.
;; Render real Magit sections without starting an adapter or session.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require '+agent-shell-plans-view)

(declare-function agent-shell--on-notification "agent-shell")
(declare-function agent-shell--on-request "agent-shell")
(declare-function agent-shell--send-permission-response "agent-shell")

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
          (+agent-shell-plans-directories nil)
          (+agent-shell-plans--document-cache (make-hash-table :test #'equal))
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

(ert-deftest +agent-shell-plans-test-proposed-plan ()
  "Record a plan proposed for approval, show it first until it is answered,
keep it across plan updates, and record the answer by the option's kind."
  (+agent-shell-plans-test--with
    (cl-letf (((symbol-function 'agent-shell--on-request) #'ignore)
              ((symbol-function 'agent-shell--send-permission-response)
               #'ignore))
      (advice-add 'agent-shell--on-request :after
                  #'+agent-shell-plans--note-request)
      (advice-add 'agent-shell--send-permission-response :after
                  #'+agent-shell-plans--note-response)
      (let ((request
             (lambda (id plan)
               (agent-shell--on-request
                :state (list (cons :buffer shell))
                :acp-request
                `((method . "session/request_permission")
                  (params
                   . ((toolCall . ((toolCallId . ,id) (kind . "switch_mode")
                                   (rawInput . ((plan . ,plan)))))
                      (options
                       . [((optionId . "implement_plan") (kind . "allow_once"))
                          ((optionId . "revise_plan")
                           (kind . "reject_once"))])))))))
            (answer (lambda (id option)
                      (agent-shell--send-permission-response
                       :state (list (cons :buffer shell))
                       :tool-call-id id :option-id option))))
        (funcall request "edit-1" nil)
        (funcall request "edit-2" "  ")
        (should-not (gethash shell +agent-shell-plans--plans))
        (+agent-shell-plans-test--notify
         other (vector (+agent-shell-plans-test--entry "in_progress" "Work")))
        (funcall request "plan-review:1"
                 "# Add a **short** README\n\nCreate `README.md`.\n")
        (+agent-shell-plans-test--notify
         shell (vector (+agent-shell-plans-test--entry "pending" "Write")))
        (should (equal "plan-review:1"
                       (plist-get (plist-get (gethash shell
                                                      +agent-shell-plans--plans)
                                             :proposal)
                                  :id)))
        (+agent-shell-plans)
        (should (string-match-p "1 needs you, 1 working" (buffer-string)))
        (let ((card (+agent-shell-plans-test--card shell)))
          (should (eq card (car (seq-filter
                                 (lambda (section)
                                   (eq (oref section type) 'agent-plan))
                                 (oref magit-root-section children)))))
          (should (looking-at "◆ NEEDS YOU  .*plan shell\\* · 0/1"))
          (magit-section-toggle card)
          (let ((proposal (car (oref card children))))
            (goto-char (oref proposal start))
            (should (looking-at "    Proposed plan  Add a short README$"))
            (should (oref proposal hidden))
            (magit-section-toggle proposal)
            (goto-char (oref proposal content))
            (should (looking-at "      # Add a \\*\\*short\\*\\* README\n\n\
      Create `README\\.md`\\.\n"))))
        (funcall answer "other-call" "implement_plan")
        (should-not (plist-get (plist-get (gethash shell
                                                   +agent-shell-plans--plans)
                                          :proposal)
                               :answer))
        (funcall answer "plan-review:1" "revise_plan")
        (should (eq 'revised (plist-get (plist-get (gethash
                                                    shell
                                                    +agent-shell-plans--plans)
                                                   :proposal)
                                        :answer)))
        (+agent-shell-plans-test--notify shell [])
        (should (equal "revised" (+agent-shell-plans--card-state
                                  (gethash shell +agent-shell-plans--plans))))
        (funcall request "plan-review:2" "Second plan")
        (funcall answer "plan-review:2" "implement_plan")
        (should (equal "approved" (+agent-shell-plans--card-state
                                   (gethash shell +agent-shell-plans--plans))))
        (revert-buffer)
        (should (string-match-p "✓ APPROVED  .*plan shell\\* · proposed plan"
                                (buffer-string)))))))

(ert-deftest +agent-shell-plans-test-claude-proposed-plan ()
  "Record Claude Code's plan approval request as a proposal.
Its shape is what claude-agent-acp sent a probe run in plan mode."
  (+agent-shell-plans-test--with
    (cl-letf (((symbol-function 'agent-shell--on-request) #'ignore)
              ((symbol-function 'agent-shell--send-permission-response)
               #'ignore))
      (advice-add 'agent-shell--on-request :after
                  #'+agent-shell-plans--note-request)
      (advice-add 'agent-shell--send-permission-response :after
                  #'+agent-shell-plans--note-response)
      (let ((request
             (lambda (id)
               (agent-shell--on-request
                :state (list (cons :buffer shell))
                :acp-request
                `((method . "session/request_permission")
                  (params
                   . ((toolCall
                       . ((toolCallId . ,id) (kind . "switch_mode")
                          (title . "Approve Plan")
                          (rawInput
                           . ((plan . "# Plan: README\n\n## Steps\n1. Create \
`README.md`.\n")
                              (planFilePath . "/home/u/.claude/plans/p.md")))))
                      (options
                       . [((optionId . "exit-plan-default")
                           (kind . "allow_once"))
                          ((optionId . "exit-plan-clear-auto")
                           (kind . "allow_always"))
                          ((optionId . "exit-plan-auto")
                           (kind . "allow_always"))
                          ((optionId . "exit-plan-bypass")
                           (kind . "allow_always"))
                          ((optionId . "reject") (kind . "reject_once"))])))))))
            (answer (lambda (id option)
                      (agent-shell--send-permission-response
                       :state (list (cons :buffer shell))
                       :tool-call-id id :option-id option)))
            (proposal (lambda ()
                        (plist-get (gethash shell +agent-shell-plans--plans)
                                   :proposal))))
        (funcall request "toolu_1")
        (should (equal "# Plan: README\n\n## Steps\n1. Create `README.md`."
                       (plist-get (funcall proposal) :text)))
        (funcall answer "toolu_1" "reject")
        (should (eq 'revised (plist-get (funcall proposal) :answer)))
        (funcall request "toolu_2")
        (funcall answer "toolu_2" "exit-plan-default")
        (should (equal "approved" (+agent-shell-plans--card-state
                                   (gethash shell
                                            +agent-shell-plans--plans))))))))

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
    (+config-test-view-keeps-other-buffers
     #'+agent-shell-plans #'+agent-shell-plans--shown
     #'+agent-shell-plans--render #'+agent-shell-plans--refresh)))

(defun +agent-shell-plans-test--write (file text)
  "Write TEXT to FILE, creating its folder."
  (make-directory (file-name-directory file) t)
  (with-temp-file file (insert text)))

(defun +agent-shell-plans-test--git (repository &rest args)
  "Run git with ARGS in REPOSITORY and return its trimmed output."
  (let ((default-directory (file-name-as-directory repository))
        (process-environment
         (append '("GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_NOSYSTEM=1"
                   "GIT_AUTHOR_NAME=Test" "GIT_AUTHOR_EMAIL=test@example.com"
                   "GIT_COMMITTER_NAME=Test"
                   "GIT_COMMITTER_EMAIL=test@example.com")
                 process-environment)))
    (with-temp-buffer
      (unless (zerop (apply #'call-process "git" nil t nil args))
        (error "git %S failed: %s" args (buffer-string)))
      (string-trim (buffer-string)))))

(defun +agent-shell-plans-test--commit (repository date)
  "Commit everything in REPOSITORY at DATE and return the short hash."
  (let ((process-environment
         (append (list (concat "GIT_AUTHOR_DATE=" date)
                       (concat "GIT_COMMITTER_DATE=" date))
                 process-environment)))
    (+agent-shell-plans-test--git repository "add" "-A")
    (+agent-shell-plans-test--git repository "commit" "-q" "-m" "Plans")
    (+agent-shell-plans-test--git repository "rev-parse" "--short" "HEAD")))

(defmacro +agent-shell-plans-test--with-documents (&rest body)
  "Run BODY in the plans fixture with ROOT, a temporary folder."
  (declare (indent 0))
  `(+agent-shell-plans-test--with
     (let ((root (file-name-as-directory (make-temp-file "plans-test" t))))
       (unwind-protect (progn ,@body)
         (dolist (buffer (buffer-list))
           (when (and (buffer-file-name buffer)
                      (string-prefix-p root (buffer-file-name buffer)))
             (kill-buffer buffer)))
         (delete-directory root t)))))

(defun +agent-shell-plans-test--row (title)
  "Move to and return the Documents row whose heading starts with TITLE."
  (goto-char (point-min))
  (should (re-search-forward (concat "^  " (regexp-quote title) " · ") nil t))
  (beginning-of-line)
  (buffer-substring-no-properties (point) (line-end-position)))

(defun +agent-shell-plans-test--project (name)
  "Move to and return the project heading that starts with NAME."
  (goto-char (point-min))
  (should (re-search-forward (concat "^" (regexp-quote name) "[ ·]") nil t))
  (beginning-of-line)
  (buffer-substring-no-properties (point) (line-end-position)))

(ert-deftest +agent-shell-plans-test-documents-per-repository ()
  (+agent-shell-plans-test--with-documents
    (let* ((first (expand-file-name "first/" root))
           (second (expand-file-name "second/" root)))
      (+agent-shell-plans-test--write
       (expand-file-name "plans/alpha/plan.md" first) "# Alpha rollout\n")
      (+agent-shell-plans-test--write
       (expand-file-name "plans/beta/design.org" second) "#+title: Beta design\n")
      (+agent-shell-plans-test--git first "init" "-q")
      (+agent-shell-plans-test--git second "init" "-q")
      (+agent-shell-plans-test--commit first "2026-01-01T00:00:00Z")
      (+agent-shell-plans-test--commit second "2026-02-01T00:00:00Z")
      (let ((+agent-shell-plans-directories
             (list (expand-file-name "plans/" first)
                   (expand-file-name "missing/" root)
                   (expand-file-name "plans/" second))))
        (+agent-shell-plans)
        (should (string-match-p "^Documents$" (buffer-string)))
        (should (string-match-p "^Alpha · 1 doc · first · [0-9]+d ago$"
                                (+agent-shell-plans-test--project "Alpha")))
        (should (string-match-p "^Beta · 1 doc · second · [0-9]+d ago$"
                                (+agent-shell-plans-test--project "Beta")))
        (should (string-match-p "^  Alpha rollout · [0-9]+d ago$"
                                (+agent-shell-plans-test--row "Alpha rollout")))
        (should (string-match-p "^  Beta design · [0-9]+d ago$"
                                (+agent-shell-plans-test--row "Beta design")))
        (should (< (progn (+agent-shell-plans-test--project "Beta") (point))
                   (progn (+agent-shell-plans-test--project "Alpha") (point))))
        (should (< (progn (goto-char (point-min))
                          (search-forward "Documents"))
                   (progn (goto-char (point-min))
                          (search-forward "No agent plans yet."))))))))

(ert-deftest +agent-shell-plans-test-document-checkboxes-and-titles ()
  (+agent-shell-plans-test--with-documents
    (let ((+agent-shell-plans-directories (list root)))
      (+agent-shell-plans-test--write
       (expand-file-name "tasks/plan.md" root)
       "Intro\n\n## Ship the tasks\n\n- [x] One\n  - [X] Two\n- [ ] Three\n\
* [x] Not a dash\n-[x] No space\n")
      (+agent-shell-plans-test--write
       (expand-file-name "tasks/notes.org" root)
       "Intro\n* First headline\n- [ ] Open\n")
      (+agent-shell-plans-test--write
       (expand-file-name "tasks/titled.org" root)
       "* Headline\n#+TITLE: Org title\n- [x] Done\n")
      (+agent-shell-plans-test--write
       (expand-file-name "tasks/bare.md" root) "No heading here\n")
      (+agent-shell-plans)
      (should (string-match-p "^Tasks · 4 docs · [0-9]+[smhd] ago$"
                              (+agent-shell-plans-test--project "Tasks")))
      (should (equal "  Ship the tasks · 2 of 3 done · not committed"
                     (+agent-shell-plans-test--row "Ship the tasks")))
      (should (equal "  First headline · 0 of 1 done · not committed"
                     (+agent-shell-plans-test--row "First headline")))
      (should (equal "  Org title · 1 of 1 done · not committed"
                     (+agent-shell-plans-test--row "Org title")))
      (should (equal "  bare.md · not committed"
                     (+agent-shell-plans-test--row "bare.md")))
      (should (< (progn (+agent-shell-plans-test--row "Ship the tasks") (point))
                 (progn (+agent-shell-plans-test--row "bare.md") (point)))))))

(ert-deftest +agent-shell-plans-test-document-copies-and-untracked ()
  (+agent-shell-plans-test--with-documents
    (let ((+agent-shell-plans-directories (list (expand-file-name "plans/" root))))
      (+agent-shell-plans-test--write
       (expand-file-name "plans/gamma/plan.md" root) "# Gamma\n")
      (+agent-shell-plans-test--write
       (expand-file-name "plans/gamma/plan.v2.md" root) "# Gamma again\n")
      (+agent-shell-plans-test--write
       (expand-file-name "plans/delta/plan.md" root) "# Delta\n")
      (+agent-shell-plans-test--git root "init" "-q")
      (+agent-shell-plans-test--commit root "2026-01-01T00:00:00Z")
      (+agent-shell-plans-test--write
       (expand-file-name "plans/delta/draft.md" root) "# Draft\n")
      (+agent-shell-plans)
      (should-not (string-match-p "Gamma again" (buffer-string)))
      (should (string-match-p "^Gamma  ⚠ 1 COPY · 1 doc · [0-9]+d ago$"
                              (+agent-shell-plans-test--project "Gamma")))
      (should-not (string-match-p "COPY"
                                  (+agent-shell-plans-test--project "Delta")))
      (should (equal "  Draft · not committed"
                     (+agent-shell-plans-test--row "Draft")))
      (+agent-shell-plans-test--project "Gamma")
      (forward-line 1)
      (should (looking-at "    About$"))
      (magit-section-toggle (magit-current-section))
      (forward-line 1)
      (should (looking-at "      Copies  plan\\.v2\\.md$")))))

(ert-deftest +agent-shell-plans-test-document-descriptions ()
  (+agent-shell-plans-test--with-documents
    (let ((+agent-shell-plans-directories (list root)))
      (+agent-shell-plans-test--write
       (expand-file-name "theta/design.md" root)
       "# Theta design\n\nStatus: draft\n\nThe real **summary** here\n\
spans lines.\n\n- a list\n")
      (+agent-shell-plans-test--write
       (expand-file-name "theta/notes.md" root)
       "# Theta notes\n\n| a | table |\n\nNotes prose.\n")
      (+agent-shell-plans)
      (+agent-shell-plans-test--project "Theta")
      (forward-line 1)
      (should (looking-at "    The real summary here spans lines\\.$"))
      (+agent-shell-plans-test--row "Theta notes")
      (magit-section-toggle (magit-current-section))
      (forward-line 1)
      (should (looking-at "      Notes prose\\.$"))
      (forward-line 1)
      (should (looking-at "      File  ")))))

(ert-deftest +agent-shell-plans-test-document-summary ()
  (+agent-shell-plans-test--with-documents
    (let ((+agent-shell-plans-directories (list root)))
      (+agent-shell-plans-test--write
       (expand-file-name "iota/plan.md" root)
       "# Iota plan\n\nDraft for review, 1 January.\n\nSummary: Move every \
caller\nto the new parser.\n\nMore prose.\n")
      (+agent-shell-plans-test--write
       (expand-file-name "iota/notes.org" root)
       "#+title: Iota notes\n#+summary: Notes on callers.\n\nProse.\n")
      (+agent-shell-plans)
      (+agent-shell-plans-test--project "Iota")
      (forward-line 1)
      (should (looking-at "    Move every caller to the new parser\\.$"))
      (+agent-shell-plans-test--row "Iota notes")
      (magit-section-toggle (magit-current-section))
      (forward-line 1)
      (should (looking-at "      Notes on callers\\.$")))))

(ert-deftest +agent-shell-plans-test-tab-cycles-a-project ()
  (+agent-shell-plans-test--with-documents
    (let ((+agent-shell-plans-directories (list root)))
      (+agent-shell-plans-test--write
       (expand-file-name "kappa/design.md" root)
       "# Kappa design\n\nSummary: Kappa in short.\n")
      (+agent-shell-plans-test--write
       (expand-file-name "kappa/notes.md" root) "# Kappa notes\n\nNotes.\n")
      (+agent-shell-plans)
      (+agent-shell-plans-test--project "Kappa")
      (let* ((project (magit-current-section))
             (children (oref project children))
             (tab (key-binding (kbd "TAB")))
             (folded (lambda () (seq-every-p (lambda (child) (oref child hidden))
                                             children)))
             (open (lambda () (seq-every-p (lambda (child)
                                             (not (oref child hidden)))
                                           children))))
        (should (eq tab #'magit-section-cycle))
        (should (= 2 (length children)))
        (should (oref project hidden))
        (call-interactively tab)
        (should-not (oref project hidden))
        (should (funcall folded))
        (call-interactively tab)
        (should (funcall open))
        (call-interactively tab)
        (should (oref project hidden))
        (call-interactively tab)
        (should-not (oref project hidden))
        (should (funcall folded))))))

(ert-deftest +agent-shell-plans-test-document-git-bound-and-refresh ()
  (+agent-shell-plans-test--with-documents
    (let ((+agent-shell-plans-directories (list root))
          (file (expand-file-name "epsilon/plan.md" root))
          (gits 0))
      (+agent-shell-plans-test--write file "# Epsilon\n")
      (+agent-shell-plans-test--write
       (expand-file-name "epsilon/notes.md" root) "# Notes\n")
      (+agent-shell-plans-test--git root "init" "-q")
      (+agent-shell-plans-test--commit root "2026-01-01T00:00:00Z")
      (advice-add 'process-file :before
                  (lambda (&rest _) (cl-incf gits))
                  '((name . +agent-shell-plans-test--count)))
      (unwind-protect
          (progn
            (+agent-shell-plans)
            (should (= 2 gits))
            (+agent-shell-plans--render)
            (should (= 2 gits))
            (+agent-shell-plans-test--write file "# Epsilon renamed\n- [x] A\n")
            (set-file-times file (time-add nil 5))
            (+agent-shell-plans--render)
            (should (= 3 gits))
            (let ((before (+agent-shell-plans-test--row "Epsilon renamed")))
              (should (string-match-p "· 1 of 1 done · " before))
              (+agent-shell-plans-test--commit root "2099-01-01T00:00:00Z")
              (revert-buffer)
              (should (= 5 gits))
              (should-not (equal before (+agent-shell-plans-test--row
                                         "Epsilon renamed")))))
        (advice-remove 'process-file '+agent-shell-plans-test--count)))))

(ert-deftest +agent-shell-plans-test-document-visit ()
  (+agent-shell-plans-test--with-documents
    (let ((+agent-shell-plans-directories (list root))
          (file (expand-file-name "zeta/plan.md" root)))
      (+agent-shell-plans-test--write file "# Zeta\n")
      (+agent-shell-plans)
      (+agent-shell-plans-test--row "Zeta")
      (let (visited)
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _) (setq visited buffer))))
          (call-interactively (key-binding (kbd "RET"))))
        (should (equal file (buffer-file-name visited))))
      (should (equal (abbreviate-file-name file)
                     (progn (magit-section-toggle (magit-current-section))
                            (forward-line 1)
                            (looking-at "      File  \\(.*\\)$")
                            (match-string 1))))
      (call-interactively #'+agent-shell-plans-keys)
      (should (string-match-p "RET     Visit this document"
                              (with-current-buffer "*agent plans keys*"
                                (buffer-string))))
      (kill-buffer "*agent plans keys*"))))

(provide 'agent-shell-plans-test)
;;; agent-shell-plans-test.el ends here
