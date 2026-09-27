;;; agent-shell-review-test.el --- Review session tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise session selection, event handling, and interactive actions.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'agent-shell-review nil t)

(defconst agent-shell-review-test--root
  (file-name-directory (directory-file-name
                        (file-name-directory load-file-name)))
  "Root of the checkout containing these tests.")

(defun agent-shell-review-test--load-local-review-config ()
  "Evaluate this checkout's local review use-package form."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "modules/mod-ai.el"
                                           agent-shell-review-test--root))
    (goto-char (point-min))
    (search-forward "(use-package agent-shell-review")
    (goto-char (match-beginning 0))
    (eval (read (current-buffer)))))

(defconst agent-shell-review-test--questions
  "{\"kind\":\"questions\",\"items\":[{\"question\":\"Which API?\",\"criterion\":\"Compatibility\",\"why\":\"Two callers\"}]}"
  "Reviewer questions fixture.")

(ert-deftest agent-shell-review-test-spec-picker-restores-sidebar ()
  "Keep review progress visible after a spec picker changes windows."
  (let* ((root (make-temp-file "review-picker-" t))
         (specs (expand-file-name "docs/specs" root))
         (chosen (expand-file-name "selected-spec.md" specs))
         (other (expand-file-name "other-spec.md" specs))
         (agent-shell-review-spec-search-roots nil)
         run)
    (make-directory specs t)
    (with-temp-file chosen (insert "Selected criteria"))
    (with-temp-file other (insert "Other criteria"))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-review--project-root)
                   (lambda () root))
                  ((symbol-function 'agent-shell-review-git-snapshot)
                   (lambda (&rest _args)
                     (list :root root :base "main" :diff "change")))
                  ((symbol-function 'agent-shell-review-acp-create)
                   (lambda (&rest _args)
                     (make-agent-shell-review-acp--transport :closed t)))
                  ((symbol-function 'agent-shell-review-acp-start)
                   (lambda (_transport) nil))
                  ((symbol-function 'agent-shell-review-acp-close)
                   (lambda (_transport) nil))
                  ((symbol-function 'completing-read)
                   (lambda (prompt _choices &rest _args)
                     (should (equal prompt "Review requirements: "))
                     (when-let* ((window (get-buffer-window
                                          (format "*Agent Review: %s*"
                                                  (file-name-nondirectory
                                                   (directory-file-name root)))
                                          t)))
                       (delete-window window))
                     chosen)))
          (with-temp-buffer
            (setq default-directory root
                  run (agent-shell-review))
            (should (eq (agent-shell-review--run-status run) 'starting))
            (should (equal (plist-get (agent-shell-review--run-requirements run)
                                      :source)
                           chosen))
            (should (get-buffer-window
                     (agent-shell-review--run-sidebar-buffer run) t))))
      (when (and run (buffer-live-p (agent-shell-review--run-sidebar-buffer run)))
        (kill-buffer (agent-shell-review--run-sidebar-buffer run)))
      (delete-directory root t))))

(ert-deftest agent-shell-review-test-sidebar-width ()
  "The review sidebar stays right, at most a third wide, without focus."
  (let* ((frame (selected-frame))
         (old-width (frame-width frame))
         (source-window (selected-window))
         (run (make-agent-shell-review--run
               :project temporary-file-directory :status 'reviewing)))
    (unwind-protect
        (progn
          (set-frame-width frame 120)
          (let ((sidebar (agent-shell-review-ui-show run)))
            (should (window-live-p sidebar))
            (should (<= (window-width sidebar) (/ (frame-width) 3)))
            (should (eq (selected-window) source-window))
            (should (eq (window-parameter sidebar 'window-side) 'right))))
      (when-let* ((window (get-buffer-window
                           (agent-shell-review--run-sidebar-buffer run))))
        (delete-window window))
      (when (buffer-live-p (agent-shell-review--run-sidebar-buffer run))
        (kill-buffer (agent-shell-review--run-sidebar-buffer run)))
      (set-frame-width frame old-width))))

(ert-deftest agent-shell-review-test-clarifications-rerun ()
  "A fresh pass carries prior answers and retains old findings as stale."
  (let* ((answers '(("Which API?" . "Use v2")))
         (old-result '(:kind findings :items ((:priority "P1" :title "Old"))))
         (old (make-agent-shell-review--run
               :project temporary-file-directory
               :origin '(:context "Old request") :transport 'old-transport
               :requirements '(:kind file :source "/tmp/spec.md" :text "Spec")
               :clarifications answers :result old-result :status 'findings))
         (new-run nil)
         (closed nil)
         (buffer (generate-new-buffer " *review-rerun*")))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-review--begin)
                   (lambda (_root origin _spec clarifications stale &optional _requirements)
                     (setq new-run
                           (make-agent-shell-review--run
                            :origin origin
                            :clarifications clarifications
                            :stale-result stale))))
                  ((symbol-function 'agent-shell-review-acp-close)
                   (lambda (transport) (push transport closed))))
          (with-current-buffer buffer
            (agent-shell-review-mode)
            (setq-local agent-shell-review--current-run old)
            (agent-shell-review-rerun))
          (should (equal (agent-shell-review--run-clarifications new-run)
                         answers))
          (should (equal (agent-shell-review--run-stale-result new-run)
                         old-result))
          (should (equal (agent-shell-review--run-origin new-run)
                         '(:context "Old request")))
          (should (equal closed '(old-transport))))
      (kill-buffer buffer))))

(ert-deftest agent-shell-review-test-rerun-keeps-earlier-stale-findings ()
  "Rerunning after an error keeps findings from the earlier pass visible."
  (let* ((findings '(:kind findings :items ((:title "Earlier issue"))))
         (old (make-agent-shell-review--run
               :project temporary-file-directory :status 'error
               :stale-result findings))
         (buffer (generate-new-buffer " *review-stale-rerun*"))
         carried)
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-review--begin)
                   (lambda (_root _origin _spec _answers stale
                            &optional _requirements)
                     (setq carried stale))))
          (with-current-buffer buffer
            (agent-shell-review-mode)
            (setq-local agent-shell-review--current-run old)
            (agent-shell-review-rerun))
          (should (equal carried findings)))
      (kill-buffer buffer))))

(ert-deftest agent-shell-review-test-handoff-callback-and-copy ()
  "Send marked fixes through the captured callback or expose them for copy."
  (let* ((sent nil)
         (kill-ring nil)
         (result '(:kind findings
                         :items ((:priority "P1" :file "src/a.el" :line 4
                                            :title "Wrong" :evidence "nil"
                                            :requirement "return path"
                                            :suggestion "return path"))))
         (run (make-agent-shell-review--run
               :project temporary-file-directory
               :requirements '(:kind file :source "spec.md"
                                     :text "Original criteria")
               :clarifications '(("Which API?" . "Use v2"))
               :result result :status 'findings
               :send-fixes (lambda (prompt) (push prompt sent) t)))
         (buffer (generate-new-buffer " *review-fixes*")))
    (unwind-protect
        (with-current-buffer buffer
          (agent-shell-review-mode)
          (setq-local agent-shell-review--current-run run)
          (setf (agent-shell-review--run-sidebar-buffer run) buffer)
          (agent-shell-review-ui-render run)
          (search-forward "Wrong")
          (agent-shell-review-mark)
          (should-not sent)
          (agent-shell-review-send-marked)
          (should (= (length sent) 1))
          (should (string-match-p "Original criteria" (car sent)))
          (should (string-match-p "Use v2" (car sent)))
          (should (string-match-p "Wrong" (car sent)))
          (setf (agent-shell-review--run-send-fixes run)
                (lambda (_prompt) (user-error "Busy")))
          (should-error (agent-shell-review-send-marked) :type 'user-error)
          (should (equal (agent-shell-review--run-marks run) '(0)))
          (should (string-match-p "Wrong"
                                  (agent-shell-review--run-fix-prompt run)))
          (should (equal (current-kill 0)
                         (agent-shell-review--run-fix-prompt run)))
          (should (eq (current-buffer) buffer))
          (setf (agent-shell-review--run-send-fixes run) nil)
          (agent-shell-review-send-marked)
          (let ((copy (get-buffer
                       (format "*Agent Review Fixes: %s*"
                               (file-name-nondirectory
                                (directory-file-name temporary-file-directory))))))
            (should (buffer-live-p copy))
            (with-current-buffer copy
              (should (string-match-p "Wrong" (buffer-string))))
            (kill-buffer copy)))
      (kill-buffer buffer))))

(ert-deftest agent-shell-review-test-rerun-entered-requirements ()
  "A fresh pass reuses manually entered criteria without prompting."
  (let* ((requirements '(:kind entered :source "entered" :text "Keep this"))
         (old (make-agent-shell-review--run
               :project temporary-file-directory :status 'clear
               :requirements requirements))
         (buffer (generate-new-buffer " *review-entered*"))
         seen)
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-review--begin)
                   (lambda (_root _origin _spec _answers _stale &optional saved)
                     (setq seen saved)))
                  ((symbol-function 'agent-shell-review-acp-close) #'ignore))
          (with-current-buffer buffer
            (agent-shell-review-mode)
            (setq-local agent-shell-review--current-run old)
            (agent-shell-review-rerun))
          (should (equal seen requirements)))
      (kill-buffer buffer))))

(ert-deftest agent-shell-review-test-replacing-sidebar-cancels-old-run ()
  "Starting again from another buffer ends the prior process and timer."
  (let* ((root (make-temp-file "review-replace-" t))
         (old (make-agent-shell-review--run
               :project root :status 'reviewing :transport 'old-transport))
         (new (make-agent-shell-review--run
               :project root :status 'reviewing))
         old-timer closed)
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-review-acp-close)
                   (lambda (transport) (push transport closed))))
          (agent-shell-review-ui-show old)
          (setq old-timer
                (buffer-local-value 'agent-shell-review-ui--refresh-timer
                                    (agent-shell-review--run-sidebar-buffer old)))
          (agent-shell-review-ui-show new)
          (should (equal closed '(old-transport)))
          (should (agent-shell-review--run-cancelled old))
          (should-not (memq old-timer timer-list))
          (should (eq (buffer-local-value 'agent-shell-review--current-run
                                          (agent-shell-review--run-sidebar-buffer new))
                      new)))
      (when (buffer-live-p (agent-shell-review--run-sidebar-buffer new))
        (kill-buffer (agent-shell-review--run-sidebar-buffer new)))
      (delete-directory root t))))

(ert-deftest agent-shell-review-test-diagnostic-view ()
  "Show retained ACP evidence in a non-shell buffer."
  (let* ((run (make-agent-shell-review--run
               :project temporary-file-directory :transport 'transport
               :text "raw answer" :error-message "parser error"))
         (buffer (generate-new-buffer " *review-diagnostics*")))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-review-acp-diagnostics)
                   (lambda (_transport) "Request: session/prompt\nError: timeout")))
          (with-current-buffer buffer
            (agent-shell-review-mode)
            (setq-local agent-shell-review--current-run run)
            (agent-shell-review-open-reviewer))
          (let ((diagnostics
                 (get-buffer
                  (format "*Agent Review Diagnostics: %s*"
                          (file-name-nondirectory
                           (directory-file-name temporary-file-directory))))))
            (should (buffer-live-p diagnostics))
            (with-current-buffer diagnostics
              (should (string-match-p "raw answer" (buffer-string)))
              (should (string-match-p "timeout" (buffer-string)))
              (should-not (derived-mode-p 'agent-shell-mode)))
            (kill-buffer diagnostics)))
      (kill-buffer buffer))))

(ert-deftest agent-shell-review-test-kill-cancels-hide-keeps-running ()
  "Hiding preserves the review; killing its sidebar cancels it."
  (let* ((run (make-agent-shell-review--run
               :project temporary-file-directory :transport 'transport
               :status 'reviewing))
         (closed nil))
    (cl-letf (((symbol-function 'agent-shell-review-acp-close)
               (lambda (transport) (push transport closed))))
      (let* ((window (agent-shell-review-ui-show run))
             (buffer (agent-shell-review--run-sidebar-buffer run)))
        (quit-window nil window)
        (should-not closed)
        (kill-buffer buffer)
        (should (equal closed '(transport)))))))

(ert-deftest agent-shell-review-test-status-rendering ()
  "Render progress, stale findings, and a brief parser error."
  (let* ((buffer (generate-new-buffer " *review-status*"))
         (run (make-agent-shell-review--run
               :project temporary-file-directory :status 'reviewing
               :snapshot '(:root "/tmp" :base "main"
                                 :fingerprint "before")
               :result '(:kind findings
                               :items ((:priority "P2" :file "a.el"
                                                  :line 2 :title "Old issue"
                                                  :evidence "old evidence"
                                                  :requirement "spec"
                                                  :suggestion "fix"))))))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-review-git-current-fingerprint)
                   (lambda (_snapshot) "after")))
          (with-current-buffer buffer
            (agent-shell-review-mode)
            (setq-local agent-shell-review--current-run run)
            (setf (agent-shell-review--run-sidebar-buffer run) buffer)
            (agent-shell-review-ui-render run)
            (should (string-match-p "stale" (buffer-string)))
            (should (string-match-p "Review: reviewing"
                                    (buffer-string)))
            (setf (agent-shell-review--run-status run) 'error
                  (agent-shell-review--run-error-message run)
                  "Reviewer result could not be parsed"
                  (agent-shell-review--run-text run)
                  "RAW_SECRET_RESPONSE")
            (agent-shell-review-ui-render run)
            (should (string-match-p "could not be parsed"
                                    (buffer-string)))
            (should-not (string-match-p "v: open reviewer"
                                        (buffer-string)))
            (should-not (string-match-p "RAW_SECRET_RESPONSE"
                                        (buffer-string)))))
      (kill-buffer buffer))))

(ert-deftest agent-shell-review-test-rerun-owns-sidebar ()
  "A late event from an older pass cannot replace the new pass's sidebar."
  (let* ((buffer (generate-new-buffer " *review-current*"))
         (old (make-agent-shell-review--run
               :project temporary-file-directory :status 'clear
               :sidebar-buffer buffer))
         (new (make-agent-shell-review--run
               :project temporary-file-directory :status 'reviewing
               :sidebar-buffer buffer)))
    (unwind-protect
        (with-current-buffer buffer
          (agent-shell-review-mode)
          (setq-local agent-shell-review--current-run new)
          (agent-shell-review-ui-render new)
          (let ((before (buffer-string)))
            (agent-shell-review-ui-render old)
            (should (eq agent-shell-review--current-run new))
            (should (equal (buffer-string) before))))
      (kill-buffer buffer))))

(ert-deftest agent-shell-review-test-new-result-replaces-stale ()
  "A finished fresh pass removes the previous findings from the sidebar."
  (let ((run (make-agent-shell-review--run
              :project temporary-file-directory :status 'reviewing
              :stale-result '(:kind findings
                                    :items ((:title "Old issue")))
              :transport 'transport
              :text "{\"kind\":\"clear\",\"items\":[]}")))
    (cl-letf (((symbol-function 'agent-shell-review-acp-close) #'ignore))
      (agent-shell-review--parse-result run)
      (should (eq (agent-shell-review--run-status run) 'clear))
      (should-not (agent-shell-review--run-stale-result run)))))

(ert-deftest agent-shell-review-test-unanswered-blocks-submission ()
  "No question round is submitted until every answer is supplied."
  (let* ((buffer (generate-new-buffer " *review-questions*"))
         (run (make-agent-shell-review--run
               :project temporary-file-directory :status 'questions
               :result '(:kind questions
                               :items ((:question "Which API?"
                                                  :criterion "Compatibility"
                                                  :why "Two callers")))
               :sidebar-buffer buffer)))
    (unwind-protect
        (with-current-buffer buffer
          (agent-shell-review-mode)
          (setq-local agent-shell-review--current-run run)
          (should-error (agent-shell-review-submit-answers) :type 'user-error))
      (kill-buffer buffer))))

(ert-deftest agent-shell-review-test-evil-sidebar-keys ()
  "Review actions remain available in Evil normal state."
  (require 'evil)
  (agent-shell-review-test--load-local-review-config)
  (evil-mode 1)
  (let ((buffer (generate-new-buffer " *review-evil*")))
    (unwind-protect
        (with-current-buffer buffer
          (agent-shell-review-mode)
          (should (eq evil-state 'normal))
          (should (eq (key-binding (kbd "m"))
                      #'agent-shell-review-mark))
          (should (eq (key-binding (kbd "g"))
                      #'agent-shell-review-rerun))
          (should (eq (key-binding (kbd "TAB"))
                      #'agent-shell-review-toggle-details)))
      (kill-buffer buffer)
      (evil-mode -1))))

(ert-deftest agent-shell-review-test-render-preserves-selected-item ()
  "Redrawing details and marks keeps point on the selected finding."
  (let* ((buffer (generate-new-buffer " *review-point*"))
         (run (make-agent-shell-review--run
               :project temporary-file-directory :status 'findings
               :result '(:kind findings
                               :items ((:priority "P1" :title "First")
                                       (:priority "P2" :title "Second")))
               :sidebar-buffer buffer)))
    (unwind-protect
        (with-current-buffer buffer
          (agent-shell-review-mode)
          (setq-local agent-shell-review--current-run run)
          (agent-shell-review-ui-render run)
          (search-forward "Second")
          (let ((column (current-column)))
            (agent-shell-review-toggle-details)
            (should (= (agent-shell-review-ui--index) 1))
            (should (= (current-column) column))
            (agent-shell-review-mark)
            (should (= (agent-shell-review-ui--index) 1))
            (should (= (current-column) column))))
      (kill-buffer buffer))))

(ert-deftest agent-shell-review-test-no-inline-key-hints-or-custom-modeline ()
  "The review buffer leaves key discovery to its map and uses the modeline."
  (let* ((buffer (generate-new-buffer " *review-display*"))
         (run (make-agent-shell-review--run
               :project temporary-file-directory :status 'findings
               :result '(:kind findings :items nil)
               :sidebar-buffer buffer)))
    (unwind-protect
        (with-current-buffer buffer
          (agent-shell-review-mode)
          (setq-local agent-shell-review--current-run run)
          (agent-shell-review-ui-render run)
          (should
           (equal mode-line-format
                  (with-temp-buffer
                    (special-mode)
                    (when (featurep 'evil) (evil-normal-state))
                    mode-line-format)))
          (should-not (string-match-p "Mark fixes with\|fresh review\|select a requirements file"
                                      (buffer-string))))
      (kill-buffer buffer))))

(ert-deftest agent-shell-review-test-navigation-boundaries ()
  "Item navigation terminates before and after the item list."
  (let ((buffer (generate-new-buffer " *review-nav*"))
        (next-original (symbol-function 'next-single-property-change))
        (previous-original (symbol-function 'previous-single-property-change))
        (calls 0))
    (unwind-protect
        (with-current-buffer buffer
          (agent-shell-review-mode)
          (let ((inhibit-read-only t))
            (insert "Header\n")
            (insert (propertize "1. Item" 'agent-shell-review-item 0))
            (insert "\nFooter"))
          (cl-letf (((symbol-function 'next-single-property-change)
                     (lambda (&rest args)
                       (when (> (cl-incf calls) 8)
                         (error "Navigation did not terminate"))
                       (apply next-original args)))
                    ((symbol-function 'previous-single-property-change)
                     (lambda (&rest args)
                       (when (> (cl-incf calls) 8)
                         (error "Navigation did not terminate"))
                       (apply previous-original args))))
            (goto-char (point-min))
            (should-error (agent-shell-review-previous-item)
                          :type 'user-error)
            (setq calls 0)
            (goto-char (point-max))
            (should-error (agent-shell-review-next-item)
                          :type 'user-error)))
      (kill-buffer buffer))))

(ert-deftest agent-shell-review-test-visible-staleness-refresh ()
  "A visible completed review updates its stale marker after file changes."
  (let* ((fingerprint "before")
         (run (make-agent-shell-review--run
               :project temporary-file-directory :status 'findings
               :snapshot '(:root "/tmp" :base "main"
                                 :fingerprint "before")
               :result '(:kind findings :items ((:priority "P2"
                                                            :title "Issue"))))))
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'agent-shell-review-git-current-fingerprint)
                     (lambda (_snapshot) fingerprint)))
            (agent-shell-review-ui-show run)
            (with-current-buffer (agent-shell-review--run-sidebar-buffer run)
              (should-not (string-match-p "stale" (buffer-string))))
            (setq fingerprint "after")
            (agent-shell-review-ui--refresh
             (agent-shell-review--run-sidebar-buffer run))
            (with-current-buffer (agent-shell-review--run-sidebar-buffer run)
              (should (string-match-p "stale" (buffer-string))))))
      (when (buffer-live-p (agent-shell-review--run-sidebar-buffer run))
        (kill-buffer (agent-shell-review--run-sidebar-buffer run))))))

(ert-deftest agent-shell-review-test-expanded-details-wrap ()
  "Expanded evidence remains readable within the narrow sidebar."
  (let* ((frame (selected-frame))
         (old-width (frame-width frame))
         (run (make-agent-shell-review--run
               :project temporary-file-directory :status 'findings
               :expanded '(0)
               :result '(:kind findings
                               :items ((:priority "P1" :file "app.el"
                                                  :line 4 :title "Issue"
                                                  :evidence "This long evidence sentence explains a concrete correctness failure that needs several visual lines at sidebar width."
                                                  :requirement "Must work"
                                                  :suggestion "Fix it"))))))
    (unwind-protect
        (save-window-excursion
          (set-frame-width frame 90)
          (let ((sidebar (agent-shell-review-ui-show run)))
            (with-selected-window sidebar
              (should-not truncate-lines)
              (should word-wrap)
              (goto-char (point-min))
              (search-forward "Evidence:")
              (beginning-of-line)
              (let ((logical-end (line-end-position)))
                (should (> (- logical-end (point)) (window-body-width)))
                (when (display-graphic-p)
                  (vertical-motion 1)
                  (should (< (point) logical-end)))))))
      (when (buffer-live-p (agent-shell-review--run-sidebar-buffer run))
        (kill-buffer (agent-shell-review--run-sidebar-buffer run)))
      (set-frame-width frame old-width))))

(ert-deftest agent-shell-review-test-acp-entry-order ()
  "Resolve criteria and store the transport before a synchronous ready event."
  (let* ((root temporary-file-directory)
         (origin '(:context "Implement the feature" :send ignore))
         (agent-shell-review-origin-provider (lambda (_root) origin))
         (steps nil)
         (sent nil)
         (callback nil))
    (cl-letf (((symbol-function 'agent-shell-review--project-root)
               (lambda () root))
              ((symbol-function 'agent-shell-review-ui-show) #'ignore)
              ((symbol-function 'agent-shell-review-ui-render) #'ignore)
              ((symbol-function 'agent-shell-review-git-snapshot)
               (lambda (&rest _args)
                 '(:root "/tmp" :base "main" :diff "change")))
              ((symbol-function 'agent-shell-review-spec-resolve)
               (lambda (_root context &optional _file)
                 (push 'requirements steps)
                 (should (equal context "Implement the feature"))
                 '(:kind conversation :source "implementation conversation"
                         :text "Implement the feature")))
              ((symbol-function 'agent-shell-review-acp-create)
               (lambda (_root _file on-event)
                 (push 'create steps)
                 (setq callback on-event)
                 'transport))
              ((symbol-function 'agent-shell-review-acp-start)
               (lambda (_transport)
                 (push 'start steps)
                 (funcall callback '(:type ready))))
              ((symbol-function 'agent-shell-review-acp-send)
               (lambda (_transport prompt)
                 (push 'send steps)
                 (setq sent prompt)
                 t)))
      (let ((run (agent-shell-review)))
        (should (equal (reverse steps)
                       '(requirements create start send)))
        (should (eq (agent-shell-review--run-transport run) 'transport))
        (should (eq (agent-shell-review--run-status run) 'reviewing))
        (should (string-match-p "Implement the feature" sent))))))

(ert-deftest agent-shell-review-test-send-keeps-transport-error ()
  "A failed send keeps the transport's specific visible error."
  (let ((run (make-agent-shell-review--run
              :project temporary-file-directory :transport 'transport
              :status 'reviewing)))
    (cl-letf (((symbol-function 'agent-shell-review-acp-send)
               (lambda (_transport _prompt)
                 (agent-shell-review--handle-event
                  run '(:type error :message "read-only mode refused"))
                 nil))
              ((symbol-function 'agent-shell-review-acp-close) #'ignore))
      (agent-shell-review--send run "Review now")
      (should (eq (agent-shell-review--run-status run) 'error))
      (should (equal (agent-shell-review--run-error-message run)
                     "Reviewer failed: read-only mode refused")))))

(ert-deftest agent-shell-review-test-send-without-transport-error ()
  "An unreported failed send still produces a visible error."
  (let ((run (make-agent-shell-review--run
              :project temporary-file-directory :transport 'transport
              :status 'reviewing)))
    (cl-letf (((symbol-function 'agent-shell-review-acp-send)
               (lambda (_transport _prompt) nil))
              ((symbol-function 'agent-shell-review-acp-close) #'ignore))
      (agent-shell-review--send run "Review now")
      (should (eq (agent-shell-review--run-status run) 'error))
      (should (equal (agent-shell-review--run-error-message run)
                     "Reviewer prompt was not sent")))))

(ert-deftest agent-shell-review-test-acp-questions-and-repair ()
  "Continue questions and one format repair on the same transport."
  (let* ((sent nil)
         (closed nil)
         (run (make-agent-shell-review--run
               :project temporary-file-directory :transport 'transport
               :status 'reviewing :text agent-shell-review-test--questions)))
    (cl-letf (((symbol-function 'agent-shell-review-acp-send)
               (lambda (transport prompt)
                 (should (eq transport 'transport))
                 (push prompt sent) t))
              ((symbol-function 'agent-shell-review-acp-close)
               (lambda (transport) (push transport closed))))
      (agent-shell-review--handle-event
       run '(:type complete :stop-reason "end_turn"))
      (should (eq (agent-shell-review--run-status run) 'questions))
      (should-not closed)
      (agent-shell-review--submit-answers run '(("Which API?" . "Use v2")))
      (should (string-match-p "Use v2" (car sent)))
      (should (eq (agent-shell-review--run-status run) 'reviewing))
      (setf (agent-shell-review--run-text run) "invalid json")
      (agent-shell-review--handle-event
       run '(:type complete :stop-reason "end_turn"))
      (should (= (length sent) 2))
      (setf (agent-shell-review--run-text run) "still invalid")
      (agent-shell-review--handle-event
       run '(:type complete :stop-reason "end_turn"))
      (should (eq (agent-shell-review--run-status run) 'error))
      (should (equal closed '(transport))))))

(ert-deftest agent-shell-review-test-acp-stale-callback-and-cancel ()
  "An old or cancelled run cannot replace a newer review."
  (let* ((buffer (generate-new-buffer " *review-acp-current*"))
         (closed nil)
         (old (make-agent-shell-review--run
               :project temporary-file-directory :transport 'old
               :sidebar-buffer buffer :status 'reviewing :text ""))
         (new (make-agent-shell-review--run
               :project temporary-file-directory :transport 'new
               :sidebar-buffer buffer :status 'reviewing)))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-review-acp-close)
                   (lambda (transport) (push transport closed))))
          (with-current-buffer buffer
            (agent-shell-review-mode)
            (setq-local agent-shell-review--current-run new))
          (agent-shell-review--handle-event old '(:type chunk :text "stale"))
          (should (equal (agent-shell-review--run-text old) ""))
          (agent-shell-review--cancel old)
          (agent-shell-review--handle-event old '(:type error :message "late"))
          (should (eq (agent-shell-review--run-status old) 'reviewing))
          (should (equal closed '(old)))
          (agent-shell-review--cancel new)
          (should (equal closed '(new old))))
      (kill-buffer buffer))))

(provide 'agent-shell-review-test)
;;; agent-shell-review-test.el ends here
