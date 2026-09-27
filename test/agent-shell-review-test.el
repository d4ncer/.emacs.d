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

(ert-deftest agent-shell-review-test-origin-shell ()
  "A shell invocation keeps that exact implementation shell."
  (let ((origin-shell (generate-new-buffer " *implementation*"))
        (fresh-shell (generate-new-buffer " *reviewer*"))
        (root temporary-file-directory)
        (captured-spec nil))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-review--project-root)
                   (lambda () root))
                  ((symbol-function 'agent-shell-review-git-snapshot)
                   (lambda (&rest _args)
                     (list :root root :base "main" :diff "change")))
                  ((symbol-function 'agent-shell-review-spec-resolve)
                   (lambda (_root _shell &optional explicit)
                     (setq captured-spec explicit)
                     '(:kind conversation :source "session" :text "Do it")))
                  ((symbol-function 'agent-shell-review--start-shell)
                   (lambda (_root) fresh-shell))
                  ((symbol-function 'agent-shell-subscribe-to)
                   (lambda (&rest _args) 1)))
          (with-current-buffer origin-shell
            (setq major-mode 'agent-shell-mode)
            (let ((run (agent-shell-review)))
              (should (eq (agent-shell-review--run-implementation-shell run)
                          origin-shell))
              (should (eq (agent-shell-review--run-reviewer-shell run)
                          fresh-shell))
              (should (null captured-spec)))
            (cl-letf (((symbol-function 'read-file-name)
                       (lambda (&rest _args) "/tmp/selected-spec.md")))
              (agent-shell-review '(4))
              (should (equal captured-spec "/tmp/selected-spec.md")))))
      (kill-buffer origin-shell)
      (kill-buffer fresh-shell))))

(ert-deftest agent-shell-review-test-fresh-session ()
  "Start a new background session and submit only after prompt readiness."
  (let* ((fresh-shell (generate-new-buffer " *reviewer*"))
         (agent-shell-preferred-agent-config '((:buffer-name . "Codex")))
         (start-args nil)
         (insertions nil)
         (run nil))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell--start)
                   (lambda (&rest args)
                     (setq start-args args)
                     fresh-shell))
                  ((symbol-function 'agent-shell-insert)
                   (lambda (&rest args) (push args insertions)))
                  ((symbol-function 'agent-shell-review--configure-read-only)
                   (lambda (_run continuation) (funcall continuation))))
          (should (eq (agent-shell-review--start-shell
                       temporary-file-directory) fresh-shell))
          (should (plist-get start-args :no-focus))
          (should (plist-get start-args :new-session))
          (setq run (make-agent-shell-review--run
                     :project temporary-file-directory
                     :reviewer-shell fresh-shell
                     :snapshot '(:diff "diff" :base "main")
                     :requirements '(:kind file :source "spec.md"
                                           :text "Criteria")
                     :status 'starting :pending-prompt "Review now"))
          (should (null insertions))
          (agent-shell-review--handle-event run '((:event . prompt-ready)))
          (should (null insertions))
          (agent-shell-review--handle-event run '((:event . init-finished)))
          (should (= (length insertions) 1))
          (should (equal (plist-get (car insertions) :text) "Review now"))
          (should (plist-get (car insertions) :submit))
          (should (plist-get (car insertions) :no-focus)))
      (kill-buffer fresh-shell))))

(ert-deftest agent-shell-review-test-spec-picker-restores-sidebar ()
  "Keep review progress visible after a spec picker changes windows."
  (let* ((root (make-temp-file "review-picker-" t))
         (specs (expand-file-name "docs/specs" root))
         (chosen (expand-file-name "selected-spec.md" specs))
         (other (expand-file-name "other-spec.md" specs))
         (reviewer (generate-new-buffer " *picker-reviewer*"))
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
                  ((symbol-function 'agent-shell-review--start-shell)
                   (lambda (_root) reviewer))
                  ((symbol-function 'agent-shell-subscribe-to)
                   (lambda (&rest _args) 1))
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
      (kill-buffer reviewer)
      (delete-directory root t))))

(ert-deftest agent-shell-review-test-source-ambiguity ()
  "A source buffer prompts when two implementation shells match."
  (let* ((root (make-temp-file "review-shell-choice-" t))
         (first (generate-new-buffer " *first-implementation*"))
         (second (generate-new-buffer " *second-implementation*"))
         (selection-count 0))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-buffers)
                   (lambda () (list first second)))
                  ((symbol-function 'agent-shell-cwd)
                   (lambda () root))
                  ((symbol-function 'completing-read)
                   (lambda (_prompt _choices &rest _args)
                     (cl-incf selection-count)
                     (buffer-name second))))
          (should (eq (agent-shell-review--choose-shell root) second))
          (should (= selection-count 1)))
      (kill-buffer first)
      (kill-buffer second)
      (delete-directory root t))))

(ert-deftest agent-shell-review-test-read-only-mode ()
  "Select an advertised read-only mode before continuing the prompt."
  (let ((shell (generate-new-buffer " *read-only-reviewer*"))
        (selected nil)
        (continued nil))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell--state)
                   (lambda () 'fake-state))
                  ((symbol-function 'agent-shell--get-available-modes)
                   (lambda (_state)
                     '(((:id . "edit") (:name . "Edit"))
                       ((:id . "read-only") (:name . "Read Only")))))
                  ((symbol-function 'agent-shell--config-option-set-mode-id)
                   (lambda (&rest args)
                     (setq selected (plist-get args :mode-id))
                     (funcall (plist-get args :on-success)))))
          (let ((run (make-agent-shell-review--run :reviewer-shell shell)))
            (agent-shell-review--configure-read-only
             run (lambda () (setq continued t)))
            (should (equal selected "read-only"))
            (should continued)))
      (kill-buffer shell))))

(ert-deftest agent-shell-review-test-questions-continue ()
  "Questions continue in the same session and statuses fire once."
  (let* ((fresh-shell (generate-new-buffer " *reviewer*"))
         (insertions nil)
         (statuses nil)
         (agent-shell-review-status-change-hook
          (list (lambda (_run status) (push status statuses))))
         (run (make-agent-shell-review--run
               :project temporary-file-directory :reviewer-shell fresh-shell
               :status 'reviewing :text agent-shell-review-test--questions)))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-insert)
                   (lambda (&rest args) (push args insertions))))
          (agent-shell-review--handle-event
           run '((:event . turn-complete)
                 (:data . ((:stop-reason . "end_turn")))))
          (should (eq (agent-shell-review--run-status run) 'questions))
          (should (equal statuses '(questions)))
          (agent-shell-review--set-status run 'questions)
          (should (equal statuses '(questions)))
          (agent-shell-review--submit-answers
           run '(("Which API?" . "Use v2")))
          (should (eq (agent-shell-review--run-reviewer-shell run)
                      fresh-shell))
          (should (equal (agent-shell-review--run-clarifications run)
                         '(("Which API?" . "Use v2"))))
          (should (string-match-p "Use v2"
                                  (plist-get (car insertions) :text)))
          (should (eq (agent-shell-review--run-status run) 'reviewing)))
      (kill-buffer fresh-shell))))

(ert-deftest agent-shell-review-test-parse-repair ()
  "Try one reformat request, then show failure without a false clear."
  (let* ((fresh-shell (generate-new-buffer " *reviewer*"))
         (repair-requests 0)
         (run (make-agent-shell-review--run
               :project temporary-file-directory :reviewer-shell fresh-shell
               :status 'reviewing :text "not json")))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-insert)
                   (lambda (&rest _args) (cl-incf repair-requests))))
          (agent-shell-review--handle-event
           run '((:event . turn-complete)
                 (:data . ((:stop-reason . "end_turn")))))
          (should (= repair-requests 1))
          (should (agent-shell-review--run-repair-attempt run))
          (should (eq (agent-shell-review--run-status run) 'reviewing))
          (setf (agent-shell-review--run-text run) "still not json")
          (agent-shell-review--handle-event
           run '((:event . turn-complete)
                 (:data . ((:stop-reason . "end_turn")))))
          (should (= repair-requests 1))
          (should (eq (agent-shell-review--run-status run) 'error))
          (should-not (agent-shell-review--run-result run))
          (setf (agent-shell-review--run-status run) 'reviewing)
          (agent-shell-review--handle-event
           run '((:event . turn-complete)
                 (:data . ((:stop-reason . "max_tokens")))))
          (should (eq (agent-shell-review--run-status run) 'error)))
      (kill-buffer fresh-shell))))

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
               :requirements '(:kind file :source "/tmp/spec.md" :text "Spec")
               :clarifications answers :result old-result :status 'findings))
         (new-run nil)
         (buffer (generate-new-buffer " *review-rerun*")))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-review--begin)
                   (lambda (_root _shell _spec clarifications stale)
                     (setq new-run
                           (make-agent-shell-review--run
                            :clarifications clarifications
                            :stale-result stale)))))
          (with-current-buffer buffer
            (agent-shell-review-mode)
            (setq-local agent-shell-review--current-run old)
            (agent-shell-review-rerun))
          (should (equal (agent-shell-review--run-clarifications new-run)
                         answers))
          (should (equal (agent-shell-review--run-stale-result new-run)
                         old-result)))
      (kill-buffer buffer))))

(ert-deftest agent-shell-review-test-replacement-fixer ()
  "Marked fixes and clarified criteria reach a new shell if the old one died."
  (let* ((old-shell (generate-new-buffer " *closed-implementation*"))
         (new-shell (generate-new-buffer " *replacement*"))
         (sent nil)
         (result '(:kind findings
                         :items ((:priority "P1" :file "src/a.el" :line 4
                                            :title "Wrong" :evidence "nil"
                                            :requirement "must return path"
                                            :suggestion "return path"))))
         (run (make-agent-shell-review--run
               :project temporary-file-directory
               :implementation-shell old-shell
               :requirements '(:kind file :source "spec.md"
                                     :text "Original criteria")
               :clarifications '(("Which API?" . "clarified criterion"))
               :result result :status 'findings))
         (buffer (generate-new-buffer " *review-fixes*")))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-review--start-implementation-shell)
                   (lambda (_root) new-shell))
                  ((symbol-function 'agent-shell-insert)
                   (lambda (&rest args) (push args sent))))
          (with-current-buffer buffer
            (agent-shell-review-mode)
            (setq-local agent-shell-review--current-run run)
            (setf (agent-shell-review--run-sidebar-buffer run) buffer)
            (agent-shell-review-ui-render run)
            (goto-char (point-min))
            (search-forward "Wrong")
            (agent-shell-review-mark)
            (should (null sent))
            (kill-buffer old-shell)
            (agent-shell-review-send-marked))
          (should (eq (agent-shell-review--run-implementation-shell run)
                      new-shell))
          (should (= (length sent) 1))
          (let ((replacement-prompt (plist-get (car sent) :text)))
            (should (string-match-p "clarified criterion"
                                    replacement-prompt))
            (should (string-match-p "Original criteria"
                                    replacement-prompt))
            (should (string-match-p "Wrong" replacement-prompt))))
      (when (buffer-live-p old-shell) (kill-buffer old-shell))
      (kill-buffer new-shell)
      (kill-buffer buffer))))

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
            (should (string-match-p "reviewing"
                                    (format-mode-line header-line-format
                                                      nil nil buffer)))
            (setf (agent-shell-review--run-status run) 'error
                  (agent-shell-review--run-error-message run)
                  "Reviewer result could not be parsed"
                  (agent-shell-review--run-text run)
                  "RAW_SECRET_RESPONSE")
            (agent-shell-review-ui-render run)
            (should (string-match-p "could not be parsed"
                                    (buffer-string)))
            (should (string-match-p "v: open reviewer"
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
              :text "{\"kind\":\"clear\",\"items\":[]}")))
    (agent-shell-review--parse-result run)
    (should (eq (agent-shell-review--run-status run) 'clear))
    (should-not (agent-shell-review--run-stale-result run))))

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
  "The local config makes review keys effective under Evil."
  (require 'evil)
  (agent-shell-review-test--load-local-review-config)
  (let ((buffer (generate-new-buffer " *review-evil*")))
    (unwind-protect
        (with-current-buffer buffer
          (agent-shell-review-mode)
          (should (eq evil-state 'emacs))
          (should (eq (key-binding (kbd "m"))
                      #'agent-shell-review-mark))
          (should (eq (key-binding (kbd "g"))
                      #'agent-shell-review-rerun)))
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
              (goto-char (point-min))
              (search-forward "Evidence:")
              (beginning-of-line)
              (let ((logical-end (line-end-position)))
                (vertical-motion 1)
                (should (< (point) logical-end))))))
      (when (buffer-live-p (agent-shell-review--run-sidebar-buffer run))
        (kill-buffer (agent-shell-review--run-sidebar-buffer run)))
      (set-frame-width frame old-width))))

(ert-deftest agent-shell-review-test-preferred-config-designator ()
  "Resolve identifier preferences before starting review and fixer shells."
  (let* ((config '((:identifier . codex) (:buffer-name . "Codex")))
         (agent-shell-agent-configs (list config))
         (agent-shell-preferred-agent-config 'codex)
         (agent-shell-review-agent-config nil)
         (seen nil)
         (shell (generate-new-buffer " *review-config*")))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell--start)
                   (lambda (&rest args)
                     (push (plist-get args :config) seen)
                     shell)))
          (agent-shell-review--start-shell temporary-file-directory)
          (agent-shell-review--start-implementation-shell
           temporary-file-directory)
          (should (equal seen (list config config))))
      (kill-buffer shell))))

(provide 'agent-shell-review-test)
;;; agent-shell-review-test.el ends here
