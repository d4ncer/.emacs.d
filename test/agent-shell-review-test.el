;;; agent-shell-review-test.el --- Review session tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise session selection, event handling, and interactive actions.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'agent-shell-review nil t)

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
          (should (= (length insertions) 1))
          (should (equal (plist-get (car insertions) :text) "Review now"))
          (should (plist-get (car insertions) :submit))
          (should (plist-get (car insertions) :no-focus)))
      (kill-buffer fresh-shell))))

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

(provide 'agent-shell-review-test)
;;; agent-shell-review-test.el ends here
