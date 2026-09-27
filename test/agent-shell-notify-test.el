;;; agent-shell-notify-test.el --- Tests for agent-shell alerts -*- lexical-binding: t; -*-

;;; Commentary:
;; Focused tests for alert classification and subscription lifecycle.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'agent-shell-notify nil t)

(ert-deftest agent-shell-notify-test-classify ()
  "Classify input, completion, stop, and failure events."
  (should (eq (plist-get (agent-shell-notify--classify
                          '((:event . permission-request))) :kind)
              'permission))
  (should (eq (plist-get (agent-shell-notify--classify
                          '((:event . turn-complete)
                            (:data . ((:stop-reason . "end_turn"))))) :kind)
              'ready))
  (should (eq (plist-get (agent-shell-notify--classify
                          '((:event . turn-complete)
                            (:data . ((:stop-reason . "max_tokens"))))) :kind)
              'stopped))
  (should (string-match-p "max_tokens"
                          (plist-get (agent-shell-notify--classify
                                      '((:event . turn-complete)
                                        (:data . ((:stop-reason . "max_tokens")))))
                                     :body)))
  (should (eq (plist-get (agent-shell-notify--classify
                          '((:event . error)
                            (:data . ((:message . "broken"))))) :kind)
              'error))
  (should-not (agent-shell-notify--classify '((:event . agent-message-chunk)))))

(ert-deftest agent-shell-notify-test-subscription-lifecycle ()
  "Subscribe once to existing and future shells and remove subscriptions."
  (let ((existing (generate-new-buffer " *notify-existing*"))
        (future (generate-new-buffer " *notify-future*"))
        (subscribe-count 0)
        (unsubscribe-count 0)
        callbacks
        (agent-shell-notify--subscriptions (make-hash-table :test #'eq)))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-buffers)
                   (lambda () (list existing)))
                  ((symbol-function 'agent-shell-subscribe-to)
                   (lambda (&rest args)
                     (cl-incf subscribe-count)
                     (push (cons (plist-get args :shell-buffer)
                                 (plist-get args :on-event)) callbacks)
                     subscribe-count))
                  ((symbol-function 'agent-shell-unsubscribe)
                   (lambda (&rest _args) (cl-incf unsubscribe-count))))
          (agent-shell-notify-mode 1)
          (agent-shell-notify-mode 1)
          (should (= subscribe-count 1))
          (should (memq #'agent-shell-notify--attach agent-shell-mode-hook))
          (with-current-buffer future
            (agent-shell-notify--attach))
          (should (= subscribe-count 2))
          (funcall (cdr (assq future callbacks)) '((:event . clean-up)))
          (should (= unsubscribe-count 1))
          (agent-shell-notify-mode -1)
          (should (= unsubscribe-count 2)))
      (agent-shell-notify-mode -1)
      (kill-buffer existing)
      (kill-buffer future))))

(ert-deftest agent-shell-notify-test-focus-and-dedupe ()
  "Only alert away from the relevant buffer, and replace pending completion."
  (let* ((shell (generate-new-buffer " *notify-shell*"))
        (sidebar (generate-new-buffer " *notify-sidebar*"))
        (agent-shell-notify--states (make-hash-table :test #'eq))
        (focused t)
        (sent nil)
        (pending nil)
        (cancelled nil)
        (agent-shell-notify-send-function
         (lambda (title body) (push (list title body) sent)))
        (agent-shell-notify-suppress-event-function
         (lambda (_buffer event)
           (eq (alist-get :event event) 'review-result)))
        (agent-shell-notify-related-buffers-function
         (lambda (_buffer) (list shell sidebar))))
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'frame-focus-state)
                     (lambda (&optional _frame) focused))
                    ((symbol-function 'run-at-time)
                     (lambda (_delay _repeat function &rest args)
                       (setq pending (lambda () (apply function args)))
                       'fake-timer))
                    ((symbol-function 'cancel-timer)
                     (lambda (_timer) (setq cancelled t))))
            (switch-to-buffer shell)
            (agent-shell-notify-send "Agent" "ready" (list shell))
            (should (null sent))
            (setq focused nil)
            (agent-shell-notify-send "Agent" "ready" (list shell))
            (should (= (length sent) 1))
            (setq sent nil focused t)
            (switch-to-buffer sidebar)
            (agent-shell-notify-send "Agent" "ready" (list shell sidebar))
            (should (null sent))
            (setq focused nil)
            (agent-shell-notify--handle
             shell '((:event . review-result)))
            (should (null sent))
            (agent-shell-notify--handle
             shell '((:event . turn-complete)
                     (:data . ((:stop-reason . "end_turn")))))
            (should pending)
            (should (null sent))
            (agent-shell-notify--handle
             shell '((:event . error) (:data . ((:message . "broken")))))
            (should cancelled)
            (should (= (length sent) 1))
            (should (string-match-p "broken" (cadar sent)))
            (funcall pending)
            (should (= (length sent) 1))
            (agent-shell-notify--handle
             shell '((:event . error) (:data . ((:message . "broken")))))
            (should (= (length sent) 1))))
      (kill-buffer shell)
      (kill-buffer sidebar))))

(ert-deftest agent-shell-notify-test-osascript-arguments ()
  "Pass alert text as argv to fixed AppleScript and log a missing sender."
  (let (argv fixed-script log-message)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_name) "/usr/bin/osascript"))
              ((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq argv (plist-get args :command))
                 nil)))
      (agent-shell-notify--macos-send "Agent" "say \"hi\"\nnext")
      (setq fixed-script (nth 2 argv))
      (should (equal (car (last argv)) "say \"hi\"\nnext"))
      (should-not (string-match-p "say hi" fixed-script)))
    (cl-letf (((symbol-function 'executable-find) (lambda (_name) nil))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq log-message (apply #'format format-string args)))))
      (agent-shell-notify--macos-send "Agent" "ready")
      (should (string-match-p "osascript" log-message)))))

(ert-deftest agent-shell-notify-test-consecutive-permissions ()
  "A later approval request alerts even after the first was handled."
  (let* ((shell (generate-new-buffer " *notify-approvals*"))
         (agent-shell-notify--states (make-hash-table :test #'eq))
         (sent nil)
         (agent-shell-notify-send-function
          (lambda (_title body) (push body sent))))
    (unwind-protect
        (cl-letf (((symbol-function 'frame-focus-state)
                   (lambda (&optional _frame) nil))
                  ((symbol-function 'agent-shell-notify--title)
                   (lambda (_shell) "Test")))
          (agent-shell-notify--handle
           shell '((:event . permission-request)
                   (:data . ((:request-id . 1)))))
          (agent-shell-notify--handle
           shell '((:event . permission-request)
                   (:data . ((:request-id . 1)))))
          (should (= (length sent) 1))
          (agent-shell-notify--handle
           shell '((:event . permission-response)
                   (:data . ((:request-id . 1)))))
          (agent-shell-notify--handle
           shell '((:event . permission-request)
                   (:data . ((:request-id . 2)))))
          (should (= (length sent) 2)))
      (kill-buffer shell))))

(provide 'agent-shell-notify-test)
;;; agent-shell-notify-test.el ends here
