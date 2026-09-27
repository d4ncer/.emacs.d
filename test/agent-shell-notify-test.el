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

(provide 'agent-shell-notify-test)
;;; agent-shell-notify-test.el ends here
