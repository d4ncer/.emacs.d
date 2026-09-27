;;; agent-shell-review-acp-test.el --- Direct review transport tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise the ACP boundary without starting an agent process.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'map)
(require 'agent-shell-review-acp nil t)

(defmacro agent-shell-review-acp-test--fake (&rest body)
  "Run BODY with a synchronous ACP peer and captured traffic."
  (declare (indent 0))
  `(let ((requests nil)
         (responses nil)
         (notifications nil)
         (incoming nil)
         (errors nil)
         (shutdowns 0)
         (session-response
          '((sessionId . "review-1")
            (modes . ((currentModeId . "default")
                      (availableModes . [((id . "default") (name . "Default"))
                                         ((id . "read-only") (name . "Read Only"))])))))
         (mode-error nil))
     (cl-letf (((symbol-function 'acp-make-client)
                (lambda (&rest _args) 'fake-client))
               ((symbol-function 'acp-subscribe-to-notifications)
                (lambda (&rest args)
                  (setq notifications (plist-get args :on-notification))))
               ((symbol-function 'acp-subscribe-to-requests)
                (lambda (&rest args)
                  (setq incoming (plist-get args :on-request))))
               ((symbol-function 'acp-subscribe-to-errors)
                (lambda (&rest args)
                  (setq errors (plist-get args :on-error))))
               ((symbol-function 'acp-send-request)
                (lambda (&rest args)
                  (let* ((request (plist-get args :request))
                         (method (map-elt request :method)))
                    (push request requests)
                    (if (and mode-error (equal method "session/set_mode"))
                        (funcall (plist-get args :on-failure) "mode refused")
                      (funcall (plist-get args :on-success)
                               (pcase method
                                 ("session/new" session-response)
                                 ("session/prompt" '((stopReason . "end_turn")))
                                 (_ nil))))
                    nil)))
               ((symbol-function 'acp-send-response)
                (lambda (&rest args)
                  (push (plist-get args :response) responses)))
               ((symbol-function 'acp-shutdown)
                (lambda (&rest _args) (cl-incf shutdowns))))
       ,@body)))

(ert-deftest agent-shell-review-acp-test-order-and-completion ()
  "Select read-only before the first prompt and report turn completion."
  (agent-shell-review-acp-test--fake
    (let* ((events nil)
           (transport (agent-shell-review-acp-create
                       temporary-file-directory nil
                       (lambda (event) (push event events)))))
      (should-not requests)
      (agent-shell-review-acp-start transport)
      (should (equal (mapcar (lambda (request) (map-elt request :method))
                             (reverse requests))
                     '("initialize" "session/new" "session/set_mode")))
      (should (eq (map-nested-elt (car (last requests))
                                  '(:params clientCapabilities fs writeTextFile))
                  :false))
      (should (eq (plist-get (car events) :type) 'ready))
      (should (agent-shell-review-acp-send transport "Review now"))
      (let* ((request (car requests))
             (block (aref (map-nested-elt request '(:params prompt)) 0)))
        (should (equal (map-elt request :method) "session/prompt"))
        (should (equal (map-elt block 'type) "text"))
        (should (equal (map-elt block 'text) "Review now")))
      (should (equal (plist-get (car events) :stop-reason) "end_turn"))
      (should (eq (plist-get (car events) :type) 'complete)))))

(ert-deftest agent-shell-review-acp-test-requires-read-only ()
  "Missing or rejected read-only mode must stop before prompting."
  (agent-shell-review-acp-test--fake
    (let* ((events nil)
           (transport (agent-shell-review-acp-create
                       temporary-file-directory nil
                       (lambda (event) (push event events)))))
      (setq session-response '((sessionId . "review-1")))
      (agent-shell-review-acp-start transport)
      (should (eq (plist-get (car events) :type) 'error))
      (should-not (member "session/prompt"
                          (mapcar (lambda (r) (map-elt r :method)) requests)))))
  (agent-shell-review-acp-test--fake
    (let* ((events nil)
           (transport (agent-shell-review-acp-create
                       temporary-file-directory nil
                       (lambda (event) (push event events)))))
      (setq mode-error t)
      (agent-shell-review-acp-start transport)
      (should (eq (plist-get (car events) :type) 'error))
      (should-not (member "session/prompt"
                          (mapcar (lambda (r) (map-elt r :method)) requests))))))

(ert-deftest agent-shell-review-acp-test-config-options ()
  "Use advertised config options for model and read-only mode."
  (agent-shell-review-acp-test--fake
    (let* ((agent-shell-review-acp-model-id "gpt-6-sol")
           (events nil)
           (transport (agent-shell-review-acp-create
                       temporary-file-directory nil
                       (lambda (event) (push event events)))))
      (setq session-response
            '((sessionId . "review-1")
              (configOptions
               . [((id . "model") (category . "model") (type . "select")
                   (options . [((value . "gpt-6-sol") (name . "Sol"))]))
                  ((id . "session_mode") (category . "mode") (type . "select")
                   (options . [((value . "read-only") (name . "Read Only"))]))])))
      (agent-shell-review-acp-start transport)
      (should (equal (mapcar (lambda (r) (map-elt r :method)) (reverse requests))
                     '("initialize" "session/new" "session/set_config_option"
                       "session/set_config_option")))
      (should (equal (map-nested-elt (nth 1 requests)
                                     '(:params configId))
                     "model"))
      (should (equal (map-nested-elt (car requests)
                                     '(:params configId))
                     "session_mode"))
      (should (eq (plist-get (car events) :type) 'ready)))))

(ert-deftest agent-shell-review-acp-test-request-boundary ()
  "Deny writes and symlink escapes while serving allowed reads."
  (let* ((root (make-temp-file "review-acp-root-" t))
         (outside (make-temp-file "review-acp-outside-" t))
         (inside (expand-file-name "code.el" root))
         (explicit (expand-file-name "spec.md" outside))
         (escape (expand-file-name "escape.md" root)))
    (unwind-protect
        (progn
          (with-temp-file inside (insert "inside"))
          (with-temp-file explicit (insert "external criteria"))
          (make-symbolic-link explicit escape)
          (agent-shell-review-acp-test--fake
            (let ((transport (agent-shell-review-acp-create root explicit #'ignore)))
              (agent-shell-review-acp-start transport)
              (funcall incoming `((id . 1) (method . "fs/read_text_file")
                                  (params . ((path . ,inside)))))
              (should (equal (map-nested-elt (car responses) '(:result content))
                             "inside"))
              (funcall incoming `((id . 2) (method . "fs/read_text_file")
                                  (params . ((path . ,explicit)))))
              (should (equal (map-nested-elt (car responses) '(:result content))
                             "external criteria"))
              (funcall incoming `((id . 3) (method . "fs/read_text_file")
                                  (params . ((path . ,escape)))))
              (should (map-elt (car responses) :error))
              (funcall incoming '((id . 4) (method . "fs/write_text_file")
                                  (params . ((path . "code.el") (content . "bad")))))
              (should (map-elt (car responses) :error))
              (should (equal (with-temp-buffer
                               (insert-file-contents inside) (buffer-string))
                             "inside"))
              (funcall incoming '((id . 5) (method . "session/request_permission")
                                  (params . ((sessionId . "review-1")))))
              (should (equal (map-nested-elt (car responses)
                                              '(:result outcome outcome))
                             "cancelled")))))
      (delete-directory root t)
      (delete-directory outside t))))

(ert-deftest agent-shell-review-acp-test-stale-notification-and-close ()
  "Ignore other sessions and preserve diagnostics after closing."
  (agent-shell-review-acp-test--fake
    (let* ((events nil)
           (transport (agent-shell-review-acp-create
                       temporary-file-directory nil
                       (lambda (event) (push event events)))))
      (agent-shell-review-acp-start transport)
      (setq events nil)
      (funcall notifications
               '((method . "session/update")
                 (params . ((sessionId . "other")
                            (update . ((sessionUpdate . "agent_message_chunk")
                                       (content . ((type . "text") (text . "wrong")))))))))
      (funcall notifications
               '((method . "session/update")
                 (params . ((sessionId . "review-1")
                            (update . ((sessionUpdate . "agent_message_chunk")
                                       (content . ((type . "image")))))))))
      (should-not events)
      (funcall notifications
               '((method . "session/update")
                 (params . ((sessionId . "review-1")
                            (update . ((sessionUpdate . "agent_message_chunk")
                                       (content . ((type . "text") (text . "answer")))))))))
      (should (equal (plist-get (car events) :text) "answer"))
      (funcall errors "peer exited")
      (should (eq (plist-get (car events) :type) 'error))
      (agent-shell-review-acp-close transport)
      (agent-shell-review-acp-close transport)
      (should (= shutdowns 1))
      (should (string-match-p "answer" (agent-shell-review-acp-diagnostics transport)))
      (should (string-match-p "peer exited"
                              (agent-shell-review-acp-diagnostics transport))))))

(provide 'agent-shell-review-acp-test)
;;; agent-shell-review-acp-test.el ends here
