;;; agent-shell-drive-test.el --- Tests for driving agent shells -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests against stubbed agent-shell functions.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require '+agent-shell-drive)

(defvar agent-shell-agent-configs)
(defvar agent-shell--state)
(defvar agent-shell-mode-hook)
(defvar agent-shell-session-strategy)

(defmacro +agent-shell-drive-test--with-upstream (&rest body)
  "Run BODY with fresh drive state and stubbed upstream subscriptions.
BODY sees `emit', a function of SHELL and an upstream event alist, and
`unsubscribed', the list of removed tokens."
  (declare (indent 0))
  `(let ((+agent-shell-drive--watchers (make-hash-table :test #'equal))
         (+agent-shell-drive--shells (make-hash-table :test #'eq))
         (handlers nil) (tokens 0) (unsubscribed nil))
     (cl-letf (((symbol-function 'agent-shell-subscribe-to)
                (cl-function
                 (lambda (&key shell-buffer event on-event)
                   (cl-incf tokens)
                   (setq handlers
                         (append handlers
                                 (list (list tokens shell-buffer event
                                             on-event))))
                   tokens)))
               ((symbol-function 'agent-shell-unsubscribe)
                (cl-function
                 (lambda (&key subscription)
                   (setq handlers (assq-delete-all subscription handlers))
                   (push subscription unsubscribed))))
               ((symbol-function 'agent-shell-session-id)
                (lambda (&rest _) "session")))
       (cl-flet ((emit (shell event)
                   (pcase-dolist (`(,_ ,buffer ,kind ,handler) handlers)
                     (when (and (eq buffer shell)
                                (memq kind (list nil (map-elt event :event))))
                       (funcall handler event)))))
         ,@body))))

(defmacro +agent-shell-drive-test--with-shells (names &rest body)
  "Bind each of NAMES to a fresh buffer around BODY."
  (declare (indent 1))
  `(let ,(mapcar (lambda (name) `(,name (generate-new-buffer " *drive*")))
                 names)
     (unwind-protect (progn ,@body)
       (mapc #'kill-buffer (list ,@names)))))

(ert-deftest +agent-shell-drive-test-config ()
  "Resolve identifiers through configs and functions, keeping alists."
  (cl-letf* ((require (symbol-function 'require))
             ((symbol-function 'require)
              (lambda (feature &rest rest)
                (unless (eq feature 'agent-shell)
                  (apply require feature rest))))
             (agent-shell-agent-configs
              (list '((:identifier . codex))
                    (lambda () '((:identifier . claude-code)
                                 (:name . "Claude"))))))
    (should (equal '((:identifier . codex)) (+agent-shell-drive-config 'codex)))
    (should (equal "Claude" (map-elt (+agent-shell-drive-config 'claude-code)
                                     :name)))
    (should (equal '((:identifier . other))
                   (+agent-shell-drive-config '((:identifier . other)))))
    (should-error (+agent-shell-drive-config 'missing) :type 'user-error)
    (let ((agent-shell-agent-configs (lambda () '(((:identifier . late))))))
      (should (equal '((:identifier . late))
                     (+agent-shell-drive-config 'late))))))

(ert-deftest +agent-shell-drive-test-send-refusals ()
  "Submit only to a ready session without an unsent draft."
  (+agent-shell-drive-test--with-shells (shell)
    (let ((session "s") (status 'ready) (sent nil))
      (cl-letf (((symbol-function 'agent-shell-session-id)
                 (lambda (&rest _) session))
                ((symbol-function 'agent-shell-status)
                 (lambda (&rest _) status))
                ((symbol-function 'agent-shell-insert)
                 (cl-function (lambda (&key text &allow-other-keys)
                                (push text sent) (list (cons :buffer shell))))))
        (let ((process (make-pipe-process :name "drive-test" :buffer shell
                                          :noquery t)))
          (unwind-protect
              (with-current-buffer shell
                (insert "Agent> ")
                (set-marker (process-mark process) (point))
                (should (+agent-shell-drive-send shell "one"))
                (setq session nil)
                (should-not (+agent-shell-drive-send shell "no session"))
                (setq session "s" status 'busy)
                (should-not (+agent-shell-drive-send shell "busy"))
                (setq status 'ready)
                (insert "half-typed")
                (should-not (+agent-shell-drive-send shell "draft"))
                (delete-region (process-mark process) (point-max))
                (insert "  \n")
                (should (+agent-shell-drive-send shell "two")))
            (delete-process process)))
        (should (equal '("two" "one") sent))
        (kill-buffer shell)
        (should-not (+agent-shell-drive-send shell "dead"))))))

(ert-deftest +agent-shell-drive-test-watch-idempotent-and-moves ()
  "Keep one subscription per shell and move a key between shells."
  (+agent-shell-drive-test--with-upstream
    (+agent-shell-drive-test--with-shells (one two)
      (let* ((seen nil)
             (tag (lambda (name) (lambda (e) (push (cons name e) seen)))))
        (+agent-shell-drive-watch one 'a 1 (funcall tag 'a1))
        (+agent-shell-drive-watch one 'a 1 (funcall tag 'dup))
        (+agent-shell-drive-watch one 'b 1 (funcall tag 'b1))
        (should (= 1 tokens))
        (emit one '((:event . prompt-ready)))
        (should (equal '(a1 b1) (sort (mapcar #'car seen) #'string<)))
        (should (eq one (plist-get (cdar seen) :shell)))
        (should (equal "session" (plist-get (cdar seen) :session)))
        (setq seen nil)
        (+agent-shell-drive-watch two 'a 1 (funcall tag 'a1))
        (should (= 2 tokens))
        (emit one '((:event . prompt-ready)))
        (should (equal '(b1) (mapcar #'car seen)))
        (setq seen nil)
        (emit two '((:event . prompt-ready)))
        (should (equal '(a1) (mapcar #'car seen)))
        (should-not unsubscribed)
        (+agent-shell-drive-unwatch 'b)
        (should (equal '(1) unsubscribed))))))

(ert-deftest +agent-shell-drive-test-clean-up-drops-watchers ()
  "Deliver clean-up, then drop every watcher of that shell."
  (+agent-shell-drive-test--with-upstream
    (+agent-shell-drive-test--with-shells (one two)
      (let* ((seen nil)
             (record (lambda (e) (push (plist-get e :kind) seen))))
        (+agent-shell-drive-watch one 'a 1 record)
        (+agent-shell-drive-watch one 'b 1 (lambda (_) (error "Boom")))
        (+agent-shell-drive-watch two 'a 2 record)
        (emit one '((:event . clean-up)))
        (should (equal '(clean-up) seen))
        (should (equal '(1) unsubscribed))
        (setq seen nil)
        (+agent-shell-drive-watch one 'a 1 record)
        (should (= 3 tokens))
        (emit one '((:event . prompt-ready)))
        (emit two '((:event . prompt-ready)))
        (should (equal '(prompt-ready prompt-ready) seen))))))

(ert-deftest +agent-shell-drive-test-unwatch-by-owner ()
  "Remove one key or all of an owner's keys, leaving other owners."
  (+agent-shell-drive-test--with-upstream
    (+agent-shell-drive-test--with-shells (one two)
      (let (seen)
        (+agent-shell-drive-watch one 'a 1 (lambda (_) (push 'a1 seen)))
        (+agent-shell-drive-watch two 'a 2 (lambda (_) (push 'a2 seen)))
        (+agent-shell-drive-watch two 'b 1 (lambda (_) (push 'b1 seen)))
        (+agent-shell-drive-unwatch 'a 2)
        (emit two '((:event . prompt-ready)))
        (should (equal '(b1) seen))
        (should-not unsubscribed)
        (+agent-shell-drive-unwatch 'a)
        (should (equal '(1) unsubscribed))
        (+agent-shell-drive-unwatch 'b 1)
        (should (equal '(2 1) unsubscribed))))))

(ert-deftest +agent-shell-drive-test-turn-text ()
  "Collect turn text, preferring the part after the last tool call."
  (+agent-shell-drive-test--with-upstream
    (+agent-shell-drive-test--with-shells (shell)
      (let (events)
        (+agent-shell-drive-watch shell 'a 1 (lambda (e) (push e events)))
        (cl-flet ((chunk (text)
                    (emit shell `((:event . agent-message-chunk)
                                  (:data (:text-chunk . ,text))))))
          (chunk "stale")
          (emit shell '((:event . input-submitted) (:data (:prompt . "go"))))
          (should (eq 'input-submitted (plist-get (car events) :kind)))
          (should (equal "" (+agent-shell-drive-partial-text shell)))
          (chunk "Looking. ")
          (emit shell '((:event . tool-call-update)
                        (:data (:tool-call (:title . "Read file")))))
          (should (eq 'tool-call-update (plist-get (car events) :kind)))
          (should (equal "Looking. " (plist-get (car events) :text)))
          (should (equal "Read file" (plist-get (car events) :title)))
          (emit shell '((:event . tool-call-update)))
          (chunk "Answer")
          (chunk nil)
          (should (= 2 (length events)))
          (should (equal "Looking. Answer"
                         (+agent-shell-drive-partial-text shell)))
          (emit shell '((:event . turn-complete)
                        (:data (:stop-reason . "end_turn"))))
          (should (equal "Answer" (plist-get (car events) :text)))
          (should (equal "end_turn" (plist-get (car events) :stop-reason)))
          (emit shell '((:event . input-submitted)))
          (chunk "Only before")
          (emit shell '((:event . tool-call-update)))
          (chunk " \n")
          (emit shell '((:event . turn-complete)))
          (should (equal "Only before \n" (plist-get (car events) :text)))
          (emit shell '((:event . error) (:data (:message . "bad"))))
          (should (equal "bad" (plist-get (car events) :message))))))))

(ert-deftest +agent-shell-drive-test-start-ignores-caller-locals ()
  "Start with the new session strategy even from a shell with its own."
  (let ((agent-shell-session-strategy 'prompt)
        (agent-shell-mode-hook nil)
        (agent-shell-agent-configs (list '((:identifier . codex))))
        (directory (make-temp-file "drive-start-" t))
        (seen nil) (shells nil))
    (cl-letf* ((require (symbol-function 'require))
               ((symbol-function 'require)
                (lambda (feature &rest rest)
                  (unless (eq feature 'agent-shell)
                    (apply require feature rest))))
               ((symbol-function 'agent-shell-start)
                (cl-function
                 (lambda (&key config)
                   (ignore config)
                   (let ((shell (generate-new-buffer " *drive-start*")))
                     (push shell shells)
                     (with-current-buffer shell
                       (push (list agent-shell-session-strategy
                                   default-directory)
                             seen))
                     shell)))))
      (unwind-protect
          (with-temp-buffer
            (setq-local agent-shell-session-strategy 'prompt)
            (+agent-shell-drive-start :config 'codex :directory directory)
            (should (equal (list (list 'new (file-name-as-directory
                                             (file-truename directory))))
                           seen))
            (should (eq 'prompt agent-shell-session-strategy)))
        (mapc #'kill-buffer shells)
        (delete-directory directory t)))))

;;;; Read-only shells

(defun +agent-shell-drive-test--permission (kind options &optional raw-input)
  "Return a permission request for a KIND tool call offering OPTIONS.
OPTIONS are option kinds; each option id is its kind.  The request records
the chosen id under `:chosen' of the returned list's car."
  (let ((chosen (list nil)))
    (cons chosen
          (list (cons :tool-call (list (cons :kind kind)
                                       (cons :raw-input raw-input)))
                (cons :options (mapcar (lambda (option)
                                         (list (cons :kind option)
                                               (cons :option-id option)))
                                       options))
                (cons :respond (lambda (id) (setcar chosen id) t))))))

(ert-deftest +agent-shell-drive-test-read-only-responder ()
  "Approve reads once, reject the rest, and defer only without a reject."
  (with-temp-buffer
    (let ((all '("allow_always" "allow_once" "reject_once" "reject_always")))
      (pcase-dolist (`(,kind ,options ,expected)
                     `(("read" ,all "allow_once")
                       ("search" ,all "allow_once")
                       ("fetch" ,all "allow_once")
                       ("think" ,all "allow_once")
                       ("execute" ,all "reject_once")
                       ("edit" ,all "reject_once")
                       (nil ,all "reject_once")
                       ("delete" ("allow_once" "reject_always") "reject_always")
                       ("read" ("allow_always" "reject_once") "reject_once")))
        (let ((request (+agent-shell-drive-test--permission kind options)))
          (should (+agent-shell-drive--respond (cdr request)))
          (should (equal expected (caar request)))))
      (let ((request (+agent-shell-drive-test--permission
                      "execute" '("allow_once" "allow_always"))))
        (should-not (+agent-shell-drive--respond (cdr request)))
        (should-not (caar request))))))

(ert-deftest +agent-shell-drive-test-read-only-plan ()
  "Expose a rejected plan on the next turn-complete only."
  (+agent-shell-drive-test--with-upstream
    (+agent-shell-drive-test--with-shells (shell)
      (let (events)
        (+agent-shell-drive-watch shell 'a 1 (lambda (e) (push e events)))
        (with-current-buffer shell
          (+agent-shell-drive--respond
           (cdr (+agent-shell-drive-test--permission
                 "read" '("allow_once" "reject_once") '((plan . "Read only")))))
          (+agent-shell-drive--respond
           (cdr (+agent-shell-drive-test--permission
                 "switch_mode" '("allow_once" "reject_once")
                 '((plan . "# Plan\nStep one"))))))
        (emit shell '((:event . turn-complete)))
        (should (equal "# Plan\nStep one" (plist-get (car events) :plan)))
        (emit shell '((:event . turn-complete)))
        (should-not (plist-member (car events) :plan))))))

(defmacro +agent-shell-drive-test--with-start (&rest body)
  "Run BODY with `agent-shell-start' stubbed to record configs.
BODY sees `started', the configs passed in, newest first, and `shell'."
  (declare (indent 0))
  `(let ((started nil)
         (shell nil)
         (directory (make-temp-file "drive-start-" t))
         (agent-shell-agent-configs
          (list '((:identifier . codex)
                  (:default-session-mode-id . (lambda () "agent")))
                (lambda () '((:identifier . claude-code)))
                '((:identifier . gemini))))
         (+agent-shell-drive-read-only-modes
          '((claude-code . "plan") (codex . "read-only")))
         (agent-shell-mode-hook nil))
     (cl-letf* ((require (symbol-function 'require))
                ((symbol-function 'require)
                 (lambda (feature &rest rest)
                   (unless (eq feature 'agent-shell)
                     (apply require feature rest))))
                ((symbol-function 'agent-shell-start)
                 (cl-function
                  (lambda (&key config)
                    (push config started)
                    (setq shell (generate-new-buffer " *drive-start*"))
                    (with-current-buffer shell
                      (setq-local agent-shell--state
                                  (list (list :session (cons :mode-id nil))))
                      (run-hooks 'agent-shell-mode-hook))
                    shell))))
       (unwind-protect (progn ,@body)
         (when (buffer-live-p shell) (kill-buffer shell))
         (delete-directory directory t)))))

(defmacro +agent-shell-drive-test--with-sandbox (&rest body)
  "Run BODY with `sandbox', a sandbox rooted at `directory'.
Its agent `codex' prepares nothing and runs in mode \"full\"."
  (declare (indent 0))
  `(let ((+agent-shell-sandbox-agents
          '((codex :mode "full" :prepare ignore)))
         (sandbox (+agent-shell-sandbox-create directory)))
     (unwind-protect (progn ,@body)
       (delete-directory (+agent-shell-sandbox-tmp sandbox) t)
       (delete-directory (+agent-shell-sandbox-config sandbox) t))))

(ert-deftest +agent-shell-drive-test-read-only-start ()
  "Force the read-only mode and responder; refuse unknown agents early."
  (+agent-shell-drive-test--with-upstream
    (+agent-shell-drive-test--with-start
      (should-error (+agent-shell-drive-start :config 'missing
                                              :directory directory
                                              :policy 'read-only)
                    :type 'user-error)
      (should-error (+agent-shell-drive-start :config 'gemini
                                              :directory directory
                                              :policy 'read-only)
                    :type 'user-error)
      (should-not started)
      (should (equal '(claude-code codex)
                     (sort (+agent-shell-drive-read-only-agents) #'string<)))
      (let ((member (+agent-shell-drive-start :config 'codex
                                              :directory directory
                                              :policy 'read-only)))
        (should (equal "read-only"
                       (funcall (map-elt (car started)
                                         :default-session-mode-id))))
        (should (= 1 (cl-count :default-session-mode-id (car started)
                               :key #'car)))
        (should (eq #'+agent-shell-drive--respond
                    (buffer-local-value
                     'agent-shell-permission-responder-function member))))
      (let ((plain (+agent-shell-drive-start :config 'gemini
                                             :directory directory)))
        (should-not (map-elt (car started) :default-session-mode-id))
        (should-not (local-variable-p
                     'agent-shell-permission-responder-function plain))
        (kill-buffer plain)))))

(ert-deftest +agent-shell-drive-test-read-only-mode-mismatch ()
  "Kill a shell left outside its read-only mode, explaining why."
  (+agent-shell-drive-test--with-upstream
    (+agent-shell-drive-test--with-start
      (let* ((member (+agent-shell-drive-start :config 'claude-code
                                               :directory directory
                                               :policy 'read-only))
             (events nil))
        (with-current-buffer member
          (add-hook 'kill-buffer-hook
                    (lambda () (emit member '((:event . clean-up))))
                    nil t))
        (+agent-shell-drive-watch member 'a 1
                                  (lambda (e) (push (plist-get e :kind) events)
                                    (when (eq (plist-get e :kind) 'clean-up)
                                      (push (plist-get e :reason) events))))
        (with-current-buffer member
          (setf (map-elt (map-elt agent-shell--state :session) :mode-id)
                "auto"))
        (emit member '((:event . init-finished)))
        (should-not events)
        (should (buffer-live-p member))
        (accept-process-output nil 0.05)
        (should-not (buffer-live-p member))
        (should (equal '("Session mode is auto, not the read-only mode plan"
                         clean-up)
                       events))))))

(ert-deftest +agent-shell-drive-test-read-only-mode-match ()
  "Deliver init-finished when the session runs in the read-only mode."
  (+agent-shell-drive-test--with-upstream
    (+agent-shell-drive-test--with-start
      (let ((member (+agent-shell-drive-start :config 'claude-code
                                              :directory directory
                                              :policy 'read-only))
            (events nil))
        (+agent-shell-drive-watch member 'a 1
                                  (lambda (e) (push (plist-get e :kind) events)))
        (with-current-buffer member
          (setf (map-elt (map-elt agent-shell--state :session) :mode-id)
                "plan"))
        (emit member '((:event . init-finished)))
        (accept-process-output nil 0.05)
        (should (buffer-live-p member))
        (should (equal '(init-finished) events))))))

(ert-deftest +agent-shell-drive-test-read-only-mode-config-option ()
  "Accept a read-only mode applied through the agent's mode config option."
  (+agent-shell-drive-test--with-upstream
    (+agent-shell-drive-test--with-start
      (let ((member (+agent-shell-drive-start :config 'codex
                                              :directory directory
                                              :policy 'read-only))
            (events nil))
        (+agent-shell-drive-watch member 'a 1
                                  (lambda (e) (push (plist-get e :kind) events)))
        (with-current-buffer member
          (setq-local agent-shell--state
                      (list (list :session
                                  (cons :mode-id "agent")
                                  (cons :config-options
                                        (list (list (cons :id "mode")
                                                    (cons :category "mode")
                                                    (cons :current-value
                                                          "read-only"))))))))
        (emit member '((:event . init-finished)))
        (accept-process-output nil 0.05)
        (should (buffer-live-p member))
        (should (equal '(init-finished) events))))))

(ert-deftest +agent-shell-drive-test-policy-hidden ()
  "Leave read-only and sandboxed shells out of lists and project reuse."
  (require 'agent-shell)
  (+agent-shell-drive-test--with-upstream
    (+agent-shell-drive-test--with-start
      (+agent-shell-drive-test--with-sandbox
        (let* ((plain (+agent-shell-drive-start :config 'gemini
                                                :directory directory))
               (member (+agent-shell-drive-start :config 'codex
                                                 :directory directory
                                                 :policy 'read-only))
               (candidate (+agent-shell-drive-start :config 'codex
                                                    :directory directory
                                                    :policy sandbox)))
          (unwind-protect
              (progn
                (dolist (shell (list plain member candidate))
                  (with-current-buffer shell
                    (setq major-mode 'agent-shell-mode)
                    (setq-local shell-maker--config '((:name . "stub")))))
                (should (eq 'read-only (+agent-shell-drive-policy member)))
                (should (eq sandbox (+agent-shell-drive-policy candidate)))
                (should-not (+agent-shell-drive-policy plain))
                (should (memq plain (agent-shell-buffers)))
                (should-not (memq member (agent-shell-buffers)))
                (should-not (memq candidate (agent-shell-buffers)))
                (with-temp-buffer
                  (setq default-directory
                        (file-name-as-directory (file-truename directory)))
                  (should (equal (list plain) (agent-shell-project-buffers)))
                  (should (eq plain (agent-shell-shell-buffer :no-create t)))))
            (kill-buffer plain)
            (kill-buffer member)))))))

;;;; Sandboxed shells

(ert-deftest +agent-shell-drive-test-sandbox-start ()
  "Confine the client, force full access, reject requests, keep no transcript."
  (+agent-shell-drive-test--with-upstream
    (+agent-shell-drive-test--with-start
      (+agent-shell-drive-test--with-sandbox
        (should-error (+agent-shell-drive-start :config 'gemini
                                                :directory directory
                                                :policy sandbox)
                      :type 'user-error)
        (should-not started)
        (let* ((candidate (+agent-shell-drive-start
                           :config '((:identifier . codex)
                                     (:client-maker
                                      . (lambda (_) '((:command . "sh")))))
                           :directory directory :policy sandbox))
               (config (car started))
               (request (+agent-shell-drive-test--permission
                         "read" '("allow_once" "reject_once"))))
          (should (equal "full" (funcall (map-elt config
                                                  :default-session-mode-id))))
          (should (equal "/usr/bin/sandbox-exec"
                         (map-elt (funcall (map-elt config :client-maker) nil)
                                  :command)))
          (with-current-buffer candidate
            (should (funcall agent-shell-permission-responder-function
                             (cdr request)))
            (should (local-variable-p
                     'agent-shell-transcript-file-path-function))
            (should-not agent-shell-transcript-file-path-function))
          (should (equal "reject_once" (caar request))))))))

(ert-deftest +agent-shell-drive-test-sandbox-mode-mismatch ()
  "Kill a sandboxed shell left outside its full-access mode."
  (+agent-shell-drive-test--with-upstream
    (+agent-shell-drive-test--with-start
      (+agent-shell-drive-test--with-sandbox
        (let ((candidate (+agent-shell-drive-start :config 'codex
                                                   :directory directory
                                                   :policy sandbox))
              (events nil))
          (with-current-buffer candidate
            (add-hook 'kill-buffer-hook
                      (lambda () (emit candidate '((:event . clean-up))))
                      nil t)
            (setf (map-elt (map-elt agent-shell--state :session) :mode-id)
                  "default"))
          (+agent-shell-drive-watch candidate 'a 1
                                    (lambda (e)
                                      (push (plist-get e :reason) events)))
          (emit candidate '((:event . init-finished)))
          (accept-process-output nil 0.05)
          (should-not (buffer-live-p candidate))
          (should (equal '("Session mode is default, not the full-access \
mode full")
                         events)))))))

(ert-deftest +agent-shell-drive-test-stop-needs-sandbox ()
  "Refuse to stop a shell that is not sandboxed."
  (+agent-shell-drive-test--with-shells (shell)
    (should-error (+agent-shell-drive-stop shell))))

(provide 'agent-shell-drive-test)
;;; agent-shell-drive-test.el ends here
