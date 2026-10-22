;;; agent-shell-drive-test.el --- Tests for driving agent shells -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests against stubbed agent-shell functions.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require '+agent-shell-drive)

(defvar agent-shell-agent-configs)

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
                 (lambda (&key shell-buffer on-event &allow-other-keys)
                   (push (cons shell-buffer on-event) handlers)
                   (cl-incf tokens))))
               ((symbol-function 'agent-shell-unsubscribe)
                (cl-function
                 (lambda (&key subscription)
                   (push subscription unsubscribed))))
               ((symbol-function 'agent-shell-session-id)
                (lambda (&rest _) "session")))
       (cl-flet ((emit (shell event)
                   (funcall (alist-get shell handlers) event)))
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
          (emit shell '((:event . tool-call-update)))
          (chunk "Answer")
          (chunk nil)
          (should (= 1 (length events)))
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

(provide 'agent-shell-drive-test)
;;; agent-shell-drive-test.el ends here
