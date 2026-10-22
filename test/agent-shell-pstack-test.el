;;; agent-shell-pstack-test.el --- Tests for pstack sessions -*- lexical-binding: t; -*-

;;; Commentary:
;; Focused tests for prompt decoration, client installation and the guard.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require '+agent-shell-pstack nil t)

(defun +agent-shell-pstack-test--text (text)
  "Return a text block holding TEXT."
  `((type . "text") (text . ,text)))

(defun +agent-shell-pstack-test--request (session &rest texts)
  "Return a prompt request for SESSION with TEXTS as blocks."
  `((:method . "session/prompt")
    (:params . ((sessionId . ,session)
                (prompt . ,(vconcat (mapcar #'+agent-shell-pstack-test--text
                                            texts)))))))

(defun +agent-shell-pstack-test--texts (request)
  "Return the text of each prompt block in REQUEST."
  (mapcar (lambda (block) (map-elt block 'text))
          (map-nested-elt request '(:params prompt))))

(defmacro +agent-shell-pstack-test--shell (identifier &rest body)
  "Run BODY in a fake shell for agent IDENTIFIER with pstack enabled."
  (declare (indent 1))
  `(with-temp-buffer
     (setq-local agent-shell--state
                 (list (cons :agent-config (list (cons :identifier ,identifier)))
                       (cons :client
                             (list (cons :outgoing-request-decorator nil)))))
     (setq-local +agent-shell-pstack-mode t)
     ,@body))

(ert-deftest +agent-shell-pstack-test-prompt ()
  "Prefix the invocation once, then append the reminder."
  (let ((+agent-shell-pstack-reminder "remember"))
    (should (equal (mapcar (lambda (b) (map-elt b 'text))
                           (+agent-shell-pstack--prompt
                            (vector (+agent-shell-pstack-test--text "fix it"))
                            "/pstack:poteto-mode"))
                   '("/pstack:poteto-mode fix it")))
    (let ((resource '((type . "resource_link") (uri . "file:///a"))))
      (should (equal (+agent-shell-pstack--prompt (vector resource) "$poteto-mode")
                     (vector (+agent-shell-pstack-test--text "$poteto-mode")
                             resource))))
    (should (equal (mapcar (lambda (b) (map-elt b 'text))
                           (+agent-shell-pstack--prompt
                            (vector (+agent-shell-pstack-test--text "next"))
                            nil))
                   '("next" "remember")))
    (dolist (command '("/pstack:how explain" "$arena go" "/clear"))
      (should-not (+agent-shell-pstack--prompt
                   (vector (+agent-shell-pstack-test--text command))
                   "/pstack:poteto-mode")))
    (should (+agent-shell-pstack--prompt
             (vector (+agent-shell-pstack-test--text "$5 is the budget"))
             nil))))

(ert-deftest +agent-shell-pstack-test-decorate ()
  "Load poteto-mode once per session and leave other requests alone."
  (let ((+agent-shell-pstack-reminder "remember"))
    (+agent-shell-pstack-test--shell 'codex
      (let ((buffer (current-buffer)))
        (should (equal (+agent-shell-pstack-test--texts
                        (+agent-shell-pstack--decorate
                         buffer (+agent-shell-pstack-test--request "s1" "a")))
                       '("$poteto-mode a")))
        (let ((second (+agent-shell-pstack--decorate
                       buffer (+agent-shell-pstack-test--request "s1" "b"))))
          (should (equal (+agent-shell-pstack-test--texts second)
                         '("b" "remember")))
          (should (= 1 (cl-count :params second :key #'car)))
          (should (= 1 (cl-count 'prompt (map-elt second :params) :key #'car))))
        (should (equal (+agent-shell-pstack-test--texts
                        (+agent-shell-pstack--decorate
                         buffer (+agent-shell-pstack-test--request "s2" "c")))
                       '("$poteto-mode c")))
        (let ((other '((:method . "session/set_mode") (:params))))
          (should (eq (+agent-shell-pstack--decorate buffer other) other)))
        (setq-local +agent-shell-pstack-mode nil)
        (let ((request (+agent-shell-pstack-test--request "s2" "d")))
          (should (eq (+agent-shell-pstack--decorate buffer request)
                      request)))))))

(ert-deftest +agent-shell-pstack-test-mode ()
  "Enable in a shell, add the indicator once, and reset on toggle."
  (let ((mode-line-misc-info nil))
    (+agent-shell-pstack-test--shell 'codex
      (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) t)))
        (setq-local +agent-shell-pstack--primed "s1")
        (+agent-shell-pstack-mode 1)
        (+agent-shell-pstack-mode 1)
        (should +agent-shell-pstack-mode)
        (should-not +agent-shell-pstack--primed)
        (should (equal mode-line-misc-info
                       (list +agent-shell-pstack--mode-line)))
        (should (eq (car +agent-shell-pstack--mode-line)
                    '+agent-shell-pstack-mode)))
      (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) nil)))
        (setq-local +agent-shell-pstack-mode nil)
        (should-error (+agent-shell-pstack-mode 1) :type 'user-error)
        (should-not +agent-shell-pstack-mode)))))

(ert-deftest +agent-shell-pstack-test-unknown-agent ()
  "Refuse agents without a configured invocation."
  (+agent-shell-pstack-test--shell 'gemini
    (should-error (+agent-shell-pstack--invocation) :type 'user-error)))

(ert-deftest +agent-shell-pstack-test-install ()
  "Chain the client's existing decorator exactly once."
  (let ((+agent-shell-pstack-reminder "remember")
        (calls 0))
    (+agent-shell-pstack-test--shell 'claude-code
      (let ((client (map-elt agent-shell--state :client)))
        (setcdr (assq :outgoing-request-decorator client)
                (lambda (request) (cl-incf calls) request))
        (+agent-shell-pstack--install)
        (+agent-shell-pstack--install)
        (let ((decorated (funcall (map-elt client :outgoing-request-decorator)
                                  (+agent-shell-pstack-test--request "s" "go"))))
          (should (= calls 1))
          (should (equal (+agent-shell-pstack-test--texts decorated)
                         '("/pstack:poteto-mode go"))))))))

(ert-deftest +agent-shell-pstack-test-irreversible ()
  "Match destructive commands and ignore routine ones."
  (dolist (command '("git push --force origin main"
                     "git push -f"
                     "git push origin +main"
                     "git push origin :old-branch"
                     "git push --force-with-lease"
                     "cd repo && git push --delete origin x"
                     "git reset --hard HEAD~1"
                     "git clean -fd"
                     "git branch -D topic"
                     "gh pr merge 12 --squash"
                     "terraform apply -auto-approve"
                     "kubectl delete pod web"
                     "psql -c 'DROP TABLE users'"))
    (should (+agent-shell-pstack-irreversible-p
             `((:kind . "execute") (:raw-input . ((command . ,command)))))))
  (dolist (command '("git push origin main"
                     "git push -u origin topic"
                     "git status && git diff"
                     "git reset HEAD file"
                     "git branch -d merged"
                     "gh pr view 12"
                     "kubectl get pods"
                     "make deploy-docs-preview"))
    (should-not (+agent-shell-pstack-irreversible-p
                 `((:kind . "execute") (:raw-input . ((command . ,command)))))))
  (should (+agent-shell-pstack-irreversible-p
           '((:title . "`git push -f`"))))
  (should-not (+agent-shell-pstack-irreversible-p
               '((:kind . "edit")
                 (:raw-input . ((new_string . "git push --force")))))))

(ert-deftest +agent-shell-pstack-test-guard ()
  "Wrap the responder, hold irreversible commands, and restore on disable."
  (let* ((approved nil)
         (previous (lambda (_permission) (setq approved t) t))
         (agent-shell-permission-responder-function previous)
         (+agent-shell-pstack--next-responder nil)
         (+agent-shell-pstack-guard-mode nil)
         (safe '((:tool-call . ((:kind . "execute")
                                (:raw-input . ((command . "ls")))))))
         (risky '((:tool-call . ((:kind . "execute")
                                 (:raw-input . ((command . "git push -f"))))))))
    (+agent-shell-pstack-guard-mode 1)
    (+agent-shell-pstack-guard-mode 1)
    (should (eq +agent-shell-pstack--next-responder previous))
    (should (funcall agent-shell-permission-responder-function safe))
    (should approved)
    (setq approved nil)
    (should-not (let ((inhibit-message t))
                  (funcall agent-shell-permission-responder-function risky)))
    (should-not approved)
    (+agent-shell-pstack-guard-mode -1)
    (should (eq agent-shell-permission-responder-function previous))))

(provide 'agent-shell-pstack-test)
;;; agent-shell-pstack-test.el ends here
