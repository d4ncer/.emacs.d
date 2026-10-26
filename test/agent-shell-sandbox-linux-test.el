;;; agent-shell-sandbox-linux-test.el --- Tests for Linux sandboxes -*- lexical-binding: t; -*-

;;; Commentary:
;; Mount, read gate and credential tests run anywhere with the bwrap
;; backend selected.  Tests that start the launcher skip unless this is
;; GNU/Linux with bwrap, unshare and slirp4netns.  Credentials are fixtures.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require '+agent-shell-sandbox)
(require '+agent-shell-drive)
(require '+agent-shell-arena-git)
(require '+agent-shell-pstack-panel)

(defun +agent-shell-sandbox-linux-test-available-p ()
  "Return whether this host can start Linux sandboxes."
  (and (eq system-type 'gnu/linux)
       (executable-find "bwrap") (executable-find "unshare")
       (executable-find "slirp4netns") (executable-find "python3")))

(defmacro +agent-shell-sandbox-linux-test--with (bindings arguments
                                                          &rest body)
  "Run BODY with the bwrap backend and BINDINGS bound to fresh folders.
Also bind `sandbox', a sandbox rooted at the first, made with the keyword
arguments ARGUMENTS evaluates to, which may use BINDINGS."
  (declare (indent 2))
  `(let* ((+agent-shell-sandbox-backend 'bwrap)
          ,@(mapcar (lambda (name)
                      `(,name (file-truename (make-temp-file "pa-test-" t))))
                    bindings)
          (sandbox (apply #'+agent-shell-sandbox-create ,(car bindings)
                          ,arguments)))
     (unwind-protect (progn ,@body)
       (dolist (directory (list ,@bindings (+agent-shell-sandbox-tmp sandbox)
                                (+agent-shell-sandbox-config sandbox)))
         (call-process "chmod" nil nil nil "-R" "u+rwX" directory)
         (delete-directory directory t)))))

(defun +agent-shell-sandbox-linux-test--mount (mounts dest)
  "Return the last of MOUNTS at DEST."
  (car (last (seq-filter (lambda (mount) (equal (cadr mount) dest)) mounts))))

(ert-deftest +agent-shell-sandbox-linux-test-mounts ()
  "Show the system read-only and the sandbox's own folders writable.
ROOT/.git stays read-only, a missing allowance is left out, and every
mount comes after the mounts that hold it."
  (+agent-shell-sandbox-linux-test--with (root shared out)
      (list :allow-reads (list shared (expand-file-name "gone" shared))
            :allow-writes (list out))
    (with-temp-file (expand-file-name ".git" root) (insert "gitdir: x\n"))
    (let ((mounts (+agent-shell-sandbox-linux-mounts sandbox)))
      (should (equal '(ro "/usr" "/usr")
                     (+agent-shell-sandbox-linux-test--mount mounts "/usr")))
      (should (equal '(tmpfs "/tmp")
                     (+agent-shell-sandbox-linux-test--mount mounts "/tmp")))
      (dolist (path (list root (+agent-shell-sandbox-tmp sandbox)
                          (+agent-shell-sandbox-config sandbox) out))
        (should (equal (list 'rw path path)
                       (+agent-shell-sandbox-linux-test--mount mounts path))))
      (should (equal (list 'ro shared shared)
                     (+agent-shell-sandbox-linux-test--mount mounts shared)))
      (should (equal (list 'ro (concat root "/.git") (concat root "/.git"))
                     (+agent-shell-sandbox-linux-test--mount
                      mounts (concat root "/.git"))))
      (should-not (+agent-shell-sandbox-linux-test--mount
                   mounts (expand-file-name "gone" shared)))
      (cl-loop for (mount . later) on mounts
               do (dolist (other later)
                    (should-not (string-prefix-p
                                 (file-name-as-directory (cadr other))
                                 (cadr mount))))))))

(ert-deftest +agent-shell-sandbox-linux-test-arguments ()
  "Give bwrap the namespaces, the user's ids and the mounts in order."
  (let ((arguments (+agent-shell-sandbox-linux-arguments
                    '((ro "/usr" "/usr") (symlink "/bin" "usr/bin")
                      (tmpfs "/tmp") (rw "/tmp/pa-1" "/tmp/pa-1"))
                    '("agent" "--flag"))))
    (should (equal (list "--unshare-user" "--uid" (number-to-string (user-uid))
                         "--gid" (number-to-string (group-gid))
                         "--unshare-ipc" "--unshare-pid" "--die-with-parent"
                         "--new-session"
                         "--ro-bind" "/usr" "/usr" "--symlink" "usr/bin" "/bin"
                         "--tmpfs" "/tmp" "--bind" "/tmp/pa-1" "/tmp/pa-1"
                         "--" "agent" "--flag")
                   arguments))))

(ert-deftest +agent-shell-sandbox-linux-test-refusals ()
  "Refuse patterns, trace mode and missing sockets on Linux."
  (let ((+agent-shell-sandbox-backend 'bwrap)
        (root (make-temp-file "pa-test-" t)))
    (unwind-protect
        (progn
          (should-error (+agent-shell-sandbox-create
                         root :allow-reads '((regexp . "^/tmp/x")))
                        :type 'user-error)
          (should (+agent-shell-sandbox-allowance-error '(regexp . "^/tmp/x")))
          (should-error (+agent-shell-sandbox-create root :trace t)
                        :type 'user-error)
          (should-error (+agent-shell-sandbox-create
                         root :allow-connects
                         (list (expand-file-name "missing.sock" root)))
                        :type 'user-error))
      (delete-directory root t))))

(ert-deftest +agent-shell-sandbox-linux-test-file-gate ()
  "Serve ACP reads only of paths the namespace shows, writes only inside.
The runner's /proc and a file outside the mounts stay refused."
  (+agent-shell-sandbox-linux-test--with (root hidden) nil
    (let ((secret (expand-file-name "secret" hidden))
          (inside (expand-file-name "inside.txt" root)))
      (with-temp-file secret (insert "hidden"))
      (with-temp-file inside (insert "inside"))
      (should (+agent-shell-drive--file-allowed-p sandbox nil inside))
      (should (+agent-shell-drive--file-allowed-p sandbox t inside))
      (should (+agent-shell-drive--file-allowed-p sandbox nil "/usr/bin/env"))
      (should-not (+agent-shell-drive--file-allowed-p
                   sandbox nil (format "/proc/%d/environ" (emacs-pid))))
      (should-not (+agent-shell-drive--file-allowed-p sandbox nil secret))
      (should-not (+agent-shell-drive--file-allowed-p sandbox nil "/run/x"))
      (should-not (+agent-shell-drive--file-allowed-p sandbox t "/usr/x"))
      (should-not (+agent-shell-drive--file-allowed-p
                   sandbox t (concat root "/.git"))))))

(defun +agent-shell-sandbox-linux-test--jwt (claims)
  "Return an unsigned JSON web token carrying CLAIMS, an alist."
  (concat (base64url-encode-string "{\"alg\":\"none\"}" t) "."
          (base64url-encode-string (json-serialize claims) t) ".sig"))

(defun +agent-shell-sandbox-linux-test--write-logins (home expires)
  "Write fixture logins in HOME expiring at EXPIRES, seconds since 1970.
Claude Code's is HOME/credentials.json and Codex's HOME/auth.json."
  (with-temp-file (expand-file-name "credentials.json" home)
    (insert (json-serialize
             (list (list 'claudeAiOauth
                         (cons 'accessToken "fixture-claude-access")
                         (cons 'refreshToken "fixture-claude-refresh")
                         (cons 'expiresAt (* 1000 expires)))))))
  (with-temp-file (expand-file-name "auth.json" home)
    (insert (json-serialize
             (list (cons 'auth_mode "chatgpt") (cons 'OPENAI_API_KEY :null)
                   (list 'tokens (cons 'id_token "fixture-id")
                         (cons 'access_token
                               (+agent-shell-sandbox-linux-test--jwt
                                (list (cons 'exp expires))))
                         (cons 'refresh_token "fixture-codex-refresh")
                         (cons 'account_id "fixture-account"))
                   (cons 'last_refresh "2026-10-26T00:00:00Z"))))))

(defmacro +agent-shell-sandbox-linux-test--with-logins (expires &rest body)
  "Run BODY with fixture Claude Code and Codex logins expiring at EXPIRES.
EXPIRES is seconds since the epoch.  BODY sees `home', the Codex home."
  (declare (indent 1))
  `(let* ((home (make-temp-file "pa-logins-" t))
          (+agent-shell-sandbox-codex-home home)
          (+agent-shell-sandbox-claude-credentials
           (expand-file-name "credentials.json" home)))
     (unwind-protect
         (progn (+agent-shell-sandbox-linux-test--write-logins home ,expires)
                ,@body)
       (delete-directory home t))))

(ert-deftest +agent-shell-sandbox-linux-test-claude-login ()
  "Sign Claude Code in with the access token alone while it outlasts the
deadline, and refuse it once it does not."
  (let ((now (truncate (float-time))))
    (+agent-shell-sandbox-linux-test--with-logins (+ now 3600)
      (+agent-shell-sandbox-linux-test--with (root) (list :deadline (+ now 1800))
        (let ((env (+agent-shell-sandbox-prepare-claude sandbox)))
          (should (member "CLAUDE_CODE_OAUTH_TOKEN=fixture-claude-access" env))
          (should (member (concat "CLAUDE_CONFIG_DIR="
                                  (+agent-shell-sandbox-config sandbox))
                          env))
          (should-not (seq-some (lambda (entry)
                                  (string-search "fixture-claude-refresh"
                                                 entry))
                                env))))
      (+agent-shell-sandbox-linux-test--with (root) (list :deadline (+ now 7200))
        (should-error (+agent-shell-sandbox-prepare-claude sandbox)
                      :type 'user-error)))))

(ert-deftest +agent-shell-sandbox-linux-test-codex-login ()
  "Give Codex a private login without a refresh token, and copy nothing
back.  Refuse a token that expires before the deadline."
  (let ((now (truncate (float-time))))
    (+agent-shell-sandbox-linux-test--with-logins (+ now 3600)
      (let ((real (expand-file-name "auth.json" home)))
        (+agent-shell-sandbox-linux-test--with (root)
            (list :deadline (+ now 1800))
          (let* ((env (+agent-shell-sandbox-prepare-codex sandbox))
                 (copy (expand-file-name "auth.json"
                                         (+agent-shell-sandbox-config sandbox)))
                 (login (with-temp-buffer
                          (insert-file-contents copy)
                          (json-parse-buffer :object-type 'alist)))
                 (tokens (alist-get 'tokens login))
                 (before (with-temp-buffer (insert-file-contents real)
                                           (buffer-string))))
            (should (equal (list (concat "CODEX_HOME="
                                         (+agent-shell-sandbox-config sandbox)))
                           env))
            (should (= #o600 (file-modes copy)))
            (should (equal "chatgpt" (alist-get 'auth_mode login)))
            (should (equal "" (alist-get 'refresh_token tokens)))
            (should (equal "fixture-id" (alist-get 'id_token tokens)))
            (should (equal "fixture-account" (alist-get 'account_id tokens)))
            (should (equal (+agent-shell-sandbox-linux-test--jwt
                            `((exp . ,(+ now 3600))))
                           (alist-get 'access_token tokens)))
            (with-temp-file copy (insert "{\"refreshed\": true}"))
            (set-file-times copy (time-add nil 5))
            (setf (+agent-shell-sandbox-agent sandbox) 'codex)
            (+agent-shell-sandbox-finish sandbox)
            (should (equal before (with-temp-buffer
                                    (insert-file-contents real)
                                    (buffer-string)))))))
      (+agent-shell-sandbox-linux-test--with (root) (list :deadline (+ now 7200))
        (should-error (+agent-shell-sandbox-prepare-codex sandbox)
                      :type 'user-error)
        (should-not (file-exists-p
                     (expand-file-name
                      "auth.json" (+agent-shell-sandbox-config sandbox))))))))

;;;; On a Linux host

(defmacro +agent-shell-sandbox-linux-test--on-linux (&rest body)
  "Run BODY when this host can start Linux sandboxes, else skip."
  (declare (indent 0))
  `(progn (skip-unless (+agent-shell-sandbox-linux-test-available-p))
          ,@body))

(defun +agent-shell-sandbox-linux-test--client (sandbox script)
  "Return the launch client running sh SCRIPT confined to SANDBOX.
Its agent's prepared environment sets PA_ENV=sandbox and the client's own
sets PA_ENV=client and PA_TEST_SECRET."
  (let ((+agent-shell-sandbox-agents
         '((fixture :mode "full" :prepare (lambda (_) '("PA_ENV=sandbox"))))))
    (funcall (+agent-shell-sandbox-client-maker
              sandbox 'fixture
              (lambda (_buffer)
                `((:command . "/bin/sh") (:command-params "-c" ,script)
                  (:environment-variables "PA_ENV=client"
                                          "PA_TEST_SECRET=client"))))
             nil)))

(defun +agent-shell-sandbox-linux-test--run (sandbox script)
  "Run sh SCRIPT confined to SANDBOX; return (STATUS . OUTPUT).
This Emacs's own environment holds PA_TEST_SECRET, which must not reach
the sandbox."
  (let* ((client (+agent-shell-sandbox-linux-test--client sandbox script))
         (default-directory (file-name-as-directory
                             (+agent-shell-sandbox-root sandbox)))
         (process-environment
          (append (alist-get :environment-variables client)
                  (cons "PA_TEST_SECRET=emacs" process-environment))))
    (with-temp-buffer
      (let ((status (apply #'call-process (alist-get :command client) nil t nil
                           (alist-get :command-params client))))
        (cons status (buffer-string))))))

(defun +agent-shell-sandbox-linux-test--lines (output)
  "Return OUTPUT's lines of the form KEY=VALUE as an alist of strings."
  (delq nil (mapcar (lambda (line)
                      (when (string-match "\\`\\([^=]+\\)=\\(.*\\)\\'" line)
                        (cons (match-string 1 line) (match-string 2 line))))
                    (split-string output "\n" t))))

(ert-deftest +agent-shell-sandbox-linux-test-confines-files ()
  "Write only in the root, and see no home, Docker socket or hidden file."
  (+agent-shell-sandbox-linux-test--on-linux
    (+agent-shell-sandbox-linux-test--with (root hidden) nil
      (let* ((secret (expand-file-name "secret" hidden))
             (home (expand-file-name "~"))
             (result
              (progn
                (with-temp-file secret (insert "hidden"))
                (with-temp-file (expand-file-name ".git" root)
                  (insert "gitdir: x\n"))
                (+agent-shell-sandbox-linux-test--run
                 sandbox
                 (format "check() { if %s; then echo \"$1=yes\"; else \
echo \"$1=no\"; fi; }
check inside 'touch inside.txt'
check outside 'touch %s/outside.txt'
check usr 'touch /usr/pa-test 2>/dev/null'
check ssh 'test -e %s/.ssh'
check home 'test -e %s'
check docker 'test -e /run/docker.sock'
check secret 'cat %s >/dev/null 2>&1'
check nix 'test -e /nix/var/nix/daemon-socket/socket'
check git 'echo gitdir: elsewhere > .git'"
                         "eval \"$2\" 2>/dev/null"
                         hidden home home secret))))
             (lines (+agent-shell-sandbox-linux-test--lines (cdr result))))
        (should (eql 0 (car result)))
        (should (equal "yes" (alist-get "inside" lines nil nil #'equal)))
        (should (file-exists-p (expand-file-name "inside.txt" root)))
        (dolist (key '("outside" "usr" "ssh" "home" "docker" "secret" "nix"
                       "git"))
          (should (equal (cons key "no") (assoc key lines))))
        (should-not (file-exists-p (expand-file-name "outside.txt" hidden)))))))

(ert-deftest +agent-shell-sandbox-linux-test-environment ()
  "Pass only the allowlisted environment, the prepared entries first."
  (+agent-shell-sandbox-linux-test--on-linux
    (+agent-shell-sandbox-linux-test--with (root) nil
      (let* ((result (+agent-shell-sandbox-linux-test--run sandbox "env"))
             (lines (+agent-shell-sandbox-linux-test--lines (cdr result)))
             (allowed '("PATH" "HOME" "LANG" "TERM" "TMPDIR" "GOTMPDIR"
                        "GOCACHE" "GOTELEMETRY" "JAVA_TOOL_OPTIONS"
                        "GIT_OPTIONAL_LOCKS" "PA_ENV" "PWD" "SHLVL" "_")))
        (should (eql 0 (car result)))
        (should (equal "sandbox" (alist-get "PA_ENV" lines nil nil #'equal)))
        (should (equal (+agent-shell-sandbox-tmp sandbox)
                       (alist-get "TMPDIR" lines nil nil #'equal)))
        (should (equal (concat (+agent-shell-sandbox-tmp sandbox) "/home")
                       (alist-get "HOME" lines nil nil #'equal)))
        (should-not (assoc "PA_TEST_SECRET" lines))
        (should (equal nil (seq-difference (mapcar #'car lines) allowed)))))))

(defun +agent-shell-sandbox-linux-test--listener (address)
  "Start a host listener on ADDRESS, a Python socket address expression.
Return the process; it prints its port once listening."
  (make-process
   :name "pa-test-listener" :noquery t :connection-type 'pipe
   :buffer (generate-new-buffer " *pa-listener*")
   :command (list "python3" "-c" (format "import socket, sys
family = socket.AF_UNIX if isinstance(%s, str) else socket.AF_INET
server = socket.socket(family)
server.bind(%s)
server.listen()
print(server.getsockname()[1] if family == socket.AF_INET else 'up', flush=True)
while True:
    server.accept()[0].close()" address address))))

(defun +agent-shell-sandbox-linux-test--output (process)
  "Wait for PROCESS's first line of output and return it."
  (let ((deadline (+ (float-time) 10)))
    (while (and (< (float-time) deadline)
                (not (string-search "\n" (with-current-buffer
                                              (process-buffer process)
                                            (buffer-string)))))
      (accept-process-output process 0.05)))
  (string-trim (with-current-buffer (process-buffer process) (buffer-string))))

(defun +agent-shell-sandbox-linux-test--connect (address)
  "Return a Python snippet printing whether it can connect to ADDRESS."
  (format "python3 -c \"
import socket
family = socket.AF_UNIX if isinstance(%s, str) else socket.AF_INET
client = socket.socket(family)
client.settimeout(3)
try:
    client.connect(%s)
    print('connected')
except OSError as error:
    print('refused', error.errno)\"" address address))

(ert-deftest +agent-shell-sandbox-linux-test-network ()
  "Reach HTTPS out, but neither host loopback nor host abstract sockets.
Both listeners take connections from this host."
  (+agent-shell-sandbox-linux-test--on-linux
    (+agent-shell-sandbox-linux-test--with (root) nil
      (let* ((name (format "pa-test-%08x" (random #x100000000)))
             (abstract (format "'\\0%s'" name))
             (tcp (+agent-shell-sandbox-linux-test--listener "('127.0.0.1', 0)"))
             (unix (+agent-shell-sandbox-linux-test--listener abstract)))
        (unwind-protect
            (let* ((port (string-to-number
                          (+agent-shell-sandbox-linux-test--output tcp)))
                   (loopback (format "('10.0.2.2', %d)" port))
                   (local (format "('127.0.0.1', %d)" port)))
              (should (> port 0))
              (should (equal "up" (+agent-shell-sandbox-linux-test--output unix)))
              (dolist (address (list local abstract))
                (should (equal "connected\n"
                               (shell-command-to-string
                                (+agent-shell-sandbox-linux-test--connect
                                 address)))))
              (let* ((result
                      (+agent-shell-sandbox-linux-test--run
                       sandbox
                       (concat
                        "printf 'loopback='; "
                        (+agent-shell-sandbox-linux-test--connect loopback)
                        "; printf 'local='; "
                        (+agent-shell-sandbox-linux-test--connect local)
                        "; printf 'abstract='; "
                        (+agent-shell-sandbox-linux-test--connect abstract)
                        (format "; printf 'listed='; grep -c %s /proc/net/unix"
                                name)
                        "; printf 'https='; python3 -c \"
import urllib.request
print(urllib.request.urlopen('https://github.com', timeout=15).status)\"")))
                     (lines (+agent-shell-sandbox-linux-test--lines (cdr result))))
                (dolist (key '("loopback" "local" "abstract"))
                  (should (string-prefix-p
                           "refused" (alist-get key lines nil nil #'equal))))
                (should (equal "0" (alist-get "listed" lines nil nil #'equal)))
                (should (equal "200" (alist-get "https" lines nil nil #'equal)))))
          (dolist (process (list tcp unix))
            (delete-process process)
            (kill-buffer (process-buffer process))))))))

(ert-deftest +agent-shell-sandbox-linux-test-terminate ()
  "Stop a detached writer in its own session along with the launcher.
The writer would write a file two seconds after the stop."
  (+agent-shell-sandbox-linux-test--on-linux
    (+agent-shell-sandbox-linux-test--with (root) nil
      (let* ((client (+agent-shell-sandbox-linux-test--client
                      sandbox "setsid sh -c 'sleep 2; touch late.txt' \
</dev/null >/dev/null 2>&1 & echo started; exec sleep 60"))
             (default-directory (file-name-as-directory root))
             (process-environment (append (alist-get :environment-variables
                                                     client)
                                          process-environment))
             (process (make-process
                       :name "pa-test-launcher" :noquery t
                       :connection-type 'pipe
                       :buffer (generate-new-buffer " *pa-launcher*")
                       :command (cons (alist-get :command client)
                                      (alist-get :command-params client))))
             (outcome 'pending))
        (unwind-protect
            (progn
              (should (equal "started"
                             (+agent-shell-sandbox-linux-test--output process)))
              (+agent-shell-sandbox-terminate
               process (lambda (survivors) (setq outcome survivors)))
              (let ((deadline (+ (float-time) 15)))
                (while (and (eq outcome 'pending) (< (float-time) deadline))
                  (accept-process-output nil 0.05)))
              (should-not outcome)
              (should-not (process-live-p process))
              (sleep-for 3)
              (should-not (file-exists-p (expand-file-name "late.txt" root)))
              (should-not
               (seq-some (lambda (pid)
                           (string-prefix-p
                            root (or (file-symlink-p
                                      (format "/proc/%s/cwd" pid))
                                     "")))
                         (directory-files "/proc" nil "\\`[0-9]+\\'"))))
          (delete-process process)
          (kill-buffer (process-buffer process)))))))

(ert-deftest +agent-shell-sandbox-linux-test-clone ()
  "Clone a private path with GNU cp, copying where reflinks fail.
Links stay links."
  (+agent-shell-sandbox-linux-test--on-linux
    (+agent-shell-sandbox-linux-test--with (toplevel worktree) nil
      (let ((source (expand-file-name "build" toplevel))
            (outcome 'pending))
        (make-directory source)
        (with-temp-file (expand-file-name "out.txt" source) (insert "built"))
        (make-symbolic-link "out.txt" (expand-file-name "link" source))
        (+agent-shell-arena-git--clone
         toplevel worktree '("build")
         (lambda (message) (setq outcome message))
         (lambda () (setq outcome 'done)))
        (let ((deadline (+ (float-time) 10)))
          (while (and (eq outcome 'pending) (< (float-time) deadline))
            (accept-process-output nil 0.05)))
        (should (eq 'done outcome))
        (should (equal "built" (with-temp-buffer
                                 (insert-file-contents
                                  (expand-file-name "build/out.txt" worktree))
                                 (buffer-string))))
        (should (equal "out.txt" (file-symlink-p
                                  (expand-file-name "build/link" worktree))))))))

(defmacro +agent-shell-sandbox-linux-test--without-lsof (&rest body)
  "Run BODY as on a Linux host, which has no /usr/sbin/lsof."
  (declare (indent 0))
  `(let ((call (symbol-function 'call-process)))
     (cl-letf (((symbol-function 'call-process)
                (lambda (program &rest arguments)
                  (if (equal program "/usr/sbin/lsof")
                      (signal 'file-missing
                              (list "Searching for program"
                                    "No such file or directory" program))
                    (apply call program arguments)))))
       ,@body)))

(defun +agent-shell-sandbox-linux-test--halt (sandbox root)
  "Halt a settled candidate in SANDBOX at ROOT whose adapter never started.
Return the candidate once its capture leaves `stopping', or after 10 s."
  (let* ((member (+agent-shell-pstack-panel--make-member
                  :role "opus" :agent 'claude-code :kind 'candidate
                  :state 'error :sandbox sandbox))
         (panel (+agent-shell-pstack-panel--make
                 :id "p1" :shape 'arena :phase 'finished :members (list member)
                 :arena (+agent-shell-arena-git--manifest
                         :roles (list (+agent-shell-arena-git--role
                                       :name "opus" :worktree root)))))
         (deadline (+ (float-time) 10)))
    (+agent-shell-pstack-panel--halt panel member)
    (while (and (memq (+agent-shell-pstack-panel-member-capture member)
                      '(nil stopping))
                (< (float-time) deadline))
      (accept-process-output nil 0.05))
    member))

(ert-deftest +agent-shell-sandbox-linux-test-halt-without-adapter ()
  "Stop a Linux candidate whose adapter is gone without macOS's tools.
Its PID namespace ended with its adapter, so its capture goes on."
  (+agent-shell-sandbox-linux-test--with (root) nil
    (+agent-shell-sandbox-linux-test--without-lsof
      (let ((member (+agent-shell-sandbox-linux-test--halt sandbox root)))
        (should (eq 'stopped (+agent-shell-pstack-panel-member-capture
                              member)))))))

(ert-deftest +agent-shell-sandbox-linux-test-halt-signals ()
  "Fail the capture of a candidate whose stop signals, rather than stall."
  (+agent-shell-sandbox-linux-test--with (root) nil
    (let ((+agent-shell-sandbox-backend 'sandbox-exec))
      (+agent-shell-sandbox-linux-test--without-lsof
        (let ((member (+agent-shell-sandbox-linux-test--halt sandbox root)))
          (should (eq 'failed (+agent-shell-pstack-panel-member-capture
                               member)))
          (should (string-match-p
                   "lsof" (plist-get (+agent-shell-pstack-panel-member-result
                                      member)
                                     :error))))))))

(provide 'agent-shell-sandbox-linux-test)
;;; agent-shell-sandbox-linux-test.el ends here
