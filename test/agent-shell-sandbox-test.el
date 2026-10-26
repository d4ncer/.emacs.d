;;; agent-shell-sandbox-test.el --- Tests for sandboxed agent shells -*- lexical-binding: t; -*-

;;; Commentary:
;; Profile, credential and process tests.  Tests that run sandbox-exec, ps
;; or lsof skip off macOS.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require '+agent-shell-sandbox)

(defmacro +agent-shell-sandbox-test--with (bindings &rest body)
  "Run BODY with each of BINDINGS bound to a fresh temporary folder.
Also bind `sandbox', a sandbox rooted at the first, denying reads of the
second when given."
  (declare (indent 1))
  `(let* (,@(mapcar (lambda (name)
                      `(,name (file-truename (make-temp-file "pa-test-" t))))
                    bindings)
          (sandbox (+agent-shell-sandbox-create
                    ,(car bindings)
                    :deny-reads ,(when (cadr bindings)
                                   `(list ,(cadr bindings))))))
     (unwind-protect (progn ,@body)
       (dolist (directory (list ,@bindings
                                (+agent-shell-sandbox-tmp sandbox)
                                (+agent-shell-sandbox-config sandbox)))
         (delete-directory directory t)))))

(defun +agent-shell-sandbox-test--write (file text)
  "Write TEXT to FILE."
  (with-temp-file file (insert text)))

(defun +agent-shell-sandbox-test--read (file)
  "Return FILE's contents."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(ert-deftest +agent-shell-sandbox-test-create ()
  "Give a sandbox real paths and a short private temp folder."
  (+agent-shell-sandbox-test--with (root denied)
    (let ((tmp (+agent-shell-sandbox-tmp sandbox)))
      (should (equal root (+agent-shell-sandbox-root sandbox)))
      (should (string-match-p "\\`/private/tmp/pa-[0-9a-f]\\{8\\}\\'" tmp))
      (should (<= (length tmp) 30))
      (should (file-directory-p tmp))
      (should (file-directory-p (+agent-shell-sandbox-config sandbox)))
      (should (equal (list denied)
                     (+agent-shell-sandbox-deny-reads sandbox))))))

(ert-deftest +agent-shell-sandbox-test-profile ()
  "Allow writes only to the sandbox's folders and keep reads of others out."
  (+agent-shell-sandbox-test--with (root denied)
    (let ((profile (+agent-shell-sandbox-profile sandbox))
          (quote-path (lambda (path) (concat "\"" path "\""))))
      (should (string-prefix-p
               "(version 1)\n(allow default)\n(deny file-write*)\n" profile))
      (should (string-match-p
               (regexp-quote
                (format "(allow file-write* (subpath %s) (subpath %s) \
(subpath %s) (subpath \"/dev\")"
                        (funcall quote-path root)
                        (funcall quote-path (+agent-shell-sandbox-tmp sandbox))
                        (funcall quote-path
                                 (+agent-shell-sandbox-config sandbox))))
               profile))
      (should (string-match-p
               (regexp-quote
                (format "(deny file-write* (literal %s))"
                        (funcall quote-path (concat root "/.git"))))
               profile))
      (should (string-match-p
               (regexp-quote (format "(deny file-read* (subpath %s))"
                                     (funcall quote-path denied)))
               profile))
      (should (string-match-p
               (regexp-quote "(deny network-outbound (remote unix-socket))")
               profile))
      (should (string-suffix-p "(allow network-outbound (remote unix-socket \
(path-literal \"/private/var/run/mDNSResponder\")))\n" profile)))))

(ert-deftest +agent-shell-sandbox-test-profile-quoting ()
  "Escape quotes and backslashes in profile paths."
  (should (equal "\"/a \\\"b\\\" \\\\c\""
                 (+agent-shell-sandbox--string "/a \"b\" \\c"))))

(ert-deftest +agent-shell-sandbox-test-client-maker ()
  "Run the client under sandbox-exec with the sandbox environment first."
  (+agent-shell-sandbox-test--with (root)
    (let* ((+agent-shell-sandbox-agents
            '((fixture :mode "full"
                       :prepare (lambda (_) '("PA_ENV=sandbox")))))
           (maker (+agent-shell-sandbox-client-maker
                   sandbox 'fixture
                   (lambda (buffer)
                     `((:command . "sh") (:command-params "-c" "true")
                       (:environment-variables "PA_ENV=client")
                       (:context-buffer . ,buffer)))))
           (client (funcall maker 'buffer))
           (profile (expand-file-name "sandbox.sb"
                                      (+agent-shell-sandbox-config sandbox))))
      (should (eq 'fixture (+agent-shell-sandbox-agent sandbox)))
      (should (equal "full" (+agent-shell-sandbox-mode 'fixture)))
      (should (equal "/usr/bin/sandbox-exec" (alist-get :command client)))
      (should (equal (list "-f" profile (executable-find "sh") "-c" "true")
                     (alist-get :command-params client)))
      (should (equal (append '("PA_ENV=sandbox")
                             (+agent-shell-sandbox-environment sandbox)
                             '("PA_ENV=client"))
                     (alist-get :environment-variables client)))
      (should (eq 'buffer (alist-get :context-buffer client)))
      (should (equal (+agent-shell-sandbox-profile sandbox)
                     (+agent-shell-sandbox-test--read profile)))
      (should-error
       (+agent-shell-sandbox-client-maker sandbox 'unknown #'ignore)
       :type 'user-error))))

(defmacro +agent-shell-sandbox-test--with-keychain (status &rest body)
  "Run BODY with a fake `security' that exits with STATUS.
On success it prints a token.  BODY sees `arguments', a function returning
the arguments it was called with."
  (declare (indent 1))
  `(let* ((bin (make-temp-file "pa-bin-" t))
          (log (expand-file-name "arguments" bin))
          (exec-path (cons bin exec-path))
          (arguments (lambda ()
                       (split-string (+agent-shell-sandbox-test--read log)))))
     (+agent-shell-sandbox-test--write
      (expand-file-name "security" bin)
      (format "#!/bin/sh\necho \"$@\" > %s\n%s"
              (shell-quote-argument log)
              (if (eql ,status 0) "echo token-123\n"
                (format "echo 'not found' >&2\nexit %d\n" ,status))))
     (set-file-modes (expand-file-name "security" bin) #o755)
     (unwind-protect (progn ,@body)
       (delete-directory bin t))))

(ert-deftest +agent-shell-sandbox-test-prepare-claude ()
  "Sign Claude Code in from the Keychain token with private folders."
  (+agent-shell-sandbox-test--with (root)
    (let ((+agent-shell-sandbox-claude-token-service "pa-test-service")
          (tmp (+agent-shell-sandbox-tmp sandbox)))
      (+agent-shell-sandbox-test--with-keychain 0
        (should (equal (list "CLAUDE_CODE_OAUTH_TOKEN=token-123"
                             "ANTHROPIC_API_KEY="
                             (concat "CLAUDE_CONFIG_DIR="
                                     (+agent-shell-sandbox-config sandbox))
                             (concat "CLAUDE_CODE_TMPDIR=" tmp))
                       (+agent-shell-sandbox-prepare-claude sandbox)))
        (should (equal '("find-generic-password" "-s" "pa-test-service" "-w")
                       (funcall arguments))))
      (+agent-shell-sandbox-test--with-keychain 44
        (should-error (+agent-shell-sandbox-prepare-claude sandbox)
                      :type 'user-error))
      (should-not (directory-files (+agent-shell-sandbox-config sandbox)
                                   nil "\\`[^.]")))))

(defmacro +agent-shell-sandbox-test--with-codex (&rest body)
  "Run BODY with a prepared Codex sandbox and a fake Codex home.
BODY sees `real' and `copy', the real and copied auth.json, and `env'."
  (declare (indent 0))
  `(+agent-shell-sandbox-test--with (root home)
     (let* ((+agent-shell-sandbox-codex-home home)
            (real (expand-file-name "auth.json" home))
            (copy (expand-file-name "auth.json"
                                    (+agent-shell-sandbox-config sandbox))))
       (+agent-shell-sandbox-test--write real "login-1")
       (+agent-shell-sandbox-test--write (expand-file-name "config.toml" home)
                                         "model = \"x\"")
       (setf (+agent-shell-sandbox-agent sandbox) 'codex)
       (let ((env (+agent-shell-sandbox-prepare-codex sandbox)))
         ,@body))))

(defun +agent-shell-sandbox-test--refresh (file text)
  "Write TEXT to FILE as a later refresh would."
  (+agent-shell-sandbox-test--write file text)
  (set-file-times file (time-add nil 5)))

(ert-deftest +agent-shell-sandbox-test-prepare-codex ()
  "Give Codex private copies of its login and config."
  (+agent-shell-sandbox-test--with-codex
    (should (equal (list (concat "CODEX_HOME="
                                 (+agent-shell-sandbox-config sandbox)))
                   env))
    (should (equal "login-1" (+agent-shell-sandbox-test--read copy)))
    (should (= #o600 (file-modes copy)))
    (should (equal "model = \"x\""
                   (+agent-shell-sandbox-test--read
                    (expand-file-name "config.toml"
                                      (+agent-shell-sandbox-config sandbox)))))
    (delete-file real)
    (should-error (+agent-shell-sandbox-prepare-codex sandbox)
                  :type 'user-error)))

(ert-deftest +agent-shell-sandbox-test-codex-copy-back ()
  "Copy a refreshed login back over an untouched real login."
  (+agent-shell-sandbox-test--with-codex
    (+agent-shell-sandbox-test--refresh copy "login-2")
    (+agent-shell-sandbox-finish sandbox)
    (should (equal "login-2" (+agent-shell-sandbox-test--read real)))
    (should (= #o600 (file-modes real)))))

(ert-deftest +agent-shell-sandbox-test-codex-real-changed ()
  "Keep a real login that changed since the copy, even if the copy is newer."
  (+agent-shell-sandbox-test--with-codex
    (+agent-shell-sandbox-test--write real "login-user")
    (+agent-shell-sandbox-test--refresh copy "login-2")
    (+agent-shell-sandbox-finish sandbox)
    (should (equal "login-user" (+agent-shell-sandbox-test--read real)))))

(ert-deftest +agent-shell-sandbox-test-codex-unrefreshed ()
  "Leave the real login alone when the agent never refreshed it."
  (+agent-shell-sandbox-test--with-codex
    (let ((before (file-attribute-modification-time (file-attributes real))))
      (+agent-shell-sandbox-finish sandbox)
      (should (equal before (file-attribute-modification-time
                             (file-attributes real))))
      (should (equal "login-1" (+agent-shell-sandbox-test--read real))))))

(ert-deftest +agent-shell-sandbox-test-descendants ()
  "Find every descendant of a pid, however deep."
  (should (equal '(2 3 4 5)
                 (sort (+agent-shell-sandbox--descendants
                        1 '((1 . 0) (2 . 1) (3 . 1) (4 . 2) (5 . 4) (6 . 0)
                            (7 . 6)))
                       #'<))))

;;;; macOS processes

(defun +agent-shell-sandbox-test--run (sandbox directory &rest command)
  "Run COMMAND in DIRECTORY under SANDBOX's profile; return its exit status."
  (let ((profile (make-temp-file "pa-profile-" nil ".sb"
                                 (+agent-shell-sandbox-profile sandbox)))
        (default-directory (file-name-as-directory directory)))
    (unwind-protect
        (apply #'call-process "/usr/bin/sandbox-exec" nil nil nil
               "-f" profile command)
      (delete-file profile))))

(defmacro +agent-shell-sandbox-test--on-macos (&rest body)
  "Run BODY only where sandbox-exec exists."
  (declare (indent 0))
  `(progn
     (skip-unless (and (eq system-type 'darwin)
                       (file-executable-p "/usr/bin/sandbox-exec")))
     ,@body))

(defun +agent-shell-sandbox-test--output (sandbox &rest command)
  "Run COMMAND under SANDBOX with its environment; return its output."
  (let ((process-environment (append (+agent-shell-sandbox-environment
                                      sandbox)
                                     process-environment)))
    (with-temp-buffer
      (let ((profile (make-temp-file "pa-profile-" nil ".sb"
                                     (+agent-shell-sandbox-profile sandbox)))
            (default-directory (file-name-as-directory
                                (+agent-shell-sandbox-root sandbox))))
        (unwind-protect
            (apply #'call-process "/usr/bin/sandbox-exec" nil t nil
                   "-f" profile command)
          (delete-file profile)))
      (buffer-string))))

(ert-deftest +agent-shell-sandbox-test-temp-environment ()
  "Point the generic, Go and Java temp folders at the private one.
Keep JAVA_TOOL_OPTIONS the user already had."
  (+agent-shell-sandbox-test--with (root)
    (let ((tmp (+agent-shell-sandbox-tmp sandbox)))
      (let ((process-environment (cons "JAVA_TOOL_OPTIONS=-Xss2m"
                                       process-environment)))
        (should (equal (list (concat "TMPDIR=" tmp) (concat "GOTMPDIR=" tmp)
                             "GOTELEMETRY=off"
                             (concat "JAVA_TOOL_OPTIONS=-Xss2m -Djava.io.tmpdir="
                                     tmp)
                             "GIT_OPTIONAL_LOCKS=0")
                       (+agent-shell-sandbox-environment sandbox))))
      (let ((process-environment (cons "JAVA_TOOL_OPTIONS"
                                       process-environment)))
        (should (member (concat "JAVA_TOOL_OPTIONS=-Djava.io.tmpdir=" tmp)
                        (+agent-shell-sandbox-environment sandbox)))))))

(ert-deftest +agent-shell-sandbox-test-toolchain-temp ()
  "Run Java and Go inside the sandbox with their temp folders private."
  (+agent-shell-sandbox-test--on-macos
    (skip-unless (and (executable-find "java") (executable-find "go")
                      (eql 0 (call-process "/usr/libexec/java_home" nil nil
                                           nil))))
    (+agent-shell-sandbox-test--with (root)
      (let ((tmp (+agent-shell-sandbox-tmp sandbox)))
        (should (string-search
                 (concat "java.io.tmpdir = " tmp "\n")
                 (+agent-shell-sandbox-test--output
                  sandbox (executable-find "java")
                  "-XshowSettings:properties" "-version")))
        (should (equal (concat tmp "\n")
                       (+agent-shell-sandbox-test--output
                        sandbox (executable-find "go") "env" "GOTMPDIR")))))))

(ert-deftest +agent-shell-sandbox-test-user-folders ()
  "Write the per-user temp files toolchains need, and nothing else there."
  (+agent-shell-sandbox-test--on-macos
    (+agent-shell-sandbox-test--with (root)
      (let* ((temp (file-truename (string-trim
                                   (shell-command-to-string
                                    "getconf DARWIN_USER_TEMP_DIR"))))
             (suffix (format "%08x" (random #x100000000)))
             (xcrun (expand-file-name (concat "xcrun_db-pa-test-" suffix) temp))
             (other (expand-file-name (concat "pa-test-other-" suffix) temp)))
        (unwind-protect
            (progn
              (should (string-prefix-p "/private/var/folders/" temp))
              (should (eql 0 (+agent-shell-sandbox-test--run
                              sandbox root "/usr/bin/touch" xcrun)))
              (should (file-exists-p xcrun))
              (should-not (eql 0 (+agent-shell-sandbox-test--run
                                  sandbox root "/usr/bin/touch" other)))
              (should-not (file-exists-p other)))
          (delete-file xcrun)
          (delete-file other))))))

(ert-deftest +agent-shell-sandbox-test-confines-files ()
  "Write inside the root and private folders, but nowhere else."
  (+agent-shell-sandbox-test--on-macos
    (+agent-shell-sandbox-test--with (root denied outside)
      (let ((secret (expand-file-name "secret" denied))
            (run (lambda (&rest command)
                   (apply #'+agent-shell-sandbox-test--run sandbox root
                          command))))
        (+agent-shell-sandbox-test--write secret "hidden")
        (should (eql 0 (funcall run "/usr/bin/touch" "inside")))
        (should (file-exists-p (expand-file-name "inside" root)))
        (should (eql 0 (funcall run "/usr/bin/touch"
                                (expand-file-name
                                 "t" (+agent-shell-sandbox-tmp sandbox)))))
        (should (eql 0 (funcall run "/bin/sh" "-c" "echo x > /dev/null")))
        (should-not (eql 0 (funcall run "/usr/bin/touch"
                                    (expand-file-name "escaped" outside))))
        (should-not (file-exists-p (expand-file-name "escaped" outside)))
        (should-not (eql 0 (funcall run "/bin/cat" secret)))
        (should (eql 0 (call-process "/bin/cat" nil nil nil secret)))
        (should-not (eql 0 (funcall run "/bin/sh" "-c" "echo x > .git")))
        (should-not (file-exists-p (expand-file-name ".git" root)))))))

(ert-deftest +agent-shell-sandbox-test-shared-links ()
  "Read, but never write, a shared folder linked from a denied checkout.
The link resolves into the denied folder, so only the allowance opens it."
  (+agent-shell-sandbox-test--on-macos
    (let* ((root (file-truename (make-temp-file "pa-test-" t)))
           (lead (file-truename (make-temp-file "pa-test-" t)))
           (shared (expand-file-name "elpaca" lead))
           (secret (expand-file-name "secret" lead))
           (link (expand-file-name "elpaca/dep.el" root))
           (closed (+agent-shell-sandbox-create root :deny-reads (list lead)))
           (open (+agent-shell-sandbox-create root :deny-reads (list lead)
                                              :allow-reads (list shared))))
      (unwind-protect
          (progn
            (make-directory shared)
            (+agent-shell-sandbox-test--write
             (expand-file-name "dep.el" shared) "dep")
            (+agent-shell-sandbox-test--write secret "hidden")
            (make-symbolic-link shared (expand-file-name "elpaca" root))
            (should (equal (list shared)
                           (+agent-shell-sandbox-allow-reads open)))
            (should-not (eql 0 (+agent-shell-sandbox-test--run
                                closed root "/bin/cat" link)))
            (should (eql 0 (+agent-shell-sandbox-test--run
                            open root "/bin/cat" link)))
            (should-not (eql 0 (+agent-shell-sandbox-test--run
                                open root "/bin/cat" secret)))
            (should-not (eql 0 (+agent-shell-sandbox-test--run
                                open root "/usr/bin/touch"
                                (expand-file-name "elpaca/new.el" root))))
            (should-not (file-exists-p (expand-file-name "new.el" shared))))
        (dolist (sandbox (list closed open))
          (delete-directory (+agent-shell-sandbox-tmp sandbox) t)
          (delete-directory (+agent-shell-sandbox-config sandbox) t))
        (delete-directory root t)
        (delete-directory lead t)))))

(ert-deftest +agent-shell-sandbox-test-quoted-root ()
  "Confine a root whose path holds quotes, spaces and backslashes."
  (+agent-shell-sandbox-test--on-macos
    (let* ((parent (file-truename (make-temp-file "pa-test-" t)))
           (root (expand-file-name "a \"b\" \\c" parent)))
      (unwind-protect
          (progn
            (make-directory root)
            (let ((sandbox (+agent-shell-sandbox-create root)))
              (unwind-protect
                  (progn
                    (should (eql 0 (+agent-shell-sandbox-test--run
                                    sandbox root "/usr/bin/touch" "inside")))
                    (should (file-exists-p (expand-file-name "inside" root)))
                    (should-not (eql 0 (+agent-shell-sandbox-test--run
                                        sandbox root "/usr/bin/touch"
                                        (expand-file-name "out" parent)))))
                (delete-directory (+agent-shell-sandbox-tmp sandbox) t)
                (delete-directory (+agent-shell-sandbox-config sandbox) t))))
        (delete-directory parent t)))))

(ert-deftest +agent-shell-sandbox-test-blocks-unix-sockets ()
  "Refuse connections to a local unix socket that works unconfined."
  (+agent-shell-sandbox-test--on-macos
    (skip-unless (executable-find "python3"))
    (+agent-shell-sandbox-test--with (root)
      (let* ((socket (format "/tmp/pa-sock-%08x" (random #x100000000)))
             (server (make-network-process :name "pa-test-socket"
                                           :family 'local :server t
                                           :service socket :noquery t))
             (connect (list "python3" "-c" "import socket, sys
socket.socket(socket.AF_UNIX).connect(sys.argv[1])" socket)))
        (unwind-protect
            (progn
              (should (eql 0 (apply #'call-process (car connect) nil nil nil
                                    (cdr connect))))
              (should-not (eql 0 (apply #'+agent-shell-sandbox-test--run
                                        sandbox root connect))))
          (delete-process server)
          (delete-file socket))))))

(defun +agent-shell-sandbox-test--gone-p (process)
  "Wait up to 5 seconds for PROCESS to exit; return whether it did."
  (let ((deadline (+ (float-time) 5)))
    (while (and (process-live-p process) (< (float-time) deadline))
      (accept-process-output nil 0.05)))
  (not (process-live-p process)))

(ert-deftest +agent-shell-sandbox-test-kill-tree ()
  "Kill a process's descendants even in their own session."
  (+agent-shell-sandbox-test--on-macos
    (let* ((default-directory "/")
           (parent (make-process
                    :name "pa-test-tree" :noquery t :connection-type 'pipe
                    :command (list "python3" "-c" "import subprocess, sys
child = subprocess.Popen(['sleep', '30'], start_new_session=True)
print(child.pid, flush=True)
child.wait()")
                    :buffer (generate-new-buffer " *pa-tree*"))))
      (unwind-protect
          (progn
            (while (and (process-live-p parent)
                        (string-empty-p (with-current-buffer
                                            (process-buffer parent)
                                          (buffer-string))))
              (accept-process-output parent 0.05))
            (let ((child (string-to-number
                          (with-current-buffer (process-buffer parent)
                            (buffer-string)))))
              (should (> child 0))
              (+agent-shell-sandbox-kill-tree (process-id parent))
              (should (+agent-shell-sandbox-test--gone-p parent))
              (should-not (process-attributes child))))
        (delete-process parent)
        (kill-buffer (process-buffer parent))))))

(ert-deftest +agent-shell-sandbox-test-sweep ()
  "Kill processes working in the root and report none left."
  (+agent-shell-sandbox-test--on-macos
    (+agent-shell-sandbox-test--with (root)
      (let* ((default-directory (file-name-as-directory root))
             (worker (make-process :name "pa-test-sweep" :noquery t
                                   :command '("sleep" "30")))
             (bystander (let ((default-directory "/"))
                          (make-process :name "pa-test-bystander" :noquery t
                                        :command '("sleep" "30"))))
             (survivors 'pending))
        (unwind-protect
            (progn
              (+agent-shell-sandbox-sweep sandbox
                                          (lambda (pids) (setq survivors pids)))
              (let ((deadline (+ (float-time) 10)))
                (while (and (eq survivors 'pending)
                            (< (float-time) deadline))
                  (accept-process-output nil 0.05)))
              (should-not survivors)
              (should (+agent-shell-sandbox-test--gone-p worker))
              (should (process-live-p bystander)))
          (delete-process worker)
          (delete-process bystander))))))

(provide 'agent-shell-sandbox-test)
;;; agent-shell-sandbox-test.el ends here
