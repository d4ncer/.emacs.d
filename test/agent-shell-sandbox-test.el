;;; agent-shell-sandbox-test.el --- Tests for sandboxed agent shells -*- lexical-binding: t; -*-

;;; Commentary:
;; Profile, credential and process tests.  Tests that run sandbox-exec, ps
;; or lsof skip off macOS.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require '+agent-shell-sandbox)
(require 'server)

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
         (+agent-shell-sandbox-test-unlock directory)
         (delete-directory directory t)))))

(defun +agent-shell-sandbox-test-read-only-tree (folder)
  "Make in FOLDER a tree like Bazel's outputs and return its top.
The top and the folder in it are mode 0555 and the file in that 0444,
so deleting the tree needs owner write permission back first."
  (let* ((top (expand-file-name "locked" folder))
         (inner (expand-file-name "inner" top))
         (file (expand-file-name "file" inner)))
    (make-directory inner t)
    (with-temp-file file (insert "output"))
    (set-file-modes file #o444)
    (set-file-modes inner #o555)
    (set-file-modes top #o555)
    top))

(defun +agent-shell-sandbox-test-immutable (file)
  "Create FILE flagged immutable, so no deletion can remove it."
  (with-temp-file file (insert "immutable"))
  (should (eql 0 (call-process "chflags" nil nil nil "uchg" file))))

(defun +agent-shell-sandbox-test-unlock (folder)
  "Make everything under FOLDER deletable again, if it exists."
  (when (file-exists-p folder)
    (call-process "chflags" nil nil nil "-R" "nouchg" folder)
    (call-process "chmod" nil nil nil "-R" "u+rwX" folder)))

(defun +agent-shell-sandbox-test-wait (predicate)
  "Wait up to 30 seconds for PREDICATE, then assert it."
  (let ((deadline (+ (float-time) 30)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.02)))
  (should (funcall predicate)))

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
      (should (equal denied
                     (car (+agent-shell-sandbox-deny-reads sandbox)))))))

(ert-deftest +agent-shell-sandbox-test-profile ()
  "Allow writes only to the sandbox's folders and keep reads of others out.
Every refusal carries the sandbox's tag."
  (+agent-shell-sandbox-test--with (root denied)
    (let* ((profile (+agent-shell-sandbox-profile sandbox))
           (quote-path (lambda (path) (concat "\"" path "\"")))
           (tag (format "(with message %s)"
                        (funcall quote-path
                                 (+agent-shell-sandbox-tag sandbox)))))
      (should (string-match-p "\\`pa-sandbox-[0-9a-f]\\{8\\}\\'"
                              (+agent-shell-sandbox-tag sandbox)))
      (should (string-prefix-p
               (format "(version 1)\n(allow default)\n(deny file-write* %s)\n"
                       tag)
               profile))
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
                (format "(deny file-write* %s (literal %s))" tag
                        (funcall quote-path (concat root "/.git"))))
               profile))
      (should (string-match-p
               (regexp-quote (format "(deny file-read* %s (subpath %s) " tag
                                     (funcall quote-path denied)))
               profile))
      (should (string-match-p
               (regexp-quote (format "(deny network-outbound %s (remote \
unix-socket))" tag))
               profile))
      (should (string-suffix-p "(allow network-outbound (remote unix-socket \
(path-literal \"/private/var/run/mDNSResponder\")))\n" profile)))))

(ert-deftest +agent-shell-sandbox-test-pattern-validation ()
  "Accept patterns anchored below a folder under the root, and reject any
that could match / or the whole filesystem."
  (dolist (re '("^/private/tmp/diff\\."
                "^/private/tmp/diff\\.[A-Za-z0-9]+$"
                "^/private/var/folders/x/T/xcode_locator-[0-9]+\\.o$"
                "^/private/tmp/(a|b)/"
                "^/private/tmp/[[:alpha:]|]+$"))
    (should-not (+agent-shell-sandbox-allowance-error (cons 'regexp re))))
  (dolist (value '((regexp . "") (regexp . ".*") (regexp . "^/")
                   (regexp . "^/.*") (regexp . "^/private")
                   (regexp . "^/private.*/") (regexp . "^/privat?e/tmp/")
                   (regexp . "^/[a-z]+/tmp/") (regexp . "/private/tmp/x")
                   (regexp . "^/private/tmp/x|.*")
                   (regexp . "^/private/tmp/a\"b") (regexp . "^/tmp/diff\\.")
                   (regexp . 3) (regexp "^/private/tmp/x") "relative" 42))
    (should (stringp (+agent-shell-sandbox-allowance-error value))))
  (should-not (+agent-shell-sandbox-allowance-error "/private/tmp")))

(ert-deftest +agent-shell-sandbox-test-pattern-allowances ()
  "Take patterns from lists and functions, render them as regex filters,
and reject a bad pattern a function returns."
  (let* ((root (file-truename (make-temp-file "pa-test-" t)))
         (pattern '(regexp . "^/private/tmp/diff\\."))
         (sandbox (+agent-shell-sandbox-create
                   root
                   :allow-writes (list pattern
                                       (lambda (_)
                                         '(regexp . "^/private/tmp/w-")))
                   :allow-reads (list (lambda (_)
                                        (list "/private/tmp"
                                              '(regexp . "^/private/var/r-")))))))
    (unwind-protect
        (let ((profile (+agent-shell-sandbox-profile sandbox)))
          (should (equal (list pattern '(regexp . "^/private/tmp/w-"))
                         (+agent-shell-sandbox-allow-writes sandbox)))
          (should (equal '("/private/tmp" (regexp . "^/private/var/r-"))
                         (+agent-shell-sandbox-allow-reads sandbox)))
          (should (string-search
                   "(regex #\"^/private/tmp/diff\\.\") (regex #\"^/private/tmp/w-\")"
                   profile))
          (should (string-search
                   (format "(allow file-read* (subpath %S) (subpath %S) \
(subpath \"/private/tmp\") (regex #\"^/private/var/r-\"))"
                           (+agent-shell-sandbox-tmp sandbox)
                           (+agent-shell-sandbox-config sandbox))
                   profile))
          (should-error (+agent-shell-sandbox-create
                         root :allow-writes (list (lambda (_)
                                                    '(regexp . "^/.*"))))
                        :type 'user-error))
      (delete-directory (+agent-shell-sandbox-tmp sandbox) t)
      (delete-directory (+agent-shell-sandbox-config sandbox) t)
      (delete-directory root t))))

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

;;;; Host logins

(defun +agent-shell-sandbox-test--jwt (expires)
  "Return an unsigned JSON web token whose exp claim is EXPIRES."
  (concat (base64url-encode-string "{\"alg\":\"none\"}" t) "."
          (base64url-encode-string (format "{\"exp\":%d}" expires) t) ".sig"))

(defun +agent-shell-sandbox-test--claude-json (expires &optional access)
  "Return a Claude Code host login expiring at EXPIRES, Unix seconds."
  (json-serialize
   `((claudeAiOauth (accessToken . ,(or access "fixture-claude-access"))
                    (refreshToken . "fixture-claude-refresh")
                    (expiresAt . ,(* 1000 expires))))))

(defun +agent-shell-sandbox-test--codex-json (expires)
  "Return a Codex host login whose access token expires at EXPIRES."
  (json-serialize
   `((auth_mode . "chatgpt") (OPENAI_API_KEY . :null)
     (tokens (id_token . "fixture-id")
             (access_token . ,(+agent-shell-sandbox-test--jwt expires))
             (refresh_token . "fixture-codex-refresh")
             (account_id . "fixture-account"))
     (last_refresh . "2026-10-26T00:00:00Z"))))

(defconst +agent-shell-sandbox-test--fake-claude
  "#!/bin/sh
printf '%s %s %s %s\\n' \"$(pwd -P)\" \"${CLAUDE_CODE_OAUTH_TOKEN-unset}\" \
\"${ANTHROPIC_API_KEY-unset}\" \"$CODEX_HOME\" >> \"$FAKE_HOST/runs\"
case \"$FAKE_CLAUDE\" in
  fail) echo 'fixture-cli-secret'; exit 1 ;;
  noop) exit 0 ;;
esac
sleep \"${FAKE_DELAY:-0}\"
expires=$(( $(date +%s) + 28800 ))
printf '{\"claudeAiOauth\":{\"accessToken\":\"fixture-new\",\
\"refreshToken\":\"fixture-r2\",\"expiresAt\":%s000}}' \"$expires\" \
> \"$FAKE_HOST/credentials.tmp\"
mv \"$FAKE_HOST/credentials.tmp\" \"$FAKE_HOST/credentials.json\"
touch \"$FAKE_HOST/refreshed\"
"
  "A claude that refreshes its fixture login for 8 hours, fails or does
nothing, as FAKE_CLAUDE says, logging where and how it ran.")

(defconst +agent-shell-sandbox-test--fake-codex
  "#!/bin/sh
printf '%s\\n' \"$(pwd -P)\" >> \"$FAKE_HOST/runs\"
[ \"$FAKE_CODEX\" = noop ] && exit 0
payload=$(printf '{\"exp\":%d}' $(( $(date +%s) + 800000 )) | base64 \
| tr -d '=\\n' | tr '+/' '-_')
printf '{\"tokens\":{\"access_token\":\"e30.%s.s\",\"id_token\":\"i\",\
\"account_id\":\"a\"}}' \"$payload\" > \"$CODEX_HOME/auth.json\"
[ -n \"$FAKE_SHORTEN_CLAUDE\" ] && printf '{\"claudeAiOauth\":\
{\"accessToken\":\"x\",\"expiresAt\":1000}}' > \"$FAKE_HOST/credentials.json\"
exit 0
"
  "A codex that refreshes its fixture login, unless FAKE_CODEX is noop.")

(defconst +agent-shell-sandbox-test--fake-security
  "#!/bin/sh
printf '%s\\n' \"$@\" > \"$FAKE_HOST/security-arguments\"
[ -f \"$FAKE_HOST/keychain\" ] || { echo 'item could not be found' >&2; \
exit 44; }
cat \"$FAKE_HOST/keychain\"
"
  "A security that prints the fixture Keychain item, if there is one.")

(defmacro +agent-shell-sandbox-test--with-host (&rest body)
  "Run BODY against a fixture host: its logins, CLIs, Keychain and locks.
BODY sees `host', its folder, and `runs', which returns the lines the
fake CLIs logged.  Claude Code's login is host/credentials.json, its
Keychain item host/keychain, and Codex's home host/codex/."
  (declare (indent 0))
  `(let* ((host (file-name-as-directory
                 (file-truename (make-temp-file "pa-host-" t))))
          (bin (expand-file-name "bin/" host))
          (+agent-shell-sandbox-claude-credentials
           (expand-file-name "credentials.json" host))
          (+agent-shell-sandbox-codex-home (expand-file-name "codex/" host))
          (+agent-shell-sandbox--login-locks (expand-file-name "locks/" host))
          (+agent-shell-sandbox--security (expand-file-name "security" bin))
          (+agent-shell-sandbox--login-tick 0.05)
          (+agent-shell-sandbox-refresh-timeout 20)
          (exec-path (cons bin exec-path))
          (process-environment
           (append (list (concat "PATH=" bin path-separator (getenv "PATH"))
                         (concat "FAKE_HOST=" host))
                   process-environment))
          (runs (lambda ()
                  (let ((file (expand-file-name "runs" host)))
                    (and (file-exists-p file)
                         (split-string (+agent-shell-sandbox-test--read file)
                                       "\n" t))))))
     (ignore runs)
     (make-directory bin t)
     (make-directory +agent-shell-sandbox-codex-home)
     (pcase-dolist (`(,name . ,script)
                    `(("claude" . ,+agent-shell-sandbox-test--fake-claude)
                      ("codex" . ,+agent-shell-sandbox-test--fake-codex)
                      ("security" . ,+agent-shell-sandbox-test--fake-security)))
       (+agent-shell-sandbox-test--write (expand-file-name name bin) script)
       (set-file-modes (expand-file-name name bin) #o755))
     (unwind-protect (progn ,@body)
       (delete-directory host t))))

(defun +agent-shell-sandbox-test--logins (host claude codex)
  "Write HOST's Claude Code and Codex logins expiring at CLAUDE and CODEX."
  (when claude
    (+agent-shell-sandbox-test--write
     (expand-file-name "credentials.json" host)
     (+agent-shell-sandbox-test--claude-json claude)))
  (when codex
    (+agent-shell-sandbox-test--write
     (expand-file-name "codex/auth.json" host)
     (+agent-shell-sandbox-test--codex-json codex))))

(defmacro +agent-shell-sandbox-test--with-deadline (deadline &rest body)
  "Run BODY with `sandbox', a fresh sandbox whose deadline is DEADLINE."
  (declare (indent 1))
  `(let* ((root (file-truename (make-temp-file "pa-test-" t)))
          (sandbox (+agent-shell-sandbox-create root :deadline ,deadline)))
     (unwind-protect (progn ,@body)
       (dolist (directory (list root (+agent-shell-sandbox-tmp sandbox)
                                (+agent-shell-sandbox-config sandbox)))
         (delete-directory directory t)))))

(defun +agent-shell-sandbox-test--secret-free (text)
  "Assert TEXT holds no fixture refresh credential, token or CLI output."
  (dolist (secret '("fixture-claude-refresh" "fixture-codex-refresh"
                    "fixture-claude-access" "fixture-cli-secret"
                    "not-json-secret"))
    (should-not (string-search secret text))))

(ert-deftest +agent-shell-sandbox-test-prepare-claude ()
  "Sign Claude Code in with the host login's access token alone.
macOS reads it from the login Keychain, Linux from its credentials file."
  (+agent-shell-sandbox-test--with-host
    (let ((later (+ (truncate (float-time)) 3600)))
      (+agent-shell-sandbox-test--write
       (expand-file-name "keychain" host)
       (+agent-shell-sandbox-test--claude-json later))
      (+agent-shell-sandbox-test--logins host later nil)
      (dolist (backend '(sandbox-exec bwrap))
        (let ((+agent-shell-sandbox-backend backend))
          (+agent-shell-sandbox-test--with-deadline (- later 60)
            (let ((env (+agent-shell-sandbox-prepare-claude sandbox)))
              (should (equal (list (concat "CLAUDE_CODE_OAUTH_TOKEN="
                                           "fixture-claude-access")
                                   "ANTHROPIC_API_KEY="
                                   (concat "CLAUDE_CONFIG_DIR="
                                           (+agent-shell-sandbox-config
                                            sandbox))
                                   (concat "CLAUDE_CODE_TMPDIR="
                                           (+agent-shell-sandbox-tmp sandbox)))
                             env))
              (should-not (directory-files (+agent-shell-sandbox-config sandbox)
                                           nil "\\`[^.]")))))))
    (should (equal (list "find-generic-password" "-s" "Claude Code-credentials"
                         "-a" (user-login-name) "-w")
                   (split-string (+agent-shell-sandbox-test--read
                                  (expand-file-name "security-arguments" host))
                                 "\n" t)))))

(ert-deftest +agent-shell-sandbox-test-prepare-claude-invalid ()
  "Refuse a missing, malformed, empty or short host login, saying nothing
of its contents, and never fall back to the other backend's store."
  (+agent-shell-sandbox-test--with-host
    (let* ((now (truncate (float-time)))
           (keychain (expand-file-name "keychain" host))
           (refuse
            (lambda (deadline)
              (+agent-shell-sandbox-test--with-deadline deadline
                (let ((err (should-error
                            (+agent-shell-sandbox-prepare-claude sandbox)
                            :type 'user-error)))
                  (+agent-shell-sandbox-test--secret-free
                   (error-message-string err)))))))
      (+agent-shell-sandbox-test--logins host (+ now 3600) nil)
      (let ((+agent-shell-sandbox-backend 'sandbox-exec))
        (funcall refuse nil)
        (dolist (text (list "{\"claudeAiOauth\": not-json-secret"
                            "[1, 2]"
                            (+agent-shell-sandbox-test--claude-json
                             (+ now 3600) "")
                            "{\"claudeAiOauth\":{\"accessToken\":\"x\"}}"
                            "{\"claudeAiOauth\":{\"accessToken\":\"x\",\
\"expiresAt\":\"soon\"}}"))
          (+agent-shell-sandbox-test--write keychain text)
          (funcall refuse nil))
        (+agent-shell-sandbox-test--write
         keychain (+agent-shell-sandbox-test--claude-json (+ now 3600)))
        (funcall refuse (+ now 3600)))
      (let ((+agent-shell-sandbox-backend 'bwrap))
        (delete-file +agent-shell-sandbox-claude-credentials)
        (funcall refuse nil)))))

(ert-deftest +agent-shell-sandbox-test-prepare-codex ()
  "Write Codex an allowlisted login without a refresh token, mode 0600,
and its settings without MCP servers, on either backend.  The token must
strictly outlast the deadline."
  (+agent-shell-sandbox-test--with-host
    (let ((expires (+ (truncate (float-time)) 3600)))
      (+agent-shell-sandbox-test--logins host nil expires)
      (+agent-shell-sandbox-test--write
       (expand-file-name "config.toml" +agent-shell-sandbox-codex-home)
       "model = \"o3\"\n[mcp_servers.docs]\ncommand = \"npx\"\n")
      (dolist (backend '(sandbox-exec bwrap))
        (let ((+agent-shell-sandbox-backend backend))
          (+agent-shell-sandbox-test--with-deadline (1- expires)
            (let* ((env (+agent-shell-sandbox-prepare-codex sandbox))
                   (config (+agent-shell-sandbox-config sandbox))
                   (auth (expand-file-name "auth.json" config))
                   (login (with-temp-buffer
                            (insert-file-contents auth)
                            (json-parse-buffer :object-type 'alist)))
                   (tokens (alist-get 'tokens login)))
              (should (equal (list (concat "CODEX_HOME=" config)
                                   "OPENAI_API_KEY=" "CODEX_API_KEY=")
                             env))
              (should (= #o600 (file-modes auth)))
              (should (equal '(auth_mode OPENAI_API_KEY tokens last_refresh)
                             (mapcar #'car login)))
              (should (eq :null (alist-get 'OPENAI_API_KEY login)))
              (should (equal `((id_token . "fixture-id")
                               (access_token
                                . ,(+agent-shell-sandbox-test--jwt expires))
                               (refresh_token . "")
                               (account_id . "fixture-account"))
                             tokens))
              (should (equal "model = \"o3\"\n"
                             (+agent-shell-sandbox-test--read
                              (expand-file-name "config.toml" config))))))
          (dolist (deadline (list expires (1+ expires)))
            (+agent-shell-sandbox-test--with-deadline deadline
              (should-error (+agent-shell-sandbox-prepare-codex sandbox)
                            :type 'user-error)
              (should-not (directory-files (+agent-shell-sandbox-config sandbox)
                                           nil "\\`[^.]"))))))
      (+agent-shell-sandbox-test--write
       (expand-file-name "codex/auth.json" host)
       "{\"OPENAI_API_KEY\":\"sk-fixture\"}")
      (+agent-shell-sandbox-test--with-deadline nil
        (should-error (+agent-shell-sandbox-prepare-codex sandbox)
                      :type 'user-error)))))

(ert-deftest +agent-shell-sandbox-test-codex-host-unchanged ()
  "What a Codex sandbox does to its login never reaches the host's, which
may itself change meanwhile."
  (+agent-shell-sandbox-test--with-host
    (let* ((expires (+ (truncate (float-time)) 3600))
           (real (expand-file-name "codex/auth.json" host)))
      (+agent-shell-sandbox-test--logins host nil expires)
      (dolist (replace '(nil t))
        (let* ((root (file-truename (make-temp-file "pa-test-" t)))
               (sandbox (+agent-shell-sandbox-create root))
               (copy (expand-file-name "auth.json"
                                       (+agent-shell-sandbox-config sandbox)))
               (outcome 'pending))
          (+agent-shell-sandbox-prepare-codex sandbox)
          (when replace
            (+agent-shell-sandbox-test--write real "{\"replaced\": true}"))
          (let ((before (+agent-shell-sandbox-test--read real)))
            (+agent-shell-sandbox-test--write copy "{\"refreshed\": true}")
            (set-file-times copy (time-add nil 5))
            (+agent-shell-sandbox-discard sandbox
                                          (lambda (error) (setq outcome error)))
            (+agent-shell-sandbox-test-wait
             (lambda () (not (eq 'pending outcome))))
            (should-not outcome)
            (should (equal before (+agent-shell-sandbox-test--read real))))
          (delete-directory root t))))))

(defconst +agent-shell-sandbox-test--toml
  (concat
   "# Codex settings\n"
   "model = \"o3\"\n"
   "mcp_servers.dotted.command = \"x\"\n"
   "\"mcp_servers\" = { inline = { command = \"y\" } }\n"
   "mcp_servers = [\n"
   "  \"multi\",\n"
   "]\n"
   "matrix = [\n"
   "  [\"mcp_servers\"],\n"
   "]\n"
   "note = \"\"\"\n"
   "[mcp_servers.inside_a_string]\n"
   "\"\"\"\n"
   "mcp_servers_note = 'kept'\n"
   "\n"
   "[mcp_servers.alpha]\r\n"
   "command = \"npx\"\r\n"
   "args = [\"-y\", \"[mcp_servers.not_a_header]\"]\r\n"
   "\r\n"
   "[mcp_servers.alpha.env]\n"
   "TOKEN = \"\"\"\n"
   "[model_providers.fake]\n"
   "\"\"\"\n"
   "# alpha's comment\n"
   "[profiles.fast]\r\n"
   "model = \"o4-mini\"\r\n"
   "mcp_servers = { scoped = { command = \"z\" } }\n"
   "[profiles.fast.mcp_servers.kept]\n"
   "command = \"kept\"\n"
   "[[ 'mcp_servers'.beta ]]\n"
   "command = \"gone\"\n"
   "[model_providers.local]\n"
   "name = \"mcp_servers = nope\"  # [mcp_servers.x]\n")
  "Codex settings interleaving MCP servers with unrelated settings.")

(ert-deftest +agent-shell-sandbox-test-codex-config-without-mcp ()
  "Drop every root MCP server table and assignment, leaving every other
line byte for byte, strings and profiles included."
  (should (equal (concat
                  "# Codex settings\n"
                  "model = \"o3\"\n"
                  "matrix = [\n"
                  "  [\"mcp_servers\"],\n"
                  "]\n"
                  "note = \"\"\"\n"
                  "[mcp_servers.inside_a_string]\n"
                  "\"\"\"\n"
                  "mcp_servers_note = 'kept'\n"
                  "\n"
                  "[profiles.fast]\r\n"
                  "model = \"o4-mini\"\r\n"
                  "mcp_servers = { scoped = { command = \"z\" } }\n"
                  "[profiles.fast.mcp_servers.kept]\n"
                  "command = \"kept\"\n"
                  "[model_providers.local]\n"
                  "name = \"mcp_servers = nope\"  # [mcp_servers.x]\n")
                 (+agent-shell-sandbox--codex-config-without-mcp
                  +agent-shell-sandbox-test--toml)))
  (should (equal "" (+agent-shell-sandbox--codex-config-without-mcp "")))
  (should (equal "a = 1" (+agent-shell-sandbox--codex-config-without-mcp
                          "a = 1"))))

(ert-deftest +agent-shell-sandbox-test-codex-config-invalid ()
  "Refuse settings the filter cannot follow before writing anything."
  (dolist (text '("[mcp_servers.x\ncommand = 1\n"
                  "key = \"unclosed\n"
                  "\"mcp\\u005fservers\".x = 1\n"
                  "note = \"\"\"\nnever closed\n"
                  "a = ]\n"
                  "= 1\n"))
    (should-error (+agent-shell-sandbox--codex-config-without-mcp text)
                  :type 'user-error))
  (+agent-shell-sandbox-test--with-host
    (+agent-shell-sandbox-test--logins host nil (+ (float-time) 3600))
    (+agent-shell-sandbox-test--write
     (expand-file-name "config.toml" +agent-shell-sandbox-codex-home)
     "[mcp_servers.x\n")
    (+agent-shell-sandbox-test--with-deadline nil
      (should-error (+agent-shell-sandbox-prepare-codex sandbox)
                    :type 'user-error)
      (should-not (directory-files (+agent-shell-sandbox-config sandbox)
                                   nil "\\`[^.]")))))

(defun +agent-shell-sandbox-test--check (agents deadline)
  "Check AGENTS' logins against DEADLINE and return the results."
  (let* ((results 'pending)
         (cancel (+agent-shell-sandbox-check-logins
                  agents deadline (lambda (value) (setq results value)))))
    (should (functionp cancel))
    (should (eq 'pending results))
    (+agent-shell-sandbox-test-wait (lambda () (not (eq 'pending results))))
    results))

(defun +agent-shell-sandbox-test--summary (results)
  "Return each of RESULTS as (AGENT REFRESHED ERROR-P)."
  (mapcar (lambda (result)
            (list (+agent-shell-sandbox-login-result-agent result)
                  (+agent-shell-sandbox-login-result-refreshed result)
                  (and (+agent-shell-sandbox-login-result-error result) t)))
          results))

(ert-deftest +agent-shell-sandbox-test-login-lasts ()
  "A login that outlasts the deadline and margin is used as it is.
Each agent is checked once, in order, and never before the check returns."
  (+agent-shell-sandbox-test--with-host
    (let ((+agent-shell-sandbox-backend 'bwrap)
          (now (float-time)))
      (+agent-shell-sandbox-test--logins host (round (+ now 36000))
                                         (round (+ now 720000)))
      (let ((results (+agent-shell-sandbox-test--check
                      '(claude-code codex claude-code) (+ now 3600))))
        (should (equal '((claude-code nil nil) (codex nil nil))
                       (+agent-shell-sandbox-test--summary results)))
        (should (< (abs (- 36000 (+agent-shell-sandbox-login-result-remaining
                                  (car results))))
                   5))
        (should-not (funcall runs))))))

(ert-deftest +agent-shell-sandbox-test-login-refreshes ()
  "An expired login is refreshed once on the host, outside any sandbox or
project, without the sandbox sign-in overrides, then used."
  (+agent-shell-sandbox-test--with-host
    (let ((+agent-shell-sandbox-backend 'bwrap)
          (process-environment
           (append '("CLAUDE_CODE_OAUTH_TOKEN=leak" "ANTHROPIC_API_KEY=leak"
                     "CODEX_HOME=/private/tmp/pa-sandbox-codex")
                   process-environment)))
      (+agent-shell-sandbox-test--logins host (- (truncate (float-time)) 60)
                                         nil)
      (let ((results (+agent-shell-sandbox-test--check
                      '(claude-code) (+ (float-time) 3600))))
        (should (equal '((claude-code t nil))
                       (+agent-shell-sandbox-test--summary results)))
        (should (< (abs (- 28800 (+agent-shell-sandbox-login-result-remaining
                                  (car results))))
                   5)))
      (pcase-let ((`(,cwd ,token ,key ,codex-home)
                   (split-string (car (funcall runs)))))
        (should (string-match-p "/pstack-login-" cwd))
        (should-not (file-exists-p cwd))
        (should (equal '("unset" "unset") (list token key)))
        (should (equal (directory-file-name +agent-shell-sandbox-codex-home)
                       codex-home)))
      (should (= 1 (length (funcall runs))))
      (should-not (file-exists-p (+agent-shell-sandbox--login-lock
                                  'claude-code))))))

(ert-deftest +agent-shell-sandbox-test-login-refresh-fails ()
  "A refresh that fails or hangs refuses the agent with why and what to
do, never with what the CLI printed."
  (+agent-shell-sandbox-test--with-host
    (let ((+agent-shell-sandbox-backend 'bwrap))
      (+agent-shell-sandbox-test--logins host (- (truncate (float-time)) 60)
                                         nil)
      (let* ((process-environment (cons "FAKE_CLAUDE=fail" process-environment))
             (result (car (+agent-shell-sandbox-test--check
                           '(claude-code) (+ (float-time) 3600))))
             (why (+agent-shell-sandbox-login-result-error result)))
        (should-not (+agent-shell-sandbox-login-result-refreshed result))
        (should (string-search "claude exited with status 1" why))
        (should (string-search "Log Claude Code in again on the host" why))
        (+agent-shell-sandbox-test--secret-free why))
      (let* ((+agent-shell-sandbox-refresh-timeout 1)
             (process-environment (cons "FAKE_DELAY=5" process-environment))
             (result (car (+agent-shell-sandbox-test--check
                           '(claude-code) (+ (float-time) 3600)))))
        (should (string-search "claude timed out after 1 s"
                               (+agent-shell-sandbox-login-result-error
                                result))))
      (let* ((+agent-shell-sandbox-agents
              '((claude-code :name "Claude Code"
                             :login +agent-shell-sandbox--claude-login
                             :refresh ("pa-no-such-cli"))))
             (result (car (+agent-shell-sandbox-test--check
                           '(claude-code) (+ (float-time) 3600)))))
        (should (string-search "pa-no-such-cli could not be started"
                               (+agent-shell-sandbox-login-result-error
                                result))))
      (should-not (file-exists-p (+agent-shell-sandbox--login-lock
                                  'claude-code))))))

(ert-deftest +agent-shell-sandbox-test-login-not-extended ()
  "A refresh that leaves the token short refuses, and a token expiring
exactly at the deadline plus margin is short."
  (+agent-shell-sandbox-test--with-host
    (let* ((+agent-shell-sandbox-backend 'bwrap)
           (deadline (+ (truncate (float-time)) 3600))
           (need (+ deadline +agent-shell-sandbox-login-margin))
           (process-environment (append '("FAKE_CLAUDE=noop" "FAKE_CODEX=noop")
                                        process-environment)))
      (+agent-shell-sandbox-test--logins host (1+ need) need)
      (let ((results (+agent-shell-sandbox-test--check
                      '(claude-code codex) deadline)))
        (should (equal '((claude-code nil nil) (codex nil t))
                       (+agent-shell-sandbox-test--summary results)))
        (let ((why (+agent-shell-sandbox-login-result-error (cadr results))))
          (should (string-search "a refresh did not extend it" why))
          (should (string-search "could not be triggered" why))))
      (should (= 1 (length (funcall runs))))
      (+agent-shell-sandbox-test--logins host need nil)
      (should (equal '((claude-code nil t))
                     (+agent-shell-sandbox-test--summary
                      (+agent-shell-sandbox-test--check
                       '(claude-code) deadline))))
      (should (= 2 (length (funcall runs)))))))

(ert-deftest +agent-shell-sandbox-test-login-refreshes-once ()
  "Two checks finding one expired login refresh it once between them."
  (+agent-shell-sandbox-test--with-host
    (let ((+agent-shell-sandbox-backend 'bwrap)
          (process-environment (cons "FAKE_DELAY=1" process-environment))
          (results nil))
      (+agent-shell-sandbox-test--logins host (- (truncate (float-time)) 60)
                                         nil)
      (dotimes (_ 2)
        (+agent-shell-sandbox-check-logins
         '(claude-code) (+ (float-time) 3600)
         (lambda (value) (push (car value) results))))
      (+agent-shell-sandbox-test-wait (lambda () (= 2 (length results))))
      (should (= 1 (length (funcall runs))))
      (should-not (seq-some #'+agent-shell-sandbox-login-result-error results))
      (should (= 1 (seq-count #'+agent-shell-sandbox-login-result-refreshed
                              results))))))

(defun +agent-shell-sandbox-test--hold (agent pid)
  "Make PID, with another nonce, the owner of AGENT's login lock."
  (let ((lock (+agent-shell-sandbox--login-lock agent)))
    (make-directory lock t)
    (+agent-shell-sandbox-test--write (expand-file-name "owner" lock)
                                      (format "%d foreign\n" pid))
    lock))

(defun +agent-shell-sandbox-test--dead-pid ()
  "Return the pid of a process that has exited."
  (let ((process (start-process "pa-dead" nil "true")))
    (while (process-live-p process) (accept-process-output nil 0.01))
    (process-id process)))

(ert-deftest +agent-shell-sandbox-test-login-lock-ownership ()
  "Only the check whose pid and nonce own a lock releases it.  A dead
owner's lock is taken over, a live one's waited for, but not forever."
  (+agent-shell-sandbox-test--with-host
    (let* ((one (+agent-shell-sandbox--make-login-check :nonce "one"))
           (two (+agent-shell-sandbox--make-login-check :nonce "two"))
           (lock (+agent-shell-sandbox--login-lock 'codex)))
      (should (string-prefix-p +agent-shell-sandbox--login-locks lock))
      (should (+agent-shell-sandbox--login-acquire one 'codex))
      (should (= #o700 (file-modes lock)))
      (should-not (+agent-shell-sandbox--login-acquire two 'codex))
      (+agent-shell-sandbox--login-release two)
      (should (file-exists-p lock))
      (+agent-shell-sandbox--login-release one)
      (should-not (file-exists-p lock))
      (+agent-shell-sandbox-test--hold 'codex
                                       (+agent-shell-sandbox-test--dead-pid))
      (should (+agent-shell-sandbox--login-acquire two 'codex))
      (should (equal (cons (emacs-pid) "two")
                     (+agent-shell-sandbox--lock-owner lock)))
      (+agent-shell-sandbox--login-release two)
      (should-not (directory-files +agent-shell-sandbox--login-locks nil
                                   "\\`[^.]")))
    (let* ((+agent-shell-sandbox-backend 'bwrap)
           (+agent-shell-sandbox-refresh-timeout 0.5)
           (holder (start-process "pa-holder" nil "sleep" "30")))
      (unwind-protect
          (progn
            (+agent-shell-sandbox-test--logins
             host (- (truncate (float-time)) 60) nil)
            (+agent-shell-sandbox-test--hold 'claude-code
                                             (process-id holder))
            (let ((why (+agent-shell-sandbox-login-result-error
                        (car (+agent-shell-sandbox-test--check
                              '(claude-code) (+ (float-time) 3600))))))
              (should (string-search "held its lock too long" why)))
            (should-not (funcall runs))
            (should (file-exists-p (+agent-shell-sandbox--login-lock
                                    'claude-code))))
        (delete-process holder)))))

(ert-deftest +agent-shell-sandbox-test-login-waiter-rereads ()
  "A check that waited for another's lock reads the login again rather
than refresh it, and refuses when the other did not extend it."
  (+agent-shell-sandbox-test--with-host
    (let ((+agent-shell-sandbox-backend 'bwrap)
          (holder (start-process "pa-holder" nil "sleep" "30")))
      (+agent-shell-sandbox-test--logins host (- (truncate (float-time)) 60)
                                         nil)
      (+agent-shell-sandbox-test--hold 'claude-code (process-id holder))
      (run-at-time 0.3 nil #'delete-process holder)
      (let ((why (+agent-shell-sandbox-login-result-error
                  (car (+agent-shell-sandbox-test--check
                        '(claude-code) (+ (float-time) 3600))))))
        (should (string-search "another refresh did not extend it" why)))
      (should-not (funcall runs))
      (should-not (file-exists-p (+agent-shell-sandbox--login-lock
                                  'claude-code))))))

(ert-deftest +agent-shell-sandbox-test-login-cancel-drains ()
  "Cancelling a waiting check ends it at once; cancelling one whose
refresh runs ends it, once, only after the refresh exits and its lock
is released."
  (+agent-shell-sandbox-test--with-host
    (let ((+agent-shell-sandbox-backend 'bwrap)
          (holder (start-process "pa-holder" nil "sleep" "30")))
      (unwind-protect
          (let ((calls nil))
            (+agent-shell-sandbox-test--logins
             host (- (truncate (float-time)) 60) nil)
            (let ((lock (+agent-shell-sandbox-test--hold 'claude-code
                                                         (process-id holder)))
                  (cancel (+agent-shell-sandbox-check-logins
                           '(claude-code codex) (+ (float-time) 3600)
                           (lambda (results) (push results calls)))))
              (accept-process-output nil 0.2)
              (should-not calls)
              (funcall cancel)
              (funcall cancel)
              (+agent-shell-sandbox-test-wait (lambda () calls))
              (accept-process-output nil 0.2)
              (should (= 1 (length calls)))
              (should (equal '((claude-code nil t) (codex nil t))
                             (+agent-shell-sandbox-test--summary (car calls))))
              (should (string-search "cancelled"
                                     (+agent-shell-sandbox-login-result-error
                                      (caar calls))))
              (should (equal (cons (process-id holder) "foreign")
                             (+agent-shell-sandbox--lock-owner lock)))
              (delete-directory lock t)))
        (delete-process holder)))
    (let* ((+agent-shell-sandbox-backend 'bwrap)
           (process-environment (cons "FAKE_DELAY=1" process-environment))
           (lock (+agent-shell-sandbox--login-lock 'claude-code))
           (calls nil)
           (cancel (+agent-shell-sandbox-check-logins
                    '(claude-code codex) (+ (float-time) 3600)
                    (lambda (results)
                      (push (list results (file-exists-p lock)
                                  (file-exists-p
                                   (expand-file-name "refreshed" host)))
                            calls)))))
      (+agent-shell-sandbox-test-wait (lambda () (funcall runs)))
      (should (file-exists-p lock))
      (funcall cancel)
      (accept-process-output nil 0.1)
      (should-not calls)
      (should (file-exists-p lock))
      (+agent-shell-sandbox-test-wait (lambda () calls))
      (funcall cancel)
      (accept-process-output nil 0.1)
      (should (= 1 (length calls)))
      (pcase-let ((`(,results ,locked ,refreshed) (car calls)))
        (should-not locked)
        (should refreshed)
        (should (equal '((claude-code t nil) (codex nil t))
                       (+agent-shell-sandbox-test--summary results)))))))

(ert-deftest +agent-shell-sandbox-test-login-final-reread ()
  "Every login is read again before the check succeeds, so one that a
later agent's refresh shortened is refused."
  (+agent-shell-sandbox-test--with-host
    (let ((+agent-shell-sandbox-backend 'bwrap)
          (now (truncate (float-time)))
          (process-environment (cons "FAKE_SHORTEN_CLAUDE=1"
                                     process-environment)))
      (+agent-shell-sandbox-test--logins host (+ now 36000) (- now 60))
      (let ((results (+agent-shell-sandbox-test--check
                      '(claude-code codex) (+ now 3600))))
        (should (equal '((claude-code nil t) (codex t nil))
                       (+agent-shell-sandbox-test--summary results)))
        (should (string-search "changed before admission finished"
                               (+agent-shell-sandbox-login-result-error
                                (car results))))))))

(ert-deftest +agent-shell-sandbox-test-login-unknown-agent ()
  "An agent with no login reader is refused without any refresh."
  (+agent-shell-sandbox-test--with-host
    (let ((+agent-shell-sandbox-agents '((fixture :refresh ("claude")))))
      (should (equal '((fixture nil t) (gemini nil t))
                     (+agent-shell-sandbox-test--summary
                      (+agent-shell-sandbox-test--check
                       '(fixture gemini) (+ (float-time) 3600)))))
      (should-not (funcall runs)))))

(ert-deftest +agent-shell-sandbox-test-delete-read-only ()
  "Delete read-only trees without following a symlink out of them."
  (+agent-shell-sandbox-test--with (root outside)
    (let* ((victim (expand-file-name "victim" root))
           (kept (+agent-shell-sandbox-test-read-only-tree outside))
           (outcome 'pending))
      (make-directory victim)
      (make-symbolic-link kept (expand-file-name "link" victim))
      (+agent-shell-sandbox-test-read-only-tree victim)
      (set-file-modes victim #o555)
      (+agent-shell-sandbox-delete (list victim)
                                   (lambda (error) (setq outcome error)))
      (should (eq 'pending outcome))
      (+agent-shell-sandbox-test-wait (lambda () (not (eq 'pending outcome))))
      (should-not outcome)
      (should-not (file-exists-p victim))
      (should (= #o555 (file-modes kept)))
      (should (file-exists-p (expand-file-name "inner/file" kept))))))

(ert-deftest +agent-shell-sandbox-test-delete-failure ()
  "Delete what it can and report what it cannot."
  (+agent-shell-sandbox-test--with (root)
    (let ((stuck (expand-file-name "stuck" root))
          (gone (expand-file-name "gone" root))
          (outcome 'pending))
      (make-directory stuck)
      (+agent-shell-sandbox-test-immutable (expand-file-name "pinned" stuck))
      (make-directory gone)
      (+agent-shell-sandbox-test-read-only-tree gone)
      (+agent-shell-sandbox-delete (list stuck gone)
                                   (lambda (error) (setq outcome error)))
      (+agent-shell-sandbox-test-wait (lambda () (not (eq 'pending outcome))))
      (should (string-search "pinned" outcome))
      (should-not (file-exists-p gone)))))

(ert-deftest +agent-shell-sandbox-test-descendants ()
  "Find every descendant of a pid, however deep."
  (should (equal '(2 3 4 5)
                 (sort (+agent-shell-sandbox--descendants
                        1 '((1 . 0) (2 . 1) (3 . 1) (4 . 2) (5 . 4) (6 . 0)
                            (7 . 6)))
                       #'<))))

;;;; Private folders and secrets

(defvar +private-config-directory)

(defvar +agent-shell-sandbox-test--secret nil
  "A secret folder `+agent-shell-sandbox-secret-reads' names by variable.")

(defmacro +agent-shell-sandbox-test--with-pair (&rest body)
  "Run BODY with sandboxes `a' and `b' and a `secret' folder neither reads.
Each private folder and the secret hold a file named key.  The secret is
named through a variable, beside a path that does not exist, and `a' may
read the folder `open' inside it."
  (declare (indent 0))
  `(let* ((scratch (file-truename (make-temp-file "pa-test-" t)))
          (secret (expand-file-name "secret" scratch))
          (open (expand-file-name "open" secret))
          (+agent-shell-sandbox-test--secret secret)
          (+agent-shell-sandbox-secret-reads
           (list (expand-file-name "missing" scratch)
                 '+agent-shell-sandbox-test--secret))
          (a nil) (b nil))
     (unwind-protect
         (progn
           (dolist (folder (list "a" "b" open))
             (make-directory (expand-file-name folder scratch) t))
           (setq a (+agent-shell-sandbox-create
                    (expand-file-name "a" scratch) :allow-reads (list open))
                 b (+agent-shell-sandbox-create
                    (expand-file-name "b" scratch)))
           (dolist (folder (list (+agent-shell-sandbox-tmp a)
                                 (+agent-shell-sandbox-config a)
                                 (+agent-shell-sandbox-tmp b)
                                 (+agent-shell-sandbox-config b)
                                 secret open))
             (+agent-shell-sandbox-test--write
              (expand-file-name "key" folder) folder))
           ,@body)
       (dolist (sandbox (delq nil (list a b)))
         (delete-directory (+agent-shell-sandbox-tmp sandbox) t)
         (delete-directory (+agent-shell-sandbox-config sandbox) t))
       (delete-directory scratch t))))

(defun +agent-shell-sandbox-test--private-reads (a b secret open)
  "Return (SANDBOX PATH READABLE) cases for sandboxes A and B.
SECRET is denied to both, and OPEN inside it is readable by A alone."
  (let ((key (lambda (folder) (expand-file-name "key" folder))))
    (list (list a (funcall key (+agent-shell-sandbox-tmp a)) t)
          (list a (funcall key (+agent-shell-sandbox-config a)) t)
          (list b (funcall key (+agent-shell-sandbox-tmp b)) t)
          (list a (funcall key (+agent-shell-sandbox-tmp b)) nil)
          (list a (funcall key (+agent-shell-sandbox-config b)) nil)
          (list b (funcall key (+agent-shell-sandbox-tmp a)) nil)
          (list b (funcall key (+agent-shell-sandbox-config a)) nil)
          (list a (funcall key secret) nil)
          (list b (funcall key secret) nil)
          (list a (funcall key open) t)
          (list b (funcall key open) nil))))

(ert-deftest +agent-shell-sandbox-test-readable-p ()
  "Refuse reads of other sandboxes' private folders and of secrets.
A sandbox reads its own private folders, a path allowance inside a
secret, and anything else not denied."
  (+agent-shell-sandbox-test--with-pair
    (pcase-dolist (`(,sandbox ,path ,readable)
                   (+agent-shell-sandbox-test--private-reads a b secret open))
      (should (eq readable (and (+agent-shell-sandbox-readable-p sandbox path)
                                t))))
    (should-not (+agent-shell-sandbox-readable-p a (+agent-shell-sandbox-tmp b)))
    (should (+agent-shell-sandbox-readable-p
             a (expand-file-name "README" (+agent-shell-sandbox-root b))))))

(ert-deftest +agent-shell-sandbox-test-readable-p-patterns ()
  "Refuse a path only a pattern allowance opens, erring towards refusal."
  (let* ((secret (file-truename (make-temp-file "pa-test-" t)))
         (+agent-shell-sandbox-secret-reads (list secret))
         (sandbox (+agent-shell-sandbox-create
                   secret :allow-reads
                   (list (cons 'regexp
                               (concat "^" (+agent-shell-sandbox-regexp-quote
                                            secret)
                                       "/public-"))))))
    (unwind-protect
        (should-not (+agent-shell-sandbox-readable-p
                     sandbox (expand-file-name "public-notes" secret)))
      (delete-directory (+agent-shell-sandbox-tmp sandbox) t)
      (delete-directory (+agent-shell-sandbox-config sandbox) t)
      (delete-directory secret t))))

(ert-deftest +agent-shell-sandbox-test-default-secrets ()
  "Deny SSH keys and the private Emacs config by default."
  (let* ((root (file-truename (make-temp-file "pa-test-" t)))
         (+private-config-directory root)
         (sandbox (+agent-shell-sandbox-create root)))
    (unwind-protect
        (progn
          (should-not (+agent-shell-sandbox-readable-p
                       sandbox "~/.ssh/id_ed25519"))
          (should-not (+agent-shell-sandbox-readable-p
                       sandbox (expand-file-name "keys.el" root))))
      (delete-directory (+agent-shell-sandbox-tmp sandbox) t)
      (delete-directory (+agent-shell-sandbox-config sandbox) t)
      (delete-directory root t))))

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
                             (concat "GOCACHE=" tmp "/go-build")
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
                        sandbox (executable-find "go") "env" "GOTMPDIR")))
        (should (equal (concat tmp "/go-build\n")
                       (+agent-shell-sandbox-test--output
                        sandbox (executable-find "go") "env" "GOCACHE")))))))

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

(ert-deftest +agent-shell-sandbox-test-read-only ()
  "Let a read-only sandbox read its root and write only its private folders."
  (+agent-shell-sandbox-test--on-macos
    (+agent-shell-sandbox-test--with (root)
      (let* ((judge (+agent-shell-sandbox-create root :read-only t))
             (run (lambda (&rest command)
                    (apply #'+agent-shell-sandbox-test--run judge root
                           command))))
        (unwind-protect
            (progn
              (+agent-shell-sandbox-test--write
               (expand-file-name "result" root) "made")
              (should (equal (list (+agent-shell-sandbox-tmp judge)
                                   (+agent-shell-sandbox-config judge))
                             (+agent-shell-sandbox-own-writes judge)))
              (should (eql 0 (funcall run "/bin/cat" "result")))
              (should-not (eql 0 (funcall run "/usr/bin/touch" "inside")))
              (should-not (file-exists-p (expand-file-name "inside" root)))
              (should (eql 0 (funcall run "/usr/bin/touch"
                                      (expand-file-name
                                       "t" (+agent-shell-sandbox-tmp judge))))))
          (+agent-shell-sandbox-discard judge))))))

(ert-deftest +agent-shell-sandbox-test-confines-private-reads ()
  "Read only a sandbox's own private folders and no secret.
A read allowance inside a secret opens just that path.  Each outcome
matches `+agent-shell-sandbox-readable-p'."
  (+agent-shell-sandbox-test--on-macos
    (+agent-shell-sandbox-test--with-pair
      (let ((cases (+agent-shell-sandbox-test--private-reads a b secret open)))
        (pcase-dolist (`(,sandbox ,path ,readable) cases)
          (should (equal (list path readable)
                         (list path
                               (eql 0 (+agent-shell-sandbox-test--run
                                       sandbox
                                       (+agent-shell-sandbox-root sandbox)
                                       "/bin/cat" path))))))
        (pcase-dolist (`(,sandbox ,path ,readable) cases)
          (should (eq readable
                      (and (+agent-shell-sandbox-readable-p sandbox path)
                           t))))))))

(ert-deftest +agent-shell-sandbox-test-confines-patterns ()
  "Write and read paths a pattern matches, and nothing that nearly does.
A pattern sandbox-exec cannot compile is rejected when the sandbox is
made."
  (+agent-shell-sandbox-test--on-macos
    (let* ((shared (format "/private/tmp/pa-pattern-%08x"
                           (random #x100000000)))
           (root (file-truename (make-temp-file "pa-test-" t)))
           (quoted (+agent-shell-sandbox-regexp-quote shared))
           (secret (concat shared "/secret"))
           (sandbox (+agent-shell-sandbox-create
                     root :deny-reads (list secret)
                     :allow-writes
                     (list (cons 'regexp
                                 (concat "^" quoted "/diff\\.[A-Za-z0-9]+$")))
                     :allow-reads
                     (list (lambda (_)
                             (cons 'regexp
                                   (concat "^" quoted "/secret/public-"))))))
           (run (lambda (&rest command)
                  (apply #'+agent-shell-sandbox-test--run sandbox root
                         command))))
      (unwind-protect
          (progn
            (make-directory secret t)
            (+agent-shell-sandbox-test--write
             (expand-file-name "public-notes" secret) "open")
            (+agent-shell-sandbox-test--write
             (expand-file-name "private-notes" secret) "closed")
            (should (eql 0 (funcall run "/usr/bin/touch"
                                    (concat shared "/diff.a1B2c3D4"))))
            (should (file-exists-p (concat shared "/diff.a1B2c3D4")))
            (dolist (near '("/diff.a1B2.bak" "/diff." "/diffX.a1B2" "/xdiff.a1"))
              (should-not (eql 0 (funcall run "/usr/bin/touch"
                                          (concat shared near))))
              (should-not (file-exists-p (concat shared near))))
            (should (eql 0 (funcall run "/bin/cat"
                                    (concat secret "/public-notes"))))
            (should-not (eql 0 (funcall run "/bin/cat"
                                        (concat secret "/private-notes"))))
            (should (string-search
                     "sandbox-exec compiles"
                     (cadr (should-error
                            (+agent-shell-sandbox-create
                             root :allow-writes
                             (list (cons 'regexp
                                         (concat "^" quoted "/(unclosed"))))
                            :type 'user-error)))))
        (delete-directory (+agent-shell-sandbox-tmp sandbox) t)
        (delete-directory (+agent-shell-sandbox-config sandbox) t)
        (delete-directory root t)
        (delete-directory shared t)))))

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

(ert-deftest +agent-shell-sandbox-test-socket-validation ()
  "Accept absolute socket paths and refuse the Emacs server's folder,
however the path reaches it."
  (let* ((folder (file-truename (make-temp-file "pa-test-" t)))
         (link (expand-file-name "server-link" folder)))
    (unwind-protect
        (progn
          (make-symbolic-link server-socket-dir link)
          (should-not (+agent-shell-sandbox-socket-error
                       "/private/tmp/build-tunnel.sock"))
          (dolist (value (list "tunnel.sock" 42 server-socket-dir
                               (expand-file-name "server" server-socket-dir)
                               (concat server-socket-dir "/")
                               (expand-file-name "server" link)))
            (should (stringp (+agent-shell-sandbox-socket-error value))))
          (should-error (+agent-shell-sandbox-create
                         folder :allow-connects
                         (list (lambda (_)
                                 (expand-file-name "server"
                                                   server-socket-dir))))
                        :type 'user-error))
      (delete-directory folder t))))

(ert-deftest +agent-shell-sandbox-test-allows-listed-sockets ()
  "Connect to a listed unix socket and to no other."
  (+agent-shell-sandbox-test--on-macos
    (skip-unless (executable-find "python3"))
    (let* ((name (format "/tmp/pa-sock-%08x" (random #x100000000)))
           (allowed (concat name "-tunnel.sock"))
           (other (concat name "-other.sock"))
           (default-directory "/")
           (listener
            (make-process
             :name "pa-test-listener" :noquery t :connection-type 'pipe
             :buffer (generate-new-buffer " *pa-listener*")
             :command (list "python3" "-c" "import os, selectors, socket, sys
selector = selectors.DefaultSelector()
for path in sys.argv[1:]:
    server = socket.socket(socket.AF_UNIX)
    server.bind(path)
    server.listen()
    selector.register(server, selectors.EVENT_READ)
print('ready', flush=True)
while True:
    for key, _ in selector.select():
        key.fileobj.accept()[0].close()" allowed other)))
           (root (file-truename (make-temp-file "pa-test-" t)))
           (sandbox (+agent-shell-sandbox-create
                     root :allow-connects (list (lambda (_) allowed))))
           (connect (lambda (socket)
                      (list "python3" "-c" "import socket, sys
socket.socket(socket.AF_UNIX).connect(sys.argv[1])" socket))))
      (unwind-protect
          (progn
            (+agent-shell-sandbox-test-wait
             (lambda () (string-search "ready" (with-current-buffer
                                                   (process-buffer listener)
                                                 (buffer-string)))))
            (should (equal (list (file-truename allowed))
                           (+agent-shell-sandbox-allow-connects sandbox)))
            (dolist (socket (list allowed other))
              (should (eql 0 (apply #'call-process
                                    (car (funcall connect socket)) nil nil nil
                                    (cdr (funcall connect socket))))))
            (should (eql 0 (apply #'+agent-shell-sandbox-test--run
                                  sandbox root (funcall connect allowed))))
            (should-not (eql 0 (apply #'+agent-shell-sandbox-test--run
                                      sandbox root (funcall connect other)))))
        (delete-process listener)
        (kill-buffer (process-buffer listener))
        (dolist (socket (list allowed other))
          (when (file-exists-p socket) (delete-file socket)))
        (delete-directory (+agent-shell-sandbox-tmp sandbox) t)
        (delete-directory (+agent-shell-sandbox-config sandbox) t)
        (delete-directory root t)))))

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
