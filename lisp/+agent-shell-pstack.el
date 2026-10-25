;;; +agent-shell-pstack.el --- pstack sessions in agent-shell -*- lexical-binding: t; -*-

;; Author: rk
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (agent-shell "0.66.1"))

;;; Commentary:
;; Keep pstack's poteto-mode active across turns of an agent shell, and hold
;; irreversible commands for a human.  The pstack skills themselves are
;; installed in each agent; this file only adapts how agent-shell talks to
;; them.  See docs/AGENT-SHELL-PSTACK.md.
;;
;; `+agent-shell-pstack-mode' prefixes the first prompt of each session with
;; the agent's poteto-mode invocation and appends a short reminder to later
;; prompts.  Both changes apply to the outgoing ACP request only, so the
;; shell buffer and transcript keep the text you typed.
;;
;; `+agent-shell-pstack-guard-mode' wraps the permission responder so that
;; irreversible commands always reach the interactive prompt.

;;; Code:

(require 'agent-shell)
(require 'json)
(require 'map)
(require 'rx)
(require 'seq)
(require 'subr-x)

(defgroup +agent-shell-pstack nil
  "pstack sessions in agent-shell."
  :group 'agent-shell)

(defcustom +agent-shell-pstack-invocations
  '((claude-code . "/pstack:poteto-mode")
    (codex . "$poteto-mode"))
  "Command that loads poteto-mode, by agent config identifier."
  :type '(alist :key-type symbol :value-type string))

(defcustom +agent-shell-pstack-reminder
  "poteto-mode is still active. Follow the poteto-mode skill loaded \
earlier in this session, and reread its SKILL.md if it is no longer in \
context."
  "Text appended to each prompt after poteto-mode has loaded."
  :type 'string)

(defcustom +agent-shell-pstack-irreversible-patterns
  (list
   (rx bow "git" (+ space) (* nonl) "push" (* nonl)
       (or "--force" "--mirror" "--delete"
           (seq space (or "-f" "-d") (or eos (not (any alnum "-"))))
           (seq space (any "+:") (not space))))
   (rx bow "git" (+ space) (* nonl)
       (or (seq "reset" (+ space) (* nonl) "--hard")
           (seq "clean" (+ space) (* nonl) "-" (* alpha) "f")
           (seq "branch" (+ space) (* nonl) "-D")))
   (rx bow "gh" (+ space)
       (or (seq "pr" (+ space) "merge")
           (seq "repo" (+ space) "delete")
           (seq "release" (+ space) (or "create" "delete"))))
   (rx bow (or "terraform" "tofu" "pulumi") (+ space) (* nonl)
       bow (or "apply" "destroy" "up") eow)
   (rx bow (or "kubectl" "helm") (+ space) (* nonl)
       bow (or "apply" "delete" "replace" "upgrade" "install" "uninstall")
       eow)
   (rx bow (or "drop" "DROP" "truncate" "TRUNCATE") (+ space)
       (or "table" "TABLE" "database" "DATABASE" "schema" "SCHEMA") eow))
  "Case-sensitive regexps matching commands only a human may approve.
See `+agent-shell-pstack-irreversible-p'."
  :type '(repeat regexp))

(defcustom +agent-shell-pstack-unguarded-modes
  '("bypassPermissions" "agent-full-access")
  "Session modes in which the agent runs commands without asking Emacs."
  :type '(repeat string))

(defface +agent-shell-pstack-mode-line
  '((t :inherit success :weight bold))
  "Face for the pstack mode-line indicator.")

(defconst +agent-shell-pstack--mode-line
  '(+agent-shell-pstack-mode
    (:propertize "pstack " face +agent-shell-pstack-mode-line
                 help-echo "pstack poteto-mode is active"))
  "Mode-line construct shown while `+agent-shell-pstack-mode' is on.")

(defvar +agent-shell-pstack-prime-texts nil
  "Extra text blocks for the prompt that loads poteto-mode.
Each element is a string or a function returning a string or nil.")

(defvar +agent-shell-pstack-reminder-texts nil
  "Extra text blocks for prompts that carry `+agent-shell-pstack-reminder'.
Each element is a string or a function returning a string or nil.")

(defvar-local +agent-shell-pstack--primed nil
  "Session id whose prompt already loaded poteto-mode.")

(defvar-local +agent-shell-pstack--client nil
  "ACP client whose requests this buffer decorates.")

(defvar +agent-shell-pstack--next-responder nil
  "Permission responder that `+agent-shell-pstack-guard-mode' wraps.")

;;;; Prompts

(defun +agent-shell-pstack--text-block (text)
  "Return an ACP text content block holding TEXT."
  `((type . "text") (text . ,text)))

(defun +agent-shell-pstack--prompt (blocks invocation)
  "Return prompt BLOCKS adapted for poteto-mode.
With INVOCATION, prefix it to the first block.  Otherwise append
`+agent-shell-pstack-reminder'.  Return nil when BLOCKS already start
with a slash or skill command, which is left for the agent."
  (let* ((first (and (length> blocks 0) (aref blocks 0)))
         (text (and (equal (map-elt first 'type) "text")
                    (map-elt first 'text))))
    (cond
     ((and text (string-match-p (rx bos (any "/$") alpha) text)) nil)
     ((and invocation text)
      (vconcat (list (+agent-shell-pstack--text-block
                      (concat invocation " " text)))
               (seq-drop blocks 1)))
     (invocation
      (vconcat (list (+agent-shell-pstack--text-block invocation)) blocks))
     (t
      (vconcat blocks (list (+agent-shell-pstack--text-block
                             +agent-shell-pstack-reminder))
               (+agent-shell-pstack--blocks
                +agent-shell-pstack-reminder-texts))))))

(defun +agent-shell-pstack--blocks (texts)
  "Return text blocks for TEXTS, strings or functions returning one or nil."
  (vconcat
   (mapcar #'+agent-shell-pstack--text-block
           (delq nil (mapcar (lambda (text)
                               (if (functionp text) (funcall text) text))
                             texts)))))

(defun +agent-shell-pstack--replace (alist key value)
  "Return a copy of ALIST with KEY bound to VALUE."
  (cons (cons key value) (assoc-delete-all key (copy-alist alist))))

(defun +agent-shell-pstack--invocation ()
  "Return the poteto-mode invocation for the current shell's agent."
  (let ((id (map-nested-elt agent-shell--state '(:agent-config :identifier))))
    (or (alist-get id +agent-shell-pstack-invocations)
        (user-error "No pstack invocation for agent %s; see %s"
                    id '+agent-shell-pstack-invocations))))

(defun +agent-shell-pstack--decorate (buffer request)
  "Return REQUEST adapted for poteto-mode in shell BUFFER."
  (if-let* (((equal (map-elt request :method) "session/prompt"))
            ((buffer-live-p buffer))
            ((buffer-local-value '+agent-shell-pstack-mode buffer)))
      (with-current-buffer buffer
        (let* ((params (map-elt request :params))
               (session (map-elt params 'sessionId))
               (prime (not (equal session +agent-shell-pstack--primed)))
               (prompt (+agent-shell-pstack--prompt
                        (map-elt params 'prompt)
                        (and prime (+agent-shell-pstack--invocation)))))
          (if (not prompt)
              request
            (when prime
              (setq +agent-shell-pstack--primed session
                    prompt (vconcat prompt
                                    (+agent-shell-pstack--blocks
                                     +agent-shell-pstack-prime-texts))))
            (+agent-shell-pstack--replace
             request :params
             (+agent-shell-pstack--replace params 'prompt prompt)))))
    request))

(defun +agent-shell-pstack--install ()
  "Route the current shell's outgoing requests through poteto-mode."
  (let* ((client (or (map-elt agent-shell--state :client)
                     (user-error "This agent shell has no ACP client yet")))
         (cell (or (assq :outgoing-request-decorator client)
                   (user-error "This ACP client has no request decorator"))))
    (unless (eq client +agent-shell-pstack--client)
      (let ((next (cdr cell))
            (buffer (current-buffer)))
        (setcdr cell
                (lambda (request)
                  (+agent-shell-pstack--decorate
                   buffer
                   (or (and next (funcall next request)) request)))))
      (setq +agent-shell-pstack--client client))))

;;;###autoload
(define-minor-mode +agent-shell-pstack-mode
  "Keep pstack's poteto-mode active in this agent shell.
The next prompt loads poteto-mode, and later prompts carry a reminder.
The mode line shows \"pstack\" through `mode-line-misc-info'.
Toggle the mode off and on to load poteto-mode again, for example after
the agent compacts its context."
  :group '+agent-shell-pstack
  (setq +agent-shell-pstack--primed nil)
  (when +agent-shell-pstack-mode
    (condition-case err
        (progn
          (unless (derived-mode-p 'agent-shell-mode)
            (user-error "Not in an agent shell"))
          (+agent-shell-pstack--invocation)
          (+agent-shell-pstack--install)
          (add-to-list 'mode-line-misc-info +agent-shell-pstack--mode-line)
          (when (member (map-nested-elt agent-shell--state '(:session :mode-id))
                        +agent-shell-pstack-unguarded-modes)
            (message "pstack: this session mode skips permission prompts, \
so irreversible commands are not guarded")))
      (error
       (setq +agent-shell-pstack-mode nil)
       (signal (car err) (cdr err))))))

;;;; Guard

(defun +agent-shell-pstack--command-text (tool-call)
  "Return searchable text for TOOL-CALL, or nil when it runs no command."
  (when (member (map-elt tool-call :kind) '(nil "execute"))
    (let ((raw (map-elt tool-call :raw-input)))
      (string-join
       (delq nil
             (list (map-elt tool-call :title)
                   (and raw
                        (condition-case nil
                            (json-encode raw)
                          (error (prin1-to-string raw))))))
       "\n"))))

(defun +agent-shell-pstack-irreversible-p (tool-call)
  "Return the command text of TOOL-CALL if it is irreversible."
  (when-let* ((text (+agent-shell-pstack--command-text tool-call)))
    (let ((case-fold-search nil))
      (with-syntax-table (standard-syntax-table)
        (and (seq-some (lambda (pattern) (string-match-p pattern text))
                       +agent-shell-pstack-irreversible-patterns)
             text)))))

(defun +agent-shell-pstack--respond (permission)
  "Show PERMISSION to the user when it is irreversible.
Otherwise defer to `+agent-shell-pstack--next-responder'."
  (if-let* ((text (+agent-shell-pstack-irreversible-p
                   (map-elt permission :tool-call))))
      (progn
        (message "pstack: approve this irreversible command yourself: %s"
                 (truncate-string-to-width
                  (car (split-string text "\n")) 80 nil nil t))
        nil)
    (and (functionp +agent-shell-pstack--next-responder)
         (funcall +agent-shell-pstack--next-responder permission))))

;;;###autoload
(define-minor-mode +agent-shell-pstack-guard-mode
  "Send irreversible agent commands to the interactive permission prompt.
Wraps `agent-shell-permission-responder-function', so automatic approval
still applies to everything else.  Agents only ask when their session
mode requires it; see `+agent-shell-pstack-unguarded-modes'.  Matching is
textual, so a quoted or indirect command can slip past; it catches
mistakes, not an adversary."
  :global t
  :group '+agent-shell-pstack
  (if +agent-shell-pstack-guard-mode
      (unless (eq (default-value 'agent-shell-permission-responder-function)
                  #'+agent-shell-pstack--respond)
        (setq +agent-shell-pstack--next-responder
              (default-value 'agent-shell-permission-responder-function))
        (setq-default agent-shell-permission-responder-function
                      #'+agent-shell-pstack--respond))
    (when (eq (default-value 'agent-shell-permission-responder-function)
              #'+agent-shell-pstack--respond)
      (setq-default agent-shell-permission-responder-function
                    +agent-shell-pstack--next-responder)
      (setq +agent-shell-pstack--next-responder nil))))

(provide '+agent-shell-pstack)
;;; +agent-shell-pstack.el ends here
