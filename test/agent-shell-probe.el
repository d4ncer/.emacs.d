;;; agent-shell-probe.el --- Probe what a real agent sends Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Run one real agent through the configured adapter with an explicit
;; prompt, policy and environment, and record what reached Emacs: the
;; agent that ran, the prompt it got, each ACP session update kind, its
;; latest plan, its tool calls, its requests such as permission prompts,
;; the plan the plans view took from them, and its final text.  Use it to
;; check that a producer exists before building what consumes its data.
;; It is opt-in and not part of the gate.  From a running Emacs:
;;
;;   (load "~/.emacs.d/test/agent-shell-probe.el")
;;   (+agent-shell-probe-start 'codex "Make a two-step plan." :policy nil)
;;   (+agent-shell-probe-start 'codex '("/plan" "Plan a README.") :policy nil)
;;   (+agent-shell-probe-result ID)   ; nil until done
;;
;; Each probe keeps its own inputs, so several can run at once.

;;; Code:

(require 'cl-lib)
(require 'map)
(require '+agent-shell-drive)

(defvar +agent-shell-probe--probes (make-hash-table :test #'equal)
  "Probe ids mapped to their plists.")

(defvar +agent-shell-probe--serial 0)

(defun +agent-shell-probe--find (shell)
  "Return the id of the running probe whose shell is SHELL, or nil."
  (catch 'found
    (maphash (lambda (id probe)
               (when (and (eq shell (plist-get probe :shell))
                          (not (plist-get probe :done)))
                 (throw 'found id)))
             +agent-shell-probe--probes)))

(defun +agent-shell-probe--update (id key function)
  "Set probe ID's KEY to FUNCTION applied to its current value."
  (let ((probe (gethash id +agent-shell-probe--probes)))
    (puthash id (plist-put probe key (funcall function (plist-get probe key)))
             +agent-shell-probe--probes)))

(defun +agent-shell-probe--note (&rest args)
  "Record the session update in notification ARGS for its shell's probe."
  (when-let* ((shell (map-elt (plist-get args :state) :buffer))
              (id (+agent-shell-probe--find shell))
              (update (map-nested-elt (plist-get args :acp-notification)
                                      '(params update)))
              (kind (map-elt update 'sessionUpdate)))
    (+agent-shell-probe--update
     id :updates (lambda (seen) (cl-incf (alist-get kind seen 0 nil #'equal))
                   seen))
    (pcase kind
      ("plan" (+agent-shell-probe--update
               id :plan (lambda (_) (append (map-elt update 'entries) nil))))
      ("plan_update" (+agent-shell-probe--update
                      id :plan-updates
                      (lambda (plans) (append plans
                                              (list (map-elt update 'plan))))))
      ("tool_call"
       (+agent-shell-probe--update
        id :tools (lambda (tools)
                    (append tools (list (list (map-elt update 'kind)
                                              (map-elt update 'title))))))))))

(defun +agent-shell-probe--note-request (&rest args)
  "Record the request in ARGS, such as a permission request, for its probe."
  (when-let* ((shell (map-elt (plist-get args :state) :buffer))
              (id (+agent-shell-probe--find shell)))
    (let* ((request (plist-get args :acp-request))
           (call (map-nested-elt request '(params toolCall)))
           (input (map-elt call 'rawInput)))
      (+agent-shell-probe--update
       id :requests
       (lambda (requests)
         (append requests
                 (list (list :method (map-elt request 'method)
                             :title (map-elt call 'title)
                             :raw-keys (and (listp input) (map-keys input))
                             :plan (map-elt input 'plan)
                             :options
                             (mapcar (lambda (option)
                                       (cons (map-elt option 'optionId)
                                             (map-elt option 'kind)))
                                     (map-nested-elt request
                                                     '(params options)))))))))))

(defun +agent-shell-probe--finish (id &optional text timeout)
  "End probe ID with its final TEXT, or as timed out when TIMEOUT.
Keep the proposal the plans view stored for its shell, which goes with
the shell."
  (let* ((probe (gethash id +agent-shell-probe--probes))
         (shell (plist-get probe :shell)))
    (unless (plist-get probe :done)
      (when-let* ((timer (plist-get probe :timer))) (cancel-timer timer))
      (+agent-shell-drive-unwatch '+agent-shell-probe id)
      (puthash id (append (list :done t :text text :timeout timeout
                                :proposal
                                (and (boundp '+agent-shell-plans--plans)
                                     (plist-get (gethash
                                                 shell
                                                 +agent-shell-plans--plans)
                                                :proposal)))
                          probe)
               +agent-shell-probe--probes)
      (when (buffer-live-p shell)
        (+agent-shell-drive-kill shell "probe done")))))

(defun +agent-shell-probe--send (id)
  "Send probe ID its next prompt once its shell is ready, retrying briefly.
Return non-nil when one was sent, nil when none is left."
  (let* ((probe (gethash id +agent-shell-probe--probes))
         (prompts (plist-get probe :pending)))
    (when (and prompts (not (plist-get probe :done)))
      (if (+agent-shell-drive-send (plist-get probe :shell) (car prompts))
          (+agent-shell-probe--update id :pending #'cdr)
        (run-at-time 1 nil #'+agent-shell-probe--send id))
      t)))

(cl-defun +agent-shell-probe-start (agent prompt &key (policy 'read-only)
                                          environment directory (timeout 300))
  "Start a probe of AGENT with PROMPT, and return its id.
PROMPT is a string, or a list of them sent one turn after another, such
as a slash command that sets a mode and then the prompt.  POLICY and
ENVIRONMENT are as `+agent-shell-drive-start' takes them, and
DIRECTORY defaults to a fresh temporary one.  The probe gives up after
TIMEOUT seconds."
  (advice-add 'agent-shell--on-notification :after #'+agent-shell-probe--note)
  (advice-add 'agent-shell--on-request :after
              #'+agent-shell-probe--note-request)
  (let* ((id (format "probe-%d" (cl-incf +agent-shell-probe--serial)))
         (shell (+agent-shell-drive-start
                 :config agent :policy policy :environment environment
                 :directory (or directory
                                (file-name-as-directory
                                 (make-temp-file "agent-probe" t))))))
    (puthash id (list :agent agent :prompt prompt
                      :pending (if (listp prompt) prompt (list prompt))
                      :policy policy
                      :environment environment :shell shell
                      :ran (with-current-buffer shell
                             (map-nested-elt agent-shell--state
                                             '(:agent-config :identifier)))
                      :timer (run-at-time timeout nil
                                          #'+agent-shell-probe--finish
                                          id nil t))
             +agent-shell-probe--probes)
    (+agent-shell-drive-watch
     shell '+agent-shell-probe id
     (lambda (event)
       (pcase (plist-get event :kind)
         ('init-finished (+agent-shell-probe--send id))
         ('turn-complete
          (unless (+agent-shell-probe--send id)
            (+agent-shell-probe--finish id (plist-get event :text)))))))
    id))

(defun +agent-shell-probe-result (id)
  "Return probe ID's record once it is done, or nil while it runs.
The record names the agent that ran (:ran), the :prompt it got, its
session update kinds with counts (:updates), its latest :plan, every
:plan-updates payload, its :tools as (KIND TITLE), its :requests with
each one's :method, :title, :raw-keys, :plan and :options as (ID . KIND),
the :proposal the plans view stored, and its last turn's final :text, or
:timeout."
  (let ((probe (gethash id +agent-shell-probe--probes)))
    (when (plist-get probe :done)
      (cl-loop for (key value) on probe by #'cddr
               unless (memq key '(:shell :timer :pending :done))
               append (list key value)))))

(provide 'agent-shell-probe)
;;; agent-shell-probe.el ends here
