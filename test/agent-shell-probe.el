;;; agent-shell-probe.el --- Probe what a real agent sends Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Run one real agent through the configured adapter with an explicit
;; prompt, policy and environment, and record what reached Emacs: the
;; agent that ran, the prompt it got, each ACP session update kind, its
;; latest plan, its tool calls and its final text.  Use it to check that a
;; producer exists before building what consumes its data.  It is opt-in
;; and not part of the gate.  From a running Emacs:
;;
;;   (load "~/.emacs.d/test/agent-shell-probe.el")
;;   (+agent-shell-probe-start 'codex "Make a two-step plan." :policy nil)
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
      ("tool_call"
       (+agent-shell-probe--update
        id :tools (lambda (tools)
                    (append tools (list (list (map-elt update 'kind)
                                              (map-elt update 'title))))))))))

(defun +agent-shell-probe--finish (id &optional text timeout)
  "End probe ID with its final TEXT, or as timed out when TIMEOUT."
  (let ((probe (gethash id +agent-shell-probe--probes)))
    (unless (plist-get probe :done)
      (when-let* ((timer (plist-get probe :timer))) (cancel-timer timer))
      (+agent-shell-drive-unwatch '+agent-shell-probe id)
      (puthash id (append (list :done t :text text :timeout timeout) probe)
               +agent-shell-probe--probes)
      (when (buffer-live-p (plist-get probe :shell))
        (+agent-shell-drive-kill (plist-get probe :shell) "probe done")))))

(defun +agent-shell-probe--send (id)
  "Send probe ID its prompt once its shell is ready, retrying briefly."
  (let ((probe (gethash id +agent-shell-probe--probes)))
    (unless (or (plist-get probe :done) (plist-get probe :sent))
      (if (+agent-shell-drive-send (plist-get probe :shell)
                                   (plist-get probe :prompt))
          (+agent-shell-probe--update id :sent #'always)
        (run-at-time 1 nil #'+agent-shell-probe--send id)))))

(cl-defun +agent-shell-probe-start (agent prompt &key (policy 'read-only)
                                          environment directory (timeout 300))
  "Start a probe of AGENT with PROMPT, and return its id.
POLICY and ENVIRONMENT are as `+agent-shell-drive-start' takes them, and
DIRECTORY defaults to a fresh temporary one.  The probe gives up after
TIMEOUT seconds."
  (advice-add 'agent-shell--on-notification :after #'+agent-shell-probe--note)
  (let* ((id (format "probe-%d" (cl-incf +agent-shell-probe--serial)))
         (shell (+agent-shell-drive-start
                 :config agent :policy policy :environment environment
                 :directory (or directory
                                (file-name-as-directory
                                 (make-temp-file "agent-probe" t))))))
    (puthash id (list :agent agent :prompt prompt :policy policy
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
          (+agent-shell-probe--finish id (plist-get event :text))))))
    id))

(defun +agent-shell-probe-result (id)
  "Return probe ID's record once it is done, or nil while it runs.
The record names the agent that ran (:ran), the :prompt it got, its
session update kinds with counts (:updates), its latest :plan, its
:tools as (KIND TITLE) and its final :text, or :timeout."
  (let ((probe (gethash id +agent-shell-probe--probes)))
    (when (plist-get probe :done)
      (cl-loop for (key value) on probe by #'cddr
               unless (memq key '(:shell :timer :sent :done))
               append (list key value)))))

(provide 'agent-shell-probe)
;;; agent-shell-probe.el ends here
