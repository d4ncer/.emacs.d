;;; +agent-shell-plans.el --- Live agent plan recording -*- lexical-binding: t; -*-

;;; Commentary:
;; Remember ACP plans without loading the view or changing agent-shell.
;; The section view loads only when `+agent-shell-plans' first opens.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require '+bootstrap)

(defgroup +agent-shell-plans nil
  "Live plans from agent shells."
  :group 'applications)

(defcustom +agent-shell-plans-directories
  (list (expand-file-name "plans/" +private-config-directory))
  "Directories whose topic folders hold plan documents.
A plan document is a .md or .org file in DIRECTORY/TOPIC/.  The plans
view lists them; directories that do not exist are skipped."
  :type '(repeat directory))

(defconst +agent-shell-plans--refresh-delay 0.1
  "Seconds of quiet before redrawing the plans view.")

(defvar +agent-shell-plans-change-hook nil
  "Hook run after a shell's plan changes or a shell is killed.
The latest entries are already stored when this hook runs.")

(defvar +agent-shell-plans--plans (make-hash-table :test #'eq)
  "Shell buffers mapped to their latest plan and update order.
Each value is a plist with :entries, the latest plan entries, :proposal,
the latest plan proposed for approval, and :updated.  A proposal is a
plist: :id, its tool call; :text, its markdown; :kinds, its options'
kinds by id; and :answer, nil while it waits, then `approved', `revised'
or `cancelled'.")

(defvar +agent-shell-plans--serial 0)
(defvar +agent-shell-plans--timer nil)
(defconst +agent-shell-plans--buffer "*agent plans*")

(declare-function +agent-shell-plans--render "+agent-shell-plans-view")
(declare-function +agent-shell-plans--open "+agent-shell-plans-view")
(declare-function +agent-shell-plans--goto "+agent-shell-plans-view")

(defun +agent-shell-plans--refresh ()
  "Redraw an existing plans view after a burst of changes."
  (setq +agent-shell-plans--timer nil)
  (when-let* ((buffer (get-buffer +agent-shell-plans--buffer)))
    (with-current-buffer buffer
      (when (derived-mode-p '+agent-shell-plans-mode)
        (+agent-shell-plans--render)))))

(defun +agent-shell-plans--schedule-refresh ()
  "Debounce redraws while the plans view exists."
  (when (timerp +agent-shell-plans--timer)
    (cancel-timer +agent-shell-plans--timer)
    (setq +agent-shell-plans--timer nil))
  (when (get-buffer +agent-shell-plans--buffer)
    (setq +agent-shell-plans--timer
          (run-at-time +agent-shell-plans--refresh-delay nil
                       #'+agent-shell-plans--refresh))))

(add-hook '+agent-shell-plans-change-hook
          #'+agent-shell-plans--schedule-refresh)

(defun +agent-shell-plans--forget ()
  "Forget the current shell when it is killed."
  (remhash (current-buffer) +agent-shell-plans--plans)
  (run-hooks '+agent-shell-plans-change-hook))

(defun +agent-shell-plans--watch ()
  "Watch the current shell's lifetime and announce its presence."
  (add-hook 'kill-buffer-hook #'+agent-shell-plans--forget nil t)
  (run-hooks '+agent-shell-plans-change-hook))

(defun +agent-shell-plans--note-notification (&rest args)
  "Record the plan in notification ARGS after agent-shell handles it."
  (let* ((shell (map-elt (plist-get args :state) :buffer))
         (update (map-nested-elt (plist-get args :acp-notification)
                                 '(params update))))
    (when (and (buffer-live-p shell)
               (equal (map-elt update 'sessionUpdate) "plan")
               (let ((entries (map-elt update 'entries)))
                 (or (listp entries) (vectorp entries))))
      (+agent-shell-plans--store
       shell :entries (and (not (seq-empty-p (map-elt update 'entries)))
                           (copy-tree (append (map-elt update 'entries) nil)
                                      t))))))

(defun +agent-shell-plans--store (shell key value)
  "Set SHELL's plan KEY to VALUE, then announce the change.
A shell left with neither entries nor a proposal is forgotten."
  (let ((plan (plist-put (copy-sequence (gethash shell +agent-shell-plans--plans))
                         key value)))
    (if (or (plist-get plan :entries) (plist-get plan :proposal))
        (puthash shell (plist-put plan :updated
                                  (cl-incf +agent-shell-plans--serial))
                 +agent-shell-plans--plans)
      (remhash shell +agent-shell-plans--plans)))
  (with-current-buffer shell
    (add-hook 'kill-buffer-hook #'+agent-shell-plans--forget nil t))
  (run-hooks '+agent-shell-plans-change-hook))

(defun +agent-shell-plans--note-request (&rest args)
  "Record the plan a permission request in ARGS proposes for approval.
Codex's plan mode ends its turn this way, with the plan's markdown as the
tool call's raw input; codex-acp sends plan_update only to clients that
declare a plan capability, which agent-shell does not."
  (let* ((shell (map-elt (plist-get args :state) :buffer))
         (request (plist-get args :acp-request))
         (call (map-nested-elt request '(params toolCall)))
         (text (map-nested-elt call '(rawInput plan))))
    (when (and (buffer-live-p shell)
               (equal (map-elt request 'method) "session/request_permission")
               (stringp text) (not (string-blank-p text)))
      (+agent-shell-plans--store
       shell :proposal
       (list :id (map-elt call 'toolCallId) :text (string-trim text)
             :kinds (mapcar (lambda (option)
                              (cons (map-elt option 'optionId)
                                    (map-elt option 'kind)))
                            (map-nested-elt request '(params options))))))))

(cl-defun +agent-shell-plans--note-response (&key state tool-call-id option-id
                                                  cancelled
                                                  &allow-other-keys)
  "Record how the user answered a proposed plan's permission request.
STATE, TOOL-CALL-ID, OPTION-ID and CANCELLED are as agent-shell sends them."
  (let* ((shell (map-elt state :buffer))
         (proposal (plist-get (gethash shell +agent-shell-plans--plans)
                              :proposal)))
    (when (and proposal (equal tool-call-id (plist-get proposal :id)))
      (+agent-shell-plans--store
       shell :proposal
       (plist-put (copy-sequence proposal) :answer
                  (cond (cancelled 'cancelled)
                        ((string-prefix-p
                          "allow" (or (alist-get option-id
                                                 (plist-get proposal :kinds)
                                                 nil nil #'equal)
                                      ""))
                         'approved)
                        (t 'revised)))))))

(advice-add 'agent-shell--on-notification :after
            #'+agent-shell-plans--note-notification)
(advice-add 'agent-shell--on-request :after #'+agent-shell-plans--note-request)
(advice-add 'agent-shell--send-permission-response :after
            #'+agent-shell-plans--note-response)
(add-hook 'agent-shell-mode-hook #'+agent-shell-plans--watch)
(dolist (buffer (buffer-list))
  (with-current-buffer buffer
    (when (derived-mode-p 'agent-shell-mode)
      (+agent-shell-plans--watch))))

(defun +agent-shell-plans--text (entry)
  "Return ENTRY's content or Codex step as one line."
  (string-join (split-string (or (map-elt entry 'content)
                                 (map-elt entry 'step) "") "[\n\r\t ]+" t)
               " "))

(defun +agent-shell-plans-progress (shell)
  "Return SHELL's plan as \"DONE/TOTAL\" and its current step, or nil."
  (when-let* ((entries (plist-get (gethash shell +agent-shell-plans--plans)
                                  :entries)))
    (let ((current (seq-find (lambda (entry)
                               (equal (map-elt entry 'status) "in_progress"))
                             entries)))
      (concat (format "%d/%d" (seq-count (lambda (entry)
                                           (equal (map-elt entry 'status)
                                                  "completed"))
                                         entries)
                      (length entries))
              (if current
                  (concat " · " (truncate-string-to-width
                                 (+agent-shell-plans--text current)
                                 70 nil nil "…"))
                "")))))

;;;###autoload
(defun +agent-shell-plans (&optional shell)
  "Show the latest plans from live agent shells.
From Lisp, SHELL names the shell whose card to show unfolded."
  (interactive)
  (require '+agent-shell-plans-view)
  (+agent-shell-plans--open)
  (when shell (+agent-shell-plans--goto shell)))

(provide '+agent-shell-plans)
;;; +agent-shell-plans.el ends here
