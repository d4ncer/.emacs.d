;;; +agent-shell-plans.el --- Live agent plan recording -*- lexical-binding: t; -*-

;;; Commentary:
;; Remember ACP plans without loading the view or changing agent-shell.
;; The section view loads only when `+agent-shell-plans' first opens.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)

(defgroup +agent-shell-plans nil
  "Live plans from agent shells."
  :group 'applications)

(defconst +agent-shell-plans--refresh-delay 0.1
  "Seconds of quiet before redrawing the plans view.")

(defvar +agent-shell-plans-change-hook nil
  "Hook run after a shell's plan changes or a shell is killed.
The latest entries are already stored when this hook runs.")

(defvar +agent-shell-plans--plans (make-hash-table :test #'eq)
  "Shell buffers mapped to their latest plan entries and update order.
Each value is a plist with :entries and :updated keys.")

(defvar +agent-shell-plans--serial 0)
(defvar +agent-shell-plans--timer nil)
(defconst +agent-shell-plans--buffer "*agent plans*")

(declare-function +agent-shell-plans--render "+agent-shell-plans-view")
(declare-function +agent-shell-plans--open "+agent-shell-plans-view")

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
      (let ((entries (map-elt update 'entries)))
        (if (seq-empty-p entries)
            (remhash shell +agent-shell-plans--plans)
          (puthash shell
                   (list :entries (copy-tree (append entries nil) t)
                         :updated (cl-incf +agent-shell-plans--serial))
                   +agent-shell-plans--plans)))
      (with-current-buffer shell
        (add-hook 'kill-buffer-hook #'+agent-shell-plans--forget nil t))
      (run-hooks '+agent-shell-plans-change-hook))))

(advice-add 'agent-shell--on-notification :after
            #'+agent-shell-plans--note-notification)
(add-hook 'agent-shell-mode-hook #'+agent-shell-plans--watch)
(dolist (buffer (buffer-list))
  (with-current-buffer buffer
    (when (derived-mode-p 'agent-shell-mode)
      (+agent-shell-plans--watch))))

;;;###autoload
(defun +agent-shell-plans ()
  "Show the latest plans from live agent shells."
  (interactive)
  (require '+agent-shell-plans-view)
  (+agent-shell-plans--open))

(provide '+agent-shell-plans)
;;; +agent-shell-plans.el ends here
