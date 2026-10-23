;;; +agent-shell-perf.el --- Streaming workarounds for agent-shell -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "30.1") (agent-shell "0"))

;;; Commentary:
;; Local workarounds for upstream agent-shell work that scales with every
;; streamed event.  `+agent-shell-perf-mode' installs each entry of
;; `+agent-shell-perf--advice'.  Every entry names the upstream function it
;; works around; delete an entry once upstream changes that function.
;; Measurements are in docs/perf/agent-shell-streaming-after.md.

;;; Code:

(require 'agent-shell)
(require 'agent-shell-chat-mode)

(defgroup +agent-shell-perf nil
  "Streaming workarounds for agent-shell."
  :group 'agent-shell)

(defcustom +agent-shell-perf-relabel-delay 0.5
  "Seconds a streaming event waits before its coalesced chat relabel."
  :type 'number)

(defconst +agent-shell-perf-relabel-boundary-events
  '(input-submitted turn-complete prompt-ready session-restored
                    permission-request permission-response)
  "Events that keep upstream's immediate chat relabel.")

(defun +agent-shell-perf--throttle-relabel (schedule &rest args)
  "Call SCHEDULE with ARGS at turn boundaries; defer other relabels.
Works around `agent-shell-chat--schedule-relabel', which relabels the
whole buffer on the next tick after every event, so the cost grows with
buffer size times chunk rate.  An event in
`+agent-shell-perf-relabel-boundary-events', or a call with no event such
as `shell-maker-finish-output-hook', replaces any pending relabel with
upstream's immediate one.  Any other event schedules one relabel
`+agent-shell-perf-relabel-delay' seconds later unless one is pending.
Delete once upstream stops rescanning the buffer per streamed event."
  (let ((event (and (consp (car args)) (alist-get :event (car args)))))
    (cond
     ((or (null event)
          (memq event +agent-shell-perf-relabel-boundary-events))
      (when (timerp agent-shell-chat--relabel-timer)
        (cancel-timer agent-shell-chat--relabel-timer)
        (setq agent-shell-chat--relabel-timer nil))
      (apply schedule args))
     ((and agent-shell-chat--labeled (not agent-shell-chat--relabel-timer))
      (setq agent-shell-chat--relabel-timer
            (run-at-time +agent-shell-perf-relabel-delay nil
                         #'agent-shell-chat--relabel-buffer
                         (current-buffer)))))))

(defconst +agent-shell-perf--advice
  '((relabel agent-shell-chat--schedule-relabel
             +agent-shell-perf--throttle-relabel))
  "Workaround name, upstream function and its `:around' advice.")

;;;###autoload
(define-minor-mode +agent-shell-perf-mode
  "Install the agent-shell streaming workarounds in `+agent-shell-perf--advice'."
  :global t
  :group '+agent-shell-perf
  (pcase-dolist (`(,_ ,upstream ,advice) +agent-shell-perf--advice)
    (if +agent-shell-perf-mode
        (advice-add upstream :around advice)
      (advice-remove upstream advice))))

(provide '+agent-shell-perf)
;;; +agent-shell-perf.el ends here
