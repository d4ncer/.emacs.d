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

(defcustom +agent-shell-perf-transcript-delay 1
  "Seconds transcript appends collect before one write."
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

(defvar-local +agent-shell-perf--transcript-pending nil
  "Unwritten transcript appends, an alist of file to texts, newest first.")

(defvar-local +agent-shell-perf--transcript-timer nil
  "Timer that writes `+agent-shell-perf--transcript-pending', or nil.")

(defvar +agent-shell-perf--flushing nil
  "Non-nil while pending appends pass through to upstream.")

(defun +agent-shell-perf--flush-transcript (&optional buffer)
  "Write BUFFER's pending transcript appends, one write per file.
BUFFER defaults to the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (when (timerp +agent-shell-perf--transcript-timer)
      (cancel-timer +agent-shell-perf--transcript-timer))
    (let ((pending (reverse +agent-shell-perf--transcript-pending))
          (+agent-shell-perf--flushing t))
      (setq +agent-shell-perf--transcript-pending nil
            +agent-shell-perf--transcript-timer nil)
      (pcase-dolist (`(,file . ,texts) pending)
        (agent-shell--append-transcript
         :text (apply #'concat (reverse texts)) :file-path file)))))

(defun +agent-shell-perf--flush-all-transcripts ()
  "Write every buffer's pending transcript appends."
  (dolist (buffer (buffer-list))
    (when (buffer-local-value '+agent-shell-perf--transcript-pending buffer)
      (+agent-shell-perf--flush-transcript buffer))))

(defun +agent-shell-perf--buffer-transcript (append &rest args)
  "Collect the text APPEND gets in ARGS and write it in one batch.
Works around `agent-shell--append-transcript', which stats and appends
to the transcript once per streamed chunk.  Appends from a shell buffer
are written in order `+agent-shell-perf-transcript-delay' seconds after
the first, when the turn ends, when the buffer is killed, when Emacs
exits, and before `agent-shell-open-transcript'.  Delete once upstream
batches transcript writes."
  (let ((text (plist-get args :text))
        (file-path (plist-get args :file-path)))
    (if (or +agent-shell-perf--flushing (not file-path)
            (not (derived-mode-p 'agent-shell-mode)))
        (apply append args)
      (let ((entry (assoc file-path +agent-shell-perf--transcript-pending)))
        (if entry
            (push text (cdr entry))
          (push (list file-path text) +agent-shell-perf--transcript-pending)))
      (unless +agent-shell-perf--transcript-timer
        (add-hook 'kill-buffer-hook #'+agent-shell-perf--flush-transcript nil t)
        (setq +agent-shell-perf--transcript-timer
              (run-at-time +agent-shell-perf-transcript-delay nil
                           #'+agent-shell-perf--flush-transcript
                           (current-buffer))))
      nil)))

(defun +agent-shell-perf--flush-at-turn-end (emit &rest args)
  "Write pending transcript appends before EMIT sends a turn-ending event.
ARGS are EMIT's arguments, from `agent-shell--emit-event'."
  (when (and +agent-shell-perf--transcript-pending
             (memq (plist-get args :event) '(turn-complete error clean-up)))
    (+agent-shell-perf--flush-transcript))
  (apply emit args))

(defun +agent-shell-perf--flush-before (function &rest args)
  "Write pending transcript appends, then call FUNCTION with ARGS."
  (+agent-shell-perf--flush-all-transcripts)
  (apply function args))

(defconst +agent-shell-perf--advice
  '((relabel agent-shell-chat--schedule-relabel
             +agent-shell-perf--throttle-relabel)
    (transcript agent-shell--append-transcript
                +agent-shell-perf--buffer-transcript)
    (transcript-turn agent-shell--emit-event
                     +agent-shell-perf--flush-at-turn-end)
    (transcript-open agent-shell-open-transcript
                     +agent-shell-perf--flush-before))
  "Workaround name, upstream function and its `:around' advice.")

;;;###autoload
(define-minor-mode +agent-shell-perf-mode
  "Install the agent-shell streaming workarounds in `+agent-shell-perf--advice'."
  :global t
  :group '+agent-shell-perf
  (pcase-dolist (`(,_ ,upstream ,advice) +agent-shell-perf--advice)
    (if +agent-shell-perf-mode
        (advice-add upstream :around advice)
      (advice-remove upstream advice)))
  (if +agent-shell-perf-mode
      (add-hook 'kill-emacs-hook #'+agent-shell-perf--flush-all-transcripts)
    (remove-hook 'kill-emacs-hook #'+agent-shell-perf--flush-all-transcripts)
    (+agent-shell-perf--flush-all-transcripts)))

(provide '+agent-shell-perf)
;;; +agent-shell-perf.el ends here
