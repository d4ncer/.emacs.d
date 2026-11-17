;;; +agent-shell-perf.el --- Streaming workarounds for agent-shell -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "30.1") (agent-shell "0"))

;;; Commentary:
;; Local workarounds for upstream agent-shell work that scales with every
;; streamed event.  `+agent-shell-perf-mode' installs each entry of
;; `+agent-shell-perf--advice' and applies each el-patch in
;; `+agent-shell-perf--patches'; turning it off restores upstream, which
;; the benchmark and the ACP tests compare against.  Every entry names the
;; upstream function it works around; delete an entry once upstream
;; changes that function.
;; Measurements are in docs/perf/agent-shell-streaming-after.md and
;; docs/perf/agent-shell-upstream-78a2c94.md.

;;; Code:

(require 'agent-shell)
(require 'agent-shell-chat-mode)
(require 'el-patch)

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

(defun +agent-shell-perf--relabel-boundary-p (args)
  "Return whether relabel ARGS mark a turn boundary, or are empty.
`shell-maker-finish-output-hook' calls with none."
  (or (null args)
      (memq (and (consp (car args)) (alist-get :event (car args)))
            +agent-shell-perf-relabel-boundary-events)))

(el-patch-defun agent-shell-chat--schedule-relabel
  (&rest (el-patch-swap _ args))
  (el-patch-concat
    "Schedule a coalesced, deferred relabel of the current buffer.

Deferred so the triggering change's own text properties (e.g. the prompt
face shell-maker applies after inserting) are in place; coalesced so a
burst yields a single relabel.  Runs from the event subscription (which
covers submissions, streaming, turn completion and `session-restored'),
`shell-maker-finish-output-hook' (`clear') and
`agent-shell-section-functions' (fragments rendered with no event)."
    (el-patch-add
      "\n\nPatched by `+agent-shell-perf-mode': an event in
`+agent-shell-perf-relabel-boundary-events', or a call with no arguments,
replaces any pending relabel with an immediate one; any other event or
section waits `+agent-shell-perf-relabel-delay' seconds.  Upstream
relabels the whole buffer on the next tick after every event and every
rendered section, so its cost grows with buffer size times chunk rate.
Remove once upstream stops rescanning the buffer per streamed event."))
  (el-patch-add
    (when (+agent-shell-perf--relabel-boundary-p args)
      (when (timerp agent-shell-chat--relabel-timer)
        (cancel-timer agent-shell-chat--relabel-timer))
      (setq agent-shell-chat--relabel-timer nil)))
  (when (and agent-shell-chat--labeled
             (not agent-shell-chat--relabel-timer))
    (setq agent-shell-chat--relabel-timer
          (run-at-time (el-patch-swap
                         0
                         (if (+agent-shell-perf--relabel-boundary-p args)
                             0
                           +agent-shell-perf-relabel-delay))
                       nil #'agent-shell-chat--relabel-buffer
                       (current-buffer)))))

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
  '((transcript agent-shell--append-transcript
     +agent-shell-perf--buffer-transcript)
    (transcript-turn agent-shell--emit-event
                     +agent-shell-perf--flush-at-turn-end)
    (transcript-open agent-shell-open-transcript
                     +agent-shell-perf--flush-before))
  "Workaround name, upstream function and its `:around' advice.")

(defconst +agent-shell-perf--patches
  '((relabel . agent-shell-chat--schedule-relabel))
  "Workaround name and the upstream function its el-patch replaces.")

(defun +agent-shell-perf--patch (name on)
  "Apply workaround NAME's el-patch when ON, else restore upstream's code.
Either way the function is byte-compiled, so both run as compiled code."
  (let* ((function (alist-get name +agent-shell-perf--patches))
         (patch (or (el-patch-get function 'defun)
                    (error "No el-patch for %s" function))))
    (if on
        (eval `(el-patch-defun ,@(cdr patch)) t)
      (el-patch-unpatch function 'defun nil))
    (byte-compile function)))

;;;###autoload
(define-minor-mode +agent-shell-perf-mode
  "Install the agent-shell streaming workarounds.
They are the advice in `+agent-shell-perf--advice' and the el-patches
in `+agent-shell-perf--patches'; turning the mode off restores upstream."
  :global t
  :group '+agent-shell-perf
  (pcase-dolist (`(,_ ,upstream ,advice) +agent-shell-perf--advice)
    (if +agent-shell-perf-mode
        (advice-add upstream :around advice)
      (advice-remove upstream advice)))
  (pcase-dolist (`(,name . ,_) +agent-shell-perf--patches)
    (+agent-shell-perf--patch name +agent-shell-perf-mode))
  (if +agent-shell-perf-mode
      (add-hook 'kill-emacs-hook #'+agent-shell-perf--flush-all-transcripts)
    (remove-hook 'kill-emacs-hook #'+agent-shell-perf--flush-all-transcripts)
    (+agent-shell-perf--flush-all-transcripts)))

(unless +agent-shell-perf-mode
  (pcase-dolist (`(,name . ,_) +agent-shell-perf--patches)
    (+agent-shell-perf--patch name nil)))

(provide '+agent-shell-perf)
;;; +agent-shell-perf.el ends here
