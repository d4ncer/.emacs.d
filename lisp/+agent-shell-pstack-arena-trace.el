;;; +agent-shell-pstack-arena-trace.el --- Trace what arena candidates need -*- lexical-binding: t; -*-

;;; Commentary:
;; `+agent-shell-pstack-arena-trace' provisions one throwaway candidate
;; worktree exactly as an arena would, runs a shell command in it under a
;; trace-mode sandbox, and summarizes the kernel's reports.  A trace-mode
;; profile allows and reports what the candidate profile would refuse, so
;; one run lists every path a repository's arena profile may need.  With a
;; prefix argument the command runs under the candidate profile itself,
;; which proves a profile works, and the summary lists what it refused.
;;
;; A run moves through preparing, running, collecting and finished.
;; Whatever happens, finishing kills what the command left running and
;; removes the throwaway worktree.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require '+agent-shell-arena-git)
(require '+agent-shell-sandbox)
(require '+agent-shell-pstack-panel)

(defconst +agent-shell-pstack-arena-trace--drain 5
  "Most seconds to wait for the log to catch up once the command exits.")

(defconst +agent-shell-pstack-arena-trace--log-timeout 10
  "Seconds to wait for log stream to start.")

(defconst +agent-shell-pstack-arena-trace--crowd 3
  "Entries under one folder that collapse into the folder.")

(defconst +agent-shell-pstack-arena-trace--tail 20
  "Output lines the summary keeps.")

(cl-defstruct (+agent-shell-pstack-arena-trace-run
               (:constructor +agent-shell-pstack-arena-trace--run)
               (:copier nil))
  "One trace.  PHASE is preparing, running, collecting or finished.
STOP, once set, ends a run still preparing at its next step.  ENFORCE
runs the command under the candidate profile rather than trace mode."
  directory command buffer (phase 'preparing) stop manifest sandbox log
  process output status enforce)

(defvar-local +agent-shell-pstack-arena-trace--current nil
  "The trace run this buffer shows.")

;;;; Reports

(defun +agent-shell-pstack-arena-trace-parse (line tag)
  "Return the report in LINE as (OPERATION PATH PROCESS), or nil.
LINE is one line of log stream --style ndjson output.  Only the
kernel's sandbox lines whose message is TAG count, whether they report
an operation a trace-mode profile allowed, as allow, or one the
candidate profile refused, as deny(COUNT)."
  (when-let* (((string-prefix-p "{" line))
              (event (ignore-errors
                       (json-parse-string line :object-type 'alist)))
              (message (alist-get 'eventMessage event))
              ((stringp message))
              ((string-match
                (concat "\\`\\(?:[0-9]+ duplicate reports for \\)?Sandbox: "
                        "\\(.+?\\)([0-9]+) \\(?:allow\\|deny([0-9]+)\\) "
                        "\\([^ \n]+\\) \\([^\n]*\\)\n"
                        (regexp-quote tag) "\\'")
                message)))
    (list (match-string 2 message) (match-string 3 message)
          (match-string 1 message))))

(defun +agent-shell-pstack-arena-trace--fold (entries folder depth home)
  "Return ENTRIES, all at or under FOLDER, with crowded folders collapsed.
Each entry is (PATH COUNT PROCESSES).  FOLDER, DEPTH levels below /,
collapses into one entry when it holds enough entries, it is at least two
levels deep, and it does not hold HOME."
  (let (items groups)
    (dolist (entry entries)
      (if (equal (file-name-as-directory (car entry)) folder)
          (push entry items)
        (let ((name (car (split-string (substring (car entry)
                                                  (length folder))
                                       "/"))))
          (push entry (alist-get name groups nil nil #'equal)))))
    (pcase-dolist (`(,name . ,group) groups)
      (setq items (append (+agent-shell-pstack-arena-trace--fold
                           group (concat folder name "/") (1+ depth) home)
                          items)))
    (if (and (>= depth 2)
             (>= (length items) +agent-shell-pstack-arena-trace--crowd)
             (not (string-prefix-p folder home)))
        (list (list folder (apply #'+ (mapcar #'cadr items))
                    (sort (seq-uniq (mapcan (lambda (item)
                                              (copy-sequence (nth 2 item)))
                                            items))
                          #'string<)))
      items)))

(defun +agent-shell-pstack-arena-trace-summarize (reports &optional home)
  "Group REPORTS by operation, collapsing paths to sensible folders.
REPORTS are (OPERATION PATH PROCESS) lists.  Return an alist sorted by
operation, each value a list of (PATH COUNT PROCESSES) sorted by path.
COUNT is the number of distinct paths PATH stands for, and a PATH ending
in a slash is a folder holding several.  A folder collapses when it holds
`+agent-shell-pstack-arena-trace--crowd' entries, unless it is under two
levels deep or holds HOME, `~' by default."
  (let ((home (file-name-as-directory (expand-file-name (or home "~"))))
        (operations nil))
    (pcase-dolist (`(,operation ,path ,process) reports)
      (let* ((paths (or (alist-get operation operations nil nil #'equal)
                        (setf (alist-get operation operations nil nil #'equal)
                              (make-hash-table :test #'equal)))))
        (cl-pushnew process (gethash path paths) :test #'equal)))
    (sort
     (mapcar
      (pcase-lambda (`(,operation . ,paths))
        (let (absolute other)
          (maphash (lambda (path processes)
                     (push (list path 1 (sort processes #'string<))
                           (if (file-name-absolute-p path) absolute other)))
                   paths)
          (cons operation
                (sort (append (+agent-shell-pstack-arena-trace--fold
                               absolute "/" 0 home)
                              other)
                      (lambda (a b) (string< (car a) (car b)))))))
      operations)
     (lambda (a b) (string< (car a) (car b))))))

;;;; Buffer

(defvar-keymap +agent-shell-pstack-arena-trace-mode-map
  :doc "Keymap for `+agent-shell-pstack-arena-trace-mode'.")

(define-derived-mode +agent-shell-pstack-arena-trace-mode special-mode
  "Arena-Trace"
  "Show an arena trace.
`+agent-shell-pstack-arena-trace-stop' stops the traced command, and
killing the buffer stops it and cleans up.")

(defun +agent-shell-pstack-arena-trace--say (run &rest lines)
  "Append LINES to RUN's buffer, if it is still live."
  (let ((buffer (+agent-shell-pstack-arena-trace-run-buffer run)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (point-max))
            (dolist (line lines) (insert line "\n"))))))))

(defun +agent-shell-pstack-arena-trace--report-text (run summary)
  "Return the text reporting RUN's SUMMARY, exit status and output tail."
  (let* ((output (+agent-shell-pstack-arena-trace-run-output run))
         (lines (split-string (string-trim-right output) "\n"))
         (tail (last lines +agent-shell-pstack-arena-trace--tail))
         (enforce (+agent-shell-pstack-arena-trace-run-enforce run)))
    (concat
     (format "\n%s\n" (+agent-shell-pstack-arena-trace-run-status run))
     (cond
      ((and (null summary) enforce)
       "\nThe candidate profile refused nothing.\n")
      ((null summary)
       "\nNothing outside the candidate's sandbox was reported.\n")
      (t
       (concat
        (if enforce
            "\nWhat the candidate profile refused, by operation.  The kernel \
drops\nthe odd report, so run again before trusting an absence.\n"
          "\nWhat the candidate profile would refuse, by operation.  The \
kernel\ndrops the odd report, so trace again before trusting an absence.\n")
        (mapconcat
         (pcase-lambda (`(,operation . ,entries))
           (concat
            "\n" operation "\n"
            (mapconcat
             (pcase-lambda (`(,path ,count ,processes))
               (format "  %s  (%s%s)\n" path
                       (if (> count 1) (format "%d paths; " count) "")
                       (string-join processes ", ")))
             entries "")))
         summary ""))))
     (if (string-empty-p (string-trim output))
         "\nThe command printed nothing.\n"
       (format "\nOutput%s:\n%s\n"
               (if (> (length lines) (length tail))
                   (format ", last %d lines" (length tail))
                 "")
               (string-join tail "\n"))))))

;;;; Running

(defun +agent-shell-pstack-arena-trace--finish (run &optional error)
  "Finish RUN, reporting ERROR if given, and clean up what it made.
Kill the command's processes, stop the log, sweep the sandbox, then
remove the throwaway worktree.  Safe to call more than once."
  (unless (eq (+agent-shell-pstack-arena-trace-run-phase run) 'finished)
    (setf (+agent-shell-pstack-arena-trace-run-phase run) 'finished)
    (when error (+agent-shell-pstack-arena-trace--say run "" error))
    (let ((process (+agent-shell-pstack-arena-trace-run-process run))
          (log (+agent-shell-pstack-arena-trace-run-log run))
          (sandbox (+agent-shell-pstack-arena-trace-run-sandbox run))
          (manifest (+agent-shell-pstack-arena-trace-run-manifest run)))
      (when (process-live-p process)
        (+agent-shell-sandbox-kill-tree (process-id process))
        (delete-process process))
      (dolist (process (list process log))
        (when (processp process)
          (when (process-live-p process) (delete-process process))
          (when (buffer-live-p (process-buffer process))
            (kill-buffer (process-buffer process)))))
      (cl-labels
          ((clean ()
             (if (not manifest)
                 (+agent-shell-pstack-arena-trace--say run "Done.")
               (+agent-shell-arena-git-clean
                manifest
                (lambda (left error)
                  (+agent-shell-pstack-arena-trace--say
                   run
                   (cond (error (format "Cleanup failed: %s" error))
                         (left (format "Cleanup left %s"
                                       (string-join left "; ")))
                         (t "Removed the throwaway worktree."))))))))
        (if (not sandbox)
            (clean)
          (+agent-shell-sandbox-sweep
           sandbox
           (lambda (survivors)
             (when survivors
               (+agent-shell-pstack-arena-trace--say
                run (format "Processes %s outlived the sweep." survivors)))
             (+agent-shell-sandbox-discard
              sandbox
              (lambda (error)
                (when error
                  (+agent-shell-pstack-arena-trace--say
                   run (format "Could not delete the private folders: %s"
                               error)))
                (clean))))))))))

(defun +agent-shell-pstack-arena-trace--step (run error next)
  "Continue preparing RUN by calling NEXT, unless ERROR or it should stop."
  (cond ((eq (+agent-shell-pstack-arena-trace-run-phase run) 'finished))
        ((+agent-shell-pstack-arena-trace-run-stop run)
         (+agent-shell-pstack-arena-trace--finish run "Stopped."))
        (error (+agent-shell-pstack-arena-trace--finish
                run (format "Preparation failed: %s" error)))
        (t (condition-case err (funcall next)
             (error (+agent-shell-pstack-arena-trace--finish
                     run (format "Preparation failed: %s"
                                 (error-message-string err))))))))

(defun +agent-shell-pstack-arena-trace--collect (run)
  "Read RUN's reports once the log has caught up, summarize, then finish."
  (let* ((log (+agent-shell-pstack-arena-trace-run-log run))
         (tag (+agent-shell-sandbox-tag
               (+agent-shell-pstack-arena-trace-run-sandbox run)))
         (text (with-current-buffer (process-buffer log) (buffer-string))))
    (delete-process log)
    (+agent-shell-pstack-arena-trace--say
     run (+agent-shell-pstack-arena-trace--report-text
          run (+agent-shell-pstack-arena-trace-summarize
               (delq nil (mapcar (lambda (line)
                                   (+agent-shell-pstack-arena-trace-parse
                                    line tag))
                                 (split-string text "\n" t))))))
    (+agent-shell-pstack-arena-trace--finish run)))

(defun +agent-shell-pstack-arena-trace--probe-profile (run word)
  "Return the profile file whose one report is tagged with RUN's tag and WORD."
  (let* ((sandbox (+agent-shell-pstack-arena-trace-run-sandbox run))
         (file (expand-file-name (concat "probe-" word ".sb")
                                 (+agent-shell-sandbox-config sandbox))))
    (unless (file-exists-p file)
      (with-temp-file file
        (insert "(version 1)\n(allow default)\n(allow file-read-data \
(with report) (with message "
                (+agent-shell-sandbox--string
                 (concat (+agent-shell-sandbox-tag sandbox) " " word))
                ") (literal \"/dev/null\"))\n")))
    file))

(defun +agent-shell-pstack-arena-trace--await (run word timeout done)
  "Probe until RUN's log shows a report tagged WORD, then call DONE with t.
Call DONE with nil after TIMEOUT seconds instead.  Each probe reads
/dev/null under a profile that reports it, so the log has caught up with
everything reported before the report appears."
  (let* ((log (+agent-shell-pstack-arena-trace-run-log run))
         (needle (concat (+agent-shell-sandbox-tag
                          (+agent-shell-pstack-arena-trace-run-sandbox run))
                         " " word))
         (profile (+agent-shell-pstack-arena-trace--probe-profile run word))
         (deadline (+ (float-time) timeout)))
    (cl-labels
        ((poll ()
           (cond
            ((eq (+agent-shell-pstack-arena-trace-run-phase run) 'finished))
            ((and (buffer-live-p (process-buffer log))
                  (with-current-buffer (process-buffer log)
                    (save-excursion
                      (goto-char (point-min))
                      (search-forward needle nil t))))
             (funcall done t))
            ((> (float-time) deadline) (funcall done nil))
            (t (let ((default-directory "/"))
                 (make-process :name "pstack-arena-trace-probe" :noquery t
                               :command (list "/usr/bin/sandbox-exec" "-f"
                                              profile "/bin/cat" "/dev/null")
                               :sentinel #'ignore))
               (run-at-time 0.2 nil #'poll)))))
      (poll))))

(defun +agent-shell-pstack-arena-trace--exited (run process)
  "Record how RUN's command PROCESS ended and start collecting."
  (when (eq (+agent-shell-pstack-arena-trace-run-phase run) 'running)
    (setf (+agent-shell-pstack-arena-trace-run-phase run) 'collecting
          (+agent-shell-pstack-arena-trace-run-status run)
          (if (eq (process-status process) 'signal)
              (format "The command was stopped by signal %d."
                      (process-exit-status process))
            (format "Exit status %d." (process-exit-status process)))
          (+agent-shell-pstack-arena-trace-run-output run)
          (with-current-buffer (process-buffer process) (buffer-string)))
    (+agent-shell-pstack-arena-trace--say run "Collecting reports...")
    (+agent-shell-pstack-arena-trace--await
     run "done" +agent-shell-pstack-arena-trace--drain
     (lambda (_)
       (condition-case err
           (+agent-shell-pstack-arena-trace--collect run)
         (error (+agent-shell-pstack-arena-trace--finish
                 run (error-message-string err))))))))

(defun +agent-shell-pstack-arena-trace--command (run directory)
  "Start RUN's command in DIRECTORY under its sandbox's profile."
  (let* ((sandbox (+agent-shell-pstack-arena-trace-run-sandbox run))
         (profile (expand-file-name "trace.sb"
                                    (+agent-shell-sandbox-config sandbox)))
         (default-directory (file-name-as-directory directory))
         (process-environment (append (+agent-shell-sandbox-environment
                                       sandbox)
                                      process-environment)))
    (with-temp-file profile (insert (+agent-shell-sandbox-profile sandbox)))
    (setf (+agent-shell-pstack-arena-trace-run-phase run) 'running)
    (+agent-shell-pstack-arena-trace--say
     run (format "Running in %s ..." directory))
    (let ((process
           (make-process
            :name "pstack-arena-trace" :noquery t :connection-type 'pipe
            :buffer (generate-new-buffer " *pstack-arena-trace-output*" t)
            :command (list "/usr/bin/sandbox-exec" "-f" profile "/bin/sh" "-c"
                           (+agent-shell-pstack-arena-trace-run-command run))
            :sentinel (lambda (process _event)
                        (unless (process-live-p process)
                          (+agent-shell-pstack-arena-trace--exited
                           run process))))))
      (setf (+agent-shell-pstack-arena-trace-run-process run) process)
      (process-send-eof process))))

(defun +agent-shell-pstack-arena-trace--start (run directory)
  "Build RUN's sandbox, start the log, then the command.
DIRECTORY is the candidate's working directory."
  (let* ((manifest (+agent-shell-pstack-arena-trace-run-manifest run))
         (sandbox (+agent-shell-pstack-arena-sandbox
                   manifest "trace" nil
                   (not (+agent-shell-pstack-arena-trace-run-enforce run))))
         (default-directory "/"))
    (setf (+agent-shell-pstack-arena-trace-run-sandbox run) sandbox)
    (setf (+agent-shell-pstack-arena-trace-run-log run)
          (make-process
           :name "pstack-arena-trace-log" :noquery t :connection-type 'pipe
           :buffer (generate-new-buffer " *pstack-arena-trace-log*" t)
           :command (list "/usr/bin/log" "stream" "--style" "ndjson"
                          "--predicate"
                          (format "sender == \"Sandbox\" AND eventMessage \
CONTAINS \"%s\"" (+agent-shell-sandbox-tag sandbox)))
           :sentinel #'ignore))
    ;; log stream prints its filter before it receives events.
    (+agent-shell-pstack-arena-trace--await
     run "ready" +agent-shell-pstack-arena-trace--log-timeout
     (lambda (ready)
       (if ready
           (+agent-shell-pstack-arena-trace--step
            run nil (lambda ()
                      (+agent-shell-pstack-arena-trace--command
                       run directory)))
         (+agent-shell-pstack-arena-trace--finish
          run "log stream never started."))))))

;;;###autoload
(defun +agent-shell-pstack-arena-trace (directory command &optional enforce)
  "Trace what COMMAND needs beyond an arena candidate's sandbox.
Provision one throwaway candidate of the repository at DIRECTORY with its
arena profile, run COMMAND there with the candidate's environment under a
profile that reports instead of refusing, and show what it reported.
With ENFORCE, interactively a prefix argument, run COMMAND under the
candidate profile itself, so what it needs is refused, and show what was
refused.  That proves a profile works.  Return the run."
  (interactive
   (let ((enforce current-prefix-arg))
     (list (read-directory-name
            (if enforce "Run an enforced arena candidate of repository: "
              "Trace an arena candidate of repository: ")
            nil nil t)
           (read-shell-command (if enforce "Command to run: "
                                 "Command to trace: "))
           enforce)))
  (unless (and (eq system-type 'darwin)
               (file-executable-p "/usr/bin/sandbox-exec")
               (file-executable-p "/usr/bin/log"))
    (user-error "Arena traces need macOS's sandbox-exec and log"))
  (let* ((buffer (generate-new-buffer "*pstack arena trace*"))
         (run (+agent-shell-pstack-arena-trace--run
               :directory (expand-file-name directory) :command command
               :buffer buffer :enforce (and enforce t))))
    (with-current-buffer buffer
      (+agent-shell-pstack-arena-trace-mode)
      (setq +agent-shell-pstack-arena-trace--current run)
      (add-hook 'kill-buffer-hook #'+agent-shell-pstack-arena-trace--killed
                nil t))
    (+agent-shell-pstack-arena-trace--say
     run (format (if enforce "Enforced arena run of `%s' in %s"
                   "Arena trace of `%s' in %s")
                 command directory)
     ""
     "Capturing a base and provisioning a candidate...")
    (when (called-interactively-p 'any) (pop-to-buffer buffer))
    (+agent-shell-arena-git-capture-base
     directory
     (lambda (base error)
       (+agent-shell-pstack-arena-trace--step
        run error
        (lambda ()
          (let ((manifest (+agent-shell-arena-git-create base "trace")))
            (setf (+agent-shell-pstack-arena-trace-run-manifest run) manifest)
            (+agent-shell-arena-git-provision
             manifest "trace"
             (lambda (directory error)
               (+agent-shell-pstack-arena-trace--step
                run error
                (lambda ()
                  (+agent-shell-pstack-arena-trace--start
                   run directory))))))))))
    run))

(defun +agent-shell-pstack-arena-trace-stop (&optional run)
  "Stop RUN, by default the trace in this buffer.
A running command is killed and its reports are still summarized."
  (interactive)
  (let ((run (or run +agent-shell-pstack-arena-trace--current
                 (user-error "No arena trace in this buffer"))))
    (pcase (+agent-shell-pstack-arena-trace-run-phase run)
      ('preparing (setf (+agent-shell-pstack-arena-trace-run-stop run) t))
      ('running
       (let ((process (+agent-shell-pstack-arena-trace-run-process run)))
         (+agent-shell-sandbox-kill-tree (process-id process))
         (signal-process process 'SIGKILL))))))

(defun +agent-shell-pstack-arena-trace--killed ()
  "Stop the trace in the buffer being killed and clean up after it."
  (when-let* ((run +agent-shell-pstack-arena-trace--current))
    (if (eq (+agent-shell-pstack-arena-trace-run-phase run) 'preparing)
        (setf (+agent-shell-pstack-arena-trace-run-stop run) t)
      (+agent-shell-pstack-arena-trace--finish run))))

(provide '+agent-shell-pstack-arena-trace)
;;; +agent-shell-pstack-arena-trace.el ends here
