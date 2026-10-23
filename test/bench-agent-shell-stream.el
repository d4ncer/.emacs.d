;;; bench-agent-shell-stream.el --- Agent shell streaming benchmark -*- lexical-binding: t; -*-

;;; Commentary:
;; Stream fixture turns into real agent shells and print one JSON result line.
;; Run through test/bench-agent-shell-stream.sh, which alternates variants in
;; fresh Emacs processes.  Not part of test/run-config.el.
;;
;; Environment:
;;   BENCH_SCENARIO  fresh, preload, panel or mixed (`+bench-scenarios').
;;   BENCH_OFF       Comma-separated toggles to disable (`+bench-toggles').
;;   BENCH_LOAD      elc (default) loads compiled packages, el loads sources,
;;                   eln also reads native code from BENCH_ELN_DIR.
;;   BENCH_PROFILE   File for a CPU profile report of the measured turn.
;;   BENCH_CHUNKS, BENCH_DELAY_MS, BENCH_PRELOAD_BYTES override the turn.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'map)
(require 'profiler)
(require 'seq)
(require 'subr-x)

(defconst +bench-root
  (file-name-directory
   (directory-file-name (file-name-directory (or load-file-name
                                                 buffer-file-name)))))

(defconst +bench-fixture
  (expand-file-name "test/fixtures/agent-shell-projects-agent.py" +bench-root))

(defconst +bench-scenarios
  '((fresh :shells 1 :preload nil)
    (preload :shells 1 :preload t)
    (panel :shells 3 :preload nil)
    (mixed :shells 3 :preload t))
  "Scenario name to shell count and whether the lead is preloaded.")

(defconst +bench-toggles '(chat transcript drive render)
  "Production features a run can disable through BENCH_OFF.")

(defun +bench--env (name default)
  "Return environment variable NAME as a number, or DEFAULT."
  (if-let* ((value (getenv name))) (string-to-number value) default))

(defvar +bench-turn
  `(:chunks ,(+bench--env "BENCH_CHUNKS" 600)
    :delay_ms ,(+bench--env "BENCH_DELAY_MS" 15)
    :tools 8 :tool_bytes 3000 :thoughts 5)
  "Measured turn spec sent to the fixture.")

(defvar +bench-preload-turn
  '(:chunks 300 :delay_ms 0 :tools 30 :tool_bytes 6000 :thoughts 20)
  "Spec of the unmeasured turns that grow the lead's buffer.")

(defvar +bench-preload-bytes (+bench--env "BENCH_PRELOAD_BYTES" 2400000))

;;;; Load the configuration like production

(pcase (getenv "BENCH_LOAD")
  ("el" (setq load-suffixes '(".el")))
  ("eln" (push (file-name-as-directory (getenv "BENCH_ELN_DIR"))
               native-comp-eln-load-path)))
(add-to-list 'load-path (expand-file-name "lisp/" +bench-root))
(dolist (directory (directory-files (expand-file-name "elpaca/builds/"
                                                      +bench-root)
                                    t "^[^. ]"))
  (when (file-directory-p directory) (add-to-list 'load-path directory t)))

(setq gc-cons-threshold (* 16 1024 1024))

(require 'evil)
(require 'agent-shell)
(require 'agent-shell-notify)
(require '+agent-shell-pstack)
(require '+agent-shell-pstack-panel)
(require '+agent-shell-mermaid)

(defvar +bench-off
  (mapcar #'intern (split-string (or (getenv "BENCH_OFF") "") "," t)))
(dolist (toggle +bench-off)
  (unless (memq toggle +bench-toggles)
    (error "Unknown toggle %s" toggle)))

(defvar +bench-notifications 0)
(defvar +bench-workdir
  (file-name-as-directory (file-truename (make-temp-file "bench-shell-" t))))

(setq agent-shell-mcp-servers nil
      shell-maker-prompt-before-killing-buffer nil
      agent-shell-chat-mode-enabled (not (memq 'chat +bench-off))
      agent-shell-transcript-file-path-function
      (unless (memq 'transcript +bench-off)
        #'agent-shell--default-transcript-file-path)
      agent-shell-notify-send-function
      (lambda (&rest _) (cl-incf +bench-notifications))
      +agent-shell-mermaid-cache-directory
      (expand-file-name "mermaid" +bench-workdir)
      +agent-shell-drive-read-only-modes '((bench . "read-only"))
      +agent-shell-pstack-invocations '((bench . "$poteto-mode"))
      agent-shell-agent-configs
      (list (agent-shell-make-agent-config
             :identifier 'bench :mode-line-name "bench"
             :buffer-name "Bench" :shell-prompt "Bench> "
             :shell-prompt-regexp "Bench> "
             :client-maker
             (lambda (buffer)
               (acp-make-client :context-buffer buffer
                                :command (executable-find "python3")
                                :command-params (list +bench-fixture))))))

(evil-mode 1)
(evil-set-initial-state 'agent-shell-mode 'insert)
(agent-shell-notify-mode 1)
(+agent-shell-pstack-guard-mode 1)
(+agent-shell-pstack-panel-mode 1)
(unless (memq 'render +bench-off)
  (+agent-shell-mermaid-mode 1))

;;;; Instrumentation

(defvar +bench-recording nil "Non-nil while the measured turn streams.")
(defvar +bench-samples (make-hash-table :test #'equal)
  "Map a sample name to its list of durations in seconds.")

(defun +bench--record (name seconds)
  "Record SECONDS under sample NAME while recording."
  (when +bench-recording
    (push seconds (gethash name +bench-samples))))

(defmacro +bench--timed (name &rest body)
  "Run BODY, recording its duration under NAME."
  (declare (indent 1))
  `(let ((start (float-time)))
     (prog1 (progn ,@body)
       (+bench--record ,name (- (float-time) start)))))

(define-advice agent-shell--on-notification
    (:around (function &rest args) bench)
  (+bench--timed (or (map-nested-elt (plist-get args :acp-notification)
                                     '(params update sessionUpdate))
                     "other")
    (apply function args)))

(define-advice agent-shell-chat--relabel-buffer
    (:around (function &rest args) bench)
  (+bench--timed "relabel" (apply function args)))

(defun +bench--wrap-filter (shell)
  "Time the ACP process filter of SHELL."
  (let ((process (map-elt (buffer-local-value 'agent-shell--state shell)
                          '(:client :process))))
    (unless (processp process)
      (setq process (map-nested-elt
                     (buffer-local-value 'agent-shell--state shell)
                     '(:client :process))))
    (add-function :around (process-filter process)
                  (lambda (filter &rest args)
                    (+bench--timed "filter" (apply filter args))))))

(defvar +bench-last-tick nil)
(defvar +bench-gaps nil)

(defun +bench--tick ()
  "Record the gap since the previous tick of the stall timer."
  (let ((now (float-time)))
    (when +bench-last-tick (push (- now +bench-last-tick) +bench-gaps))
    (setq +bench-last-tick now)))

;;;; Driving shells

(defun +bench--wait-for (predicate seconds what)
  "Wait up to SECONDS for PREDICATE, signalling WHAT on timeout."
  (let ((deadline (+ (float-time) seconds)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (unless (funcall predicate) (error "Timed out waiting for %s" what))))

(defun +bench--ready-p (shell)
  "Return whether SHELL has a session and is ready for a prompt."
  (and (agent-shell-session-id :shell-buffer shell)
       (eq (agent-shell-status :shell-buffer shell) 'ready)))

(defun +bench--start (index)
  "Start shell INDEX: a pstack lead first, read-only panel members after."
  (let* ((lead (= index 0))
         (shell (+agent-shell-drive-start :config 'bench
                                          :directory +bench-workdir
                                          :read-only (not lead))))
    (+bench--wait-for (lambda () (+bench--ready-p shell)) 30 "a ready shell")
    (unless (memq 'drive +bench-off)
      (if lead
          (with-current-buffer shell (+agent-shell-pstack-mode 1))
        (+agent-shell-drive-watch shell 'pstack-panel (cons "bench" index)
                                  #'ignore)))
    shell))

(defun +bench--prompt (spec tag)
  "Return the fixture prompt streaming SPEC with TAG."
  (concat "BENCH-STREAM "
          (json-encode (append spec (list :tag tag :seed (sxhash tag))))))

(defun +bench--turns (shells spec tag)
  "Run one turn of SPEC on every shell in SHELLS concurrently.
Return each shell's wall time in seconds."
  (let ((pending (length shells))
        (walls (make-vector (length shells) nil))
        (start (float-time))
        tokens)
    (cl-loop for shell in shells for index from 0 do
             (let ((index index))
               (push (cons shell
                           (agent-shell-subscribe-to
                            :shell-buffer shell :event 'turn-complete
                            :on-event
                            (lambda (_event)
                              (aset walls index (- (float-time) start))
                              (cl-decf pending))))
                     tokens)))
    (cl-loop for shell in shells for index from 0 do
             (unless (+agent-shell-drive-send
                      shell (+bench--prompt spec (format "%s-%d" tag index)))
               (error "Shell %s refused the prompt" shell)))
    (+bench--wait-for (lambda () (zerop pending)) 600 "turn completion")
    (pcase-dolist (`(,shell . ,token) tokens)
      (with-current-buffer shell
        (agent-shell-unsubscribe :subscription token)))
    (append walls nil)))

(defun +bench--preload (shell)
  "Grow SHELL's buffer to `+bench-preload-bytes'.
Return a vector of [BYTES-BEFORE WALL-MS] per turn."
  (let ((turn 0) walls)
    (while (< (buffer-size shell) +bench-preload-bytes)
      (push (vector (buffer-size shell)
                    (* 1000 (car (+bench--turns
                                  (list shell) +bench-preload-turn
                                  (format "pre%d" (cl-incf turn))))))
            walls))
    (vconcat (nreverse walls))))

;;;; Statistics

(defun +bench--quantile (values q)
  "Return the Q quantile of VALUES in milliseconds, or nil."
  (when values
    (let ((sorted (vconcat (sort (copy-sequence values) #'<))))
      (* 1000 (aref sorted (min (1- (length sorted))
                                (floor (* q (length sorted)))))))))

(defun +bench--summary (name)
  "Return count, median, p95, max and total milliseconds of sample NAME."
  (let ((values (gethash name +bench-samples)))
    (list :count (length values)
          :median (+bench--quantile values 0.5)
          :p95 (+bench--quantile values 0.95)
          :max (+bench--quantile values 1.0)
          :total (* 1000 (apply #'+ 0 values)))))

(defun +bench--messages-matching (regexp)
  "Count *Messages* lines matching REGEXP."
  (with-current-buffer (messages-buffer)
    (count-matches regexp (point-min) (point-max))))

(defun +bench--transcript-bytes ()
  "Return the total size of transcripts written under the work directory."
  (let ((directory (expand-file-name ".agent-shell/transcripts"
                                     +bench-workdir)))
    (if (file-directory-p directory)
        (apply #'+ (mapcar (lambda (file) (file-attribute-size
                                           (file-attributes file)))
                           (directory-files directory t "\\.md\\'")))
      0)))

(defun +bench--cpu-seconds ()
  "Return this process's CPU time in seconds."
  (let ((time (current-cpu-time)))
    (/ (float (car time)) (cdr time))))

;;;; Entry point

(defun +bench-run ()
  "Run BENCH_SCENARIO and print a BENCH-RESULT JSON line."
  (let* ((scenario (intern (or (getenv "BENCH_SCENARIO") "fresh")))
         (shape (or (alist-get scenario +bench-scenarios)
                    (error "Unknown scenario %s" scenario)))
         (profile (getenv "BENCH_PROFILE"))
         (shells (save-window-excursion
                   (mapcar #'+bench--start
                           (number-sequence 0 (1- (plist-get shape :shells))))))
         (preload (when (plist-get shape :preload)
                    (+bench--preload (car shells))))
         (sizes-before (mapcar #'buffer-size shells))
         (transcript-before (+bench--transcript-bytes))
         (errors-before (+bench--messages-matching "errored\\|[Ee]rror"))
         (timer (run-at-time 0.01 0.01 #'+bench--tick))
         (gcs gcs-done) (gc-time gc-elapsed)
         (cpu (+bench--cpu-seconds)) (start (float-time))
         walls)
    (mapc #'+bench--wrap-filter shells)
    (clrhash +bench-samples)
    (setq +bench-gaps nil +bench-last-tick nil +bench-recording t)
    (when profile (profiler-start 'cpu))
    (setq walls (+bench--turns shells +bench-turn "run"))
    ;; Let deferred relabels and other zero-delay timers run.
    (let ((settle (+ (float-time) 0.1)))
      (while (< (float-time) settle) (accept-process-output nil 0.01)))
    (setq +bench-recording nil)
    (cancel-timer timer)
    (let* ((elapsed (- (float-time) start))
           (cpu-used (- (+bench--cpu-seconds) cpu))
           (gaps +bench-gaps)
           (chunks (+bench--summary "agent_message_chunk"))
           (result
            (list
             :scenario scenario :off +bench-off
             :load (or (getenv "BENCH_LOAD") "elc")
             :shells (length shells) :turn +bench-turn
             :lead-bytes-before (car sizes-before)
             :lead-bytes-after (buffer-size (car shells))
             :preload-turns preload
             :wall-ms (mapcar (lambda (wall) (* 1000 wall)) walls)
             :elapsed-ms (* 1000 elapsed)
             :cpu-ms (* 1000 cpu-used) :cpu-share (/ cpu-used elapsed)
             :chunk chunks
             :tool-call (+bench--summary "tool_call")
             :tool-call-update (+bench--summary "tool_call_update")
             :thought (+bench--summary "agent_thought_chunk")
             :relabel (+bench--summary "relabel")
             :filter (+bench--summary "filter")
             :stall-max-ms (+bench--quantile gaps 1.0)
             :stall-p99-ms (+bench--quantile gaps 0.99)
             :stalls-over-50ms (seq-count (lambda (gap) (> gap 0.05)) gaps)
             :stalls-over-100ms (seq-count (lambda (gap) (> gap 0.1)) gaps)
             :gc-count (- gcs-done gcs)
             :gc-ms (* 1000 (- gc-elapsed gc-time))
             :checks
             (list
              :chunks-seen (plist-get chunks :count)
              :chunks-expected (* (length shells)
                                  (1+ (plist-get +bench-turn :chunks)))
              :ends-rendered
              (seq-count (lambda (shell)
                           (with-current-buffer shell
                             (save-excursion
                               (goto-char (point-min))
                               (search-forward "BENCH-END-run-" nil t))))
                         shells)
              :transcript-bytes (- (+bench--transcript-bytes)
                                   transcript-before)
              :drive-turn-chars
              (mapcar (lambda (shell)
                        (length (or (+agent-shell-drive-partial-text shell)
                                    "")))
                      shells)
              :chat-labeled
              (seq-count (lambda (shell)
                           (buffer-local-value 'agent-shell-chat--labeled
                                               shell))
                         shells)
              :code-types
              (mapcar (lambda (symbol)
                        (type-of (advice--cd*r (symbol-function symbol))))
                      '(agent-shell--on-notification
                        agent-shell-chat--relabel
                        +agent-shell-drive--parse))
              :notifications +bench-notifications
              :new-errors (- (+bench--messages-matching
                              "errored\\|[Ee]rror")
                             errors-before)))))
      (when profile
        (+bench--write-profile profile)
        (profiler-stop))
      (princ (format "BENCH-RESULT %s\n" (json-encode result)))
      (dolist (shell shells) (+agent-shell-drive-kill shell))
      (delete-directory +bench-workdir t))))

(defun +bench--write-profile (file)
  "Write the CPU profile top-down and bottom-up to FILE.
Expand every entry holding at least 0.5% of the samples."
  (let* ((profile (profiler-make-profile :type 'cpu :log (profiler-cpu-log)
                                       :timestamp (current-time)))
         (total 0)
         (text ""))
    (maphash (lambda (_ count) (cl-incf total count))
             (profiler-profile-log profile))
    (dolist (reversed '(nil t))
      (with-current-buffer (profiler-report-setup-buffer profile)
        (setq profiler-report-reversed reversed)
        (profiler-report-rerender-calltree)
        (goto-char (point-min))
        (while (not (eobp))
          (when-let* ((tree (save-excursion
                              (profiler-report-move-to-entry)
                              (profiler-report-calltree-at-point))))
            (when (>= (profiler-calltree-count tree) (* 0.005 total))
              (profiler-report-expand-entry)))
          (forward-line 1))
        (setq text (concat text
                           (format "=== %s, %d samples\n"
                                   (if reversed "Bottom-up" "Top-down") total)
                           (buffer-substring-no-properties (point-min)
                                                           (point-max))
                           "\n"))
        (kill-buffer)))
    (with-temp-file file (insert text))))

(provide 'bench-agent-shell-stream)
;;; bench-agent-shell-stream.el ends here
