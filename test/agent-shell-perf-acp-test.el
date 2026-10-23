;;; agent-shell-perf-acp-test.el --- agent-shell workarounds over real ACP -*- lexical-binding: t; -*-

;;; Commentary:
;; Stream fixture turns into real agent shells with and without
;; `+agent-shell-perf-mode' and compare what the user sees.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'agent-shell)
(require '+agent-shell-drive)
(require '+agent-shell-perf)

(defvar +agent-shell-perf-acp-test--fixture
  (expand-file-name "fixtures/agent-shell-projects-agent.py"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun +agent-shell-perf-acp-test--wait-for (predicate &optional seconds)
  "Wait up to SECONDS (default 20) for PREDICATE, then assert it."
  (let ((deadline (+ (float-time) (or seconds 20))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01)))
  (should (funcall predicate)))

(defmacro +agent-shell-perf-acp-test--with-env (&rest body)
  "Run BODY with fixture agent config `fixture' and a scratch `cwd'.
Restores `+agent-shell-perf-mode' and kills the shells in `shells'."
  (declare (indent 0))
  `(let* ((cwd (file-name-as-directory
                (file-truename (make-temp-file "agent-shell-perf-" t))))
          (perf-mode +agent-shell-perf-mode)
          (agent-shell-show-welcome-message nil)
          (shell-maker-prompt-before-killing-buffer nil)
          (agent-shell-show-config-icons nil)
          (agent-shell-prefer-viewport-interaction nil)
          (agent-shell-mcp-servers nil)
          (agent-shell-chat-mode-enabled t)
          (agent-shell-transcript-file-path-function nil)
          (agent-shell-agent-configs
           (list (agent-shell-make-agent-config
                  :identifier 'fixture :mode-line-name "fixture"
                  :buffer-name "Perf fixture" :shell-prompt "Fixture> "
                  :shell-prompt-regexp "Fixture> "
                  :client-maker
                  (lambda (buffer)
                    (acp-make-client
                     :context-buffer buffer
                     :command (executable-find "python3")
                     :command-params
                     (list +agent-shell-perf-acp-test--fixture))))))
          (shells nil))
     (unwind-protect
         (save-window-excursion ,@body)
       (mapc #'+agent-shell-drive-kill shells)
       (+agent-shell-perf-mode (if perf-mode 1 -1))
       (delete-directory cwd t))))

(defun +agent-shell-perf-acp-test--start (cwd)
  "Start a ready fixture shell in CWD."
  (let ((shell (+agent-shell-drive-start :config 'fixture :directory cwd)))
    (+agent-shell-perf-acp-test--wait-for
     (lambda () (and (agent-shell-session-id :shell-buffer shell)
                     (eq (agent-shell-status :shell-buffer shell) 'ready))))
    shell))

(defun +agent-shell-perf-acp-test--labels (shell)
  "Return SHELL's chat label overlays as (START END PROPERTIES) lists."
  (with-current-buffer shell
    (sort (seq-keep (lambda (overlay)
                      (when (overlay-get overlay 'agent-shell-chat--tag)
                        (list (overlay-start overlay) (overlay-end overlay)
                              (seq-sort-by #'prin1-to-string #'string<
                                           (seq-partition
                                            (overlay-properties overlay) 2)))))
                    (overlays-in (point-min) (point-max)))
          :key #'prin1-to-string)))

(defun +agent-shell-perf-acp-test--agent-label-p (shell)
  "Return non-nil once SHELL shows an agent response label."
  (with-current-buffer shell
    (seq-find (lambda (overlay)
                (eq (overlay-get overlay 'agent-shell-chat--tag) 'agent))
              (overlays-in (point-min) (point-max)))))

(defun +agent-shell-perf-acp-test--turn (shell spec)
  "Stream fixture SPEC into SHELL and wait for the turn to end.
Return a plist of the first chunk, first agent label and turn-complete
times, read by a 10 ms timer and event subscriptions."
  (let* (first-chunk first-label complete
         (chunks (agent-shell-subscribe-to
                  :shell-buffer shell :event 'agent-message-chunk
                  :on-event (lambda (_) (unless first-chunk
                                          (setq first-chunk (float-time))))))
         (done (agent-shell-subscribe-to
                :shell-buffer shell :event 'turn-complete
                :on-event (lambda (_) (setq complete (float-time)))))
         (timer (run-at-time
                 0.01 0.01
                 (lambda ()
                   (when (and first-chunk (not first-label)
                              (+agent-shell-perf-acp-test--agent-label-p
                               shell))
                     (setq first-label (float-time)))))))
    (unwind-protect
        (progn
          (should (+agent-shell-drive-send
                   shell (concat "BENCH-STREAM " (json-encode spec))))
          (+agent-shell-perf-acp-test--wait-for (lambda () complete) 60)
          (accept-process-output nil 0.05))
      (cancel-timer timer)
      (with-current-buffer shell
        (agent-shell-unsubscribe :subscription chunks)
        (agent-shell-unsubscribe :subscription done)))
    (list :first-chunk first-chunk :first-label first-label
          :complete complete)))

(defconst +agent-shell-perf-acp-test--spec
  '(:chunks 80 :delay_ms 25 :tools 2 :tool_bytes 800 :thoughts 2
    :tag "perf" :seed 7)
  "A turn streaming for about two seconds.")

(ert-deftest +agent-shell-perf-acp-test-relabel-matches-upstream ()
  "Deferred relabels leave the same labels and text as upstream's."
  (+agent-shell-perf-acp-test--with-env
    (let (results)
      (dolist (mode '(-1 1))
        (+agent-shell-perf-mode mode)
        (let ((shell (+agent-shell-perf-acp-test--start cwd)))
          (push shell shells)
          (+agent-shell-perf-acp-test--turn
           shell +agent-shell-perf-acp-test--spec)
          (push (list (with-current-buffer shell
                        (buffer-substring-no-properties (point-min)
                                                        (point-max)))
                      (+agent-shell-perf-acp-test--labels shell))
                results)))
      (pcase-let ((`((,text ,labels) (,upstream-text ,upstream-labels))
                   results))
        (should (string-search "BENCH-END-perf" text))
        (should (equal text upstream-text))
        (should (member '(agent-shell-chat--tag agent)
                        (apply #'append (mapcar #'caddr labels))))
        (should (equal labels upstream-labels))))))

(ert-deftest +agent-shell-perf-acp-test-relabel-shows-label-while-streaming ()
  "The first response label appears within the relabel delay, mid-turn."
  (+agent-shell-perf-acp-test--with-env
    (+agent-shell-perf-mode 1)
    (let* ((shell (car (push (+agent-shell-perf-acp-test--start cwd) shells)))
           (times (+agent-shell-perf-acp-test--turn
                   shell +agent-shell-perf-acp-test--spec))
           (first-chunk (plist-get times :first-chunk))
           (first-label (plist-get times :first-label)))
      (should first-label)
      (should (< (- first-label first-chunk)
                 (+ +agent-shell-perf-relabel-delay 0.3)))
      (should (< first-label (- (plist-get times :complete) 0.5))))))

(provide 'agent-shell-perf-acp-test)
;;; agent-shell-perf-acp-test.el ends here
