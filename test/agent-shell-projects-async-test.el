;;; agent-shell-projects-async-test.el --- Responsiveness regressions -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise the real event loop and child workers, including slow Git commands.
;; Load after agent-shell-projects-test.el for its disposable fixtures.

;;; Code:

(require 'agent-shell-projects-test)

(defun +agent-shell-projects-test--slow-git (directory seconds)
  "Install a Git wrapper taking SECONDS per command under DIRECTORY."
  (let* ((binary (or (seq-find #'file-executable-p '("/opt/homebrew/bin/git" "/usr/bin/git"))
                     (executable-find "git")))
         (bin (expand-file-name "slow-bin/" directory))
         (file (expand-file-name "git" bin)))
    (make-directory bin t)
    (with-temp-file file
      (insert "#!/bin/sh\nsleep " (number-to-string seconds) "\nexec "
              (shell-quote-argument binary) " \"$@\"\n"))
    (set-file-modes file #o755)
    bin))

(ert-deftest +agent-shell-projects-test-slow-git-keeps-event-loop-responsive ()
  (+agent-shell-projects-test--isolated
    (let* ((root (+agent-shell-projects-test--repository directory))
           (_ (+agent-shell-projects-test--file root "tracked.txt" "changed\n"))
           (bin (+agent-shell-projects-test--slow-git directory 0.12))
           (process-environment (cons (concat "PATH=" bin ":" (getenv "PATH")) process-environment))
           (ticks 0) artefact heartbeat)
      (unwind-protect
          (with-temp-buffer
            (setq heartbeat (run-at-time 0 0.03 (lambda () (cl-incf ticks))))
            (cl-letf (((symbol-function 'process-file) (lambda (&rest _) (error "Synchronous process on UI thread")))
                      ((symbol-function 'call-process-region) (lambda (&rest _) (error "Synchronous process on UI thread"))))
              (let ((started (float-time)))
                (setq artefact (+agent-shell-projects-enrol-code project root))
                (should (< (- (float-time) started) 0.25))
                (should-not (plist-get artefact :current))
                ;; Actual editor actions can run while Git is sleeping in the child.
                (insert "Editing continues")
                (+agent-shell-projects-test--captured artefact)))
            (should (> ticks 10))
            (should (equal "Editing continues" (buffer-string))))
        (when heartbeat (cancel-timer heartbeat))))))

(ert-deftest +agent-shell-projects-test-batched-base-cache-and-shared-discovery ()
  (+agent-shell-projects-test--isolated
    (let* ((root (+agent-shell-projects-test--repository directory))
           (base (string-trim (+agent-shell-projects--git-text root "rev-parse" "HEAD"))))
      (dotimes (n 100)
        (+agent-shell-projects-test--file root (format "file-%d.txt" n) "before\n"))
      (+agent-shell-projects-git root "add" ".")
      (+agent-shell-projects-git root "commit" "-qm" "Many files")
      (let ((second-base (string-trim (+agent-shell-projects--git-text root "rev-parse" "HEAD"))))
        (dotimes (n 100)
          (+agent-shell-projects-test--file root (format "file-%d.txt" n) "after\n"))
        (let* ((request (list :root root :targets (list (list :project "p" :artefact "a" :base second-base))))
               (+agent-shell-projects--worker-git-count 0)
               (first (+agent-shell-projects--worker-capture request))
               (calls +agent-shell-projects--worker-git-count))
          (should (<= calls 7))
          (setq +agent-shell-projects--worker-git-count 0)
          (should (equal first (+agent-shell-projects--worker-capture request)))
          (should (<= +agent-shell-projects--worker-git-count 5))
          ;; Two bases in one workspace share both untracked scans.
          (let ((original (symbol-function '+agent-shell-projects-git)) (scans 0))
            (cl-letf (((symbol-function '+agent-shell-projects-git)
                       (lambda (workspace &rest args)
                         (when (equal (car args) "ls-files") (cl-incf scans))
                         (apply original workspace args))))
              (+agent-shell-projects--worker-capture
               (list :root root :targets
                     (list (list :project "p" :artefact "a" :base base)
                           (list :project "p" :artefact "b" :base second-base)))))
            (should (= 2 scans))))))))

(ert-deftest +agent-shell-projects-test-capture-coalescing-and-stale-results ()
  (+agent-shell-projects-test--isolated
    (let* ((root (+agent-shell-projects-test--repository directory))
           (artefact (list :id "a" :name "Code" :kind "code" :path root :base "HEAD" :history nil))
           (+agent-shell-projects-capture-delay 60)
           (blob (+agent-shell-projects-store-content "{}"))
           (callbacks 0) completions)
      (+agent-shell-projects--set project :artefacts (list artefact))
      (+agent-shell-projects-save project)
      (cl-letf (((symbol-function '+agent-shell-projects-start-worker)
                 (lambda (_request callback &optional _progress)
                   (push callback completions) (list :fake t))))
        (dotimes (_ 20)
          (+agent-shell-projects-capture-code project artefact nil (lambda (_v) (cl-incf callbacks))))
        (let ((state (car (hash-table-values +agent-shell-projects--jobs))))
          (should (= 1 (length (plist-get state :pending))))
          (cancel-timer (plist-get state :timer))
          (+agent-shell-projects--start-capture state)
          (dotimes (_ 20) (+agent-shell-projects-capture-code project artefact))
          (should (= 1 (length (plist-get state :pending))))
          (funcall (pop completions)
                   (list :ok t :result (list (list :artefact "a" :root root :base "old" :blob blob))))
          (should-not (plist-get artefact :current))
          (should (= 0 callbacks))
          (cancel-timer (plist-get state :timer))
          (+agent-shell-projects--start-capture state)
          (funcall (pop completions)
                   (list :ok t :result (list (list :artefact "a" :root root :base "fresh" :blob blob))))
          (should (equal "fresh" (plist-get artefact :base)))
          (should (= 20 callbacks))
          (should (= 1 (length (plist-get artefact :history))))
          (should (= 0 (hash-table-count +agent-shell-projects--jobs))))))))

(ert-deftest +agent-shell-projects-test-cancel-and-timeout-preserve-review ()
  (dolist (timeout '(nil t))
    (+agent-shell-projects-test--isolated
      (let* ((root (+agent-shell-projects-test--repository directory))
             (artefact (+agent-shell-projects-test--enrol-code project root))
             (old (plist-get artefact :current))
             (bin (+agent-shell-projects-test--slow-git directory 0.2))
             (process-environment (cons (concat "PATH=" bin ":" (getenv "PATH")) process-environment))
             (+agent-shell-projects-worker-timeout (if timeout 0.1 10)))
        (+agent-shell-projects-test--file root "tracked.txt" "new content\n")
        (+agent-shell-projects-capture-code project artefact)
        (+agent-shell-projects-test--await (lambda () (> (hash-table-count +agent-shell-projects--workers) 0)))
        (unless timeout (+agent-shell-projects-cancel-capture root))
        (+agent-shell-projects-test--await
         (lambda () (and (= 0 (hash-table-count +agent-shell-projects--workers))
                         (= 0 (hash-table-count +agent-shell-projects--jobs)))))
        (should (equal old (plist-get artefact :current)))
        (should (string-match-p (if timeout "timed out" "cancelled") (plist-get artefact :capture-error)))))))

(ert-deftest +agent-shell-projects-test-streaming-and-hidden-ui-stay-quiet ()
  (+agent-shell-projects-test--isolated
    (+agent-shell-projects-test--agent
      (let* ((file (+agent-shell-projects-test--file directory "spec.md" "Proposal\n"))
             (artefact (+agent-shell-projects-enrol project file))
             (agent (+agent-shell-projects-associate project shell))
             (+agent-shell-projects-change-hook (list #'+agent-shell-projects--refresh-ui))
             (saves 0) (captures 0) sidebar view)
        (save-window-excursion
          (+agent-shell-projects-open project (plist-get artefact :id))
          (setq view (current-buffer)
                sidebar (get-buffer (format "*Comments: %s*" (plist-get artefact :id))))
          (delete-windows-on sidebar)
          (switch-to-buffer (get-buffer-create " *Unrelated project test*"))
          (cl-letf (((symbol-function '+agent-shell-projects-save) (lambda (&rest _) (cl-incf saves)))
                    ((symbol-function '+agent-shell-projects-capture) (lambda (&rest _) (cl-incf captures))))
            (dotimes (_ 100)
              (+agent-shell-projects--agent-event (plist-get project :id) (plist-get agent :id)
                                                 '((:event . agent-message-chunk))))
            (should (= 0 saves))
            (should (= 0 captures))
            (should (= 0 (hash-table-count +agent-shell-projects--ui-pending)))
            (+agent-shell-projects--set agent :role "side")
            (+agent-shell-projects--agent-event (plist-get project :id) (plist-get agent :id)
                                               '((:event . turn-complete)))
            (should (= 0 (hash-table-count +agent-shell-projects--event-timers))))
          (+agent-shell-projects--set artefact :event 100)
          (+agent-shell-projects--refresh-ui project nil)
          (+agent-shell-projects--refresh-visible)
          (should-not (get-buffer-window sidebar t))
          (should-not (get-buffer-window view t))
          (should-not (= 100 (buffer-local-value '+agent-shell-projects--observed-event view))))
        (kill-buffer " *Unrelated project test*")))))

(ert-deftest +agent-shell-projects-test-lazy-diff-bounded-and-old-new-offsets ()
  (+agent-shell-projects-test--isolated
    (let* ((root (+agent-shell-projects-test--repository directory))
           (_ (+agent-shell-projects-test--file root "tracked.txt" "α original\n"))
           (artefact (+agent-shell-projects-test--enrol-code project root))
           (old (plist-get artefact :current)))
      (+agent-shell-projects-acknowledge project (list (list (plist-get artefact :id) old 0)))
      (+agent-shell-projects-test--file root "tracked.txt"
                                        (mapconcat (lambda (n) (format "β line %d\n" n)) (number-sequence 1 1000) ""))
      (let ((version (+agent-shell-projects-test--capture-code project artefact)))
        (save-window-excursion
          (cl-letf (((symbol-function 'process-file) (lambda (&rest _) (error "Synchronous render process"))))
            (+agent-shell-projects-render-code project artefact version "diff" "reviewed")
            (should-not (string-match-p "β line 1" (buffer-string)))
            (should (= 0 (hash-table-count +agent-shell-projects--workers)))
            (let ((file-section (car (oref magit-root-section children))))
              (magit-section-show file-section)
              (+agent-shell-projects-test--await
               (lambda () (and (null +agent-shell-projects--worker-queue)
                               (= 0 (hash-table-count +agent-shell-projects--workers)))))
              (should-not (string-match-p "β line 1" (buffer-string)))
              (magit-section-show (car (oref file-section children))))
            (should (string-match-p "-α original" (buffer-string)))
            (should (string-match-p "+β line 1" (buffer-string)))
            (should-not (string-match-p "β line 1000" (buffer-string)))
            (should (string-match-p "Load more lines" (buffer-string)))
            (goto-char (point-min)) (search-forward "+β line 1") (beginning-of-line)
            (should (= 0 (get-text-property (point) 'project-diff-offset)))
            (should (equal "new" (get-text-property (point) 'project-diff-side)))
            (goto-char (point-min)) (search-forward "-α original") (beginning-of-line)
            (should (= 0 (get-text-property (point) 'project-diff-offset)))
            (should (equal "old" (get-text-property (point) 'project-diff-side)))))))))

(ert-deftest +agent-shell-projects-test-opening-saved-code-never-captures ()
  (+agent-shell-projects-test--isolated
    (let* ((root (+agent-shell-projects-test--repository directory))
           (artefact (+agent-shell-projects-test--enrol-code project root)))
      (save-window-excursion
        (cl-letf (((symbol-function '+agent-shell-projects-capture-code)
                   (lambda (&rest _) (error "Unexpected capture while viewing")))
                  ((symbol-function 'process-file) (lambda (&rest _) (error "Synchronous process while viewing"))))
          (+agent-shell-projects-open project (plist-get artefact :id))
          (+agent-shell-projects-review-code)
          (should (derived-mode-p '+agent-shell-projects-code-mode)))))))

(ert-deftest +agent-shell-projects-test-line-offset-vectors-are-independent ()
  (should (equal [0 2 4] (+agent-shell-projects--line-offsets "α\nβ\n")))
  (should (equal [0 4] (+agent-shell-projects--line-offsets "abc\n")))
  (should (equal [0] (+agent-shell-projects--line-offsets "single"))))

(ert-deftest +agent-shell-projects-test-cached-diff-navigation-and-annotation-reveal ()
  (+agent-shell-projects-test--isolated
    (let* ((root (+agent-shell-projects-test--repository directory))
           (_ (+agent-shell-projects-test--file root "tracked.txt" "new behavior\n"))
           (artefact (+agent-shell-projects-test--enrol-code project root))
           (version (+agent-shell-projects-version artefact (plist-get artefact :current))) annotation)
      (save-window-excursion
        (+agent-shell-projects-test--render-code project artefact version)
        (goto-char (point-min)) (search-forward "+new behavior")
        (set-mark (- (point) 12))
        (let* ((transient-mark-mode t) (mark-active t)
               (selection (+agent-shell-projects--selection)))
          (setq annotation (+agent-shell-projects-annotate
                            project artefact version (plist-get selection :start)
                            (plist-get selection :end) "Explain" "user" (plist-get selection :context))))
        ;; Reusing prepared content must wait until Magit has finished section markers.
        (+agent-shell-projects-test--render-code project artefact version)
        (should (string-match-p "+new behavior" (buffer-string)))
        (+agent-shell-projects--visit-code-annotation project annotation)
        (+agent-shell-projects-test--await (lambda () (looking-at "new behavior")))
        (should (equal "diff" +agent-shell-projects--code-view))))))

(ert-deftest +agent-shell-projects-test-workers-respect-global-limit ()
  (+agent-shell-projects-test--isolated
    (let* ((root (+agent-shell-projects-test--repository directory))
           (bin (+agent-shell-projects-test--slow-git directory 0.03))
           (process-environment (cons (concat "PATH=" bin ":" (getenv "PATH")) process-environment))
           (+agent-shell-projects-worker-limit 1)
           (completed 0))
      (dotimes (n 3)
        (+agent-shell-projects-start-worker
         (list :kind "capture" :root root
               :targets (list (list :project "p" :artefact (number-to-string n) :base "HEAD")))
         (lambda (result) (should (eq (plist-get result :ok) t)) (cl-incf completed))))
      (+agent-shell-projects-test--await
       (lambda () (should (<= (hash-table-count +agent-shell-projects--workers) 1))
         (= completed 3))))))

(provide 'agent-shell-projects-async-test)
;;; agent-shell-projects-async-test.el ends here
