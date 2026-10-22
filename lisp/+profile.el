;;; +profile.el --- Profiling commands and file measurements -*- lexical-binding: t; -*-

;;; Commentary:
;; Profile one command or a file visit. Measurements never close existing files.

;;; Code:

(require 'cl-lib)
(require 'profiler)

(defvar +profile--owned nil)
(defvar +profile--timer nil)

(defun +profile-cancel ()
  "Cancel pending profiling and stop a recording started by this library."
  (interactive)
  (remove-hook 'pre-command-hook #'+profile--start-next)
  (remove-hook 'post-command-hook #'+profile--finish-next)
  (when (timerp +profile--timer) (cancel-timer +profile--timer))
  (setq +profile--timer nil)
  (when +profile--owned (profiler-stop))
  (setq +profile--owned nil))

(defun +profiler-stop-and-report (&optional continue-p)
  "Show the profiler report, stopping unless CONTINUE-P is non-nil."
  (interactive "P")
  (unless continue-p
    (+profile-cancel)
    (when (profiler-running-p) (profiler-stop)))
  (profiler-report))

(defun +profile--prepare ()
  "Clear our previous session and check for an independent recording."
  (+profile-cancel)
  (when (profiler-running-p)
    (user-error "Profiler already recording; stop it before starting another")))

(defun +profile--start-next ()
  "Start recording just before the next command."
  (unless (memq this-command '(+profile-next-command +profile-cancel))
    (remove-hook 'pre-command-hook #'+profile--start-next)
    (profiler-start 'cpu+mem)
    (setq +profile--owned t)))

(defun +profile--finish-next ()
  "Stop and report after the recorded command, including a quit command."
  (when +profile--owned (+profiler-stop-and-report)))

;;;###autoload
(defun +profile-next-command ()
  "Record the next command, then stop and display its profiler report."
  (interactive)
  (+profile--prepare)
  (add-hook 'pre-command-hook #'+profile--start-next)
  (add-hook 'post-command-hook #'+profile--finish-next)
  (message "Execute the command to profile; M-x +profile-cancel cancels"))

;;;###autoload
(defun +profile-find-file ()
  "Profile a file visit and its next two idle seconds."
  (interactive)
  (let ((file (read-file-name "File to profile: " nil nil t)))
    (+profile--prepare)
    (profiler-start 'cpu+mem)
    (setq +profile--owned t)
    (condition-case err
        (progn
          (find-file file)
          (setq +profile--timer
                (run-with-idle-timer 2 nil #'+profiler-stop-and-report)))
      ((error quit) (+profile-cancel) (signal (car err) (cdr err))))))

;;;###autoload
(defun +benchmark-find-file (file-path &optional iterations)
  "Measure visits to unvisited FILE-PATH, returning a plist of samples.
ITERATIONS defaults to five and accepts a numeric prefix argument.
The first sample includes package activation; subsequent samples reuse
loaded packages and filesystem caches.  No garbage collection is forced.
A buffer modified during a visit is retained and the benchmark stops."
  (interactive (list (read-file-name "File to benchmark: " nil nil t)
                     current-prefix-arg))
  (setq file-path (expand-file-name file-path)
        iterations (if iterations (prefix-numeric-value iterations) 5))
  (unless (and (integerp iterations) (> iterations 0))
    (user-error "Iterations must be a positive integer"))
  (unless (file-regular-p file-path) (user-error "Choose an existing file"))
  (when (find-buffer-visiting file-path)
    (user-error "File already visited; use an unvisited disposable fixture"))
  (let (samples)
    (dotimes (_ iterations)
      (let ((start (float-time)) (gc-start gc-elapsed) (gcs-start gcs-done)
            buffer)
        (unwind-protect
            (progn
              (setq buffer (find-file-noselect file-path))
              (push (list :seconds (- (float-time) start)
                          :gc-seconds (- gc-elapsed gc-start)
                          :gc-count (- gcs-done gcs-start))
                    samples))
          (when (buffer-live-p buffer)
            (when (buffer-modified-p buffer)
              (user-error "Benchmark buffer changed; kept %s" (buffer-name buffer)))
            (unless (kill-buffer buffer)
              (user-error "Benchmark buffer could not be closed"))))))
    (setq samples (nreverse samples))
    (let ((result (list :file file-path :samples samples)))
      (message "First visit %.3fs; %d visits, %.3fs average, %.3fs GC"
               (plist-get (car samples) :seconds) iterations
               (/ (apply #'+ (mapcar (lambda (s) (plist-get s :seconds)) samples))
                  (float iterations))
               (apply #'+ (mapcar (lambda (s) (plist-get s :gc-seconds)) samples)))
      result)))

(provide '+profile)
;;; +profile.el ends here
