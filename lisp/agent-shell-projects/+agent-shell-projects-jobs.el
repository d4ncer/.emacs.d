;;; +agent-shell-projects-jobs.el --- Asynchronous workspace jobs -*- lexical-binding: t; -*-

;;; Commentary:
;; One debounced capture per workspace. Child Emacs processes prepare immutable
;; blobs; only this Emacs commits versions and changes project metadata.

;;; Code:

(require '+agent-shell-projects-core)
(defcustom +agent-shell-projects-capture-delay 0.3
  "Seconds to coalesce workspace capture requests."
  :type 'number :group '+agent-shell-projects)
(defcustom +agent-shell-projects-worker-timeout 300
  "Maximum seconds allowed for a workspace worker."
  :type 'number :group '+agent-shell-projects)
(defvar +agent-shell-projects--jobs (make-hash-table :test #'equal))
(defcustom +agent-shell-projects-worker-limit 2
  "Maximum number of isolated workers running at once."
  :type 'integer :group '+agent-shell-projects)
(defvar +agent-shell-projects--worker-queue nil)
(defvar +agent-shell-projects--worker-pump nil)
(defvar +agent-shell-projects--workers (make-hash-table :test #'eq))
(defvar-local +agent-shell-projects--project-id nil)
(defvar-local +agent-shell-projects--artefact-id nil)
(defvar +agent-shell-projects-job-hook nil
  "Functions called with PROJECT when background job status changes.")
(defconst +agent-shell-projects--worker-library
  (expand-file-name "+agent-shell-projects-worker.el"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun +agent-shell-projects--worker-finish (process)
  "Deliver PROCESS's completed result, then release its resources."
  (when-let* ((job (gethash process +agent-shell-projects--workers)))
    (remhash process +agent-shell-projects--workers)
    (unwind-protect
        (let* ((file (plist-get job :file))
               (result (if (file-exists-p (concat file ".result"))
                           (+agent-shell-projects--read-json (concat file ".result"))
                         (list :ok :false :error
                               (if (plist-get job :timed-out) "Worker timed out"
                                 (format "Worker exited (%s)" (process-exit-status process)))))))
          (funcall (plist-get job :callback) result))
      (dolist (key '(:timeout :progress-timer))
        (when (timerp (plist-get job key)) (+agent-shell-projects-cancel-timer (plist-get job key))))
      (dolist (suffix '("" ".result" ".progress"))
        (when (file-exists-p (concat (plist-get job :file) suffix))
          (delete-file (concat (plist-get job :file) suffix))))
      (when (buffer-live-p (process-buffer process)) (kill-buffer (process-buffer process)))
      (+agent-shell-projects--pump-workers))))

(defun +agent-shell-projects--stop-worker (handle)
  "Cancel a queued or running worker HANDLE and its subprocess group."
  (let ((process (if (processp handle) handle (plist-get handle :process))))
    (if process
        (when (process-live-p process)
          (ignore-errors (interrupt-process process))
          (delete-process process))
      (setq +agent-shell-projects--worker-queue
            (delq handle +agent-shell-projects--worker-queue))
      (when (file-exists-p (plist-get handle :file))
        (delete-file (plist-get handle :file)))
      (funcall (plist-get handle :callback) (list :ok :false :error "Cancelled")))))

(defun +agent-shell-projects--pump-workers ()
  "Launch queued workers within the concurrency limit."
  (setq +agent-shell-projects--worker-pump nil)
  (while (and +agent-shell-projects--worker-queue
              (< (hash-table-count +agent-shell-projects--workers)
                 +agent-shell-projects-worker-limit))
    (let ((job (pop +agent-shell-projects--worker-queue)))
      (condition-case err (+agent-shell-projects--launch-worker job)
        (error (funcall (plist-get job :callback)
                        (list :ok :false :error (error-message-string err)))
               (when (file-exists-p (plist-get job :file))
                 (delete-file (plist-get job :file))))))))

(defun +agent-shell-projects-start-worker (request callback &optional progress)
  "Run REQUEST outside the editor; deliver CALLBACK and optional PROGRESS."
  (let* ((file (make-temp-file "agent-project-job-"))
         (request (append request (list :store (expand-file-name +agent-shell-projects-directory))))
         (job (list :file file :callback callback :progress progress :process nil)))
    (+agent-shell-projects--atomic-write file (+agent-shell-projects--json request))
    (setq +agent-shell-projects--worker-queue
          (if (equal (plist-get request :kind) "capture")
              (cons job +agent-shell-projects--worker-queue)
            (nconc +agent-shell-projects--worker-queue (list job))))
    (unless (timerp +agent-shell-projects--worker-pump)
      (setq +agent-shell-projects--worker-pump
            (+agent-shell-projects-run-at-time 0 nil #'+agent-shell-projects--pump-workers)))
    job))

(defun +agent-shell-projects--launch-worker (job)
  "Start the queued worker JOB."
  (let* ((file (plist-get job :file))
         (progress (plist-get job :progress))
         (form `(progn (require '+agent-shell-projects-worker)
                       (+agent-shell-projects-worker-main ,file)))
         (default-directory temporary-file-directory)
         process)
    (condition-case err
        (setq process
              (make-process
               :name "agent-project-worker" :buffer (generate-new-buffer " *Project worker*")
               :command (list (expand-file-name invocation-name invocation-directory)
                              "-Q" "--batch" "-L" (file-name-directory +agent-shell-projects--worker-library)
                              "--eval" (prin1-to-string form))
               :connection-type 'pipe :noquery t :sentinel #'ignore))
      (error (delete-file file) (signal (car err) (cdr err))))
    (+agent-shell-projects--set job :process process)
    (puthash process job +agent-shell-projects--workers)
    (+agent-shell-projects--set
     job :timeout (+agent-shell-projects-run-at-time +agent-shell-projects-worker-timeout nil
                              (lambda ()
                                (+agent-shell-projects--set job :timed-out t)
                                (+agent-shell-projects--stop-worker process))))
    (when progress
      (+agent-shell-projects--set
       job :progress-timer
       (+agent-shell-projects-run-at-time
        0.25 0.25
        (lambda ()
          (when (file-exists-p (concat file ".progress"))
            (let ((phase (plist-get (+agent-shell-projects--read-json (concat file ".progress")) :phase)))
              (unless (equal phase (plist-get job :phase))
                (+agent-shell-projects--set job :phase phase)
                (funcall progress phase))))))))
    (set-process-sentinel process (lambda (p _event)
                                   (when (memq (process-status p) '(exit signal))
                                     (+agent-shell-projects--worker-finish p))))
    (when (memq (process-status process) '(exit signal)) (+agent-shell-projects--worker-finish process))
    process))

(defun +agent-shell-projects--job-notify (state)
  "Notify the projects associated with capture STATE."
  (dolist (id (delete-dups (mapcar (lambda (r) (plist-get r :project))
                                 (append (plist-get state :active) (plist-get state :pending)))))
    (run-hook-with-args '+agent-shell-projects-job-hook (+agent-shell-projects-load id))))

(defun +agent-shell-projects-schedule-capture (project artefact &optional actor callback)
  "Coalesce capture of PROJECT ARTEFACT, attributing edits to ACTOR.
CALLBACK receives a version after a successful, current capture."
  (let* ((root (file-name-as-directory (file-truename (plist-get artefact :path))))
         (state (or (gethash root +agent-shell-projects--jobs)
                    (puthash root (list :root root :pending nil :active nil :phase "Capture queued")
                             +agent-shell-projects--jobs)))
         (pending (seq-find (lambda (r) (equal (plist-get r :artefact) (plist-get artefact :id)))
                            (plist-get state :pending))))
    (unless pending
      (setq pending (list :project (plist-get project :id) :artefact (plist-get artefact :id)
                          :actor actor :callbacks nil))
      (+agent-shell-projects--set state :pending (cons pending (plist-get state :pending))))
    (+agent-shell-projects--set pending :actor actor)
    (when callback
      (+agent-shell-projects--set pending :callbacks (cons callback (plist-get pending :callbacks))))
    (unless (or (plist-get state :process) (timerp (plist-get state :timer)))
      (+agent-shell-projects--set state :timer
                                 (+agent-shell-projects-run-at-time +agent-shell-projects-capture-delay nil
                                              #'+agent-shell-projects--start-capture state)))
    (+agent-shell-projects--job-notify state)
    state))

(defun +agent-shell-projects--capture-target (request)
  "Freeze the proposal and base references for capture REQUEST."
  (let* ((project (+agent-shell-projects-load (plist-get request :project)))
         (artefact (+agent-shell-projects-artefact project (plist-get request :artefact)))
         (proposal (and (plist-get project :proposal)
                        (+agent-shell-projects-artefact project (plist-get project :proposal)))))
    (list :project (plist-get project :id) :artefact (plist-get artefact :id)
          :base (plist-get artefact :base)
          :proposal (when proposal (list :artefact (plist-get proposal :id)
                                        :version (plist-get proposal :current))))))

(defun +agent-shell-projects--start-capture (state)
  "Start the next coalesced capture in STATE."
  (+agent-shell-projects--set state :timer nil)
  (+agent-shell-projects--set state :active (plist-get state :pending))
  (+agent-shell-projects--set state :pending nil)
  (+agent-shell-projects--set state :phase "Starting capture")
  (condition-case err
      (+agent-shell-projects--set
       state :process
       (+agent-shell-projects-start-worker
        (list :kind "capture" :root (plist-get state :root)
              :targets (mapcar #'+agent-shell-projects--capture-target (plist-get state :active)))
        (lambda (result) (+agent-shell-projects--capture-finished state result))
        (lambda (phase)
          (+agent-shell-projects--set state :phase phase)
          (+agent-shell-projects--job-notify state))))
    (error (+agent-shell-projects--capture-finished
            state (list :ok :false :error (error-message-string err))))))

(defun +agent-shell-projects--capture-finished (state result)
  "Commit capture RESULT from STATE, discarding superseded results."
  (when (eq state (gethash (plist-get state :root) +agent-shell-projects--jobs))
    (+agent-shell-projects--set state :process nil)
    (dolist (request (plist-get state :active))
      (let* ((id (plist-get request :artefact))
             (project (+agent-shell-projects-load (plist-get request :project)))
             (artefact (+agent-shell-projects-artefact project id))
             (newer (seq-find (lambda (r) (equal (plist-get r :artefact) id)) (plist-get state :pending)))
             (entry (seq-find (lambda (r) (equal (plist-get r :artefact) id)) (plist-get result :result))))
        (cond
         (newer (+agent-shell-projects--set
                 newer :callbacks (append (plist-get request :callbacks) (plist-get newer :callbacks))))
         ((and (eq (plist-get result :ok) t) entry)
          (+agent-shell-projects--set artefact :base (plist-get entry :base))
          (+agent-shell-projects--set artefact :path (file-name-as-directory (plist-get entry :root)))
          (+agent-shell-projects--set artefact :capture-error nil)
          (let ((version (+agent-shell-projects-snapshot-blob
                          project artefact (plist-get entry :blob) (plist-get request :actor))))
            (+agent-shell-projects-save project)
            (dolist (callback (plist-get request :callbacks))
              (condition-case err (funcall callback version)
                (error (message "Project capture callback: %s" (error-message-string err)))))))
         (t (+agent-shell-projects--set artefact :capture-error (or (plist-get result :error) "Capture failed"))
            (+agent-shell-projects-save project)))))
    (if (plist-get state :pending)
        (progn
          (+agent-shell-projects--set state :phase "Capture queued")
          (+agent-shell-projects--set state :timer (+agent-shell-projects-run-at-time +agent-shell-projects-capture-delay nil
                                                               #'+agent-shell-projects--start-capture state)))
      (remhash (plist-get state :root) +agent-shell-projects--jobs))
    (+agent-shell-projects--job-notify state)))

(defun +agent-shell-projects-cancel-capture (&optional root)
  "Cancel background capture for ROOT or the workspace at point."
  (interactive
   (let* ((project (+agent-shell-projects-load +agent-shell-projects--project-id))
          (roots (delete-dups
                  (mapcar (lambda (a) (plist-get a :path))
                          (seq-filter (lambda (a) (equal (plist-get a :kind) "code"))
                                      (plist-get project :artefacts))))))
     (list (if (= (length roots) 1) (car roots)
             (completing-read "Cancel workspace capture: " roots nil t)))))
  (let* ((root (or root (plist-get (+agent-shell-projects-artefact
                                   (+agent-shell-projects-load +agent-shell-projects--project-id)
                                   +agent-shell-projects--artefact-id) :path)))
         (key (file-name-as-directory (file-truename root)))
         (state (gethash key +agent-shell-projects--jobs)))
    (unless state (user-error "No capture running for this workspace"))
    (remhash key +agent-shell-projects--jobs)
    (dolist (request (append (plist-get state :active) (plist-get state :pending)))
      (let* ((project (+agent-shell-projects-load (plist-get request :project)))
             (artefact (+agent-shell-projects-artefact project (plist-get request :artefact))))
        (+agent-shell-projects--set artefact :capture-error "Capture cancelled")
        (+agent-shell-projects-save project)))
    (when (timerp (plist-get state :timer)) (+agent-shell-projects-cancel-timer (plist-get state :timer)))
    (when (plist-get state :process) (+agent-shell-projects--stop-worker (plist-get state :process)))
    (+agent-shell-projects--job-notify state)))

(defun +agent-shell-projects-stop-workers ()
  "Cancel queued captures and workers, releasing their temporary files."
  (when (timerp +agent-shell-projects--worker-pump)
    (+agent-shell-projects-cancel-timer +agent-shell-projects--worker-pump))
  (setq +agent-shell-projects--worker-pump nil)
  (maphash (lambda (_root state)
             (when (timerp (plist-get state :timer))
               (+agent-shell-projects-cancel-timer (plist-get state :timer))))
           +agent-shell-projects--jobs)
  (clrhash +agent-shell-projects--jobs)
  (let ((queue +agent-shell-projects--worker-queue))
    (setq +agent-shell-projects--worker-queue nil)
    (dolist (handle queue) (+agent-shell-projects--stop-worker handle)))
  (dolist (process (hash-table-keys +agent-shell-projects--workers))
    (+agent-shell-projects--stop-worker process)
    (when (gethash process +agent-shell-projects--workers)
      (+agent-shell-projects--worker-finish process))))

(provide '+agent-shell-projects-jobs)
;;; +agent-shell-projects-jobs.el ends here
