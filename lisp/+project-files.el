;;; +project-files.el --- Async file finding for large projects -*- lexical-binding: t; -*-

;;; Commentary:
;; Keep Projectile's discovery rules, but index in a child Emacs and search a
;; saved NUL-separated manifest outside the editor.  Consult and Vertico only
;; receive a bounded set of ranked matches.  Queries are literal AND terms.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'projectile)
(require 'seq)
(require 'subr-x)

(declare-function consult--read "consult")
(declare-function consult--async-pipeline "consult")
(declare-function consult--async-throttle "consult")
(declare-function consult--async-highlight "consult")
(declare-function consult--file-preview "consult")
(declare-function orderless--highlight "orderless" (regexps ignore-case string))
(defvar consult-async-split-style)
(defvar consult-async-refresh-delay)
(defvar recentf-list)

(defgroup +project-files nil
  "Asynchronous file finding in large projects."
  :group 'projectile)

(defcustom +project-files-directory
  (locate-user-emacs-file ".cache/project-files/")
  "Directory for persistent filename manifests."
  :type 'directory :group '+project-files)

(defcustom +project-files-large-projects nil
  "Project roots which should always use the asynchronous finder."
  :type '(repeat directory) :group '+project-files)

(defcustom +project-files-threshold 100000
  "Cached file count above which to use the asynchronous finder."
  :type 'natnum :group '+project-files)

(defcustom +project-files-git-index-threshold (* 16 1024 1024)
  "Git index size suggesting a large project before files are cached."
  :type 'natnum :group '+project-files)

(defcustom +project-files-refresh-age 60
  "Age in seconds after which opening the picker starts a refresh.
The previous snapshot remains searchable while refreshing."
  :type 'number :group '+project-files)

(defcustom +project-files-candidate-limit 500
  "Initial number of ranked results to display.
Inside the picker, C-c C-a doubles this limit."
  :type 'natnum :group '+project-files)

(defcustom +project-files-index-timeout 300
  "Maximum seconds allowed for a background index."
  :type 'number :group '+project-files)

(defconst +project-files--library-directory
  (file-name-directory (or load-file-name buffer-file-name)))

(defconst +project-files--settings
  '(projectile-indexing-method projectile-alien-honors-ignores
    projectile-globally-ignored-files projectile-globally-ignored-directories
    projectile-globally-ignored-file-suffixes
    projectile-globally-ignored-file-regexps
    projectile-globally-unignored-files projectile-globally-unignored-directories
    projectile-fd-executable projectile-git-use-fd projectile-git-fd-args
    projectile-git-command projectile-git-submodule-command
    projectile-hg-command projectile-fossil-command projectile-bzr-command
    projectile-darcs-command projectile-pijul-command projectile-svn-command
    projectile-sapling-command projectile-jj-command projectile-generic-command)
  "Discovery settings inherited by the isolated indexer.")

(cl-defstruct (+project-files--state (:constructor +project-files--make-state))
  root manifest process timeout error signature count
  (revision 0) (overlay (make-hash-table :test #'equal)))

(defvar +project-files--states (make-hash-table :test #'equal))
(defvar +project-files--history nil)
(defvar-local +project-files--more-function nil)
(defvar-local +project-files--refresh-function nil)

(defun +project-files--state (root)
  "Return the manifest state for ROOT, reading its small metadata file."
  (or (gethash root +project-files--states)
      (let* ((manifest (expand-file-name (concat (secure-hash 'sha1 root) ".nul")
                                         +project-files-directory))
             (meta (ignore-errors
                     (with-temp-buffer
                       (insert-file-contents (concat manifest ".meta"))
                       (read (current-buffer)))))
             (state (+project-files--make-state
                     :root root :manifest manifest
                     :signature (plist-get meta :signature)
                     :count (plist-get meta :count))))
        (puthash root state +project-files--states)
        state)))

(defun +project-files--git-directory (root)
  "Return ROOT's Git directory, including for linked worktrees."
  (let ((path (expand-file-name ".git" root)))
    (cond ((file-directory-p path) path)
          ((file-regular-p path)
           (with-temp-buffer
             (insert-file-contents path nil 0 4096)
             (when (looking-at "gitdir: \\(.*\\)")
               (expand-file-name (string-trim (match-string 1)) root)))))))

(defun +project-files--large-p (root)
  "Decide whether ROOT is large without enumerating its files."
  (and (not (file-remote-p root))
       (or (seq-some (lambda (dir)
                       (equal root (file-name-as-directory
                                    (expand-file-name dir))))
                     +project-files-large-projects)
           (and (nthcdr (max 0 (1- +project-files-threshold))
                        (gethash root projectile-projects-cache))
                t)
           (when-let* ((git (+project-files--git-directory root))
                       (attrs (file-attributes (expand-file-name "index" git))))
             (>= (file-attribute-size attrs)
                 +project-files-git-index-threshold)))))

(defun +project-files--settings ()
  "Capture the current buffer's Projectile discovery settings."
  (cl-loop for symbol in +project-files--settings
           when (boundp symbol) collect (cons symbol (symbol-value symbol))))

(defun +project-files--signature (root settings)
  "Fingerprint ROOT's discovery SETTINGS and common invalidation markers."
  (let* ((git (+project-files--git-directory root))
         (markers (append '(".projectile" ".gitignore" ".ignore" ".fdignore")
                          (when git
                            (list (expand-file-name "index" git)
                                  (expand-file-name "HEAD" git))))))
    (secure-hash
     'sha1 (prin1-to-string
            (list settings
                  (mapcar (lambda (file)
                            (let ((attrs (file-attributes
                                          (expand-file-name file root))))
                              (and attrs
                                   (list (file-attribute-size attrs)
                                         (file-attribute-modification-time attrs)
                                         (file-attribute-inode-number attrs)))))
                          markers))))))

(defun +project-files--generation (state)
  "Return a cheap identity for STATE's current snapshot and overlay."
  (let ((attrs (file-attributes (+project-files--state-manifest state))))
    (list (+project-files--state-revision state)
          (and attrs (file-attribute-modification-time attrs))
          (and attrs (file-attribute-inode-number attrs)))))

(defun +project-files--finish (state process request signature revision)
  "Collect PROCESS's index result for STATE and remove REQUEST."
  (when (eq process (+project-files--state-process state))
    (when (timerp (+project-files--state-timeout state))
      (cancel-timer (+project-files--state-timeout state)))
    (setf (+project-files--state-process state) nil
          (+project-files--state-timeout state) nil)
    (if (and (eq (process-status process) 'exit)
             (zerop (process-exit-status process)))
        (let ((meta (with-temp-buffer
                      (insert-file-contents
                       (concat (+project-files--state-manifest state) ".meta"))
                      (read (current-buffer)))))
          (setf (+project-files--state-count state) (plist-get meta :count)
                (+project-files--state-signature state) signature
                (+project-files--state-error state) nil)
          (when (= revision (+project-files--state-revision state))
            (clrhash (+project-files--state-overlay state)))
          (cl-incf (+project-files--state-revision state))
          (kill-buffer (process-buffer process)))
      (setf (+project-files--state-error state)
            (format "Index failed; see %s" (buffer-name (process-buffer process))))
      (message "Project files: %s" (+project-files--state-error state))))
  (when (file-exists-p request) (delete-file request)))

(defun +project-files--refresh (state &optional force)
  "Refresh STATE outside Emacs if stale, or unconditionally with FORCE."
  (unless (process-live-p (+project-files--state-process state))
    (let* ((root (+project-files--state-root state))
           (settings (+project-files--settings))
           (signature (+project-files--signature root settings))
           (attrs (file-attributes (+project-files--state-manifest state))))
      (when (or force (null attrs)
                (not (equal signature (+project-files--state-signature state)))
                (> (- (float-time)
                      (float-time (file-attribute-modification-time attrs)))
                   +project-files-refresh-age))
        (make-directory +project-files-directory t)
        (let* ((request (make-temp-file
                         (expand-file-name "index-" +project-files-directory)))
               (revision (+project-files--state-revision state))
               (worker (expand-file-name "+project-files-worker.el"
                                         +project-files--library-directory))
               (buffer (generate-new-buffer " *Project files index*"))
               process)
          (with-temp-file request
            (let ((print-length nil) (print-level nil))
              (prin1
               (list :root root :output (+project-files--state-manifest state)
                     :settings settings :signature signature
                     :load-path
                     (delete-dups
                      (mapcar (lambda (name)
                                (file-name-directory (locate-library name)))
                              '("projectile" "compat"))))
               (current-buffer))))
          (condition-case err
              (setq process
                    (make-process
                     :name "project-files-index" :buffer buffer :noquery t
                     :connection-type 'pipe :sentinel #'ignore
                     :command
                     (list (expand-file-name invocation-name invocation-directory)
                           "-Q" "--batch" "-l" worker "--eval"
                           (prin1-to-string
                            `(+project-files-worker-main ,request)))))
            (error (delete-file request) (kill-buffer buffer)
                   (signal (car err) (cdr err))))
          (setf (+project-files--state-process state) process
                (+project-files--state-error state) nil
                (+project-files--state-timeout state)
                (run-at-time +project-files-index-timeout nil
                             (lambda ()
                               (when (process-live-p process)
                                 (delete-process process)))))
          (set-process-sentinel
           process (lambda (proc _event)
                     (when (memq (process-status proc) '(exit signal))
                       (+project-files--finish state proc request signature
                                               revision))))
          (when (memq (process-status process) '(exit signal))
            (+project-files--finish state process request signature revision)))))))

(defun +project-files--recent (root)
  "Return up to 200 recently used relative filenames in ROOT."
  (let* ((history (gethash root (projectile--frecency-data)))
         (now (projectile-time-seconds))
         (ranked (when history
                   (sort (hash-table-keys history)
                         (lambda (a b)
                           (> (projectile--frecency-score (gethash a history) now)
                              (projectile--frecency-score (gethash b history) now))))))
         (absolute (append (delq nil (mapcar #'buffer-file-name (buffer-list)))
                           (mapcar (lambda (file) (expand-file-name file root))
                                   ranked)
                           (bound-and-true-p recentf-list)))
         (seen (make-hash-table :test #'equal))
         result)
    (dolist (file absolute)
      (when (and (< (length result) 200) (string-prefix-p root file))
        (let ((relative (substring file (length root))))
          (when (and (not (gethash relative seen))
                     (not (projectile-ignored-file-p relative root))
                     (file-regular-p file))
            (puthash relative t seen)
            (push relative result)))))
    (nreverse result)))

(defun +project-files--preferences (state recent directory)
  "Return STATE's overlay, RECENT files and current DIRECTORY."
  (let (include exclude)
    (maphash (lambda (file action)
               (if (eq action 'add) (push file include) (push file exclude)))
             (+project-files--state-overlay state))
    `(:recent ,(vconcat recent) :directory ,directory
      :include ,(vconcat include) :exclude ,(vconcat exclude))))

(defun +project-files-more-candidates ()
  "Double the number of displayed matches in the active file picker."
  (interactive)
  (when +project-files--more-function (funcall +project-files--more-function)))

(defun +project-files-refresh-picker ()
  "Refresh the active file picker's snapshot in the background."
  (interactive)
  (when +project-files--refresh-function
    (funcall +project-files--refresh-function)))

(defun +project-files--highlight (input)
  "Highlight INPUT's literal terms using the user's Orderless match faces."
  (when-let* ((terms (split-string input nil t)))
    (require 'orderless)
    (let ((regexps (mapcar #'regexp-quote terms)))
      (lambda (candidate)
        (orderless--highlight regexps t candidate)))))

(defun +project-files--start-search (rg filter ready sentinel)
  "Start a Python worker using RG, with FILTER, READY and SENTINEL callbacks."
  (let* ((python (or (executable-find "python3")
                     (user-error "File finder needs python3")))
         (process
          (make-process
           :name "project-files-search" :noquery t
           :connection-type 'pipe :coding 'utf-8-unix
           :stderr (get-buffer-create "*Project file search errors*")
           :command (list python "-I" "-S"
                          (expand-file-name "+project-files-search.py"
                                            +project-files--library-directory)
                          "--serve" "--rg" rg)
           :filter filter :sentinel sentinel)))
    (process-put process 'channel process)
    (funcall ready process)
    (list :process process)))

(defun +project-files--stop-search (handle)
  "Close HANDLE's input and let its worker release resources."
  (let ((process (plist-get handle :process)))
    (when (process-live-p process)
      (process-send-eof process)
      (run-at-time 0.5 nil
                   (lambda ()
                     (when (process-live-p process)
                       (delete-process process)))))))

(defun +project-files--search-response (_process line)
  "Decode a response LINE from the search worker."
  (json-parse-string line :object-type 'plist :array-type 'list))

(defun +project-files--source (state recent directory rg)
  "Build a Consult source searching STATE in a Python worker using RG."
  (lambda (sink)
    (let* ((limit (max 1 +project-files-candidate-limit))
           (input "") (timer nil) (generation nil) (destroyed nil)
           (handle nil) (channel nil) (request-id 0) (pending "")
           (cancel (lambda ()
                     (cl-incf request-id)
                     (when (process-live-p channel)
                       (process-send-string
                        channel (format "{\"id\":%d,\"query\":null}\n"
                                        request-id)))))
           (update
            (lambda ()
              (funcall sink input)
              (cl-incf request-id)
              (if (< (length (string-trim input)) 2)
                  (let* ((case-fold-search t)
                         (terms (split-string input nil t))
                         (matches
                          (seq-filter
                           (lambda (file)
                             (seq-every-p
                              (lambda (term)
                                (string-match-p (regexp-quote term) file))
                              terms))
                           recent)))
                    (funcall cancel)
                    (funcall sink 'flush)
                    (when matches
                      (funcall sink (mapcar #'substring-no-properties matches)))
                    (funcall sink 'refresh))
                (when (process-live-p channel)
                  (funcall sink [indicator running])
                  (process-send-string
                   channel
                   (concat
                    (json-serialize
                     (list :id request-id
                           :manifest (+project-files--state-manifest state)
                           :query input :limit limit
                           :preferences
                           (+project-files--preferences state recent directory)))
                    "\n")))))))
      (lambda (action)
        (pcase action
          ('setup
           (funcall sink action)
           (setq handle
                 (+project-files--start-search
                  rg
                  (lambda (process output)
                    (unless destroyed
                      (setq pending (concat pending output))
                      (while (string-match "\n" pending)
                        (let* ((end (match-end 0))
                               (response
                                (+project-files--search-response
                                 process (substring pending 0 (1- end)))))
                          (setq pending (substring pending end))
                          (when (eql (plist-get response :id) request-id)
                            (if-let* ((error (plist-get response :error)))
                                (progn
                                  (funcall sink [indicator failed])
                                  (message "Project file search: %s" error))
                              (funcall sink 'flush)
                              (when-let* ((files
                                           (plist-get response :candidates)))
                                (funcall sink files))
                              (funcall sink [indicator finished])
                              (funcall sink 'refresh)))))))
                  (lambda (connection)
                    (setq channel connection)
                    (unless destroyed (funcall update)))
                  (lambda (proc _event)
                    (when (and (not destroyed)
                               (memq (process-status proc) '(exit signal)))
                      (funcall sink [indicator failed])
                      (message "Project file search stopped; see %s"
                               "*Project file search errors*")))))
           (setq-local
            +project-files--more-function
            (lambda ()
              (setq limit (* 2 limit))
              (funcall update)
              (minibuffer-message "Showing up to %d matches" limit))
            +project-files--refresh-function
            (lambda ()
              (+project-files--refresh state t)
              (minibuffer-message "Refreshing project files…")))
           (setq generation (+project-files--generation state))
           (funcall update)
           (setq timer
                 (run-at-time
                  0.2 0.2
                  (lambda ()
                    (unless destroyed
                      (let ((next (+project-files--generation state)))
                        (unless (equal generation next)
                          (setq generation next)
                          (funcall update))))))))
          ('destroy
           (setq destroyed t)
           (when timer (cancel-timer timer))
           (+project-files--stop-search handle)
           (funcall sink action))
          ('cancel (funcall cancel) (funcall sink action))
          ((pred stringp)
           (setq input action)
           (funcall update))
          (_ (funcall sink action)))))))

;;;###autoload
(defun +project-find-file-fast (&optional refresh)
  "Find project files asynchronously using literal AND terms.
With prefix argument REFRESH, rebuild the snapshot in the background.
C-c C-r refreshes; C-c C-a doubles the displayed result limit.
Use `projectile-find-file' for full Orderless matching syntax."
  (interactive "P")
  (let* ((root (projectile-acquire-root))
         (rg (executable-find "rg")))
    (when (file-remote-p root)
      (user-error "Use projectile-find-file for remote projects"))
    (unless rg (user-error "File finder needs rg"))
    (unless (executable-find "python3")
      (user-error "File finder needs python3"))
    (require 'consult)
    (let* ((state (+project-files--state root))
           (recent (+project-files--recent root))
           (directory (if (string-prefix-p root
                                           (expand-file-name default-directory))
                          (let ((relative
                                 (file-relative-name default-directory root)))
                            (if (equal relative "./") "" relative)) ""))
           (default-directory root)
           (consult-async-split-style 'none)
           (consult-async-refresh-delay 0.02)
           (map (make-sparse-keymap)))
      (+project-files--refresh state refresh)
      (when (+project-files--state-error state)
        (message "Project files: %s" (+project-files--state-error state)))
      (define-key map (kbd "C-c C-r") #'+project-files-refresh-picker)
      (define-key map (kbd "C-c C-a") #'+project-files-more-candidates)
      (when-let* ((file
                   (consult--read
                    (consult--async-pipeline
                     (consult--async-throttle 0.03 0.015)
                     (+project-files--source state recent directory rg)
                     (consult--async-highlight #'+project-files--highlight))
                    :prompt (format "[%s] Find file (terms): "
                                    (projectile-project-name root))
                    :sort nil :require-match t :category 'project-file
                    :history '+project-files--history :keymap map
                    :preview-key "C-SPC" :state (consult--file-preview))))
        (let ((absolute (expand-file-name file root)))
          (unless (file-exists-p absolute)
            (+project-files--refresh state t)
            (user-error "File disappeared; refreshing project files"))
          (find-file absolute)
          (run-hooks 'projectile-find-file-hook))))))

;;;###autoload
(defun +project-find-file (&optional refresh)
  "Find a file, using asynchronous search for large local projects.
Pass prefix argument REFRESH to the chosen finder."
  (interactive "P")
  (let ((root (projectile-acquire-root)))
    (if (and (+project-files--large-p root)
             (executable-find "rg")
             (executable-find "python3"))
        (+project-find-file-fast refresh)
      (projectile-find-file refresh))))

;;;###autoload
(defun +project-files-refresh ()
  "Rebuild the current project's filename snapshot in the background."
  (interactive)
  (let ((root (projectile-acquire-root)))
    (when (file-remote-p root) (user-error "Remote snapshots are unsupported"))
    (+project-files--refresh (+project-files--state root) t)
    (message "Refreshing files for %s" root)))

(defun +project-files--remember-file (file action)
  "Record a local FILE addition or deletion ACTION in active snapshots."
  (unless (or (file-remote-p file)
              (string-prefix-p (file-name-as-directory
                                (expand-file-name +project-files-directory))
                               (expand-file-name file)))
    (setq file (expand-file-name file))
    (maphash
     (lambda (root state)
       (when (string-prefix-p root file)
         (let ((relative (substring file (length root))))
           (when (or (eq action 'delete)
                     (and (not (projectile-ignored-file-p relative root))
                          (seq-some (lambda (dir) (string-prefix-p dir file))
                                    (projectile-get-project-directories root))
                          (not (projectile-vcs-ignored-file-p file root))))
             (puthash relative action (+project-files--state-overlay state))
             (cl-incf (+project-files--state-revision state))))))
     +project-files--states)))

(defun +project-files--after-save ()
  "Make saved files immediately searchable in active snapshots."
  (when buffer-file-name
    (+project-files--remember-file buffer-file-name 'add)))

(defun +project-files--after-delete (file &rest _)
  "Hide deleted FILE immediately in active snapshots."
  (+project-files--remember-file file 'delete))

(defun +project-files--invalidate (root)
  "Refresh an existing ROOT snapshot when Projectile invalidates its cache."
  (when-let* ((state (gethash root +project-files--states)))
    (+project-files--refresh state t)))

(defun +project-files--watch-directories (_original project files)
  "Count PROJECT's FILES only until the watch limit is exceeded.
Return at most `projectile-watch-max-directories' plus one directories."
  (let ((dirs (make-hash-table :test #'equal)))
    (puthash (file-name-as-directory (expand-file-name project)) t dirs)
    (catch 'limit
      (dolist (file files)
        (dolist (dir (projectile--directory-ancestors file))
          (puthash (expand-file-name dir project) t dirs)
          (when (> (hash-table-count dirs) projectile-watch-max-directories)
            (throw 'limit nil)))))
    (hash-table-keys dirs)))

(defun +project-files-setup ()
  "Install bounded watch counting and snapshot update hooks."
  (advice-add 'projectile--watch-directories :around
              #'+project-files--watch-directories)
  (advice-add 'projectile--invalidate-project-cache :after
              #'+project-files--invalidate)
  (advice-add 'delete-file :after #'+project-files--after-delete)
  (add-hook 'after-save-hook #'+project-files--after-save))

(provide '+project-files)
;;; +project-files.el ends here
