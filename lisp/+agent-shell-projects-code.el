;;; +agent-shell-projects-code.el --- Git workspace review -*- lexical-binding: t; -*-

;;; Commentary:
;; Capture exact workspace bytes without touching Git's index or working files.
;; Present saved comparisons with expandable Magit-style files and hunks.

;;; Code:

(require '+agent-shell-projects)
(require 'diff-mode)

(defvar-local +agent-shell-projects--comparison "base")
(defvar-local +agent-shell-projects--code-view "diff")
(defvar-local +agent-shell-projects--focus-files nil)
(defvar +agent-shell-projects-code-render-functions nil)
(defvar +agent-shell-projects-code-selection-functions nil)
(defvar +agent-shell-projects--positions (make-hash-table :test #'equal))
(defvar +agent-shell-projects--manifest-cache (make-hash-table :test #'equal))

(require '+agent-shell-projects-worker)
(require '+agent-shell-projects-jobs)

(defun +agent-shell-projects-insert-job-status (_project artefact)
  "Insert capture progress and cancellation for ARTEFACT in a sidebar."
  (when (equal (plist-get artefact :kind) "code")
    (let* ((root (file-name-as-directory (file-truename (plist-get artefact :path))))
           (state (gethash root +agent-shell-projects--jobs)))
      (when state
        (insert (or (plist-get state :phase) "Capturing") " · ")
        (insert-text-button "Cancel" 'follow-link t
                            'action (lambda (_button) (+agent-shell-projects-cancel-capture root)))
        (insert "\n\n")))))

(defun +agent-shell-projects--job-ui (project)
  "Refresh PROJECT's visible job status and fill first-capture placeholders."
  (+agent-shell-projects--refresh-ui project nil t)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (equal +agent-shell-projects--project-id (plist-get project :id))
                 (derived-mode-p '+agent-shell-projects-code-mode)
                 (null +agent-shell-projects--version-id))
        (when-let* ((window (get-buffer-window buffer t))
                    (artefact (+agent-shell-projects--artefact))
                    (id (plist-get artefact :current)))
          (with-selected-window window
            (+agent-shell-projects-render-code project artefact
                                               (+agent-shell-projects-version artefact id)
                                               +agent-shell-projects--code-view)))))))

(add-hook '+agent-shell-projects-job-hook #'+agent-shell-projects--job-ui)

(defun +agent-shell-projects-capture-current ()
  "Capture asynchronously and refresh this view when it is still visible."
  (interactive)
  (let ((project (+agent-shell-projects--project))
        (artefact (+agent-shell-projects--artefact))
        (buffer (current-buffer))
        (previous +agent-shell-projects--version-id))
    (+agent-shell-projects-capture-code
     project artefact nil
     (lambda (version)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (when (equal previous +agent-shell-projects--version-id)
             (when-let* ((window (get-buffer-window buffer t)))
               (with-selected-window window
                 (+agent-shell-projects-render-code project artefact version
                                                    +agent-shell-projects--code-view
                                                    +agent-shell-projects--comparison))))))))))

(defun +agent-shell-projects-capture-code (project artefact &optional actor callback)
  "Schedule PROJECT ARTEFACT capture, optionally calling CALLBACK with a version.
Return the last completed version, or nil while the first capture is pending."
  (+agent-shell-projects-schedule-capture project artefact actor callback)
  (when (plist-get artefact :current)
    (+agent-shell-projects-version artefact (plist-get artefact :current))))

(defun +agent-shell-projects-enrol-code (project root &optional base name)
  "Register ROOT in PROJECT and schedule its first capture against BASE."
  (let* ((root (or (locate-dominating-file root ".git")
                   (user-error "Not a Git workspace: %s" root)))
         (artefact (list :id (+agent-shell-projects--id)
                         :name (or name "Implementation") :kind "code"
                         :path (file-truename root) :base (or base "HEAD")
                         :history nil :current nil :ack nil :event 0 :ack-event 0)))
    (+agent-shell-projects--set project :root (plist-get artefact :path))
    (+agent-shell-projects--set project :artefacts
                               (append (plist-get project :artefacts) (list artefact)))
    (+agent-shell-projects--changed project nil "user")
    (+agent-shell-projects-capture-code project artefact)
    artefact))

(defun +agent-shell-projects-add-code (root base)
  "Register ROOT and display its saved capture or capture progress."
  (interactive (list (read-directory-name "Git workspace: ")
                     (read-string "Base revision: " "HEAD")))
  (let* ((project (+agent-shell-projects--project))
         (artefact (+agent-shell-projects-enrol-code project root base)))
    (+agent-shell-projects-open project (plist-get artefact :id))))

(defun +agent-shell-projects--manifest-file (manifest path &optional fallback _root)
  "Find PATH's saved state in MANIFEST, using FALLBACK base contents or ROOT."
  (or (+agent-shell-projects--indexed-file manifest path)
      (and (equal (plist-get manifest :base) (plist-get fallback :base))
           (+agent-shell-projects--indexed-file fallback path t))
      (list :path path :content nil :mode nil)))

(defun +agent-shell-projects--comparison-pair (artefact version comparison)
  "Return (LEFT RIGHT) manifests for ARTEFACT VERSION and COMPARISON."
  (let* ((right (+agent-shell-projects-code-manifest version))
         (ack (if (string-prefix-p "version:" comparison)
                  (substring comparison 8) (plist-get artefact :ack)))
         (left (if (and (not (equal comparison "base")) ack)
                   (+agent-shell-projects-code-manifest
                    (+agent-shell-projects-version artefact ack))
                 (list :base (plist-get right :base)
                       :files (plist-get right :base-files)))))
    (list left right)))

(defun +agent-shell-projects--line-offset (text number)
  "Return zero-based character offset of line NUMBER in TEXT."
  (with-temp-buffer
    (insert text) (goto-char (point-min)) (forward-line (max 0 (1- number)))
    (1- (point))))

(defun +agent-shell-projects--insert-diff-lines (path diff old new)
  "Insert DIFF for PATH, tracking source locations within OLD and NEW."
  (let ((old-line 1) (new-line 1) (lines (split-string diff "\n"))
        (old-offsets (+agent-shell-projects--line-offsets old))
        (new-offsets (+agent-shell-projects--line-offsets new)) hunk current)
    (dolist (line (cddr lines))
      (cond
       ((string-match "^@@ -\\([0-9]+\\)\\(?:,[0-9]+\\)? +\\+\\([0-9]+\\)" line)
        (setq old-line (string-to-number (match-string 1 line))
              new-line (string-to-number (match-string 2 line))
              current (list :header line :old old-line :new new-line :lines nil))
        (push current hunk))
       ((and hunk (not (string-empty-p line)))
        (let* ((record current)
               (side (if (string-prefix-p "-" line) "old" "new"))
               (number (if (equal side "old") old-line new-line)))
          (+agent-shell-projects--set
           record :lines (cons (list :line line :side side :number number) (plist-get record :lines)))
          (cond ((string-prefix-p "-" line) (cl-incf old-line))
                ((string-prefix-p "+" line) (cl-incf new-line))
                ((string-prefix-p " " line) (cl-incf old-line) (cl-incf new-line)))))))
    (dolist (record (nreverse hunk))
      (magit-insert-section (hunk (cons path (plist-get record :header)))
        (magit-insert-heading
         (propertize (plist-get record :header) 'face 'diff-hunk-header))
        (dolist (entry (nreverse (plist-get record :lines)))
          (let* ((line (plist-get entry :line)) (side (plist-get entry :side))
                 (text (if (equal side "old") old new)) (start (point)))
            (insert (propertize line 'face
                                (cond ((string-prefix-p "+" line) 'diff-added)
                                      ((string-prefix-p "-" line) 'diff-removed)
                                      (t 'default))) "\n")
            (unless (string-prefix-p "\\" line)
              (add-text-properties
               start (point)
               (list 'project-diff-file path 'project-diff-side side
                     'project-diff-text text
                     'project-diff-offset
                     (aref (if (equal side "old") old-offsets new-offsets)
                           (max 0 (1- (plist-get entry :number)))))))))))))

(define-derived-mode +agent-shell-projects-code-mode magit-section-mode
  "Project Code" "Inspect captured project code and its explanations.")

(defvar-local +agent-shell-projects--render-token nil)
(defvar-local +agent-shell-projects--render-pending 0)
(defvar +agent-shell-projects--diff-cache (make-hash-table :test #'equal))
(defvar +agent-shell-projects--file-index (make-hash-table :test #'eq :weakness 'key))

(defun +agent-shell-projects--indexed-file (manifest path &optional base-only)
  "Look up PATH in MANIFEST using an index, optionally BASE-ONLY."
  (let ((indexes (gethash manifest +agent-shell-projects--file-index)))
    (unless indexes
      (let ((files (make-hash-table :test #'equal))
            (bases (make-hash-table :test #'equal)))
        (dolist (file (plist-get manifest :base-files))
          (puthash (plist-get file :path) file bases)
          (puthash (plist-get file :path) file files))
        (dolist (file (plist-get manifest :files))
          (puthash (plist-get file :path) file files))
        (setq indexes (cons files bases))
        (puthash manifest indexes +agent-shell-projects--file-index)))
    (gethash path (if base-only (cdr indexes) (car indexes)))))

(defun +agent-shell-projects--section-insert (section function &optional replace)
  "Insert FUNCTION's output inside SECTION, optionally REPLACE its body."
  (let ((inhibit-read-only t)
        (magit-insert-section--parent section)
        (magit-insert-section--current section)
        (lineage (magit-section-lineage section t)))
    (save-excursion
      (dolist (s lineage) (set-marker-insertion-type (oref s end) t))
      (unwind-protect
          (progn
            (if replace
                (progn (goto-char (oref section content))
                       (delete-region (point) (oref section end))
                       (oset section children nil))
              (goto-char (oref section end)))
            (funcall function)
            (magit-section--set-section-properties section))
        (dolist (s lineage) (set-marker-insertion-type (oref s end) nil)))
      (when (oref section hidden) (magit-section-hide section)))))

(defun +agent-shell-projects--more (label section callback)
  "Insert a LABEL button continuing SECTION through CALLBACK."
  (let ((start (point-marker)) end)
    (insert-text-button label 'follow-link t
                        'action (lambda (_button)
                                  (let ((inhibit-read-only t)) (delete-region start end))
                                  (+agent-shell-projects--section-insert section callback)))
    (insert "\n")
    (setq end (copy-marker (point)))))

(defun +agent-shell-projects--insert-file-list (artefact version comparison focus)
  "Insert a bounded list of lazy file sections for VERSION."
  (pcase-let* ((`(,left ,right) (+agent-shell-projects--comparison-pair artefact version comparison))
               (paths (sort (delete-dups
                             (mapcar (lambda (f) (plist-get f :path))
                                     (append (plist-get left :files) (plist-get right :files)))) #'string<)))
    (setq paths (seq-filter
                 (lambda (path)
                   (let ((old (+agent-shell-projects--manifest-file left path right))
                         (new (+agent-shell-projects--manifest-file right path left)))
                     (and (or (not focus) (member path focus))
                          (not (and (equal (plist-get old :content) (plist-get new :content))
                                    (equal (plist-get old :mode) (plist-get new :mode))))))) paths))
    (+agent-shell-projects--insert-files artefact version comparison left right paths)))

(defun +agent-shell-projects--insert-files (artefact version comparison left right paths)
  "Insert the next hundred file headers from PATHS."
  (let ((batch (seq-take paths 100)) (parent magit-insert-section--current))
    (dolist (path batch)
      (let ((old (+agent-shell-projects--manifest-file left path right))
            (new (+agent-shell-projects--manifest-file right path left)))
        (magit-insert-section (file path t)
          (magit-insert-heading
           (format "%s %s" (cond ((null (plist-get new :content)) "deleted")
                                  ((null (plist-get old :content)) "new") (t "modified")) path))
          (magit-insert-section-body
            (when (not (equal (plist-get old :mode) (plist-get new :mode)))
              (insert (format "mode %s → %s\n" (plist-get old :mode) (plist-get new :mode))))
            (if (or (plist-get old :binary) (plist-get new :binary))
                (insert "Binary contents changed\n")
              (+agent-shell-projects--load-file-diff artefact version comparison path
                                                    magit-insert-section--current))))))
    (when (nthcdr (length batch) paths)
      (+agent-shell-projects--more
       "Load more files" parent
       (lambda () (+agent-shell-projects--insert-files
                   artefact version comparison left right (nthcdr (length batch) paths)))))))

(defun +agent-shell-projects--load-file-diff (artefact version comparison path section)
  "Prepare PATH outside Emacs and populate SECTION when ready."
  (let* ((ack (if (string-prefix-p "version:" comparison) (substring comparison 8)
                (and (not (equal comparison "base")) (plist-get artefact :ack))))
         (left (and ack (plist-get (+agent-shell-projects-version artefact ack) :blob)))
         (right (plist-get version :blob))
         (key (list +agent-shell-projects-directory left right path))
         (entry (gethash key +agent-shell-projects--diff-cache))
         (buffer (current-buffer)) (token +agent-shell-projects--render-token)
         (ready (lambda (result)
                  (when (buffer-live-p buffer)
                    (with-current-buffer buffer
                      (when (eq token +agent-shell-projects--render-token)
                        (cl-decf +agent-shell-projects--render-pending)
                        (+agent-shell-projects--section-insert
                         section
                         (lambda ()
                           (if (eq (plist-get result :ok) t)
                               (let ((data (+agent-shell-projects--read-json
                                            (+agent-shell-projects--blob-file
                                             (plist-get (plist-get result :result) :blob)))))
                                 (unless (equal (plist-get (plist-get data :old) :mode)
                                                (plist-get (plist-get data :new) :mode))
                                   (insert (format "mode %s → %s\n"
                                                   (plist-get (plist-get data :old) :mode)
                                                   (plist-get (plist-get data :new) :mode))))
                                 (+agent-shell-projects--insert-hunks path data (plist-get data :hunks)))
                             (insert (format "Diff preparation failed: %s\n" (plist-get result :error)))))
                         t)))))))
    (cl-incf +agent-shell-projects--render-pending)
    (insert "Preparing diff…\n")
    (cond
     ((plist-get entry :result) (run-at-time 0 nil ready (plist-get entry :result)))
     (entry (+agent-shell-projects--set entry :waiters (cons ready (plist-get entry :waiters))))
     (t (setq entry (list :waiters (list ready)))
        (puthash key entry +agent-shell-projects--diff-cache)
        (+agent-shell-projects-start-worker
         (list :kind "diff" :left left :right right :path path)
         (lambda (result)
           (if (eq (plist-get result :ok) t)
               (+agent-shell-projects--set entry :result result)
             (remhash key +agent-shell-projects--diff-cache))
           (dolist (callback (plist-get entry :waiters)) (funcall callback result))
           (+agent-shell-projects--set entry :waiters nil)))))))

(defun +agent-shell-projects--insert-hunks (path data hunks)
  "Insert the next hundred lazy hunk headers in HUNKS."
  (let ((batch (seq-take hunks 100)) (parent magit-insert-section--current))
    (dolist (hunk batch)
      (magit-insert-section (hunk (cons path (plist-get hunk :header)) t)
        (magit-insert-heading (propertize (plist-get hunk :header) 'face 'diff-hunk-header))
        (magit-insert-section-body
          (+agent-shell-projects--insert-diff-blocks path data (plist-get hunk :blocks)))))
    (when (nthcdr (length batch) hunks)
      (+agent-shell-projects--more "Load more hunks" parent
                                  (lambda () (+agent-shell-projects--insert-hunks
                                              path data (nthcdr (length batch) hunks)))))))

(defun +agent-shell-projects--insert-diff-blocks (path data blocks)
  "Insert at most 200 prepared diff lines from BLOCKS, without subprocesses."
  (when blocks
    (let ((start (point)))
    (dolist (entry (+agent-shell-projects--read-json (+agent-shell-projects--blob-file (car blocks))))
      (let* ((line (plist-get entry :line)) (start (point))
             (side (plist-get entry :side))
             (record (plist-get data (if (equal side "old") :old :new))))
        (insert (propertize line 'face (cond ((string-prefix-p "+" line) 'diff-added)
                                             ((string-prefix-p "-" line) 'diff-removed)
                                             (t 'default))) "\n")
        (unless (string-prefix-p "\\" line)
          (add-text-properties start (point)
                               (list 'project-diff-file path 'project-diff-side side
                                     'project-diff-offset (plist-get entry :offset)
                                     'project-diff-blob (plist-get record :content))))))
    (+agent-shell-projects--decorate-code
     (+agent-shell-projects--project) (+agent-shell-projects--artefact)
     (+agent-shell-projects-version (+agent-shell-projects--artefact) +agent-shell-projects--version-id)
     start (point)))
    (when (cdr blocks)
      (+agent-shell-projects--more "Load more lines" magit-insert-section--current
                                  (lambda () (+agent-shell-projects--insert-diff-blocks path data (cdr blocks)))))))

(defun +agent-shell-projects--diff-text-at (position)
  "Return saved source text for the diff line at POSITION."
  (or (get-text-property position 'project-diff-text)
      (let ((blob (get-text-property position 'project-diff-blob)))
        (if blob (decode-coding-string (+agent-shell-projects--file-bytes (list :content blob)) 'utf-8-unix)
          ""))))

(defun +agent-shell-projects-render-code (project artefact version &optional view comparison focus)
  "Render PROJECT's code ARTEFACT VERSION in VIEW using COMPARISON and FOCUS."
  (let* ((buffer (get-buffer-create (format "*Code: %s [%s]*"
                                           (plist-get artefact :name)
                                           (substring (plist-get artefact :id) 0 6))))
         (view (or view "diff"))
         (comparison (or comparison "base")))
    (with-current-buffer buffer
      (unless (derived-mode-p '+agent-shell-projects-code-mode)
        (+agent-shell-projects-code-mode))
      (when +agent-shell-projects--version-id
        (puthash (list +agent-shell-projects--artefact-id
                       +agent-shell-projects--version-id
                       +agent-shell-projects--code-view
                       +agent-shell-projects--comparison
                       +agent-shell-projects--focus-files)
                 (point) +agent-shell-projects--positions))
      (setq +agent-shell-projects--render-token (make-symbol "render")
            +agent-shell-projects--render-pending 0
            +agent-shell-projects--project-id (plist-get project :id)
            +agent-shell-projects--artefact-id (plist-get artefact :id)
            +agent-shell-projects--version-id (plist-get version :id)
            +agent-shell-projects--observed-event (plist-get artefact :event)
            +agent-shell-projects--code-view view
            +agent-shell-projects--comparison comparison
            +agent-shell-projects--focus-files focus)
      (+agent-shell-projects-artefact-mode 1)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (unless (run-hook-with-args-until-success
                 '+agent-shell-projects-code-render-functions project artefact version view)
          (magit-insert-section (code)
            (magit-insert-heading (format "%s · %s" (plist-get artefact :name)
                                         (if (equal comparison "reviewed") "since review" "whole solution")))
            (if version
                (+agent-shell-projects--insert-file-list artefact version comparison focus)
              (insert "Waiting for the first capture. Progress is shown in the sidebar.\n"))))
        (goto-char
         (min (point-max)
              (or (gethash (list (plist-get artefact :id) (plist-get version :id)
                                 view comparison focus) +agent-shell-projects--positions)
                  (point-min))))
        (set-buffer-modified-p nil)))
    (pop-to-buffer buffer)
    (when version
      (run-hook-with-args '+agent-shell-projects-view-functions project artefact version))
    (+agent-shell-projects-sidebar buffer)
    buffer))

(defun +agent-shell-projects--visit-code (project artefact version)
  "Visit code ARTEFACT VERSION in PROJECT when appropriate."
  (when (equal (plist-get artefact :kind) "code")
    (+agent-shell-projects-render-code project artefact version)
    t))

(defun +agent-shell-projects--diff-selection ()
  "Return context for a selected diff passage or another code view."
  (when (derived-mode-p '+agent-shell-projects-code-mode)
    (or (run-hook-with-args-until-success
         '+agent-shell-projects-code-selection-functions)
        (let* ((begin (region-beginning)) (end (region-end))
               (last (if (and (> end begin) (eq (char-before end) ?\n)) (1- end) end))
               (path (get-text-property begin 'project-diff-file))
               (side (get-text-property begin 'project-diff-side))
               (text (+agent-shell-projects--diff-text-at begin)))
          (unless path (user-error "Select source lines inside a diff hunk"))
          (save-excursion
            (goto-char begin)
            (while (< (point) last)
              (unless (and (equal path (get-text-property (point) 'project-diff-file))
                           (equal side (get-text-property (point) 'project-diff-side)))
                (user-error "Select one file and one diff side"))
              (forward-line 1)))
          (let ((start (+ (get-text-property begin 'project-diff-offset)
                          (save-excursion (goto-char begin)
                                          (max 0 (1- (- begin (line-beginning-position)))))))
                (finish (+ (get-text-property last 'project-diff-offset)
                           (save-excursion (goto-char last)
                                           (max 0 (1- (- last (line-beginning-position)))))
                           (if (= last end) 0 1))))
            (list :project +agent-shell-projects--project-id
                  :artefact +agent-shell-projects--artefact-id
                  :version +agent-shell-projects--version-id
                  :start start :end (min finish (length text))
                  :context (list :space "file" :path path :side side :text text
                                 :comparison
                                 (if (and (equal +agent-shell-projects--comparison "reviewed")
                                          (plist-get (+agent-shell-projects--artefact) :ack))
                                     (concat "version:" (plist-get (+agent-shell-projects--artefact) :ack))
                                   +agent-shell-projects--comparison))))))))

(defun +agent-shell-projects-compare (&optional target)
  "Toggle whole-solution versus since-review comparison for code TARGET."
  (interactive)
  (when target
    (let ((context (+agent-shell-projects--target-context target)))
      (+agent-shell-projects-open
       (+agent-shell-projects-load (plist-get context :project))
       (plist-get context :artefact))))
  (unless (derived-mode-p '+agent-shell-projects-code-mode)
    (user-error "Open a code artefact first"))
  (+agent-shell-projects-render-code
   (+agent-shell-projects--project) (+agent-shell-projects--artefact)
   (+agent-shell-projects-version (+agent-shell-projects--artefact)
                                 +agent-shell-projects--version-id)
   "diff" (if (equal +agent-shell-projects--comparison "base") "reviewed" "base")))

(defun +agent-shell-projects-code-edit ()
  "Edit the current source file at this diff location."
  (interactive)
  (if (button-at (point)) (push-button)
  (let* ((path (get-text-property (point) 'project-diff-file))
         (offset (get-text-property (point) 'project-diff-offset))
         (project (+agent-shell-projects--project))
         (artefact (+agent-shell-projects--artefact)))
    (unless path (user-error "Move to a source line in the diff"))
    (let* ((file (expand-file-name path (plist-get artefact :path)))
           (saved (+agent-shell-projects--diff-text-at (point))))
      (find-file file)
      (widen)
      (goto-char (point-min))
      (if (equal saved (buffer-substring-no-properties (point-min) (point-max)))
          (goto-char (min (point-max) (1+ offset)))
        (message "Source differs from this diff; inspect current contents"))
      (setq +agent-shell-projects--project-id (plist-get project :id)
            +agent-shell-projects--artefact-id (plist-get artefact :id)
            +agent-shell-projects--version-id (plist-get artefact :current)
            +agent-shell-projects--observed-event (plist-get artefact :event))
      (+agent-shell-projects-artefact-mode 1)
      (add-hook 'after-save-hook #'+agent-shell-projects--code-source-saved nil t)
      (+agent-shell-projects-sidebar (current-buffer))))))

(defun +agent-shell-projects--code-source-saved ()
  "Schedule capture after a source save without delaying normal editing."
  (when (and +agent-shell-projects--project-id +agent-shell-projects--artefact-id)
    (let ((buffer (current-buffer))
          (project (+agent-shell-projects--project))
          (artefact (+agent-shell-projects--artefact)))
      (+agent-shell-projects-capture-code
       project artefact "user"
       (lambda (version)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (setq +agent-shell-projects--version-id (plist-get version :id)))))))))

(defun +agent-shell-projects--code-selection ()
  "Capture a passage selected in a code diff, explanation, or source file."
  (if (derived-mode-p '+agent-shell-projects-code-mode)
      (+agent-shell-projects--diff-selection)
    (when (and buffer-file-name +agent-shell-projects--artefact-id
               (equal (plist-get (+agent-shell-projects--artefact) :kind) "code"))
      (when (buffer-modified-p) (user-error "Save source edits before attaching code feedback"))
      (let* ((project (+agent-shell-projects--project))
             (artefact (+agent-shell-projects--artefact))
             (version (and (plist-get artefact :current)
                           (+agent-shell-projects-version artefact (plist-get artefact :current)))))
        (unless (and version
                     (let* ((path (file-relative-name buffer-file-name (plist-get artefact :path)))
                            (record (+agent-shell-projects--manifest-file
                                     (+agent-shell-projects-code-manifest version) path)))
                       (equal (decode-coding-string (or (+agent-shell-projects--file-bytes record) "") 'utf-8-unix)
                              (save-restriction (widen) (buffer-substring-no-properties (point-min) (point-max))))))
          (+agent-shell-projects-capture-code project artefact "user")
          (user-error "Capturing source edits; annotate after capture completes"))
        (save-restriction
        (widen)
        (list :project (plist-get project :id) :artefact (plist-get artefact :id)
              :version (plist-get version :id)
              :start (1- (region-beginning)) :end (1- (region-end))
              :context (list :space "file" :side "new" :comparison "base"
                             :path (file-relative-name buffer-file-name (plist-get artefact :path))
                             :text (buffer-substring-no-properties (point-min) (point-max)))))))))

(defun +agent-shell-projects--code-annotation-text (artefact version annotation)
  "Return the appropriate source text for ANNOTATION on code VERSION."
  (let* ((context (plist-get (or (plist-get annotation :anchor) annotation) :context))
         (path (plist-get context :path)))
    (if (member (plist-get context :space) '("summary" "walkthrough"))
        (if (equal (plist-get context :space) +agent-shell-projects--code-view)
            (buffer-substring-no-properties (point-min) (point-max))
          (plist-get context :text))
    (when path
      (pcase-let* ((`(,left ,right) (+agent-shell-projects--comparison-pair
                                    artefact version +agent-shell-projects--comparison))
                   (side (if (equal (plist-get context :side) "old") left right))
                   (other (if (eq side left) right left))
                   (file (+agent-shell-projects--manifest-file
                          side path other (plist-get artefact :path))))
        (decode-coding-string (or (+agent-shell-projects--file-bytes file) "") 'utf-8-unix))))))

(defun +agent-shell-projects--visit-code-annotation (project annotation)
  "Visit a code ANNOTATION's exact original comparison in PROJECT."
  (let* ((anchor (or (plist-get annotation :anchor) annotation))
         (context (plist-get anchor :context))
         (path (plist-get context :path)))
    (if (member (plist-get context :space) '("summary" "walkthrough"))
        (let ((artefact (+agent-shell-projects-artefact project (plist-get annotation :artefact))))
          (+agent-shell-projects-render-code
           project artefact (+agent-shell-projects-version artefact (plist-get anchor :version))
           (plist-get context :space))
          (goto-char (min (point-max) (1+ (plist-get anchor :start))))
          t)
    (when path
      (let ((artefact (+agent-shell-projects-artefact
                       project (plist-get annotation :artefact))))
        (+agent-shell-projects-render-code
         project artefact
         (+agent-shell-projects-version artefact (plist-get anchor :version))
         "diff" (or (plist-get context :comparison) "base") (list path))
        (+agent-shell-projects--reveal-annotation (current-buffer)
                                                  +agent-shell-projects--render-token anchor)
        t)))))

(defun +agent-shell-projects--reveal-annotation (buffer token anchor)
  "Reveal ANCHOR in BUFFER incrementally while its render TOKEN is current."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (eq token +agent-shell-projects--render-token)
        (let* ((context (plist-get anchor :context))
               (target (plist-get anchor :start)) found pending)
          (save-excursion
            (goto-char (point-min))
            (while (and (not found) (< (point) (point-max)))
              (let ((offset (get-text-property (point) 'project-diff-offset)))
                (when (and offset
                           (equal (get-text-property (point) 'project-diff-file) (plist-get context :path))
                           (equal (get-text-property (point) 'project-diff-side) (plist-get context :side))
                           (<= offset target)
                           (<= target (+ offset (max 0 (- (line-end-position) (point) 1)))))
                  (setq found (+ (point) 1 (- target offset)))))
              (unless found (forward-line 1))))
          (cond
           (found (goto-char found)
                  (when-let* ((window (get-buffer-window buffer t))) (set-window-point window found)))
           ((> +agent-shell-projects--render-pending 0) (setq pending t))
           (t (let (section)
                (magit-map-sections
                 (lambda (s) (when (and (null section) (oref s washer)) (setq section s))))
                (if section (progn (magit-section-show section) (setq pending t))
                  (save-excursion
                    (goto-char (point-min))
                    (when (search-forward "Load more " nil t)
                      (when-let* ((button (button-at (1- (point)))))
                        (button-activate button) (setq pending t))))))))
          (when pending
            (run-at-time 0.01 nil #'+agent-shell-projects--reveal-annotation buffer token anchor)))))))

(defun +agent-shell-projects--decorate-code (project artefact version &optional start end)
  "Highlight annotations on code ARTEFACT VERSION in PROJECT."
  (when (derived-mode-p '+agent-shell-projects-code-mode)
    (when start (remove-overlays start end 'project-annotation t))
    (dolist (annotation (plist-get project :annotations))
      (let* ((anchor (or (plist-get annotation :anchor) annotation))
             (context (plist-get anchor :context)))
        (when (and (equal (plist-get annotation :artefact) (plist-get artefact :id))
                   context (not (plist-get annotation :resolved)))
          (let* ((text (+agent-shell-projects--code-annotation-text artefact version annotation))
                 (attachment (and text (+agent-shell-projects-attachment
                                        annotation text (plist-get context :text)))))
            (when (plist-get attachment :start)
              (save-excursion
                (goto-char (or start (point-min)))
                (while (< (point) (or end (point-max)))
                  (let* ((offset (get-text-property (point) 'project-diff-offset))
                         (path (get-text-property (point) 'project-diff-file))
                         (side (get-text-property (point) 'project-diff-side))
                         (narrative (and (not (plist-get context :path))
                                         (equal (plist-get context :space)
                                                +agent-shell-projects--code-view)))
                         (origin (if narrative (1- (point)) offset)))
                    (when (and origin (or narrative
                                          (and (equal path (plist-get context :path))
                                               (equal side (plist-get context :side)))))
                      (let* ((prefix (if narrative 0 1))
                             (start (+ (point) prefix
                                       (max 0 (- (plist-get attachment :start) origin))))
                             (end (min (line-end-position)
                                       (+ (point) prefix (- (plist-get attachment :end) origin)))))
                        (when (< start end)
                          (let ((overlay (make-overlay start end)))
                            (overlay-put overlay 'project-annotation t)
                            (overlay-put overlay 'project-annotation-id (plist-get annotation :id))
                            (overlay-put overlay 'face 'highlight)
                            (overlay-put overlay 'help-echo (plist-get annotation :body))))))
                  (forward-line 1)))))))))))

(define-key +agent-shell-projects-code-mode-map (kbd "RET") #'+agent-shell-projects-code-edit)
(define-key +agent-shell-projects-code-mode-map (kbd "C-c C-e") #'+agent-shell-projects-code-edit)
(define-key +agent-shell-projects-code-mode-map (kbd "C-c C-d") #'+agent-shell-projects-compare)
(setq +agent-shell-projects-code-capture-function #'+agent-shell-projects-capture-code
      +agent-shell-projects-selection-function #'+agent-shell-projects--code-selection
      +agent-shell-projects-annotation-text-function #'+agent-shell-projects--code-annotation-text
      +agent-shell-projects-annotation-visit-function #'+agent-shell-projects--visit-code-annotation)
(add-hook '+agent-shell-projects-visit-functions #'+agent-shell-projects--visit-code)
(add-hook '+agent-shell-projects-view-functions #'+agent-shell-projects--decorate-code t)

(provide '+agent-shell-projects-code)
;;; +agent-shell-projects-code.el ends here
