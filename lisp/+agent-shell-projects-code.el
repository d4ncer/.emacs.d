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

(defun +agent-shell-projects-git (root &rest args)
  "Run Git ARGS in ROOT and return raw stdout, signalling failures."
  (let ((stderr (make-temp-file "project-git-error-")))
    (unwind-protect
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (let* ((default-directory (file-name-as-directory root))
                 (process-environment (cons "GIT_OPTIONAL_LOCKS=0" process-environment))
                 (coding-system-for-read 'no-conversion)
                 (status (apply #'process-file "git" nil (list t stderr) nil
                                "-c" "core.fsmonitor=false" args)))
            (unless (zerop status)
              (error "Git: %s" (with-temp-buffer
                                 (insert-file-contents stderr)
                                 (string-trim (buffer-string)))))
            (buffer-string)))
      (delete-file stderr))))

(defun +agent-shell-projects--git-text (root &rest args)
  "Return decoded output of Git ARGS in ROOT."
  (decode-coding-string (apply #'+agent-shell-projects-git root args) 'utf-8-unix))

(defun +agent-shell-projects--workspace-changes (root base)
  "Return changed paths in ROOT relative to BASE, including untracked files."
  (let ((fields (split-string
                 (+agent-shell-projects--git-text
                  root "diff" "--no-ext-diff" "--name-status" "-z" "--find-renames" base "--")
                 "\0" t))
        records)
    (while fields
      (let* ((status (pop fields)) (first (pop fields))
             (renamed (member (substring status 0 1) '("R" "C")))
             (path (if renamed (pop fields) first)))
        (push (list :path path :status status
                    :old-path (and renamed first)) records)))
    (dolist (path (split-string
                   (+agent-shell-projects--git-text
                    root "ls-files" "--others" "--exclude-standard" "-z") "\0" t))
      (unless (file-in-directory-p (expand-file-name path root)
                                   +agent-shell-projects-directory)
        (push (list :path path :status "A" :old-path nil) records)))
    (sort records (lambda (a b) (string< (plist-get a :path) (plist-get b :path))))))

(defun +agent-shell-projects--working-bytes (root path)
  "Return raw working bytes for PATH in ROOT, or nil when absent."
  (let ((file (expand-file-name path root)))
    (cond ((file-symlink-p file) (encode-coding-string (file-symlink-p file) 'utf-8))
          ((file-directory-p file)
           (encode-coding-string
            (concat "Submodule " (string-trim
                                    (+agent-shell-projects--git-text
                                     file "rev-parse" "HEAD"))) 'utf-8))
          ((file-exists-p file)
           (with-temp-buffer
             (set-buffer-multibyte nil)
             (insert-file-contents-literally file)
             (buffer-string))))))

(defun +agent-shell-projects--base-bytes (root base path)
  "Return PATH's raw contents at BASE in ROOT, or nil when absent."
  (let ((entry (+agent-shell-projects--git-text
                root "ls-tree" "-z" base "--" path)))
    (unless (string-empty-p entry)
      (if (string-prefix-p "160000 " entry)
          (encode-coding-string
           (concat "Submodule " (nth 2 (split-string (car (split-string entry "\t")))) )
           'utf-8)
        (+agent-shell-projects-git root "show" (concat base ":" path))))))

(defun +agent-shell-projects--working-mode (root path)
  "Return Git's mode representation for working PATH in ROOT."
  (let ((file (expand-file-name path root)))
    (cond ((file-symlink-p file) "120000")
          ((file-directory-p file) "160000")
          ((file-exists-p file)
           (if (zerop (logand #o111 (file-modes file))) "100644" "100755")))))

(defun +agent-shell-projects--base-mode (root base path)
  "Return PATH's mode at BASE in ROOT."
  (let ((entry (+agent-shell-projects--git-text root "ls-tree" "-z" base "--" path)))
    (unless (string-empty-p entry) (car (split-string entry " ")))))

(defun +agent-shell-projects--file-record (path bytes &optional mode)
  "Persist BYTES for PATH and return a file record."
  (list :path path
        :content (and bytes (+agent-shell-projects-store-content
                             (base64-encode-string bytes t)))
        :binary (and bytes (string-match-p "\0" bytes) t) :mode mode))

(defun +agent-shell-projects--file-bytes (record)
  "Read exact bytes from a saved file RECORD."
  (when (plist-get record :content)
    (base64-decode-string (+agent-shell-projects-content
                          (list :blob (plist-get record :content))))))

(defun +agent-shell-projects-code-manifest (version)
  "Read the saved workspace manifest in VERSION."
  (json-parse-string (+agent-shell-projects-content version)
                     :object-type 'plist :array-type 'list
                     :null-object nil :false-object :false))

(defun +agent-shell-projects-capture-code (project artefact &optional actor)
  "Capture PROJECT's code ARTEFACT, rechecking moving source contents."
  (let* ((root (plist-get artefact :path)) (base (plist-get artefact :base))
         (attempt 0) snapshot)
    (while (and (not snapshot) (< attempt 3))
      (cl-incf attempt)
      (let* ((changes (+agent-shell-projects--workspace-changes root base))
             (paths (sort (delete-dups
                           (delq nil (cl-mapcan
                                      (lambda (change)
                                        (list (plist-get change :path)
                                              (plist-get change :old-path))) changes)))
                          #'string<))
             (working (mapcar (lambda (path)
                                (cons path (+agent-shell-projects--working-bytes root path)))
                              paths))
             (files (mapcar (lambda (entry)
                              (+agent-shell-projects--file-record
                               (car entry) (cdr entry)
                               (+agent-shell-projects--working-mode root (car entry))))
                            working))
             (bases (mapcar (lambda (path)
                              (+agent-shell-projects--file-record
                               path (+agent-shell-projects--base-bytes root base path)
                               (+agent-shell-projects--base-mode root base path))) paths)))
        (when (and (equal changes (+agent-shell-projects--workspace-changes root base))
                   (cl-every (lambda (entry)
                               (and (equal (cdr entry)
                                           (+agent-shell-projects--working-bytes root (car entry)))
                                    (equal (plist-get
                                            (seq-find (lambda (f) (equal (plist-get f :path)
                                                                         (car entry))) files)
                                            :mode)
                                           (+agent-shell-projects--working-mode root (car entry)))))
                             working))
          (setq snapshot (+agent-shell-projects-snapshot
                          project artefact
                          (+agent-shell-projects--json
                           (list :base base :changes changes :files files :base-files bases
                                 :proposal (when (plist-get project :proposal)
                                             (let ((proposal (+agent-shell-projects-artefact
                                                              project (plist-get project :proposal))))
                                               (list :artefact (plist-get proposal :id)
                                                     :version (plist-get proposal :current))))))
                          actor)))))
    (or snapshot (user-error "Workspace changed during capture; refresh again"))))

(defun +agent-shell-projects-enrol-code (project root &optional base name)
  "Attach ROOT's workspace to PROJECT, comparing against BASE."
  (let* ((root (string-trim (+agent-shell-projects--git-text
                            root "rev-parse" "--show-toplevel")))
         (base (string-trim (+agent-shell-projects--git-text
                            root "rev-parse" "--verify"
                            (concat (or base "HEAD") "^{commit}"))))
         (artefact (list :id (+agent-shell-projects--id)
                         :name (or name "Implementation") :kind "code"
                         :path root :base base :history nil :current nil
                         :ack nil :event 0 :ack-event 0)))
    (+agent-shell-projects--set project :root root)
    (+agent-shell-projects--set
     project :artefacts (append (plist-get project :artefacts) (list artefact)))
    (+agent-shell-projects-capture-code project artefact)
    artefact))

(defun +agent-shell-projects-add-code (root base)
  "Enrol ROOT in the current project using BASE as its comparison commit."
  (interactive (list (read-directory-name "Git workspace: ")
                     (read-string "Base revision: " "HEAD")))
  (let* ((project (+agent-shell-projects--project))
         (artefact (+agent-shell-projects-enrol-code project root base)))
    (+agent-shell-projects-open project (plist-get artefact :id))))

(defun +agent-shell-projects--manifest-file (manifest path &optional fallback root)
  "Find PATH's saved state in MANIFEST, using FALLBACK base contents or ROOT."
  (or (seq-find (lambda (entry) (equal (plist-get entry :path) path))
                (plist-get manifest :files))
      (seq-find (lambda (entry) (equal (plist-get entry :path) path))
                (plist-get manifest :base-files))
      (and (equal (plist-get manifest :base) (plist-get fallback :base))
           (seq-find (lambda (entry) (equal (plist-get entry :path) path))
                     (plist-get fallback :base-files)))
      (+agent-shell-projects--file-record
       path (and root (+agent-shell-projects--base-bytes
                       root (plist-get manifest :base) path))
       (and root (+agent-shell-projects--base-mode root (plist-get manifest :base) path)))))

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

(defun +agent-shell-projects--diff (path old new)
  "Return a unified diff for PATH between raw OLD and NEW bytes."
  (let ((left (make-temp-file "project-left-"))
        (right (make-temp-file "project-right-")))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'no-conversion))
            (with-temp-file left (set-buffer-multibyte nil) (insert (or old "")))
            (with-temp-file right (set-buffer-multibyte nil) (insert (or new ""))))
          (with-temp-buffer
            (let ((status (process-file "diff" nil t nil "-u" "-L"
                                         (concat "a/" path) "-L" (concat "b/" path)
                                         left right)))
              (unless (memq status '(0 1)) (error "Diff failed for %s" path)))
            (buffer-string)))
      (delete-file left) (delete-file right))))

(defun +agent-shell-projects--line-offset (text number)
  "Return zero-based character offset of line NUMBER in TEXT."
  (with-temp-buffer
    (insert text) (goto-char (point-min)) (forward-line (max 0 (1- number)))
    (1- (point))))

(defun +agent-shell-projects--insert-diff-lines (path diff old new)
  "Insert DIFF for PATH, tracking source locations within OLD and NEW."
  (let ((old-line 1) (new-line 1) (lines (split-string diff "\n")) hunk)
    (dolist (line (cddr lines))
      (cond
       ((string-match "^@@ -\\([0-9]+\\)\\(?:,[0-9]+\\)? +\\+\\([0-9]+\\)" line)
        (setq old-line (string-to-number (match-string 1 line))
              new-line (string-to-number (match-string 2 line))
              hunk (append hunk (list (list :header line :old old-line :new new-line
                                           :lines nil)))))
       ((and hunk (not (string-empty-p line)))
        (let* ((record (car (last hunk)))
               (side (if (string-prefix-p "-" line) "old" "new"))
               (number (if (equal side "old") old-line new-line)))
          (+agent-shell-projects--set
           record :lines (append (plist-get record :lines)
                                 (list (list :line line :side side :number number))))
          (cond ((string-prefix-p "-" line) (cl-incf old-line))
                ((string-prefix-p "+" line) (cl-incf new-line))
                ((string-prefix-p " " line) (cl-incf old-line) (cl-incf new-line)))))))
    (dolist (record hunk)
      (magit-insert-section (hunk (cons path (plist-get record :header)))
        (magit-insert-heading
         (propertize (plist-get record :header) 'face 'diff-hunk-header))
        (dolist (entry (plist-get record :lines))
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
                     (+agent-shell-projects--line-offset
                      text (plist-get entry :number)))))))))))

(define-derived-mode +agent-shell-projects-code-mode magit-section-mode
  "Project Code" "Inspect captured project code and its explanations.")

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
      (setq +agent-shell-projects--project-id (plist-get project :id)
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
          (pcase-let* ((`(,left ,right) (+agent-shell-projects--comparison-pair
                                        artefact version comparison))
                       (paths (sort (delete-dups
                                     (mapcar (lambda (file) (plist-get file :path))
                                             (append (plist-get left :files)
                                                     (plist-get right :files)))) #'string<)))
            (magit-insert-section (code)
              (magit-insert-heading (format "%s · %s" (plist-get artefact :name)
                                            (if (equal comparison "reviewed")
                                                "since review" "whole solution")))
              (dolist (path paths)
                (when (or (not focus) (member path focus))
                  (let* ((old-record (+agent-shell-projects--manifest-file
                                      left path right (plist-get artefact :path)))
                         (new-record (+agent-shell-projects--manifest-file
                                      right path left (plist-get artefact :path)))
                         (old-bytes (+agent-shell-projects--file-bytes old-record))
                         (new-bytes (+agent-shell-projects--file-bytes new-record)))
                    (unless (and (equal old-bytes new-bytes)
                                 (equal (plist-get old-record :mode)
                                        (plist-get new-record :mode)))
                      (magit-insert-section (file path)
                        (magit-insert-heading
                         (format "%s %s" (cond ((null new-bytes) "deleted")
                                                ((null old-bytes) "new")
                                                (t "modified")) path))
                        (when (and old-bytes new-bytes
                                   (not (equal (plist-get old-record :mode)
                                               (plist-get new-record :mode))))
                          (insert (format "mode %s → %s\n"
                                          (plist-get old-record :mode)
                                          (plist-get new-record :mode))))
                        (if (or (plist-get old-record :binary)
                                (plist-get new-record :binary))
                            (insert "Binary contents changed\n")
                          (+agent-shell-projects--insert-diff-lines
                           path (+agent-shell-projects--diff path old-bytes new-bytes)
                           (decode-coding-string (or old-bytes "") 'utf-8-unix)
                           (decode-coding-string (or new-bytes "") 'utf-8-unix)))
                        (insert "\n")))))))))
        (goto-char
         (min (point-max)
              (or (gethash (list (plist-get artefact :id) (plist-get version :id)
                                 view comparison focus) +agent-shell-projects--positions)
                  (point-min))))
        (set-buffer-modified-p nil)))
    (pop-to-buffer buffer)
    (run-hook-with-args '+agent-shell-projects-view-functions project artefact version)
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
               (text (get-text-property begin 'project-diff-text)))
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
           (saved (get-text-property (point) 'project-diff-text)))
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
  "Capture direct edits to the current code artefact after saving."
  (when (and +agent-shell-projects--project-id +agent-shell-projects--artefact-id)
    (let* ((project (+agent-shell-projects--project))
           (artefact (+agent-shell-projects--artefact))
           (version (+agent-shell-projects-capture-code project artefact "user")))
      (setq +agent-shell-projects--version-id (plist-get version :id)
            +agent-shell-projects--observed-event (plist-get artefact :event)))))

(defun +agent-shell-projects--code-selection ()
  "Capture a passage selected in a code diff, explanation, or source file."
  (if (derived-mode-p '+agent-shell-projects-code-mode)
      (+agent-shell-projects--diff-selection)
    (when (and buffer-file-name +agent-shell-projects--artefact-id
               (equal (plist-get (+agent-shell-projects--artefact) :kind) "code"))
      (when (buffer-modified-p) (user-error "Save source edits before attaching code feedback"))
      (let* ((project (+agent-shell-projects--project))
             (artefact (+agent-shell-projects--artefact))
             (version (+agent-shell-projects-capture-code project artefact "user")))
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
        (goto-char (point-min))
        (let (found)
          (while (and (not found) (< (point) (point-max)))
            (when (and (equal path (get-text-property (point) 'project-diff-file))
                       (equal (plist-get context :side)
                              (get-text-property (point) 'project-diff-side))
                       (<= (get-text-property (point) 'project-diff-offset)
                           (plist-get anchor :start)))
              (let ((offset (get-text-property (point) 'project-diff-offset)))
                (when (<= (plist-get anchor :start)
                          (+ offset (max 0 (1- (- (line-end-position) (point))))))
                  (forward-char (1+ (- (plist-get anchor :start) offset)))
                  (setq found t))))
            (unless found (forward-line 1))))
        t)))))

(defun +agent-shell-projects--decorate-code (project artefact version)
  "Highlight annotations on code ARTEFACT VERSION in PROJECT."
  (when (derived-mode-p '+agent-shell-projects-code-mode)
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
                (goto-char (point-min))
                (while (< (point) (point-max))
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
