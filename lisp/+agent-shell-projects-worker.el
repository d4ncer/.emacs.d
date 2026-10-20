;;; +agent-shell-projects-worker.el --- Isolated workspace worker -*- lexical-binding: t; -*-

;;; Commentary:
;; Git and diff preparation run in a separate Emacs process.  Workers write
;; immutable blobs only; the interactive Emacs owns project manifests.

;;; Code:

(require '+agent-shell-projects-core)
(defvar +agent-shell-projects--worker-git-count 0)

(defun +agent-shell-projects-git (root &rest args)
  "Run Git ARGS in ROOT and return raw stdout, signalling failures."
  (cl-incf +agent-shell-projects--worker-git-count)
  (let ((stderr (make-temp-file "project-git-error-")))
    (unwind-protect
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (let* ((default-directory (file-name-as-directory root))
                 (process-environment (cons "GIT_OPTIONAL_LOCKS=0" process-environment))
                 (coding-system-for-read 'no-conversion)
                 (status (apply #'process-file "git" nil (list t stderr) nil
                                args)))
            (unless (zerop status)
              (error "Git: %s" (with-temp-buffer
                                 (insert-file-contents stderr)
                                 (string-trim (buffer-string)))))
            (buffer-string)))
      (delete-file stderr))))

(defun +agent-shell-projects--git-text (root &rest args)
  "Return decoded output of Git ARGS in ROOT."
  (decode-coding-string (apply #'+agent-shell-projects-git root args) 'utf-8-unix))

(defun +agent-shell-projects--workspace-changes (root base &optional untracked)
  "Return changed paths in ROOT relative to BASE, including untracked files."
  (let ((fields (split-string
                 (+agent-shell-projects--git-text
                  root "diff" "--no-ext-diff" "--no-color" "--name-status" "-z" "--find-renames" base "--")
                 "\0" t))
        records)
    (while fields
      (let* ((status (pop fields)) (first (pop fields))
             (renamed (member (substring status 0 1) '("R" "C")))
             (path (if renamed (pop fields) first)))
        (push (list :path path :status status
                    :old-path (and renamed first)) records)))
    (dolist (path (split-string
                   (or untracked (+agent-shell-projects--git-text
                                  root "ls-files" "--others" "--exclude-standard" "-z")) "\0" t))
      (unless (file-in-directory-p (expand-file-name path root)
                                   +agent-shell-projects-directory)
        (push (list :path path :status "A" :old-path nil) records)))
    (sort records (lambda (a b) (string< (plist-get a :path) (plist-get b :path))))))

(defun +agent-shell-projects--working-bytes (root path)
  "Return raw working bytes for PATH in ROOT, or nil when absent."
  (let ((file (expand-file-name path root)))
    (cond ((file-symlink-p file) (encode-coding-string (file-symlink-p file) 'utf-8))
          ((file-directory-p file)
           (when (file-exists-p (expand-file-name ".git" file))
           (encode-coding-string
            (concat "Submodule " (string-trim
                                    (+agent-shell-projects--git-text
                                     file "rev-parse" "HEAD"))) 'utf-8)))
          ((file-exists-p file)
           (with-temp-buffer
             (set-buffer-multibyte nil)
             (insert-file-contents-literally file)
             (buffer-string))))))

(defun +agent-shell-projects--working-mode (root path)
  "Return Git's mode representation for working PATH in ROOT."
  (let ((file (expand-file-name path root)))
    (cond ((file-symlink-p file) "120000")
          ((file-directory-p file)
           (when (file-exists-p (expand-file-name ".git" file)) "160000"))
          ((file-exists-p file)
           (if (zerop (logand #o111 (file-modes file))) "100644" "100755")))))

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

(defvar +agent-shell-projects--manifest-cache (make-hash-table :test #'equal))

(defun +agent-shell-projects-code-manifest (version)
  "Read VERSION's immutable workspace manifest, caching decoded metadata."
  (let ((key (list +agent-shell-projects-directory (plist-get version :blob))))
    (or (gethash key +agent-shell-projects--manifest-cache)
        (progn
          (when (> (hash-table-count +agent-shell-projects--manifest-cache) 64)
            (clrhash +agent-shell-projects--manifest-cache))
          (puthash key
                   (json-parse-string (+agent-shell-projects-content version)
                                      :object-type 'plist :array-type 'list
                                      :null-object nil :false-object :false)
                   +agent-shell-projects--manifest-cache)))))

(defvar +agent-shell-projects--worker-progress-file nil)

(defun +agent-shell-projects--worker-progress (text)
  "Publish a small progress record containing TEXT."
  (when +agent-shell-projects--worker-progress-file
    (+agent-shell-projects--atomic-write
     +agent-shell-projects--worker-progress-file
     (+agent-shell-projects--json (list :phase text)))))

(defun +agent-shell-projects--batch-blobs (root objects)
  "Read OBJECTS using one binary-safe cat-file process in ROOT."
  (let ((result (make-hash-table :test #'equal)))
    (when objects
      (cl-incf +agent-shell-projects--worker-git-count)
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert (mapconcat #'identity objects "\n") "\n")
        (let ((default-directory (file-name-as-directory root))
              (process-environment (cons "GIT_OPTIONAL_LOCKS=0" process-environment))
              (coding-system-for-read 'no-conversion)
              (coding-system-for-write 'no-conversion))
          (unless (zerop (call-process-region (point-min) (point-max)
                                              "git" t t nil "cat-file" "--batch"))
            (error "Cannot read base objects")))
        (goto-char (point-min))
        (dolist (object objects)
          (unless (looking-at "[[:xdigit:]]+ blob \\([0-9]+\\)\n")
            (error "Invalid cat-file response for %s" object))
          (let ((size (string-to-number (match-string 1))))
            (goto-char (match-end 0))
            (puthash object (buffer-substring-no-properties (point) (+ (point) size)) result)
            (forward-char (1+ size))))))
    result))

(defun +agent-shell-projects--base-records (root base paths)
  "Return cached base records for PATHS, fetching missing objects in batches."
  (let* ((cache (expand-file-name
                 (concat "base-cache/" (secure-hash 'sha256 (concat root "\0" base)) ".json")
                 +agent-shell-projects-directory))
         (records (make-hash-table :test #'equal))
         (entries (make-hash-table :test #'equal))
         missing objects)
    (when (file-exists-p cache)
      (dolist (record (+agent-shell-projects--read-json cache))
        (puthash (plist-get record :path) record records)))
    (setq missing (seq-remove (lambda (path) (gethash path records)) paths))
    (when missing
      (let ((remaining missing))
        (while remaining
          (let* ((batch (seq-take remaining 500))
                 (output (apply #'+agent-shell-projects--git-text root
                                "--literal-pathspecs" "ls-tree" "-z" base "--" batch)))
            (setq remaining (nthcdr (length batch) remaining))
            (dolist (line (split-string output "\0" t))
              (when (string-match "\\`\\([0-9]+\\) \\([^ ]+\\) \\([[:xdigit:]]+\\)\t" line)
                (let ((mode (match-string 1 line)) (kind (match-string 2 line))
                      (object (match-string 3 line)) (path (substring line (match-end 0))))
                  (puthash path (list mode kind object) entries)
                  (when (equal kind "blob") (push object objects))))))))
      (let ((blobs (+agent-shell-projects--batch-blobs root (delete-dups objects))))
        (dolist (path missing)
          (let* ((entry (gethash path entries))
                 (bytes (pcase (cadr entry)
                          ("blob" (gethash (caddr entry) blobs))
                          ("commit" (encode-coding-string (concat "Submodule " (caddr entry)) 'utf-8)))))
            (puthash path (+agent-shell-projects--file-record path bytes (car entry)) records))))
      (+agent-shell-projects--atomic-write
       cache (+agent-shell-projects--json
              (sort (hash-table-values records)
                    (lambda (a b) (string< (plist-get a :path) (plist-get b :path)))))))
    (mapcar (lambda (path) (gethash path records)) paths)))

(defun +agent-shell-projects--worker-capture (request)
  "Capture all workspace targets in REQUEST, sharing discovery and file reads."
  (let* ((root (string-trim (+agent-shell-projects--git-text
                            (plist-get request :root) "rev-parse" "--show-toplevel")))
         (targets (plist-get request :targets))
         (bases (make-hash-table :test #'equal))
         (attempt 0) result)
    (dolist (target targets)
      (let* ((input (plist-get target :base))
             (base (or (gethash input bases)
                       (puthash input
                                (if (string-match-p "\\`[[:xdigit:]]\\{40,64\\}\\'" input) input
                                  (string-trim (+agent-shell-projects--git-text
                                                root "rev-parse" "--verify" (concat input "^{commit}"))))
                                bases))))
        (+agent-shell-projects--set target :base base)))
    (while (and (not result) (< attempt 3))
      (cl-incf attempt)
      (+agent-shell-projects--worker-progress "Finding changed and untracked files")
      (let* ((untracked (+agent-shell-projects--git-text root "ls-files" "--others" "--exclude-standard" "-z"))
             (changes (make-hash-table :test #'equal))
             (working (make-hash-table :test #'equal))
             (files (make-hash-table :test #'equal))
             prepared)
        (dolist (base (delete-dups (mapcar (lambda (v) (plist-get v :base)) targets)))
          (puthash base (+agent-shell-projects--workspace-changes root base untracked) changes))
        (+agent-shell-projects--worker-progress "Reading changed files and cached base objects")
        (dolist (target targets)
          (let* ((base (plist-get target :base))
                 (changed (gethash base changes))
                 (paths (sort (delq nil (delete-dups (cl-mapcan
                                           (lambda (c) (list (plist-get c :path) (plist-get c :old-path)))
                                           (copy-tree changed)))) #'string<)))
            (setq paths (delq nil paths))
            (dolist (path paths)
              (unless (gethash path files)
                (let ((bytes (+agent-shell-projects--working-bytes root path)))
                  (puthash path bytes working)
                  (puthash path (+agent-shell-projects--file-record
                                 path bytes (+agent-shell-projects--working-mode root path)) files))))
            (push (list :project (plist-get target :project) :artefact (plist-get target :artefact)
                        :root root :base base
                        :blob (+agent-shell-projects-store-content
                               (+agent-shell-projects--json
                                (list :base base :changes changed
                                      :files (mapcar (lambda (p) (gethash p files)) paths)
                                      :base-files (+agent-shell-projects--base-records root base paths)
                                      :proposal (plist-get target :proposal))))) prepared)))
        (+agent-shell-projects--worker-progress "Checking workspace consistency")
        (when (and (equal untracked (+agent-shell-projects--git-text
                                    root "ls-files" "--others" "--exclude-standard" "-z"))
                   (cl-every (lambda (base)
                               (equal (gethash base changes)
                                      (+agent-shell-projects--workspace-changes root base untracked)))
                             (hash-table-keys changes))
                   (cl-every (lambda (path)
                               (and (equal (gethash path working)
                                           (+agent-shell-projects--working-bytes root path))
                                    (equal (plist-get (gethash path files) :mode)
                                           (+agent-shell-projects--working-mode root path))))
                             (hash-table-keys files)))
          (setq result (nreverse prepared)))))
    (or result (error "Workspace kept changing during capture; retry when it settles"))))

(defun +agent-shell-projects--worker-file (manifest path fallback)
  "Look up PATH in saved MANIFEST or FALLBACK base records."
  (or (seq-find (lambda (f) (equal (plist-get f :path) path)) (plist-get manifest :files))
      (seq-find (lambda (f) (equal (plist-get f :path) path)) (plist-get manifest :base-files))
      (seq-find (lambda (f) (equal (plist-get f :path) path)) (plist-get fallback :base-files))
      (list :path path)))

(defun +agent-shell-projects--worker-diff (request)
  "Prepare a lazy, bounded diff representation for REQUEST."
  (let* ((right (+agent-shell-projects-code-manifest (list :blob (plist-get request :right))))
         (left (if (plist-get request :left)
                   (+agent-shell-projects-code-manifest (list :blob (plist-get request :left)))
                 (list :files (plist-get right :base-files))))
         (path (plist-get request :path))
         (old-record (+agent-shell-projects--worker-file left path right))
         (new-record (+agent-shell-projects--worker-file right path left))
         (old (decode-coding-string (or (+agent-shell-projects--file-bytes old-record) "") 'utf-8-unix))
         (new (decode-coding-string (or (+agent-shell-projects--file-bytes new-record) "") 'utf-8-unix))
         (old-offsets (+agent-shell-projects--line-offsets old))
         (new-offsets (+agent-shell-projects--line-offsets new))
         (diff (+agent-shell-projects--diff path (+agent-shell-projects--file-bytes old-record)
                                          (+agent-shell-projects--file-bytes new-record)))
         (old-line 1) (new-line 1) hunks current)
    (dolist (line (cddr (split-string diff "\n")))
      (cond
       ((string-match "^@@ -\\([0-9]+\\)\\(?:,[0-9]+\\)? +\\+\\([0-9]+\\)" line)
        (setq old-line (string-to-number (match-string 1 line))
              new-line (string-to-number (match-string 2 line))
              current (list :header line :lines nil))
        (push current hunks))
       ((and current (not (string-empty-p line)))
        (let* ((side (if (string-prefix-p "-" line) "old" "new"))
               (offsets (if (equal side "old") old-offsets new-offsets))
               (number (if (equal side "old") old-line new-line)))
          (push (list :line line :side side :offset (aref offsets (min (1- (length offsets)) (max 0 (1- number)))))
                (plist-get current :lines))
          (cond ((string-prefix-p "-" line) (cl-incf old-line))
                ((string-prefix-p "+" line) (cl-incf new-line))
                ((string-prefix-p " " line) (cl-incf old-line) (cl-incf new-line)))))))
    (dolist (hunk hunks)
      (let ((lines (nreverse (plist-get hunk :lines))) blocks)
        (while lines
          (let ((chunk (seq-take lines 200)))
            (push (+agent-shell-projects-store-content (+agent-shell-projects--json chunk)) blocks)
            (setq lines (nthcdr (length chunk) lines))))
        (+agent-shell-projects--set hunk :lines nil)
        (+agent-shell-projects--set hunk :blocks (nreverse blocks))))
    (list :blob (+agent-shell-projects-store-content
                 (+agent-shell-projects--json
                  (list :old old-record :new new-record :hunks (nreverse hunks)))))))

(defun +agent-shell-projects--line-offsets (text)
  "Build a character-offset vector for TEXT's lines in a single pass."
  (let ((position 0) (offsets (list 0)))
    (while (string-match "\n" text position)
      (setq position (match-end 0))
      (push position offsets))
    (vconcat (nreverse offsets))))

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
                                         (concat "a/" path) "-L" (concat "b/" path) left right)))
              (unless (memq status '(0 1)) (error "Diff failed for %s" path)))
            (buffer-string)))
      (delete-file left) (delete-file right))))

(defun +agent-shell-projects-worker-main (file)
  "Execute the isolated request in FILE and publish a small result."
    (let* ((request (+agent-shell-projects--read-json file))
         (+agent-shell-projects-directory (plist-get request :store))
         (+agent-shell-projects--worker-progress-file (concat file ".progress"))
         (+agent-shell-projects--worker-git-count 0)
         (started (float-time)))
    (+agent-shell-projects--atomic-write
     (concat file ".result")
     (+agent-shell-projects--json
      (condition-case err
          (let ((result (pcase (plist-get request :kind)
                  ("capture" (+agent-shell-projects--worker-capture request))
                  ("diff" (+agent-shell-projects--worker-diff request))
                  (_ (error "Unknown worker request")))))
            (list :ok t :result result :git-calls +agent-shell-projects--worker-git-count
                  :seconds (- (float-time) started)))
        (error (list :ok :false :error (error-message-string err))))))))

(provide '+agent-shell-projects-worker)
;;; +agent-shell-projects-worker.el ends here
