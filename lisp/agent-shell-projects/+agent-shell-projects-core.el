;;; +agent-shell-projects-core.el --- Project records and snapshots -*- lexical-binding: t; -*-

;;; Commentary:
;; Durable project state shared by document, agent, and code review interfaces.

;;; Code:

(require 'cl-lib)
(require '+agent-shell-projects-lifecycle)
(require 'json)
(require 'seq)
(require 'subr-x)

(defgroup +agent-shell-projects nil
  "Author and review project artefacts in Emacs."
  :group 'tools)

(defcustom +agent-shell-projects-directory
  (locate-user-emacs-file "var/agent-shell-projects/")
  "Directory for project manifests and immutable contents."
  :type 'directory)

(defvar +agent-shell-projects--projects (make-hash-table :test #'equal))
(defvar +agent-shell-projects-change-hook nil
  "Functions called with PROJECT and ARTEFACT after a state change.
ARTEFACT can be nil for a project-wide change.")
(autoload '+agent-shell-projects-capture-code "+agent-shell-projects-capture")
(defvar +agent-shell-projects-code-capture-function #'+agent-shell-projects-capture-code)
(defvar +agent-shell-projects--defer-save nil)
(defvar-local +agent-shell-projects--source-project nil)
(defvar-local +agent-shell-projects--source-artefacts nil)

(define-error '+agent-shell-projects-source-missing
  "Cannot locate enrolled Org heading" 'user-error)

(declare-function org-id-get-create "org-id")
(declare-function org-id-find "org-id")
(declare-function org-id-add-location "org-id")
(declare-function org-back-to-heading "org")
(declare-function org-end-of-subtree "org")

(defun +agent-shell-projects--id ()
  "Return a new persistent identifier."
  (secure-hash 'sha256
               (format "%s:%s:%s:%s" (float-time) (random) (emacs-pid)
                       (current-time))))

(defun +agent-shell-projects--set (record key value)
  "Mutate nonempty plist RECORD at KEY to VALUE."
  (if-let* ((cell (memq key record)))
      (setcar (cdr cell) value)
    (nconc record (list key value)))
  value)

(defun +agent-shell-projects--json-value (value)
  "Convert plist VALUE and its collections to JSON-compatible data."
  (cond ((or (null value) (eq value t) (eq value :false)) value)
        ((and (listp value) (keywordp (car value)))
         (cl-loop for (key item) on value by #'cddr
                  append (list key (+agent-shell-projects--json-value item))))
        ((listp value)
         (vconcat (mapcar #'+agent-shell-projects--json-value value)))
        ((vectorp value)
         (vconcat (mapcar #'+agent-shell-projects--json-value value)))
        ((symbolp value) (symbol-name value))
        (t value)))

(defun +agent-shell-projects--json (value)
  "Serialize VALUE as JSON."
  (json-serialize (+agent-shell-projects--json-value value)
                  :null-object nil :false-object :false))

(defun +agent-shell-projects--read-json (file)
  "Read a JSON record from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (json-parse-buffer :object-type 'plist :array-type 'list
                       :null-object nil :false-object :false)))

(defun +agent-shell-projects--atomic-write (file content)
  "Atomically write text CONTENT to FILE."
  (make-directory (file-name-directory file) t)
  (let ((temporary (make-temp-file (concat file ".tmp-")))
        (coding-system-for-write 'utf-8-unix))
    (unwind-protect
        (progn
          (with-temp-file temporary (insert content))
          (set-file-modes temporary #o600)
          (rename-file temporary file t))
      (when (file-exists-p temporary) (delete-file temporary)))))

(defun +agent-shell-projects--manifest (id)
  "Return the manifest filename for project ID."
  (unless (and (stringp id) (string-match-p "\\`[[:xdigit:]]+\\'" id))
    (error "Invalid project identifier"))
  (expand-file-name (concat "projects/" id ".json")
                    +agent-shell-projects-directory))

(defun +agent-shell-projects-save (project)
  "Persist PROJECT."
  (unless +agent-shell-projects--defer-save
    (+agent-shell-projects--atomic-write
     (+agent-shell-projects--manifest (plist-get project :id))
     (+agent-shell-projects--json project)))
  project)

(defun +agent-shell-projects-load (id &optional reload)
  "Load project ID, bypassing the cache when RELOAD is non-nil."
  (or (and (not reload) (gethash id +agent-shell-projects--projects))
      (let ((project (+agent-shell-projects--read-json
                      (+agent-shell-projects--manifest id))))
        (unless (and (equal (plist-get project :id) id)
                     (eql (plist-get project :schema) 1))
          (error "Unsupported or mismatched project manifest"))
        (puthash id project +agent-shell-projects--projects)
        project)))

(defun +agent-shell-projects-list (&optional include-archived)
  "Return persisted projects, including archived ones with INCLUDE-ARCHIVED."
  (let ((directory (expand-file-name "projects/"
                                     +agent-shell-projects-directory)))
    (when (file-directory-p directory)
      (seq-filter
       (lambda (project) (or include-archived (not (plist-get project :archived))))
       (mapcar (lambda (file)
                 (+agent-shell-projects-load (file-name-base file)))
               (directory-files directory t "\\.json\\'"))))))

(defun +agent-shell-projects-assert-active (project &optional artefact)
  "Require an active PROJECT and, when supplied, enrolled ARTEFACT."
  (when (plist-get project :archived) (user-error "Restore this archived project first"))
  (when (plist-get artefact :unenrolled) (user-error "This artefact is no longer enrolled")))

(defun +agent-shell-projects-create (name &optional root)
  "Create a project called NAME, optionally rooted at ROOT."
  (when (string-empty-p (string-trim name)) (user-error "Name is empty"))
  (let ((project (list :schema 1 :id (+agent-shell-projects--id)
                       :name name :root (and root (expand-file-name root))
                       :seq 0 :artefacts nil :agents nil :annotations nil
                       :checkpoints nil :batches nil :requests nil
                       :operations nil)))
    (puthash (plist-get project :id) project +agent-shell-projects--projects)
    (+agent-shell-projects-save project)))

(defun +agent-shell-projects-artefact (project id)
  "Return PROJECT's artefact ID or signal an error."
  (or (seq-find (lambda (item) (equal (plist-get item :id) id))
                (append (plist-get project :artefacts)
                        (plist-get project :unenrolled-artefacts)))
      (error "Unknown artefact %s" id)))

(defun +agent-shell-projects-version (artefact id)
  "Return version ID of ARTEFACT or signal an error."
  (or (seq-find (lambda (item) (equal (plist-get item :id) id))
                (plist-get artefact :history))
      (error "Unknown artefact version %s" id)))

(defun +agent-shell-projects--blob-file (hash)
  "Return the content address HASH's filename."
  (unless (and (stringp hash) (string-match-p "\\`[[:xdigit:]]\\{64\\}\\'" hash))
    (error "Invalid content address"))
  (expand-file-name (concat "contents/" hash)
                    +agent-shell-projects-directory))

(defun +agent-shell-projects-store-content (text)
  "Persist TEXT and return its content address."
  (let* ((hash (secure-hash 'sha256 (encode-coding-string text 'utf-8-unix)))
         (file (+agent-shell-projects--blob-file hash)))
    (unless (file-exists-p file)
      (+agent-shell-projects--atomic-write file text))
    hash))

(defun +agent-shell-projects-content (version)
  "Return the exact saved contents of VERSION."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents
       (+agent-shell-projects--blob-file (plist-get version :blob))))
    (buffer-string)))

(defun +agent-shell-projects--changed (project &optional artefact actor)
  "Record a change to PROJECT and ARTEFACT by ACTOR."
  (+agent-shell-projects--set project :seq (1+ (plist-get project :seq)))
  (when (and artefact (not (equal actor "user")))
    (+agent-shell-projects--set artefact :event (plist-get project :seq)))
  (+agent-shell-projects-save project)
  (run-hook-with-args '+agent-shell-projects-change-hook project artefact))

(defun +agent-shell-projects-snapshot (project artefact text &optional actor)
  "Save TEXT as ARTEFACT's current version in PROJECT, attributed to ACTOR."
  (+agent-shell-projects-snapshot-blob
   project artefact (+agent-shell-projects-store-content text) actor))

(defun +agent-shell-projects-snapshot-blob (project artefact blob &optional actor)
  "Adopt immutable BLOB as ARTEFACT's current version in PROJECT."
  (+agent-shell-projects-assert-active project artefact)
  (unless (file-exists-p (+agent-shell-projects--blob-file blob))
    (error "Missing snapshot blob"))
  (let* ((current (and (plist-get artefact :current)
                       (+agent-shell-projects-version
                        artefact (plist-get artefact :current)))))
    (if (equal blob (plist-get current :blob)) current
      (let ((version (list :id (+agent-shell-projects--id) :blob blob
                           :time (float-time) :actor (or actor "external"))))
        (+agent-shell-projects--set
         artefact :history (append (plist-get artefact :history) (list version)))
        (+agent-shell-projects--set artefact :current (plist-get version :id))
        (+agent-shell-projects--changed project artefact actor)
        version))))

(defun +agent-shell-projects-needs-attention-p (artefact)
  "Return non-nil if ARTEFACT has new unacknowledged activity."
  (or (not (equal (plist-get artefact :current) (plist-get artefact :ack)))
      (> (or (plist-get artefact :event) 0)
         (or (plist-get artefact :ack-event) 0))))

(defun +agent-shell-projects-source (artefact)
  "Return (BUFFER START END) for document ARTEFACT's live source."
  (let* ((path (plist-get artefact :path))
         (id (plist-get artefact :org-id))
         (buffer (or (seq-find
                      (lambda (buf)
                        (with-current-buffer buf
                          (member (plist-get artefact :id)
                                  +agent-shell-projects--source-artefacts)))
                      (buffer-list))
                     (find-file-noselect path)))
         marker)
    (when id
      (require 'org-id)
      (setq marker (ignore-errors (org-id-find id t)))
      (when (and marker (not (buffer-file-name (marker-buffer marker))))
        (setq marker nil))
      (unless marker
        (dolist (buf (buffer-list))
          (when (and (not marker)
                     (with-current-buffer buf
                       (and buffer-file-name (derived-mode-p 'org-mode))))
            (with-current-buffer buf
              (save-restriction
                (widen)
                (save-excursion
                  (goto-char (point-min))
                  (when (re-search-forward
                         (concat "^[ \t]*:ID:[ \t]+" (regexp-quote id)
                                 "[ \t]*$") nil t)
                    (org-back-to-heading t)
                    (setq marker (point-marker)))))))))
      (unless marker
        (signal '+agent-shell-projects-source-missing (list id)))
      (setq buffer (marker-buffer marker)))
    (with-current-buffer buffer
      (when (and buffer-file-name (file-exists-p buffer-file-name)
                 (not (verify-visited-file-modtime buffer)))
        (if (buffer-modified-p)
            (progn
              (+agent-shell-projects--set artefact :conflict t)
              (+agent-shell-projects--set
               artefact :disk-copy
               (let ((disk-file buffer-file-name))
                 (+agent-shell-projects-store-content
                  (with-temp-buffer
                    (insert-file-contents disk-file) (buffer-string))))))
          (revert-buffer t t t)
          (+agent-shell-projects--set artefact :conflict nil)))
      (save-restriction
        (widen)
        (save-excursion
          (when buffer-file-name
            (+agent-shell-projects--set artefact :path buffer-file-name))
          (if marker
              (progn
                (goto-char (point-min))
                (unless (re-search-forward
                         (concat "^[ \t]*:ID:[ \t]+" (regexp-quote id)
                                 "[ \t]*$") nil t)
                  (signal '+agent-shell-projects-source-missing (list id)))
                (org-back-to-heading t)
                (let ((start (point)))
                  (org-end-of-subtree t t)
                  (org-id-add-location id buffer-file-name)
                  (list buffer start (point))))
            (list buffer (point-min) (point-max))))))))

(defun +agent-shell-projects-capture (project artefact &optional actor)
  "Capture ARTEFACT's live source in PROJECT, attributed to ACTOR."
  (if (or (plist-get project :archived) (plist-get artefact :unenrolled))
      (when (plist-get artefact :current)
        (+agent-shell-projects-version artefact (plist-get artefact :current)))
    (if (equal (plist-get artefact :kind) "code")
	(funcall +agent-shell-projects-code-capture-function project artefact actor)
      (let ((previous-conflict (plist-get artefact :disk-copy))
            (previous-missing (plist-get artefact :missing))
            (source (condition-case nil (+agent-shell-projects-source artefact)
                      (+agent-shell-projects-source-missing nil))))
	(if (not source)
            (progn
              (+agent-shell-projects--set artefact :missing t)
              (unless previous-missing
		(+agent-shell-projects--changed project artefact actor))
              (if (plist-get artefact :current)
                  (+agent-shell-projects-version artefact (plist-get artefact :current))
		(signal '+agent-shell-projects-source-missing
			(list (plist-get artefact :org-id)))))
	  (pcase-let ((`(,buffer ,start ,end) source))
            (let* ((missing (and (not (file-exists-p (plist-get artefact :path)))
				 (not (buffer-modified-p buffer))))
		   (_ (+agent-shell-projects--set artefact :missing missing))
		   (version (+agent-shell-projects-snapshot
                             project artefact
                             (if missing "" (with-current-buffer buffer
                                              (buffer-substring-no-properties start end))) actor)))
              (when (or (not (equal previous-conflict (plist-get artefact :disk-copy)))
			(not (eq previous-missing missing)))
		(+agent-shell-projects--changed project artefact actor))
              version)))))))

(defun +agent-shell-projects--source-saved ()
  "Capture enrolled artefacts after this source buffer is saved."
  (when (and +agent-shell-projects--source-project
             (not (plist-get (+agent-shell-projects-load
                             +agent-shell-projects--source-project) :archived)))
    (let ((project (+agent-shell-projects-load
                    +agent-shell-projects--source-project)))
      (dolist (id +agent-shell-projects--source-artefacts)
        (+agent-shell-projects--set
         (+agent-shell-projects-artefact project id) :conflict nil)
        (+agent-shell-projects-capture
         project (+agent-shell-projects-artefact project id) "user")
        (when (and (boundp '+agent-shell-projects--artefact-id)
                   (equal (symbol-value '+agent-shell-projects--artefact-id) id))
          (set '+agent-shell-projects--version-id
               (plist-get (+agent-shell-projects-artefact project id) :current))))
      (+agent-shell-projects-save project))))

(defun +agent-shell-projects-track-source (project artefact)
  "Associate PROJECT and ARTEFACT with its current source buffer."
  (pcase-let ((`(,buffer ,_ ,_) (+agent-shell-projects-source artefact)))
    (with-current-buffer buffer
      (setq +agent-shell-projects--source-project (plist-get project :id))
      (cl-pushnew (plist-get artefact :id)
                  +agent-shell-projects--source-artefacts :test #'equal)
      (add-hook 'after-save-hook #'+agent-shell-projects--source-saved nil t))))

(defun +agent-shell-projects-enrol (project file &optional name subtree)
  "Enrol FILE in PROJECT under NAME, or its current SUBTREE heading."
  (+agent-shell-projects-assert-active project)
  (let* ((file (expand-file-name file))
         (buffer (find-file-noselect file))
         (kind (if subtree "org-heading"
                 (if (string-match-p "\\.org\\'" file) "org" "markdown")))
         (artefact (list :id (+agent-shell-projects--id)
                         :name (or name (file-name-nondirectory file))
                         :kind kind :path file :org-id nil :history nil
                         :current nil :ack nil :event 0 :ack-event 0)))
    (when subtree
      (require 'org-id)
      (with-current-buffer buffer
        (unless (derived-mode-p 'org-mode) (user-error "Not an Org buffer"))
        (org-back-to-heading t)
        (+agent-shell-projects--set artefact :org-id (org-id-get-create))
        (save-buffer)))
    (+agent-shell-projects--set
     project :artefacts (append (plist-get project :artefacts) (list artefact)))
    (+agent-shell-projects-track-source project artefact)
    (+agent-shell-projects-capture project artefact)
    artefact))

(provide '+agent-shell-projects-core)
;;; +agent-shell-projects-core.el ends here
