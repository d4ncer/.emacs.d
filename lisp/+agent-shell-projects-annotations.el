;;; +agent-shell-projects-annotations.el --- Anchored feedback -*- lexical-binding: t; -*-

;;; Commentary:
;; Annotation records, conservative attachment mapping, and Embark actions.

;;; Code:

(require '+agent-shell-projects-core)
(require 'magit-section)

(defvar +agent-shell-projects-view-functions)
(defvar +agent-shell-projects-sidebar-functions)
(defvar +agent-shell-projects-sidebar-mode-map)
(defvar +agent-shell-projects--targets (make-hash-table :test #'equal))
(defvar +agent-shell-projects-selection-function nil)
(defvar +agent-shell-projects-annotation-text-function nil)
(defvar +agent-shell-projects-annotation-visit-function nil)
(defvar-local +agent-shell-projects--project-id nil)
(defvar-local +agent-shell-projects--artefact-id nil)
(defvar-local +agent-shell-projects--version-id nil)
(defvar-local +agent-shell-projects--observed-event 0)
(defvar-local +agent-shell-projects--show-resolved nil)
(defvar-local +agent-shell-projects--view-buffer nil)
(defvar +agent-shell-projects-artefact-mode)
(defvar embark-keymap-alist)
(declare-function +agent-shell-projects--project "+agent-shell-projects")
(declare-function +agent-shell-projects--artefact "+agent-shell-projects")
(declare-function +agent-shell-projects-open "+agent-shell-projects")
(declare-function +agent-shell-projects-sidebar "+agent-shell-projects")
(declare-function +agent-shell-projects-ask "+agent-shell-projects-agent")
(declare-function +agent-shell-projects-compare "+agent-shell-projects-code")

(defun +agent-shell-projects-annotation (project id)
  "Return annotation ID from PROJECT."
  (or (seq-find (lambda (item) (equal (plist-get item :id) id))
                (plist-get project :annotations))
      (error "Unknown annotation %s" id)))

(defun +agent-shell-projects-annotate
    (project artefact version start end body &optional actor context)
  "Annotate START through END in VERSION of ARTEFACT in PROJECT.
Offsets are zero-based.  BODY is attributed to ACTOR, defaulting to the user.
CONTEXT optionally identifies another text space, such as a diff side."
  (let* ((text (or (plist-get context :text)
                   (+agent-shell-projects-content version)))
         (length (length text)))
    (unless (and (integerp start) (integerp end) (<= 0 start)
                 (< start end) (<= end length))
      (error "Annotation selection is outside the saved text"))
    (unless (and (stringp body) (not (string-empty-p (string-trim body))))
      (error "Annotation is empty"))
    (let ((annotation
           (list :id (+agent-shell-projects--id)
                 :artefact (plist-get artefact :id)
                 :version (plist-get version :id) :start start :end end
                 :quote (substring text start end)
                 :prefix (substring text (max 0 (- start 80)) start)
                 :suffix (substring text end (min length (+ end 80)))
                 :context context :author (or actor "user") :body body
                 :time (float-time) :replies nil :resolved nil
                 :submitted nil)))
      (+agent-shell-projects--set
       project :annotations
       (append (plist-get project :annotations) (list annotation)))
      (+agent-shell-projects--changed project artefact (or actor "user"))
      annotation)))

(defun +agent-shell-projects-reply (project annotation body &optional actor)
  "Append BODY by ACTOR to ANNOTATION in PROJECT."
  (when (string-empty-p (string-trim body)) (error "Reply is empty"))
  (let ((reply (list :id (+agent-shell-projects--id) :body body
                     :author (or actor "user") :time (float-time)
                     :submitted nil)))
    (+agent-shell-projects--set
     annotation :replies (append (plist-get annotation :replies) (list reply)))
    (+agent-shell-projects--changed
     project (+agent-shell-projects-artefact
              project (plist-get annotation :artefact)) (or actor "user"))
    reply))

(defun +agent-shell-projects-set-resolution (project annotation resolved actor)
  "Set ANNOTATION's RESOLVED state in PROJECT, only for ACTOR user."
  (unless (equal actor "user") (error "Only the user can resolve or reopen"))
  (+agent-shell-projects--set annotation :resolved (and resolved t))
  (+agent-shell-projects--changed project nil "user"))

(defun +agent-shell-projects--mapped-range (old new start end)
  "Map START and END through replacement of OLD by NEW using Emacs markers."
  (with-temp-buffer
    (insert old)
    (let ((begin (copy-marker (1+ start) t))
          (finish (copy-marker (1+ end) nil)))
      (unwind-protect
          (progn
            (replace-region-contents (point-min) (point-max) (lambda () new))
            (cons (1- (marker-position begin)) (1- (marker-position finish))))
        (set-marker begin nil) (set-marker finish nil)))))

(defun +agent-shell-projects-attachment (annotation text &optional original)
  "Locate ANNOTATION in TEXT, using ORIGINAL to map edits when provided.
Return a plist with :state and, for a usable attachment, :start and :end."
  (let* ((annotation (or (plist-get annotation :anchor) annotation))
         (quote (plist-get annotation :quote))
         (prefix (plist-get annotation :prefix))
         (suffix (plist-get annotation :suffix))
         (offset 0) matches)
    (if (and original (equal text original))
        (list :state "attached" :start (plist-get annotation :start)
              :end (plist-get annotation :end))
      (while (string-match (regexp-quote quote) text offset)
        (let* ((start (match-beginning 0)) (end (match-end 0))
               (score (+ (if (and (not (string-empty-p prefix))
                                  (string-suffix-p prefix (substring text 0 start)))
                             1 0)
                         (if (and (not (string-empty-p suffix))
                                  (string-prefix-p suffix (substring text end)))
                             1 0))))
          (push (list score start end) matches)
          (setq offset (max (1+ start) end))))
      (setq matches (sort matches (lambda (a b) (> (car a) (car b)))))
      (cond
       ((and matches
             (or (= (length matches) 1)
                 (> (caar matches) (car (cadr matches)))))
        (list :state "attached" :start (cadar matches) :end (caddar matches)))
       (matches (list :state "ambiguous"))
       (original
        (let* ((range (+agent-shell-projects--mapped-range
                       original text (plist-get annotation :start)
                       (plist-get annotation :end)))
               (start (car range)) (end (cdr range)))
          (if (<= end start) (list :state "deleted")
            ;; A mapped replacement is useful to inspect, but still needs review.
            (list :state "changed" :start start :end end))))
       (t (list :state "detached"))))))

(defun +agent-shell-projects--annotation-original (artefact annotation)
  "Return ANNOTATION's original text in ARTEFACT."
  (let ((anchor (or (plist-get annotation :anchor) annotation)))
    (or (plist-get (plist-get anchor :context) :text)
        (+agent-shell-projects-content
         (+agent-shell-projects-version artefact (plist-get anchor :version))))))

(defun +agent-shell-projects--decorate (project artefact version)
  "Display PROJECT's annotation anchors on ARTEFACT at VERSION."
  (remove-overlays (point-min) (point-max) 'project-annotation t)
  (let* ((source (and buffer-file-name
                      (not (equal (plist-get artefact :kind) "code"))
                      (+agent-shell-projects-source artefact)))
         (origin (if source (cadr source) (point-min)))
         (text (if source (buffer-substring-no-properties origin (caddr source))
                 (+agent-shell-projects-content version))))
    (dolist (annotation (plist-get project :annotations))
      (when (and (equal (plist-get annotation :artefact) (plist-get artefact :id))
                 (not (plist-get annotation :context))
                 (not (plist-get annotation :resolved)))
        (let ((attachment (+agent-shell-projects-attachment
                           annotation text
                           (+agent-shell-projects--annotation-original
                            artefact annotation))))
          (when (plist-get attachment :start)
            (let ((overlay (make-overlay
                            (+ origin (plist-get attachment :start))
                            (+ origin (plist-get attachment :end)))))
              (overlay-put overlay 'project-annotation t)
              (overlay-put overlay 'project-annotation-id (plist-get annotation :id))
              (overlay-put overlay 'face 'highlight)
              (overlay-put overlay 'mouse-face 'highlight)
              (overlay-put overlay 'keymap +agent-shell-projects-annotation-overlay-map)
              (overlay-put overlay 'help-echo (plist-get annotation :body)))))))))

(defvar +agent-shell-projects-annotation-overlay-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'+agent-shell-projects-show-thread)
    map)
  "Mouse actions on annotated passages; keyboard actions use Embark.")

(defun +agent-shell-projects-show-thread (event)
  "Show the sidebar thread for the annotated passage at mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (when-let* ((id (seq-some (lambda (o) (overlay-get o 'project-annotation-id))
                           (overlays-at (point))))
              (window (+agent-shell-projects-sidebar (current-buffer))))
    (with-selected-window window
      (goto-char (point-min))
      (let ((position (text-property-any (point-min) (point-max)
                                         'project-annotation-id id)))
        (when position
          (goto-char position)
          (magit-section-show (magit-current-section)))))))

(defun +agent-shell-projects--render-comments (project artefact view)
  "Insert annotation threads for PROJECT, ARTEFACT, and VIEW."
  (let* ((version (and (buffer-local-value '+agent-shell-projects--version-id view)
                   (+agent-shell-projects-version
                   artefact (buffer-local-value
                             '+agent-shell-projects--version-id view))))
         (text (if version (+agent-shell-projects-content version) ""))
         (attachments (make-hash-table :test #'equal)))
    (dolist (annotation (plist-get project :annotations))
      (when (equal (plist-get annotation :artefact) (plist-get artefact :id))
        (let ((target-text
               (or (and +agent-shell-projects-annotation-text-function
                        (with-current-buffer view
                          (funcall +agent-shell-projects-annotation-text-function
                                   artefact version annotation))) text)))
          (puthash (plist-get annotation :id)
                   (+agent-shell-projects-attachment
                    annotation target-text
                    (+agent-shell-projects--annotation-original artefact annotation))
                   attachments))))
    (magit-insert-section (comments)
      (magit-insert-heading "Comments")
      (dolist (annotation
               (sort (copy-sequence (plist-get project :annotations))
                     (lambda (a b)
                       (< (or (plist-get (gethash (plist-get a :id) attachments) :start)
                              most-positive-fixnum)
                          (or (plist-get (gethash (plist-get b :id) attachments) :start)
                              most-positive-fixnum)))))
        (when (and (equal (plist-get annotation :artefact) (plist-get artefact :id))
                   (or +agent-shell-projects--show-resolved
                       (not (plist-get annotation :resolved))))
          (let* ((attachment (gethash (plist-get annotation :id) attachments))
                 (start (point)))
            (magit-insert-section (annotation (plist-get annotation :id) t)
              (magit-insert-heading
               (format "“%s”%s\n"
                       (truncate-string-to-width
                        (replace-regexp-in-string "\n" " "
                                                  (plist-get annotation :quote)) 42)
                       (if (equal (plist-get attachment :state) "attached") ""
                         (format " [%s]" (plist-get attachment :state)))))
              (insert (format "%s%s:\n%s\n"
                              (plist-get annotation :author)
                              (if (plist-get annotation :resolved) " · resolved" "")
                              (plist-get annotation :body)))
              (dolist (reply (plist-get annotation :replies))
                (insert (format "\n%s:\n%s\n" (plist-get reply :author)
                                (plist-get reply :body))))
              (insert "\n"))
            (add-text-properties start (point)
                                 (list 'project-annotation-id
                                       (plist-get annotation :id)))))))))

(defun +agent-shell-projects--selection ()
  "Capture the exact selected passage and return a context record."
  (unless (use-region-p) (user-error "Select a passage first"))
  (or (and +agent-shell-projects-selection-function
           (funcall +agent-shell-projects-selection-function))
      (let* ((project (+agent-shell-projects--project))
             (artefact (+agent-shell-projects--artefact))
             (live (and buffer-file-name t))
             (version (if live
                          (+agent-shell-projects-capture project artefact "user")
                        (+agent-shell-projects-version
                         artefact +agent-shell-projects--version-id)))
             (origin (if live (cadr (+agent-shell-projects-source artefact))
                       (point-min))))
        (list :project (plist-get project :id)
              :artefact (plist-get artefact :id) :version (plist-get version :id)
              :start (- (region-beginning) origin)
              :end (- (region-end) origin)))))

(defun +agent-shell-projects--register-target (type context &optional bounds)
  "Register CONTEXT as a TYPE target, optionally with BOUNDS."
  (let ((id (+agent-shell-projects--id)))
    (puthash id context +agent-shell-projects--targets)
    (if bounds (cl-list* type id (car bounds) (cdr bounds)) (cons type id))))

(defun +agent-shell-projects--target-context (target)
  "Return the saved context for TARGET."
  (or (gethash target +agent-shell-projects--targets)
      (user-error "This action target has expired")))

(defun +agent-shell-projects-embark-target ()
  "Find a project target without affecting unrelated buffers."
  (when +agent-shell-projects--project-id
    (cond
     ((and +agent-shell-projects--artefact-id (use-region-p)
           (not (derived-mode-p '+agent-shell-projects-sidebar-mode)))
      (+agent-shell-projects--register-target
       '+agent-shell-projects-passage (+agent-shell-projects--selection)
       (cons (region-beginning) (region-end))))
     ((or (get-text-property (point) 'project-annotation-id)
          (seq-some (lambda (o) (overlay-get o 'project-annotation-id))
                    (overlays-at (point))))
      (+agent-shell-projects--register-target
       '+agent-shell-projects-annotation
       (list :project +agent-shell-projects--project-id
             :annotation
             (or (get-text-property (point) 'project-annotation-id)
                 (seq-some (lambda (o) (overlay-get o 'project-annotation-id))
                           (overlays-at (point)))))))
     ((button-at (point))
      (let ((target (button-get (button-at (point)) 'project-target)))
        (if (bufferp target) (cons 'buffer (buffer-name target))
          (when target
            (+agent-shell-projects--register-target
             '+agent-shell-projects-artefact
             (list :project +agent-shell-projects--project-id
                   :artefact target)))))))))

(defun +agent-shell-projects-add-comment (&optional target)
  "Add a comment on the selection or Embark TARGET."
  (interactive)
  (+agent-shell-projects-compose
   (if target (+agent-shell-projects--target-context target)
     (+agent-shell-projects--selection))))

(defun +agent-shell-projects-compose (context &optional initial callback)
  "Compose feedback for CONTEXT, starting with INITIAL.
CALLBACK receives the accepted body when non-nil, instead of adding a comment."
  (let ((buffer (generate-new-buffer "*Project comment*")))
    (with-current-buffer buffer
      (text-mode)
      (setq +agent-shell-projects--project-id (plist-get context :project))
      (insert (or initial ""))
      (setq-local header-line-format "C-c C-c attach · C-c C-k cancel")
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map text-mode-map)
        (define-key map (kbd "C-c C-k") #'kill-current-buffer)
        (define-key map (kbd "C-c C-c")
                    (lambda ()
                      (interactive)
                      (let* ((body (string-trim (buffer-string)))
                             (project (+agent-shell-projects-load
                                       (plist-get context :project))))
                        (if callback (funcall callback body)
                          (let ((artefact (+agent-shell-projects-artefact
                                           project (plist-get context :artefact))))
                            (+agent-shell-projects-annotate
                             project artefact
                             (+agent-shell-projects-version
                              artefact (plist-get context :version))
                             (plist-get context :start) (plist-get context :end)
                             body "user" (plist-get context :context))))
                        (kill-buffer buffer))))
        (use-local-map map)))
    (pop-to-buffer buffer)))

(defun +agent-shell-projects-target-open (target)
  "Visit artefact Embark TARGET."
  (interactive "sArtefact target: ")
  (let ((context (+agent-shell-projects--target-context target)))
    (+agent-shell-projects-open
     (+agent-shell-projects-load (plist-get context :project))
     (plist-get context :artefact))))

(defun +agent-shell-projects-target-history (target)
  "Open TARGET's previous saved version."
  (interactive "sArtefact target: ")
  (+agent-shell-projects-target-open target)
  (+agent-shell-projects-previous))

(defun +agent-shell-projects-target-compare (target)
  "Compare artefact TARGET with its reviewed or previous saved contents."
  (interactive "sArtefact target: ")
  (+agent-shell-projects-target-open target)
  (require '+agent-shell-projects-code)
  (let* ((project (+agent-shell-projects--project))
         (artefact (+agent-shell-projects--artefact))
         (current (+agent-shell-projects-version artefact +agent-shell-projects--version-id)))
    (if (equal (plist-get artefact :kind) "code")
        (+agent-shell-projects-render-code project artefact current "diff" "reviewed")
      (let* ((history (plist-get artefact :history))
             (index (cl-position current history :test #'eq))
             (previous (if (plist-get artefact :ack)
                           (+agent-shell-projects-version artefact (plist-get artefact :ack))
                         (and index (> index 0) (nth (1- index) history)))))
        (unless previous (user-error "No earlier saved version"))
        (let ((text (+agent-shell-projects--diff
                     (plist-get artefact :name)
                     (encode-coding-string (+agent-shell-projects-content previous) 'utf-8-unix)
                     (encode-coding-string (+agent-shell-projects-content current) 'utf-8-unix)))
              (buffer (get-buffer-create "*Project document comparison*")))
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (erase-buffer) (insert text) (diff-mode) (setq buffer-read-only t)))
          (pop-to-buffer buffer))))))

(defun +agent-shell-projects-visit-at-point ()
  "Visit the artefact or annotation at point."
  (interactive)
  (let ((target (+agent-shell-projects-embark-target)))
    (pcase (car target)
      ('+agent-shell-projects-annotation (+agent-shell-projects-target-visit (cdr target)))
      ('+agent-shell-projects-artefact (+agent-shell-projects-target-open (cdr target)))
      (_ (push-button)))))

(defun +agent-shell-projects-target-comment (target)
  "Compose a comment for Embark TARGET."
  (interactive "sPassage target: ") (+agent-shell-projects-add-comment target))

(defun +agent-shell-projects-target-visit (target)
  "Visit the passage of annotation TARGET."
  (interactive "sAnnotation target: ")
  (let* ((context (+agent-shell-projects--target-context target))
         (project (+agent-shell-projects-load (plist-get context :project)))
         (annotation (+agent-shell-projects-annotation
                      project (plist-get context :annotation))))
    (unless (and +agent-shell-projects-annotation-visit-function
                 (funcall +agent-shell-projects-annotation-visit-function
                          project annotation))
      (let ((anchor (or (plist-get annotation :anchor) annotation)))
        (+agent-shell-projects-open project (plist-get annotation :artefact)
                                    (plist-get anchor :version))
        (goto-char (min (point-max) (1+ (plist-get anchor :start))))))))

(defun +agent-shell-projects-target-reply (target)
  "Compose a reply to annotation TARGET."
  (interactive "sAnnotation target: ")
  (let* ((context (+agent-shell-projects--target-context target))
         (project (+agent-shell-projects-load (plist-get context :project)))
         (annotation (+agent-shell-projects-annotation
                      project (plist-get context :annotation))))
    (+agent-shell-projects-compose
     (list :project (plist-get project :id)
           :artefact (plist-get annotation :artefact)) nil
     (lambda (body) (+agent-shell-projects-reply project annotation body)))))

(defun +agent-shell-projects-target-resolve (target)
  "Toggle resolution of annotation TARGET as the user."
  (interactive "sAnnotation target: ")
  (let* ((context (+agent-shell-projects--target-context target))
         (project (+agent-shell-projects-load (plist-get context :project)))
         (annotation (+agent-shell-projects-annotation
                      project (plist-get context :annotation))))
    (+agent-shell-projects-set-resolution
     project annotation (not (plist-get annotation :resolved)) "user")))

(defun +agent-shell-projects-reattach (target)
  "Reattach annotation TARGET using a passage selected in its artefact buffer."
  (interactive "sAnnotation target: ")
  (let* ((context (+agent-shell-projects--target-context target))
         (project (+agent-shell-projects-load (plist-get context :project)))
         (annotation (+agent-shell-projects-annotation
                      project (plist-get context :annotation)))
         (buffers (seq-filter
                   (lambda (buf)
                     (with-current-buffer buf
                       (and (equal +agent-shell-projects--artefact-id
                                   (plist-get annotation :artefact))
                            (use-region-p)))) (buffer-list))))
    (unless buffers (user-error "Select the replacement passage in the artefact"))
    (let ((selection (with-current-buffer (car buffers)
                       (+agent-shell-projects--selection))))
      (let* ((artefact (+agent-shell-projects-artefact
                        project (plist-get annotation :artefact)))
             (text (or (plist-get (plist-get selection :context) :text)
                       (+agent-shell-projects-content
                        (+agent-shell-projects-version
                         artefact (plist-get selection :version)))))
             (start (plist-get selection :start)) (end (plist-get selection :end)))
        (+agent-shell-projects--set
         annotation :anchor
         (list :version (plist-get selection :version) :start start :end end
               :quote (substring text start end)
               :prefix (substring text (max 0 (- start 80)) start)
               :suffix (substring text end (min (length text) (+ end 80)))
               :context (plist-get selection :context))))
      (+agent-shell-projects--changed project nil "user"))))

(defun +agent-shell-projects-toggle-resolved ()
  "Toggle resolved comments in the sidebar."
  (interactive)
  (setq +agent-shell-projects--show-resolved
        (not +agent-shell-projects--show-resolved))
  (let ((view +agent-shell-projects--view-buffer))
    (when view (+agent-shell-projects-sidebar view))))

(defvar +agent-shell-projects-passage-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'+agent-shell-projects-target-comment)
    map))
(defvar +agent-shell-projects-annotation-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'+agent-shell-projects-target-visit)
    (define-key map (kbd "r") #'+agent-shell-projects-target-reply)
    (define-key map (kbd "s") #'+agent-shell-projects-target-resolve)
    (define-key map (kbd "a") #'+agent-shell-projects-reattach)
    map))
(defvar +agent-shell-projects-artefact-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'+agent-shell-projects-target-open)
    (define-key map (kbd "h") #'+agent-shell-projects-target-history)
    (define-key map (kbd "d") #'+agent-shell-projects-target-compare)
    map))

(with-eval-after-load 'embark
  (add-hook 'embark-target-finders #'+agent-shell-projects-embark-target)
  (dolist (entry '((+agent-shell-projects-passage . +agent-shell-projects-passage-map)
                   (+agent-shell-projects-annotation . +agent-shell-projects-annotation-map)
                   (+agent-shell-projects-artefact . +agent-shell-projects-artefact-map)))
    (setf (alist-get (car entry) embark-keymap-alist) (list (cdr entry)))))

(add-hook '+agent-shell-projects-view-functions #'+agent-shell-projects--decorate)
(add-hook '+agent-shell-projects-sidebar-functions #'+agent-shell-projects--render-comments)
(define-key +agent-shell-projects-sidebar-mode-map (kbd "f")
            #'+agent-shell-projects-toggle-resolved)

(provide '+agent-shell-projects-annotations)
;;; +agent-shell-projects-annotations.el ends here
