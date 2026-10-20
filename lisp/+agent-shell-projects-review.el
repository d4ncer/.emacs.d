;;; +agent-shell-projects-review.el --- Connected code explanations -*- lexical-binding: t; -*-

;;; Commentary:
;; Structured, version-specific summaries and walkthroughs linked to code.

;;; Code:

(require '+agent-shell-projects-code)

(defvar +agent-shell-projects--positions (make-hash-table :test #'equal))
(defvar-local +agent-shell-projects--navigation nil)
(defvar +agent-shell-projects--proposal-drafts (make-hash-table :test #'equal))

(defun +agent-shell-projects-explanation (artefact version)
  "Return ARTEFACT's explanation for VERSION, if available."
  (seq-find (lambda (entry) (equal (plist-get entry :version)
                                  (plist-get version :id)))
            (plist-get artefact :explanations)))

(defun +agent-shell-projects--explanation-operation (project agent operation)
  "Accept a version-specific explanation OPERATION from AGENT for PROJECT."
  (let* ((request (+agent-shell-projects--request
                   project (plist-get operation :request)))
         (artefact (+agent-shell-projects-artefact
                    project (plist-get operation :artefact)))
         (version (+agent-shell-projects-version
                   artefact (plist-get operation :version)))
         (manifest (+agent-shell-projects-code-manifest version))
         (paths (mapcar (lambda (file) (plist-get file :path))
                        (plist-get manifest :files)))
         (summary (plist-get operation :summary))
         (walkthrough (plist-get operation :walkthrough))
         ids)
    (when (+agent-shell-projects-explanation artefact version)
      (error "An explanation is already recorded for this version"))
    (unless (and (equal (plist-get request :kind) "explanation")
                 (equal (plist-get request :status) "sent")
                 (equal (plist-get request :agent) (plist-get agent :id))
                 (equal (plist-get request :artefact) (plist-get artefact :id))
                 (equal (plist-get request :version) (plist-get version :id))
                 (stringp summary) (not (string-empty-p (string-trim summary)))
                 (listp walkthrough) walkthrough)
      (error "Explanation does not match a pending request"))
    (dolist (entry walkthrough)
      (unless (and (stringp (plist-get entry :id))
                   (not (member (plist-get entry :id) ids))
                   (stringp (plist-get entry :title))
                   (stringp (plist-get entry :body))
                   (member (plist-get entry :basis) '("recorded" "inferred"))
                   (cl-every (lambda (path) (member path paths))
                             (plist-get entry :files)))
        (error "Invalid walkthrough entry or code reference"))
      (push (plist-get entry :id) ids)
      (dolist (link (plist-get entry :proposal))
        (let ((document (+agent-shell-projects-artefact
                         project (plist-get link :artefact))))
          (+agent-shell-projects-version document (plist-get link :version))
          (unless (seq-find
                   (lambda (v) (and (equal (plist-get v :artefact)
                                           (plist-get link :artefact))
                                    (equal (plist-get v :version)
                                           (plist-get link :version))))
                   (plist-get request :versions))
            (error "Proposal link is outside the captured explanation context")))))
    (let ((explanation (list :version (plist-get version :id) :summary summary
                             :walkthrough walkthrough :request (plist-get request :id)
                             :author (plist-get agent :id) :time (float-time))))
      (+agent-shell-projects--set
       artefact :explanations
       (append (seq-remove (lambda (e) (equal (plist-get e :version)
                                             (plist-get version :id)))
                           (plist-get artefact :explanations)) (list explanation)))
      (+agent-shell-projects--set request :status "ready")
      (+agent-shell-projects--changed project artefact (plist-get agent :id))
      (plist-get version :id))))

(defun +agent-shell-projects--code-artefact (project)
  "Choose a code artefact from PROJECT."
  (if (and +agent-shell-projects--artefact-id
           (equal (plist-get (+agent-shell-projects--artefact) :kind) "code"))
      (+agent-shell-projects--artefact)
    (let ((items (seq-filter (lambda (a) (equal (plist-get a :kind) "code"))
                             (plist-get project :artefacts))))
      (unless items (user-error "Enrol a Git workspace first"))
      (if (= 1 (length items)) (car items)
        (cdr (assoc (completing-read "Code artefact: "
                                     (mapcar (lambda (a) (cons (plist-get a :name) a)) items)
                                     nil t)
                    (mapcar (lambda (a) (cons (plist-get a :name) a)) items)))))))

(defun +agent-shell-projects-request-explanation (&optional project artefact version)
  "Ask the author for explanations of PROJECT ARTEFACT at VERSION."
  (interactive)
  (let* ((project (or project (+agent-shell-projects--project)))
         (artefact (or artefact (+agent-shell-projects--code-artefact project)))
         (version (or version
                      (and (plist-get artefact :current)
                           (+agent-shell-projects-version artefact (plist-get artefact :current)))
                      (user-error "Wait for the first capture before generating explanations")))
         (agent (+agent-shell-projects-agent project (plist-get project :author)))
         (existing (seq-find
                    (lambda (r)
                      (and (equal (plist-get r :kind) "explanation")
                           (equal (plist-get r :version) (plist-get version :id))
                           (member (plist-get r :status) '("queued" "sent"))))
                    (plist-get project :requests)))
         (versions (mapcar
                    (lambda (a) (list :artefact (plist-get a :id)
                                      :version (plist-get a :current)))
                    (plist-get project :artefacts)))
         (request
          (or existing
              (+agent-shell-projects-request
               project "explanation" agent
               :artefact (plist-get artefact :id) :version (plist-get version :id)
               :versions versions
               :instructions
               (concat
                "Return an explanation operation with this request ID, artefact and version. "
                "summary is Markdown covering problem, outcome, success criteria, constraints, priorities or actual weights, major alternatives and tradeoffs, and validation gaps. "
                "walkthrough is an array of entries with unique id, title, body, basis (recorded or inferred), files (changed paths), optional proposal links (artefact/version), and optional validation text. "
                "Explain behaviors, decisions, why they contribute to the outcome, alternatives and tradeoffs in body. Only describe alternatives as considered when evidence supports that. "
                "Tie validation to observable criteria and actual checks; identify unverified behavior. Inspect source against the supplied immutable capture; live files may have advanced.")))))
    (+agent-shell-projects-flush project)
    request))

(defun +agent-shell-projects-review-code ()
  "Enter the connected summary, walkthrough, and diff workspace."
  (interactive)
  (let* ((project (+agent-shell-projects--project))
         (artefact (+agent-shell-projects--code-artefact project))
         (version (and (plist-get artefact :current)
                       (+agent-shell-projects-version artefact (plist-get artefact :current)))))
    (+agent-shell-projects-render-code project artefact version "summary")))

(defun +agent-shell-projects--navigation-state ()
  "Return a position-preserving code navigation state."
  (list :view +agent-shell-projects--code-view :point (point)
        :version +agent-shell-projects--version-id
        :comparison +agent-shell-projects--comparison
        :focus +agent-shell-projects--focus-files))

(defun +agent-shell-projects--navigate (view &optional files entry)
  "Navigate to VIEW, optionally focusing FILES or walkthrough ENTRY."
  (push (+agent-shell-projects--navigation-state) +agent-shell-projects--navigation)
  (+agent-shell-projects-render-code
   (+agent-shell-projects--project) (+agent-shell-projects--artefact)
   (+agent-shell-projects-version (+agent-shell-projects--artefact)
                                 +agent-shell-projects--version-id)
   view +agent-shell-projects--comparison files)
  (when entry
    (goto-char (point-min))
    (when-let* ((position (text-property-any (point-min) (point-max)
                                            'project-walkthrough-id entry)))
      (goto-char position))))

(defun +agent-shell-projects-cycle-view ()
  "Cycle summary, walkthrough, and diff while preserving each position."
  (interactive)
  (unless (derived-mode-p '+agent-shell-projects-code-mode)
    (user-error "Open code review first"))
  (+agent-shell-projects--navigate
   (pcase +agent-shell-projects--code-view
     ("summary" "walkthrough") ("walkthrough" "diff") (_ "summary"))))

(defun +agent-shell-projects-back ()
  "Return to the previous code view and position."
  (interactive)
  (unless +agent-shell-projects--navigation (user-error "No previous review location"))
  (let ((state (pop +agent-shell-projects--navigation)))
    (+agent-shell-projects-render-code
     (+agent-shell-projects--project) (+agent-shell-projects--artefact)
     (+agent-shell-projects-version (+agent-shell-projects--artefact)
                                   (plist-get state :version))
     (plist-get state :view) (plist-get state :comparison) (plist-get state :focus))
    (goto-char (min (point-max) (plist-get state :point)))))

(defun +agent-shell-projects-whole-diff ()
  "Show all files in the current code comparison."
  (interactive) (+agent-shell-projects--navigate "diff"))

(defun +agent-shell-projects--render-explanation (project artefact version view)
  "Render PROJECT ARTEFACT's explanation at VERSION in VIEW."
  (when (member view '("summary" "walkthrough"))
    (let ((explanation (+agent-shell-projects-explanation artefact version)))
      (magit-insert-section (explanation view)
        (magit-insert-heading (if (equal view "summary") "Summary" "Walkthrough"))
        (if (not explanation)
            (progn
              (insert "No explanation has been generated for this captured version.\n\n")
              (when (plist-get artefact :explanations)
                (insert "Explanations for older versions remain available through history.\n\n"))
              (insert-text-button
               "Generate explanations" 'follow-link t
               'action (lambda (_button)
                         (+agent-shell-projects-request-explanation project artefact version)))
              (insert "\n"))
          (if (equal view "summary")
              (progn
                (insert (plist-get explanation :summary) "\n\n")
                (dolist (entry (plist-get explanation :walkthrough))
                  (insert-text-button
                   (plist-get entry :title) 'follow-link t
                   'action (lambda (_button)
                             (+agent-shell-projects--navigate
                              "walkthrough" nil (plist-get entry :id))))
                  (insert "\n")))
            (dolist (entry (plist-get explanation :walkthrough))
              (let ((start (point)))
                (magit-insert-section (decision (plist-get entry :id))
                  (magit-insert-heading
                   (format "%s [%s]" (plist-get entry :title) (plist-get entry :basis)))
                  (insert (plist-get entry :body) "\n\n")
                  (when (plist-get entry :validation)
                    (insert "Validation\n" (plist-get entry :validation) "\n\n"))
                  (dolist (link (plist-get entry :proposal))
                    (let ((document (+agent-shell-projects-artefact
                                     project (plist-get link :artefact))))
                      (insert-text-button
                       (concat "Proposal: " (plist-get document :name)) 'follow-link t
                       'action (lambda (_button)
                                 (+agent-shell-projects-open
                                  project (plist-get document :id) (plist-get link :version)))))
                    (insert "\n"))
                  (when (plist-get entry :files)
                    (insert-text-button
                     "Inspect related changes" 'follow-link t
                     'action (lambda (_button)
                               (+agent-shell-projects--navigate
                                "diff" (plist-get entry :files))))
                    (insert "\n")))
                (add-text-properties start (point)
                                     (list 'project-walkthrough-id (plist-get entry :id)))))))))
    t))

(defun +agent-shell-projects--explanation-selection ()
  "Return an annotation context for selected explanation text."
  (when (member +agent-shell-projects--code-view '("summary" "walkthrough"))
    (list :project +agent-shell-projects--project-id
          :artefact +agent-shell-projects--artefact-id
          :version +agent-shell-projects--version-id
          :start (1- (region-beginning)) :end (1- (region-end))
          :context (list :space +agent-shell-projects--code-view
                         :text (buffer-substring-no-properties (point-min) (point-max))))))

(defun +agent-shell-projects--updated-code (project artefact)
  "Queue updated explanations when PROJECT's code ARTEFACT has advanced."
  (when (and artefact (equal (plist-get artefact :kind) "code")
             (plist-get artefact :explanations) (plist-get project :author)
             (not (+agent-shell-projects-explanation
                   artefact (+agent-shell-projects-version
                             artefact (plist-get artefact :current)))))
    (+agent-shell-projects-request-explanation
     project artefact (+agent-shell-projects-version artefact (plist-get artefact :current)))))

(defun +agent-shell-projects-request-proposal (file)
  "Ask the author to reconstruct a proposal, to be saved at FILE after editing."
  (interactive (list (read-file-name "Proposal destination: " nil nil nil "proposal.md")))
  (let* ((project (+agent-shell-projects--project))
         (agent (+agent-shell-projects-agent project (plist-get project :author)))
         (artefacts (plist-get project :artefacts))
         (existing (seq-find
                    (lambda (r)
                      (and (equal (plist-get r :kind) "proposal")
                           (equal (plist-get r :destination) (expand-file-name file))
                           (member (plist-get r :status) '("queued" "sent" "ready"))))
                    (reverse (plist-get project :requests))))
         (request (or existing (+agent-shell-projects-request
                   project "proposal" agent :destination (expand-file-name file)
                   :versions (mapcar (lambda (a) (list :artefact (plist-get a :id)
                                                       :version (plist-get a :current))) artefacts)
                   :instructions
                   "Reconstruct a proposal from the supplied existing work: problem, success criteria, scope and exclusions, constraints and priorities, possible solutions and tradeoffs, and recommendation. Distinguish recorded intent, inferred intent, and questions. Return a draft operation for this request; the user will edit and save it."))))
    (+agent-shell-projects-flush project)
    (when (equal (plist-get request :status) "ready")
      (+agent-shell-projects--proposal-ready project request))
    request))

(defun +agent-shell-projects--proposal-ready (project request)
  "Let the user edit and save PROJECT's reconstructed proposal REQUEST."
  (when (equal (plist-get request :kind) "proposal")
    (+agent-shell-projects-compose
     (list :project (plist-get project :id)) (plist-get request :result)
     (lambda (body)
       (unless (equal (plist-get request :status) "ready")
         (user-error "Proposal already saved"))
       (let ((artefact (+agent-shell-projects-extract-prose
                        project body (plist-get request :destination))))
         (+agent-shell-projects--set project :proposal (plist-get artefact :id))
         (+agent-shell-projects--set request :status "captured")
         (+agent-shell-projects-save project)
         (+agent-shell-projects-open project (plist-get artefact :id)))))))

(defun +agent-shell-projects-set-proposal ()
  "Choose an existing document as the current project's proposal."
  (interactive)
  (let* ((project (+agent-shell-projects--project))
         (items (seq-filter (lambda (a) (not (equal (plist-get a :kind) "code")))
                             (plist-get project :artefacts)))
         (choices (mapcar (lambda (a) (cons (plist-get a :name) (plist-get a :id))) items))
         (id (cdr (assoc (completing-read "Proposal: " choices nil t) choices))))
    (+agent-shell-projects--set project :proposal id)
    (+agent-shell-projects--changed project nil "user")))

(setf (alist-get "explanation" +agent-shell-projects-operation-handlers nil nil #'equal)
      #'+agent-shell-projects--explanation-operation)
(add-hook '+agent-shell-projects-code-render-functions #'+agent-shell-projects--render-explanation)
(add-hook '+agent-shell-projects-code-selection-functions #'+agent-shell-projects--explanation-selection)
(add-hook '+agent-shell-projects-change-hook #'+agent-shell-projects--updated-code)
(add-hook '+agent-shell-projects-request-ready-hook #'+agent-shell-projects--proposal-ready)
(define-key +agent-shell-projects-code-mode-map (kbd "C-c C-v") #'+agent-shell-projects-cycle-view)
(define-key +agent-shell-projects-code-mode-map (kbd "M-,") #'+agent-shell-projects-back)
(define-key +agent-shell-projects-code-mode-map (kbd "C-c C-a") #'+agent-shell-projects-whole-diff)

(provide '+agent-shell-projects-review)
;;; +agent-shell-projects-review.el ends here
