;;; agent-shell-projects-test.el --- Project acceptance tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Focused tests for durable authoring and review state, using disposable data.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-id)
(require '+agent-shell-projects)
(require 'agent-shell)
(require '+agent-shell-projects-code)
(require '+agent-shell-projects-review)
(+agent-shell-projects-setup)

(defun +agent-shell-projects-test--await (predicate)
  "Let the actual event loop run until PREDICATE succeeds."
  (let ((deadline (+ (float-time) 20)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (should (funcall predicate))))

(defun +agent-shell-projects-test--captured (artefact)
  "Wait for ARTEFACT's asynchronous capture, surfacing worker errors."
  (+agent-shell-projects-test--await
   (lambda () (not (gethash (file-name-as-directory (file-truename (plist-get artefact :path)))
                            +agent-shell-projects--jobs))))
  (when (plist-get artefact :capture-error) (error "%s" (plist-get artefact :capture-error)))
  (+agent-shell-projects-version artefact (plist-get artefact :current)))

(defun +agent-shell-projects-test--enrol-code (&rest args)
  "Enrol then await a real worker for legacy functional assertions."
  (let ((artefact (apply #'+agent-shell-projects-enrol-code args)))
    (+agent-shell-projects-test--captured artefact)
    artefact))

(defun +agent-shell-projects-test--capture-code (project artefact &optional actor)
  "Capture through the production asynchronous path and await completion."
  (+agent-shell-projects-capture-code project artefact actor)
  (+agent-shell-projects-test--captured artefact))

(defun +agent-shell-projects-test--expand ()
  "Expand the small functional fixture, including prepared diff blocks."
  (let ((again t) (deadline (+ (float-time) 20)))
    (save-excursion
      (while (and again (< (float-time) deadline))
        (setq again nil)
        (magit-map-sections
         (lambda (section)
           (when (oref section washer)
             (setq again t) (magit-section-show section))))
        (goto-char (point-min))
        (when (search-forward "Load more " nil t)
          (when-let* ((button (button-at (1- (point)))))
            (button-activate button) (setq again t)))
        (when (or (> +agent-shell-projects--render-pending 0)
                  +agent-shell-projects--worker-queue
                  (> (hash-table-count +agent-shell-projects--workers) 0))
          (accept-process-output nil 0.02) (setq again t))))))

(defun +agent-shell-projects-test--render-code (&rest args)
  "Render a fixture and expand it after its real diff workers finish."
  (let ((buffer (apply #'+agent-shell-projects-render-code args)))
    (+agent-shell-projects-test--expand)
    buffer))

(defmacro +agent-shell-projects-test--isolated (&rest body)
  "Run BODY with a disposable store and source directory."
  (declare (indent 0))
  `(let* ((directory (make-temp-file "agent-projects-test-" t))
          (+agent-shell-projects-directory (expand-file-name "store/" directory))
          (+agent-shell-projects-inbox-directory (expand-file-name "inbox/" directory))
          (+agent-shell-projects--projects (make-hash-table :test #'equal))
          (+agent-shell-projects--jobs (make-hash-table :test #'equal))
          (+agent-shell-projects--workers (make-hash-table :test #'eq))
          (+agent-shell-projects--worker-queue nil)
          (+agent-shell-projects--worker-pump nil)
          (+agent-shell-projects--event-timers (make-hash-table :test #'equal))
          (+agent-shell-projects--ui-pending (make-hash-table :test #'equal))
          (+agent-shell-projects--ui-timer nil)
          (+agent-shell-projects-job-hook nil)
          (+agent-shell-projects-capture-delay 0.01)
          (+agent-shell-projects-change-hook nil)
          (+agent-shell-projects--shells (make-hash-table :test #'equal))
          (+agent-shell-drive--watchers (make-hash-table :test #'equal))
          (+agent-shell-drive--shells (make-hash-table :test #'eq))
          (org-id-locations nil)
          (org-id-track-globally nil)
          (project (+agent-shell-projects-create "Example" directory)))
     (unwind-protect (progn ,@body)
       (dolist (state (hash-table-values +agent-shell-projects--jobs))
         (+agent-shell-projects-cancel-capture (plist-get state :root)))
       (dolist (handle (copy-sequence +agent-shell-projects--worker-queue))
         (+agent-shell-projects--stop-worker handle))
       (dolist (process (hash-table-keys +agent-shell-projects--workers))
         (+agent-shell-projects--stop-worker process))
       (when (timerp +agent-shell-projects--ui-timer) (cancel-timer +agent-shell-projects--ui-timer))
       (maphash (lambda (_id timer) (cancel-timer timer)) +agent-shell-projects--event-timers)
       (dolist (buffer (buffer-list))
         (when (and (buffer-live-p buffer)
                    (or (with-current-buffer buffer
                     (and buffer-file-name
                          (file-in-directory-p buffer-file-name directory)))
                   (with-current-buffer buffer
                     (equal +agent-shell-projects--project-id
                            (plist-get project :id)))))
           (with-current-buffer buffer (set-buffer-modified-p nil))
           (kill-buffer buffer)))
       (delete-directory directory t))))

(defun +agent-shell-projects-test--file (directory name text)
  "Write NAME containing TEXT beneath DIRECTORY and return its path."
  (let ((file (expand-file-name name directory)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (insert text))
    file))

(ert-deftest +agent-shell-projects-test-persistence-and-deduplication ()
  (+agent-shell-projects-test--isolated
    (let* ((file (+agent-shell-projects-test--file directory "spec.md" "α\n"))
           (artefact (+agent-shell-projects-enrol project file))
           (first (plist-get artefact :current)))
      (+agent-shell-projects-capture project artefact)
      (should (= 1 (length (plist-get artefact :history))))
      (with-current-buffer (find-buffer-visiting file)
        (goto-char (point-max)) (insert "second\n") (save-buffer))
      (should (= 2 (length (plist-get artefact :history))))
      (should (equal "α\n" (+agent-shell-projects-content
                             (+agent-shell-projects-version artefact first))))
      (clrhash +agent-shell-projects--projects)
      (let* ((restored (+agent-shell-projects-load (plist-get project :id)))
             (item (car (plist-get restored :artefacts))))
        (should (equal "Example" (plist-get restored :name)))
        (should (= 2 (length (plist-get item :history))))
        (should (+agent-shell-projects-needs-attention-p item)))
      (with-temp-buffer
        (should
         (zerop
          (call-process
           (expand-file-name invocation-name invocation-directory) nil t nil
           "-Q" "--batch" "-L"
           (file-name-directory (locate-library "+agent-shell-projects-core"))
           "--eval"
           (prin1-to-string
            `(progn
               (require '+agent-shell-projects-core)
               (setq +agent-shell-projects-directory ,+agent-shell-projects-directory)
               (let* ((p (+agent-shell-projects-load ,(plist-get project :id)))
                      (a (car (plist-get p :artefacts))))
                 (unless (and (= 2 (length (plist-get a :history)))
                              (equal "α\n"
                                     (+agent-shell-projects-content
                                      (+agent-shell-projects-version a ,first))))
                   (error "Fresh-process history recovery failed"))))))))))))

(ert-deftest +agent-shell-projects-test-subtree-move-and-rename ()
  (+agent-shell-projects-test--isolated
    (let* ((file (+agent-shell-projects-test--file
                  directory "notes.org" "* First\nBody\n* Other\nKeep\n"))
           (buffer (find-file-noselect file))
           (artefact (with-current-buffer buffer
                       (goto-char (point-min))
                       (+agent-shell-projects-enrol project file "First" t))))
      (with-current-buffer buffer
        (goto-char (point-min))
        (delete-region (point) (line-end-position))
        (insert "* Renamed")
        (let ((text (buffer-substring-no-properties
                     (point-min) (save-excursion (org-end-of-subtree t t) (point)))))
          (delete-region (point-min)
                         (save-excursion (org-end-of-subtree t t) (point)))
          (goto-char (point-max)) (insert text))
        (save-buffer))
      (let ((text (+agent-shell-projects-content
                   (+agent-shell-projects-capture project artefact))))
        (should (string-prefix-p "* Renamed" text))
        (should-not (string-match-p "Other" text))
        (should (string-match-p "Body" text))))))

(ert-deftest +agent-shell-projects-test-history-does-not-write-source ()
  (+agent-shell-projects-test--isolated
    (let* ((file (+agent-shell-projects-test--file directory "spec.md" "one\n"))
           (artefact (+agent-shell-projects-enrol project file))
           (first (plist-get artefact :current)))
      (with-current-buffer (find-buffer-visiting file)
        (erase-buffer) (insert "two\n") (save-buffer))
      (save-window-excursion
        (+agent-shell-projects-open project (plist-get artefact :id) first)
        (should (equal "one\n" (buffer-string)))
        (should buffer-read-only)
        (should-not (plist-get artefact :ack))
        (+agent-shell-projects-next)
        (should (equal "two\n" (buffer-string))))
      (should (equal "two\n" (with-temp-buffer
                                (insert-file-contents file) (buffer-string)))))))

(ert-deftest +agent-shell-projects-test-annotations-survive-edits ()
  (+agent-shell-projects-test--isolated
    (let* ((file (+agent-shell-projects-test--file
                  directory "spec.md" "Intro\nCompatibility is required.\nEnd\n"))
           (artefact (+agent-shell-projects-enrol project file))
           (version (+agent-shell-projects-version
                     artefact (plist-get artefact :current)))
           (annotation (+agent-shell-projects-annotate
                        project artefact version 6 32 "Why?")))
      (dolist (new '("New intro\nIntro\nCompatibility is required.\nEnd\n"
                     "End\nCompatibility is required.\nIntro\n"))
        (let ((location (+agent-shell-projects-attachment
                         annotation new (+agent-shell-projects-content version))))
          (should (equal "attached" (plist-get location :state)))
          (should (equal "Compatibility is required."
                         (substring new (plist-get location :start)
                                    (plist-get location :end))))))
      (let ((location (+agent-shell-projects-attachment
                       annotation "Completely replaced.\n"
                       (+agent-shell-projects-content version))))
        (should-not (equal "attached" (plist-get location :state))))
      (should (equal "Compatibility is required."
                     (plist-get annotation :quote)))
      (should-not (plist-get annotation :resolved)))))

(ert-deftest +agent-shell-projects-test-ambiguous-and-deleted-targets ()
  (+agent-shell-projects-test--isolated
    (let* ((file (+agent-shell-projects-test--file directory "spec.md" "target"))
           (artefact (+agent-shell-projects-enrol project file))
           (version (+agent-shell-projects-version
                     artefact (plist-get artefact :current)))
           (annotation (+agent-shell-projects-annotate
                        project artefact version 0 6 "Inspect")))
      (should (equal "ambiguous"
                     (plist-get (+agent-shell-projects-attachment
                                 annotation "target target" "target") :state)))
      (should (equal "deleted"
                     (plist-get (+agent-shell-projects-attachment
                                 annotation "" "target") :state)))
      (should (equal "attached"
                     (plist-get (+agent-shell-projects-attachment
                                 annotation "target" "target") :state))))))

(ert-deftest +agent-shell-projects-test-user-only-resolution-and-restart ()
  (+agent-shell-projects-test--isolated
    (let* ((file (+agent-shell-projects-test--file directory "spec.md" "text"))
           (artefact (+agent-shell-projects-enrol project file))
           (annotation (+agent-shell-projects-annotate
                        project artefact
                        (+agent-shell-projects-version
                         artefact (plist-get artefact :current)) 0 4 "Check")))
      (should-error (+agent-shell-projects-set-resolution
                     project annotation t "agent"))
      (+agent-shell-projects-reply project annotation "Explanation" "agent")
      (should-not (plist-get annotation :resolved))
      (+agent-shell-projects-set-resolution project annotation t "user")
      (let ((restored (+agent-shell-projects-load (plist-get project :id) t)))
        (should (plist-get (car (plist-get restored :annotations)) :resolved)))
      (+agent-shell-projects-set-resolution project annotation nil "user")
      (should-not (plist-get annotation :resolved)))))

(ert-deftest +agent-shell-projects-test-embark-targets-are-local ()
  (require 'embark)
  (with-temp-buffer
    (insert "ordinary prose")
    (should-not (+agent-shell-projects-embark-target)))
  (+agent-shell-projects-test--isolated
    (let* ((file (+agent-shell-projects-test--file directory "spec.md" "text"))
           (artefact (+agent-shell-projects-enrol project file)))
      (save-window-excursion
        (+agent-shell-projects-open project (plist-get artefact :id))
        (goto-char (point-min))
        (set-mark (point-max))
        (let ((transient-mark-mode t) (mark-active t))
          (let ((target (+agent-shell-projects-embark-target)))
            (should (eq '+agent-shell-projects-passage (car target)))
            (should (equal (plist-get artefact :current)
                           (plist-get (+agent-shell-projects--target-context
                                       (cadr target)) :version)))))))))

(defmacro +agent-shell-projects-test--agent (&rest body)
  "Run BODY with a simulated shell and public agent-shell API."
  (declare (indent 0))
  `(let ((shell (generate-new-buffer " *Project test agent*"))
         (subscriptions 0) (unsubscriptions 0) (handler nil))
     (unwind-protect
         (cl-letf (((symbol-function 'agent-shell-session-id)
                    (lambda (&rest _) "test-session"))
                   ((symbol-function 'agent-shell-cwd)
                    (lambda () (plist-get project :root)))
                   ((symbol-function 'agent-shell-status)
                    (lambda (&rest _) 'ready))
                   ((symbol-function 'agent-shell-subscribe-to)
                    (cl-function
                     (lambda (&key on-event &allow-other-keys)
                       (setq handler on-event)
                       (cl-incf subscriptions))))
                   ((symbol-function 'agent-shell-unsubscribe)
                    (lambda (&rest _) (cl-incf unsubscriptions))))
           (with-current-buffer shell (setq major-mode 'agent-shell-mode))
           ,@body)
       (kill-buffer shell))))

(ert-deftest +agent-shell-projects-test-refuses-read-only-shell ()
  "Refuse a read-only panel member as a project shell."
  (+agent-shell-projects-test--isolated
    (+agent-shell-projects-test--agent
      (with-current-buffer shell
        (setq-local +agent-shell-drive--policy 'read-only))
      (should-error (+agent-shell-projects-associate project shell)
                    :type 'user-error)
      (should-not (plist-get project :agents)))))

(ert-deftest +agent-shell-projects-test-feedback-checkpoint-and-newer-work ()
  (+agent-shell-projects-test--isolated
    (+agent-shell-projects-test--agent
      (let* ((file (+agent-shell-projects-test--file directory "spec.md" "one"))
             (artefact (+agent-shell-projects-enrol project file))
             (unopened (+agent-shell-projects-enrol
                        project (+agent-shell-projects-test--file
                                 directory "other.md" "other")))
             (old (plist-get artefact :current))
             (event (plist-get artefact :event))
             (annotation (+agent-shell-projects-annotate
                          project artefact (+agent-shell-projects-version artefact old)
                          0 3 "Change this"))
             (accepted nil) (sends 0) request)
        (+agent-shell-projects-associate project shell)
        (with-temp-buffer
          (setq +agent-shell-projects--project-id (plist-get project :id))
          (let ((+agent-shell-projects-send-function
                 (lambda (_shell _text)
                   (when accepted (cl-incf sends) t))))
            (cl-letf (((symbol-function '+agent-shell-projects-observations)
                       (lambda (_) (list (list (plist-get artefact :id) old event)))))
              (setq request (+agent-shell-projects-submit))
              (should (equal "queued" (plist-get request :status)))
              (should-not (plist-get artefact :ack))
              (should-error (+agent-shell-projects-submit))
              (+agent-shell-projects-snapshot project artefact "two" "agent")
              (+agent-shell-projects-reply project annotation "Done" "agent")
              (setq accepted t)
              (+agent-shell-projects-flush project)
              (+agent-shell-projects-flush project)
              (should (= 1 sends))
              (should (equal old (plist-get artefact :ack)))
              (should (+agent-shell-projects-needs-attention-p artefact))
              (should-not (plist-get unopened :ack))
              (should-not (plist-get annotation :resolved))
              (should (plist-get annotation :submitted)))))))))

(ert-deftest +agent-shell-projects-test-agent-operations-idempotent ()
  (+agent-shell-projects-test--isolated
    (+agent-shell-projects-test--agent
      (let* ((artefact (+agent-shell-projects-enrol
                        project (+agent-shell-projects-test--file
                                 directory "spec.md" "text")))
             (agent (+agent-shell-projects-associate project shell))
             (operation (list :id "one-operation" :project (plist-get project :id)
                              :agent (plist-get agent :id) :op "annotate"
                              :artefact (plist-get artefact :id)
                              :version (plist-get artefact :current)
                              :start 0 :end 4 :body "A question"))
             (id (+agent-shell-projects-apply-operation operation)))
        (should (equal id (+agent-shell-projects-apply-operation operation)))
        (should (= 1 (length (plist-get project :annotations))))
        (let ((forbidden (copy-tree operation)))
          (+agent-shell-projects--set forbidden :id "forbidden")
          (+agent-shell-projects--set forbidden :op "resolve")
          (should-error (+agent-shell-projects-apply-operation forbidden)))
        (let ((file (expand-file-name "inbox/op.json"
                                      +agent-shell-projects-directory)))
          (+agent-shell-projects--atomic-write file (+agent-shell-projects--json operation))
          (should (eq t (plist-get (+agent-shell-projects-receive-file file) :ok)))
          (should (file-exists-p (concat file ".result")))
          (should-not (file-exists-p file)))
        (+agent-shell-projects--set agent :role "side")
        (should-error
         (+agent-shell-projects-apply-operation
          (list :id "side-reply" :project (plist-get project :id)
                :agent (plist-get agent :id) :op "reply"
                :annotation id :body "Uncaptured outcome")))))))

(ert-deftest +agent-shell-projects-test-agent-subscription-lifecycle ()
  (+agent-shell-projects-test--isolated
    (+agent-shell-projects-test--agent
      (progn
        (+agent-shell-projects-associate project shell)
        (+agent-shell-projects-associate project shell)
        (should (= 1 subscriptions))
        (funcall handler '((:event . clean-up)))
        (should (= 1 unsubscriptions))
        (+agent-shell-projects-associate project shell)
        (should (= 2 subscriptions))))))

(ert-deftest +agent-shell-projects-test-context-and-failed-delivery ()
  (+agent-shell-projects-test--isolated
    (+agent-shell-projects-test--agent
      (let* ((artefact (+agent-shell-projects-enrol
                        project (+agent-shell-projects-test--file
                                 directory "spec.md" "Original")))
             (agent (+agent-shell-projects-associate project shell))
             (request (+agent-shell-projects-request
                       project "question" agent :artefact (plist-get artefact :id)
                       :version (plist-get artefact :current) :question "Why?"))
             (text (+agent-shell-projects-content
                    (list :blob (plist-get request :prompt)))))
        (+agent-shell-projects-snapshot project artefact "Newer" "agent")
        (should (string-match-p "Original" text))
        (should-not (string-match-p "Newer" text))
        (let ((+agent-shell-projects-send-function (lambda (&rest _) (error "offline"))))
          (+agent-shell-projects-flush project))
        (should (equal "queued" (plist-get request :status)))
        (should (equal "offline" (plist-get request :delivery-error)))
        (should-not (plist-get artefact :ack))))))

(ert-deftest +agent-shell-projects-test-side-outcome-is-explicit-and-editable ()
  (+agent-shell-projects-test--isolated
    (+agent-shell-projects-test--agent
      (let* ((artefact (+agent-shell-projects-enrol
                        project (+agent-shell-projects-test--file
                                 directory "spec.md" "Original passage")))
             (old (plist-get artefact :current))
             (selection (list :project (plist-get project :id)
                              :artefact (plist-get artefact :id)
                              :version old :start 0 :end 8))
             (target (cdr (+agent-shell-projects--register-target
                           '+agent-shell-projects-passage selection)))
             (+agent-shell-projects-send-function (lambda (&rest _) t))
             question outcome)
        (cl-letf (((symbol-function '+agent-shell-projects-mode) #'ignore)
                  ((symbol-function '+agent-shell-projects-start-agent)
                   (lambda (p role)
                     (+agent-shell-projects-associate p shell role) shell)))
          (save-window-excursion
            (setq question (+agent-shell-projects-ask target "Why?"))
            (+agent-shell-projects-snapshot project artefact "Revised passage" "agent")
            (setq outcome (+agent-shell-projects-capture-outcome))
            (should-not (plist-get project :annotations))
            (+agent-shell-projects-apply-operation
             (list :id "outcome-one" :project (plist-get project :id)
                   :agent (plist-get question :agent) :op "draft"
                   :request (plist-get outcome :id) :body "Draft conclusion"))
            (should-not (plist-get project :annotations))
            (should (equal "Draft conclusion" (buffer-string)))
            (erase-buffer) (insert "User-edited conclusion")
            (call-interactively (lookup-key (current-local-map) (kbd "C-c C-c")))
            (let ((annotation (car (plist-get project :annotations))))
              (should (equal old (plist-get annotation :version)))
              (should (equal "User-edited conclusion" (plist-get annotation :body)))
              (should-not (plist-get annotation :submitted)))
            (with-current-buffer shell
              (let ((second (+agent-shell-projects-capture-outcome)))
                (should-not (equal (plist-get outcome :id) (plist-get second :id)))))
            (should-error (+agent-shell-projects-apply-operation
                           (list :id "side-auto" :project (plist-get project :id)
                                 :agent (plist-get question :agent) :op "annotate"
                                 :artefact (plist-get artefact :id) :version old
                                 :start 0 :end 8 :body "Unapproved")))))))))

(ert-deftest +agent-shell-projects-test-draft-rejects-wrong-agent-and-cancel ()
  (+agent-shell-projects-test--isolated
    (+agent-shell-projects-test--agent
      (let* ((agent (+agent-shell-projects-associate project shell "side"))
             (request (+agent-shell-projects-request project "outcome" agent))
             (op (list :id "draft" :project (plist-get project :id)
                       :agent (plist-get agent :id) :op "draft"
                       :request (plist-get request :id) :body "Result")))
        (+agent-shell-projects--set request :status "cancelled")
        (should-error (+agent-shell-projects-apply-operation op))
        (+agent-shell-projects--set request :status "sent")
        (+agent-shell-projects--set op :agent "another-agent")
        (should-error (+agent-shell-projects-apply-operation op))
        (should-not (plist-get project :annotations))))))

(ert-deftest +agent-shell-projects-test-default-agent-selection ()
  (let ((agent-shell-preferred-agent-config 'example)
        (agent-shell-agent-configs '(((:identifier . example))))
        (+agent-shell-projects-agent-config nil)
        (+agent-shell-projects-agent-policy 'default)
        (prompts 0))
    (cl-letf (((symbol-function 'agent-shell-select-config)
               (lambda (&rest _) (cl-incf prompts) '((:identifier . selected)))))
      (should (equal '((:identifier . example)) (+agent-shell-projects--select-config)))
      (should (= 0 prompts))
      (let ((+agent-shell-projects-agent-policy 'ask))
        (should (equal '((:identifier . selected)) (+agent-shell-projects--select-config))))
      (should (= 1 prompts)))))

(ert-deftest +agent-shell-projects-test-extract-into-existing-org-subtree ()
  (+agent-shell-projects-test--isolated
    (let* ((file (+agent-shell-projects-test--file
                  directory "notes.org" "* Keep\nUnrelated\n* Target\n"))
           (buffer (find-file-noselect file))
           (marker (with-current-buffer buffer
                     (goto-char (point-min)) (re-search-forward "^\\* Target")
                     (beginning-of-line) (point-marker)))
           (artefact (+agent-shell-projects-extract-prose
                      project "## Decision\nSee [reference](https://example.com).\n```elisp\n# unchanged\n```"
                      file marker))
           (text (+agent-shell-projects-content
                  (+agent-shell-projects-version artefact (plist-get artefact :current)))))
      (should (string-prefix-p "* Target" text))
      (should (string-match-p "^\\*\\*\\* Decision" text))
      (should (string-match-p (regexp-quote "[[https://example.com][reference]]") text))
      (should (string-match-p "# unchanged" text))
      (should-not (string-match-p "Unrelated" text)))))

(defun +agent-shell-projects-test--repository (directory)
  "Create a disposable Git repository inside DIRECTORY."
  (let ((root (expand-file-name "repo/" directory)))
    (make-directory root t)
    (+agent-shell-projects-git root "init" "-q")
    (+agent-shell-projects-git root "config" "user.name" "Fixture")
    (+agent-shell-projects-git root "config" "user.email" "fixture@example.invalid")
    (+agent-shell-projects-git root "config" "commit.gpgsign" "false")
    (+agent-shell-projects-git root "config" "core.filemode" "true")
    (dolist (name '("tracked.txt" "deleted.txt" "rename.txt" "mode.txt"))
      (+agent-shell-projects-test--file root name "original\n"))
    (+agent-shell-projects-test--file root ".gitignore" "ignored/\n")
    (+agent-shell-projects-git root "add" ".")
    (+agent-shell-projects-git root "commit" "-qm" "Initial")
    root))

(ert-deftest +agent-shell-projects-test-checkout-and-worktree-capture ()
  (dolist (linked '(nil t))
    (+agent-shell-projects-test--isolated
      (let* ((repository (+agent-shell-projects-test--repository directory))
             (root (if linked (expand-file-name "linked/" directory) repository)))
        (when linked (+agent-shell-projects-git
                      repository "worktree" "add" "-q" "-b" "review" root "HEAD"))
        (let ((base (string-trim (+agent-shell-projects--git-text root "rev-parse" "HEAD"))))
          (+agent-shell-projects-test--file root "tracked.txt" "committed\n")
          (+agent-shell-projects-git root "add" "tracked.txt")
          (+agent-shell-projects-git root "commit" "-qm" "Feature")
          (+agent-shell-projects-test--file root "tracked.txt" "staged\n")
          (+agent-shell-projects-git root "add" "tracked.txt")
          (+agent-shell-projects-test--file root "tracked.txt" "working\n")
          (+agent-shell-projects-git root "mv" "rename.txt" "renamed.txt")
          (delete-file (expand-file-name "deleted.txt" root))
          (set-file-modes (expand-file-name "mode.txt" root) #o755)
          (+agent-shell-projects-test--file root "new space.txt" "new\n")
          (+agent-shell-projects-test--file root "image.bin" (string 0 1 2 255))
          (make-symbolic-link "tracked.txt" (expand-file-name "alias.txt" root))
          (+agent-shell-projects-test--file root "ignored/output.txt" "generated\n")
          (let* ((before (+agent-shell-projects-git root "status" "--porcelain=v1" "-z"))
                 (artefact (+agent-shell-projects-test--enrol-code project root base))
                 (manifest (+agent-shell-projects-code-manifest
                            (+agent-shell-projects-version
                             artefact (plist-get artefact :current))))
                 (files (plist-get manifest :files)))
            (should (equal before (+agent-shell-projects-git
                                   root "status" "--porcelain=v1" "-z")))
            (should (equal "working\n"
                           (+agent-shell-projects--file-bytes
                            (seq-find (lambda (f) (equal (plist-get f :path) "tracked.txt")) files))))
            (should (seq-find (lambda (f) (equal (plist-get f :path) "new space.txt")) files))
            (should (plist-get (seq-find (lambda (f) (equal (plist-get f :path) "image.bin")) files)
                               :binary))
            (should (equal "120000"
                           (plist-get (seq-find (lambda (f) (equal (plist-get f :path) "alias.txt")) files)
                                      :mode)))
            (should-not (seq-find (lambda (f) (equal (plist-get f :path) "ignored/output.txt")) files))
            (should (seq-find (lambda (f) (plist-get f :old-path))
                              (plist-get manifest :changes)))
            (save-window-excursion
              (+agent-shell-projects-test--render-code
               project artefact (+agent-shell-projects-version artefact (plist-get artefact :current)))
              (should (string-match-p "mode 100644 → 100755" (buffer-string)))
              (should (string-match-p "Binary contents changed" (buffer-string)))
              (should (string-match-p "deleted deleted.txt" (buffer-string))))))))))

(ert-deftest +agent-shell-projects-test-diff-selection-and-review-comparison ()
  (+agent-shell-projects-test--isolated
    (let* ((root (+agent-shell-projects-test--repository directory))
           (_ (+agent-shell-projects-test--file root "tracked.txt" "first change\n"))
           (artefact (+agent-shell-projects-test--enrol-code project root))
           (old (plist-get artefact :current)))
      (+agent-shell-projects-acknowledge project (list (list (plist-get artefact :id) old 0)))
      (+agent-shell-projects-test--file root "tracked.txt" "second change\n")
      (let ((version (+agent-shell-projects-test--capture-code project artefact)))
        (save-window-excursion
          (+agent-shell-projects-test--render-code project artefact version "diff" "reviewed")
          (should (string-match-p "-first change" (buffer-string)))
          (should (string-match-p "+second change" (buffer-string)))
          (goto-char (point-min)) (search-forward "+second change")
          (let* ((end (point))
                 (begin (save-excursion (beginning-of-line) (1+ (point)))))
            (set-mark begin)
            (goto-char end)
            (let* ((transient-mark-mode t) (mark-active t)
                   (selection (+agent-shell-projects--selection))
                   (annotation (+agent-shell-projects-annotate
                                project artefact version
                                (plist-get selection :start) (plist-get selection :end)
                                "Why?" "user" (plist-get selection :context))))
              (should (equal "second change" (plist-get annotation :quote)))
              (should (equal "file" (plist-get (plist-get annotation :context) :space)))
              (should (equal "attached"
                             (plist-get (+agent-shell-projects-attachment
                                         annotation "second change\n" "second change\n") :state))))))))))

(defun +agent-shell-projects-test--explanation (project artefact agent version)
  "Supply a structured explanation for PROJECT's ARTEFACT VERSION from AGENT."
  (let ((request (+agent-shell-projects-request-explanation project artefact version)))
    (+agent-shell-projects-apply-operation
     (list :id (+agent-shell-projects--id) :project (plist-get project :id)
           :agent (plist-get agent :id) :op "explanation"
           :request (plist-get request :id)
           :artefact (plist-get artefact :id) :version (plist-get version :id)
           :summary "Preserve compatibility while changing the implementation."
           :walkthrough (list (list :id "compatibility" :title "Preserve consumers"
                                   :body "Existing consumers retain their behavior."
                                   :basis "inferred" :files (list "tracked.txt")
                                   :validation "Not yet verified in Storybook."))))))

(ert-deftest +agent-shell-projects-test-connected-code-views-and-position ()
  (+agent-shell-projects-test--isolated
    (+agent-shell-projects-test--agent
      (let* ((root (+agent-shell-projects-test--repository directory))
             (_ (+agent-shell-projects-test--file root "tracked.txt" "changed\n"))
             (artefact (+agent-shell-projects-test--enrol-code project root))
             (version (+agent-shell-projects-version artefact (plist-get artefact :current)))
             (agent (+agent-shell-projects-associate project shell))
             (+agent-shell-projects-send-function (lambda (&rest _) t)))
        (+agent-shell-projects-test--explanation project artefact agent version)
        (save-window-excursion
          (+agent-shell-projects-test--render-code project artefact version "summary")
          (search-forward "compatibility")
          (let ((position (point)))
            (+agent-shell-projects-cycle-view)
            (should (equal "walkthrough" +agent-shell-projects--code-view))
            (should (string-match-p "Preserve consumers \\[inferred\\]" (buffer-string)))
            (goto-char (point-min)) (search-forward "Inspect related changes")
            (let ((button (button-at (1- (point)))))
              (button-activate button))
            (should (equal "diff" +agent-shell-projects--code-view))
            (should (equal '("tracked.txt") +agent-shell-projects--focus-files))
            (+agent-shell-projects-back)
            (should (equal "walkthrough" +agent-shell-projects--code-view))
            (+agent-shell-projects-back)
            (should (= position (point)))
            (should (equal (plist-get version :id) +agent-shell-projects--version-id))))))))

(ert-deftest +agent-shell-projects-test-old-explanation-does-not-label-new-source ()
  (+agent-shell-projects-test--isolated
    (+agent-shell-projects-test--agent
      (let* ((root (+agent-shell-projects-test--repository directory))
             (_ (+agent-shell-projects-test--file root "tracked.txt" "first\n"))
             (artefact (+agent-shell-projects-test--enrol-code project root))
             (old (+agent-shell-projects-version artefact (plist-get artefact :current)))
             (agent (+agent-shell-projects-associate project shell))
             (+agent-shell-projects-send-function (lambda (&rest _) t)))
        (+agent-shell-projects-test--explanation project artefact agent old)
        (+agent-shell-projects-test--file root "tracked.txt" "second\n")
        (let ((new (+agent-shell-projects-test--capture-code project artefact)))
          (should-not (+agent-shell-projects-explanation artefact new))
          (save-window-excursion
            (+agent-shell-projects-test--render-code project artefact new "summary")
            (should (string-match-p "No explanation" (buffer-string)))
            (should-not (string-match-p "Preserve compatibility" (buffer-string))))
          (should (+agent-shell-projects-explanation artefact old)))))))

(ert-deftest +agent-shell-projects-test-explanation-selection-and-original-visit ()
  (+agent-shell-projects-test--isolated
    (+agent-shell-projects-test--agent
      (let* ((root (+agent-shell-projects-test--repository directory))
             (_ (+agent-shell-projects-test--file root "tracked.txt" "changed\n"))
             (artefact (+agent-shell-projects-test--enrol-code project root))
             (version (+agent-shell-projects-version artefact (plist-get artefact :current)))
             (agent (+agent-shell-projects-associate project shell))
             (+agent-shell-projects-send-function (lambda (&rest _) t)))
        (+agent-shell-projects-test--explanation project artefact agent version)
        (save-window-excursion
          (+agent-shell-projects-test--render-code project artefact version "summary")
          (search-forward "Preserve compatibility")
          (let ((end (point)))
            (set-mark (- end (length "compatibility")))
            (let* ((transient-mark-mode t) (mark-active t)
                   (selection (+agent-shell-projects--selection))
                   (annotation (+agent-shell-projects-annotate
                                project artefact version
                                (plist-get selection :start) (plist-get selection :end)
                                "Define compatibility" "user" (plist-get selection :context))))
              (should (equal "compatibility" (plist-get annotation :quote)))
              (+agent-shell-projects--navigate "diff")
              (+agent-shell-projects--visit-code-annotation project annotation)
              (should (equal "summary" +agent-shell-projects--code-view))
              (should (looking-at "compatibility")))))))))

(ert-deftest +agent-shell-projects-test-import-proposal-is-editable ()
  (+agent-shell-projects-test--isolated
    (+agent-shell-projects-test--agent
      (let* ((file (+agent-shell-projects-test--file directory "spec.md" "Existing work"))
             (_ (+agent-shell-projects-enrol project file))
             (agent (+agent-shell-projects-associate project shell))
             (+agent-shell-projects-send-function (lambda (&rest _) t))
             request draft)
        (save-window-excursion
          (with-temp-buffer
            (setq +agent-shell-projects--project-id (plist-get project :id))
            (setq request (+agent-shell-projects-request-proposal
                           (expand-file-name "proposal.md" directory))))
          (+agent-shell-projects-apply-operation
           (list :id "import-draft" :project (plist-get project :id)
                 :agent (plist-get agent :id) :op "draft"
                 :request (plist-get request :id) :body "# Inferred proposal\nQuestion: success?"))
          (should (string-match-p "Inferred proposal" (buffer-string)))
          (kill-buffer (current-buffer))
          (with-temp-buffer
            (setq +agent-shell-projects--project-id (plist-get project :id))
            (should (eq request (+agent-shell-projects-request-proposal
                                 (expand-file-name "proposal.md" directory))))
            (setq draft (current-buffer)))
          (pop-to-buffer draft)
          (goto-char (point-max)) (insert "\nUser correction.")
          (call-interactively (lookup-key (current-local-map) (kbd "C-c C-c")))
          (let ((proposal (+agent-shell-projects-artefact project (plist-get project :proposal))))
            (should (string-match-p "User correction"
                                    (+agent-shell-projects-content
                                     (+agent-shell-projects-version proposal (plist-get proposal :current)))))))))))

(defvar +agent-shell-projects-test--fixture
  (expand-file-name "fixtures/agent-shell-projects-agent.py"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun +agent-shell-projects-test--wait-for (predicate)
  "Wait for asynchronous PREDICATE while draining the real operation inbox."
  (let ((deadline (+ (float-time) 12)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.03)
      (+agent-shell-projects-poll))
    (should (funcall predicate))))

(ert-deftest +agent-shell-projects-test-real-acp-feedback-and-side-outcome ()
  (+agent-shell-projects-test--isolated
    (let* ((agent-shell-show-welcome-message nil)
           (shell-maker-prompt-before-killing-buffer nil)
           (agent-shell-show-config-icons nil)
           (agent-shell-prefer-viewport-interaction nil)
           (agent-shell-mcp-servers nil)
           (agent-shell-transcript-file-path-function nil)
           (config (agent-shell-make-agent-config
                    :identifier 'project-fixture :mode-line-name "Fixture"
                    :buffer-name "Project Fixture" :shell-prompt "Fixture> "
                    :shell-prompt-regexp "Fixture> "
                    :client-maker
                    (lambda (buffer)
                      (acp-make-client :context-buffer buffer
                                       :command (executable-find "python3")
                                       :command-params
                                       (list +agent-shell-projects-test--fixture)))))
           (+agent-shell-projects-agent-config config)
           (+agent-shell-projects-agent-policy 'default)
           (file (+agent-shell-projects-test--file directory "spec.md" "Before\n"))
           (artefact (+agent-shell-projects-enrol project file))
           (old (plist-get artefact :current))
           author side)
      (unwind-protect
          (save-window-excursion
            (setq author (+agent-shell-projects-start-agent project))
            (+agent-shell-projects-test--wait-for
             (lambda () (agent-shell-session-id :shell-buffer author)))
            (+agent-shell-projects-open project (plist-get artefact :id))
            (+agent-shell-projects-annotate project artefact
                                           (+agent-shell-projects-version artefact old)
                                           0 6 "Change Before to After")
            (+agent-shell-projects-submit)
            (+agent-shell-projects-test--wait-for
             (lambda () (plist-get (car (plist-get project :annotations)) :replies)))
            (+agent-shell-projects-test--wait-for
             (lambda () (not (equal old (plist-get artefact :current)))))
            (should (equal "After\n" (+agent-shell-projects-content
                                      (+agent-shell-projects-version artefact (plist-get artefact :current)))))
            (should (equal old (plist-get artefact :ack)))
            (let* ((selection (list :project (plist-get project :id)
                                    :artefact (plist-get artefact :id) :version old
                                    :start 0 :end 6))
                   (target (cdr (+agent-shell-projects--register-target
                                 '+agent-shell-projects-passage selection))))
              (cl-letf (((symbol-function '+agent-shell-projects-mode) #'ignore))
                (+agent-shell-projects-ask target "Discuss the choice"))
              (setq side (current-buffer))
              (+agent-shell-projects-test--wait-for
               (lambda () (and (agent-shell-session-id :shell-buffer side)
                               (eq (agent-shell-status :shell-buffer side) 'ready))))
              (with-current-buffer side
                (+agent-shell-projects-capture-outcome))
              (+agent-shell-projects-test--wait-for
               (lambda () (seq-find
                           (lambda (r) (and (equal (plist-get r :kind) "outcome")
                                            (equal (plist-get r :status) "ready")))
                           (plist-get project :requests))))
              (should (string-match-p "Outcome from the real ACP" (buffer-string)))
              (should (= 1 (length (plist-get project :annotations))))
              (call-interactively (lookup-key (current-local-map) (kbd "C-c C-c")))
              (should (= 2 (length (plist-get project :annotations))))))
        (dolist (buffer (list side author))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest +agent-shell-projects-test-operation-rolls-back-on-error ()
  (+agent-shell-projects-test--isolated
    (+agent-shell-projects-test--agent
      (let* ((agent (+agent-shell-projects-associate project shell))
             (before (copy-tree project))
             (+agent-shell-projects-operation-handlers
              (list (cons "fail-after-mutation"
                          (lambda (working _agent _operation)
                            (+agent-shell-projects--set working :name "Corrupted")
                            (+agent-shell-projects-save working)
                            (error "Interrupted operation"))))))
        (should-error
         (+agent-shell-projects-apply-operation
          (list :id "rollback" :project (plist-get project :id)
                :agent (plist-get agent :id) :op "fail-after-mutation")))
        (should (equal before project))
        (should (eq project (+agent-shell-projects-load (plist-get project :id))))
        (clrhash +agent-shell-projects--projects)
        (should (equal before (+agent-shell-projects-load (plist-get project :id))))))))

(ert-deftest +agent-shell-projects-test-unsaved-source-conflict-preserves-both ()
  (+agent-shell-projects-test--isolated
    (+agent-shell-projects-test--agent
      (+agent-shell-projects-associate project shell)
    (let* ((file (+agent-shell-projects-test--file directory "spec.md" "Original\n"))
           (artefact (+agent-shell-projects-enrol project file))
           (source (find-buffer-visiting file)))
      (with-current-buffer source
        (erase-buffer) (insert "My unsaved edits\n"))
      (+agent-shell-projects-test--file directory "spec.md" "Agent edits on disk\n")
      (with-current-buffer source (set-visited-file-modtime '(0 0 0 0)))
      (+agent-shell-projects-capture project artefact)
      (should (plist-get artefact :conflict))
      (should (equal "Agent edits on disk\n"
                     (+agent-shell-projects-content
                      (list :blob (plist-get artefact :disk-copy)))))
      (with-current-buffer source
        (should (buffer-modified-p))
        (should (equal "My unsaved edits\n" (buffer-string))))
      (save-window-excursion
        (+agent-shell-projects-open project (plist-get artefact :id))
        (should-error (+agent-shell-projects-submit) :type 'user-error))))))

(ert-deftest +agent-shell-projects-test-native-subtree-anchors-and-current-order ()
  (+agent-shell-projects-test--isolated
    (let* ((file (+agent-shell-projects-test--file
                  directory "spec.org" "* Unrelated\nKeep\n* Proposal\nAlpha\nBeta\n"))
           (source (find-file-noselect file))
           (artefact (with-current-buffer source
                       (goto-char (point-min)) (search-forward "* Proposal")
                       (+agent-shell-projects-enrol project file "Proposal" t)))
           (version (+agent-shell-projects-version artefact (plist-get artefact :current)))
           (text (+agent-shell-projects-content version))
           (alpha (string-match "Alpha" text))
           (beta (string-match "Beta" text)))
      (+agent-shell-projects-annotate project artefact version alpha (+ alpha 5) "First")
      (+agent-shell-projects-annotate project artefact version beta (+ beta 4) "Second")
      (save-window-excursion
        (+agent-shell-projects-open project (plist-get artefact :id))
        (+agent-shell-projects-edit)
        (goto-char (point-min)) (search-forward "Alpha")
        (should (seq-some (lambda (o) (overlay-get o 'project-annotation-id))
                          (overlays-at (1- (point)))))
        (delete-region (- (point) 5) (point))
        (goto-char (point-max)) (insert "Alpha\n") (save-buffer)
        (+agent-shell-projects-current)
        (let ((sidebar (get-buffer (format "*Comments: %s*" (plist-get artefact :id)))))
          (with-current-buffer sidebar
            (goto-char (point-min))
            (should (< (search-forward "Beta") (search-forward "Alpha")))))))))

(ert-deftest +agent-shell-projects-test-code-native-edit-and-feedback ()
  (+agent-shell-projects-test--isolated
    (let* ((root (+agent-shell-projects-test--repository directory))
           (_ (+agent-shell-projects-test--file root "tracked.txt" "Changed\n"))
           (artefact (+agent-shell-projects-test--enrol-code project root))
           (old (plist-get artefact :current)))
      (save-window-excursion
        (+agent-shell-projects-test--render-code
         project artefact (+agent-shell-projects-version artefact old))
        (goto-char (point-min)) (search-forward "+Changed")
        (beginning-of-line)
        (+agent-shell-projects-code-edit)
        (should (file-equal-p (expand-file-name "tracked.txt" root) buffer-file-name))
        (goto-char (point-max)) (insert "Direct edit\n") (save-buffer)
        (+agent-shell-projects-test--captured artefact)
        (should-not (equal old (plist-get artefact :current)))
        (goto-char (point-min)) (set-mark (point)) (forward-char 7)
        (let* ((transient-mark-mode t) (mark-active t)
               (selection (+agent-shell-projects--selection))
               (annotation (+agent-shell-projects-annotate
                            project artefact
                            (+agent-shell-projects-version artefact (plist-get selection :version))
                            (plist-get selection :start) (plist-get selection :end)
                            "Clarify this behavior" "user" (plist-get selection :context))))
          (should (equal "Changed" (plist-get annotation :quote)))
          (should (equal "tracked.txt" (plist-get (plist-get annotation :context) :path))))))))

(provide 'agent-shell-projects-test)
;;; agent-shell-projects-test.el ends here
