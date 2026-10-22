;;; agent-shell-projects-manage-test.el --- Lifecycle tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Removal and archival preserve history and stop stale asynchronous work.

;;; Code:

(require '+agent-shell-projects-manage)

(ert-deftest +agent-shell-projects-test-unenrol-agent-keeps-shell ()
  (+agent-shell-projects-test--isolated
   (+agent-shell-projects-test--agent
    (let* ((agent (+agent-shell-projects-associate project shell))
           (request (+agent-shell-projects-request project "exploration" agent :intent "Build"))
           (annotation (list :id "comment" :queued (plist-get request :id))))
      (+agent-shell-projects--set project :annotations (list annotation))
      (+agent-shell-projects-remove-agent project (plist-get agent :id))
      (should (buffer-live-p shell))
      (should (= 1 unsubscriptions))
      (should-not (plist-get project :agents))
      (should-not (plist-get project :author))
      (should-not (plist-get annotation :queued))
      (should (equal "cancelled" (plist-get request :status)))
      (should-not (buffer-local-value '+agent-shell-projects--project-id shell))
      (+agent-shell-projects-poll)
      (should (= 1 subscriptions))
      (should-error (+agent-shell-projects-apply-operation
                     (list :project (plist-get project :id) :agent (plist-get agent :id)
                           :id "late" :op "progress" :body "Still here")))
      (+agent-shell-projects--agent-event (plist-get project :id)
                                          (plist-get agent :id) '(:kind turn-complete))
      (should (= 0 (hash-table-count +agent-shell-projects--event-timers)))))))

(ert-deftest +agent-shell-projects-test-unenrol-document-preserves-history ()
  (+agent-shell-projects-test--isolated
   (let* ((file (+agent-shell-projects-test--file directory "spec.md" "Proposal"))
          (artefact (+agent-shell-projects-enrol project file))
          (version (+agent-shell-projects-version artefact (plist-get artefact :current)))
          (annotation (+agent-shell-projects-annotate project artefact version 0 8 "Why?")))
     (+agent-shell-projects--set project :proposal (plist-get artefact :id))
     (+agent-shell-projects-remove-artefact project (plist-get artefact :id))
     (should (file-exists-p file))
     (should-not (plist-get project :artefacts))
     (should-not (plist-get project :proposal))
     (should (eq artefact (+agent-shell-projects-artefact project (plist-get artefact :id))))
     (should (equal "Proposal" (+agent-shell-projects-content version)))
     (should-not (+agent-shell-projects--feedback project))
     (should-error (+agent-shell-projects-reply project annotation "Late" "agent"))
     (with-current-buffer (find-file-noselect file)
       (should-not +agent-shell-projects--source-artefacts)
       (goto-char (point-max)) (insert " edited") (save-buffer))
     (should (= 1 (length (plist-get artefact :history))))
     (should (eq version (+agent-shell-projects-capture project artefact))))))

(ert-deftest +agent-shell-projects-test-delete-document-retains-last-copy ()
  (+agent-shell-projects-test--isolated
   (let* ((delete-by-moving-to-trash nil)
          (file (+agent-shell-projects-test--file directory "spec.md" "Proposal"))
          (artefact (+agent-shell-projects-enrol project file)))
     (with-current-buffer (find-file-noselect file)
       (goto-char (point-max)) (insert " unsaved")
       (should-error (+agent-shell-projects-remove-artefact project (plist-get artefact :id) t))
       (should (plist-get project :artefacts))
       (should (file-exists-p file))
       (save-buffer))
     (+agent-shell-projects-remove-artefact project (plist-get artefact :id) t)
     (should-not (file-exists-p file))
     (should (equal "Proposal unsaved\n"
                    (+agent-shell-projects-content
                     (+agent-shell-projects-version artefact (plist-get artefact :current)))))
     (should-not (plist-get project :artefacts)))))

(ert-deftest +agent-shell-projects-test-delete-org-subtree-keeps-siblings ()
  (+agent-shell-projects-test--isolated
   (let* ((file (+agent-shell-projects-test--file
                 directory "spec.org" "* Keep\nBefore\n* Remove\nBody\n** Child\nNested\n* Also keep\nAfter\n"))
          (buffer (find-file-noselect file))
          (artefact (with-current-buffer buffer
                      (goto-char (point-min)) (search-forward "* Remove")
                      (beginning-of-line)
                      (+agent-shell-projects-enrol project file "Remove" t))))
     (with-current-buffer buffer (save-buffer))
     (+agent-shell-projects-remove-artefact project (plist-get artefact :id) t)
     (should (file-exists-p file))
     (with-temp-buffer
       (insert-file-contents file)
       (should (equal "* Keep\nBefore\n* Also keep\nAfter\n" (buffer-string))))
     (should (string-match-p "Nested"
                             (+agent-shell-projects-content
                              (+agent-shell-projects-version artefact (plist-get artefact :current))))))))

(ert-deftest +agent-shell-projects-test-archive-restores-without-replaying ()
  (+agent-shell-projects-test--isolated
   (+agent-shell-projects-test--agent
    (let* ((file (+agent-shell-projects-test--file directory "spec.md" "Proposal"))
           (artefact (+agent-shell-projects-enrol project file))
           (agent (+agent-shell-projects-associate project shell))
           (request (+agent-shell-projects-request project "proposal" agent))
           (version (plist-get artefact :current)))
      (+agent-shell-projects-set-archived project t)
      (should (buffer-live-p shell))
      (should (= 1 unsubscriptions))
      (should-not (+agent-shell-projects-list))
      (should (= 1 (length (+agent-shell-projects-list t))))
      (should (equal "cancelled" (plist-get request :status)))
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-max)) (insert " changed") (save-buffer))
      (+agent-shell-projects-capture project artefact)
      (+agent-shell-projects-poll)
      (should (equal version (plist-get artefact :current)))
      (should (= 1 subscriptions))
      (should-error (+agent-shell-projects-apply-operation
                     (list :project (plist-get project :id) :agent (plist-get agent :id)
                           :id "late" :op "progress" :body "Late")))
      (let ((id (plist-get project :id)))
        (remhash id +agent-shell-projects--projects)
        (setq project (+agent-shell-projects-load id)))
      (should (plist-get project :archived))
      (+agent-shell-projects-set-archived project nil)
      (+agent-shell-projects-poll)
      (should (= 2 subscriptions))
      (should (= 1 (length (+agent-shell-projects-list))))
      (should (equal "cancelled" (plist-get (car (plist-get project :requests)) :status)))
      (let ((artefact (car (plist-get project :artefacts))))
        (+agent-shell-projects-capture project artefact)
        (should (= 2 (length (plist-get artefact :history)))))))))

(ert-deftest +agent-shell-projects-test-remove-shared-capture-target ()
  (+agent-shell-projects-test--isolated
   (let* ((root directory)
          (artefact (list :id "first" :kind "code" :path root :history nil))
          (other (+agent-shell-projects-create "Other" directory))
          (second (list :id "second" :kind "code" :path root :history nil))
          (+agent-shell-projects-capture-delay 60)
          (blob (+agent-shell-projects-store-content "{}"))
          (completed 0) finish)
     (+agent-shell-projects--set project :artefacts (list artefact))
     (+agent-shell-projects--set other :artefacts (list second))
     (cl-letf (((symbol-function '+agent-shell-projects-start-worker)
                (lambda (_request callback &optional _progress)
                  (setq finish callback) (list :fake t))))
       (+agent-shell-projects-capture-code project artefact nil
                                           (lambda (_v) (cl-incf completed)))
       (+agent-shell-projects-capture-code other second)
       (let ((state (car (hash-table-values +agent-shell-projects--jobs))))
         (cancel-timer (plist-get state :timer))
         (+agent-shell-projects--start-capture state)
         (+agent-shell-projects-remove-artefact project "first")
         (should (= 1 (length (plist-get state :active))))
         (funcall finish (list :ok t :result
                               (list (list :artefact "first" :root root :base "HEAD" :blob blob)
                                     (list :artefact "second" :root root :base "HEAD" :blob blob))))
         (should-not (plist-get artefact :current))
         (should (= completed 0))
         (should (plist-get second :current))
         (should (= 0 (hash-table-count +agent-shell-projects--jobs))))))))

(ert-deftest +agent-shell-projects-test-archive-cancels-queued-workers ()
  (+agent-shell-projects-test--isolated
   (let* ((other (+agent-shell-projects-create "Other" directory))
          (cancelled 0)
          (first (+agent-shell-projects-start-worker
                  (list :kind "init" :project (plist-get project :id))
                  (lambda (_result) (cl-incf cancelled))))
          (second (+agent-shell-projects-start-worker
                   (list :kind "init" :project (plist-get other :id)) #'ignore)))
     (+agent-shell-projects-set-archived project t)
     (should (= cancelled 1))
     (should-not (memq first +agent-shell-projects--worker-queue))
     (should (memq second +agent-shell-projects--worker-queue)))))

(ert-deftest +agent-shell-projects-test-home-management-targets ()
  (+agent-shell-projects-test--isolated
   (+agent-shell-projects-test--agent
    (let ((agent (+agent-shell-projects-associate project shell)))
      (with-temp-buffer
        (+agent-shell-projects-home-mode)
        (setq +agent-shell-projects--project-id (plist-get project :id))
        (+agent-shell-projects--render-home)
        (goto-char (point-min)) (forward-line 2)
        (let* ((target (+agent-shell-projects-embark-target))
               (context (+agent-shell-projects--target-context (cdr target))))
          (should (eq '+agent-shell-projects-agent (car target)))
          (should (equal (plist-get agent :id) (plist-get context :agent))))
        (+agent-shell-projects-set-archived project t)
        (+agent-shell-projects--render-home)
        (should (string-match-p "\\[archived\\]" (buffer-string))))
      (should (eq (lookup-key +agent-shell-projects-agent-map "u")
                  '+agent-shell-projects-unenrol-agent))
      (should (eq (lookup-key +agent-shell-projects-artefact-map "D")
                  '+agent-shell-projects-delete-artefact))))))

(ert-deftest +agent-shell-projects-test-cancel-discards-already-written-result ()
  (+agent-shell-projects-test--isolated
    (let* ((file (make-temp-file "projects-cancel-test-"))
           (process (make-process :name "projects-cancel-test" :command '("/usr/bin/true")
                                  :noquery t :sentinel #'ignore))
           result
           (job (list :file file :process process :callback (lambda (r) (setq result r)))))
      (+agent-shell-projects-test--await (lambda () (not (process-live-p process))))
      (puthash process job +agent-shell-projects--workers)
      (+agent-shell-projects--atomic-write (concat file ".result")
                                           (+agent-shell-projects--json '(:ok t :result "stale")))
      (+agent-shell-projects--stop-worker process)
      (+agent-shell-projects--worker-finish process)
      (should (eq :false (plist-get result :ok)))
      (should (equal "Cancelled" (plist-get result :error)))
      (should-not (file-exists-p (concat file ".result"))))))

(ert-deftest +agent-shell-projects-test-unenrol-native-view-detaches-sidebar ()
  (+agent-shell-projects-test--isolated
    (let* ((file (+agent-shell-projects-test--file directory "spec.md" "Proposal"))
           (artefact (+agent-shell-projects-enrol project file)))
      (save-window-excursion
        (+agent-shell-projects-open project (plist-get artefact :id))
        (+agent-shell-projects-edit)
        (let ((source (current-buffer))
              (sidebar (get-buffer (format "*Comments: %s*" (plist-get artefact :id)))))
          (+agent-shell-projects-remove-artefact project (plist-get artefact :id))
          (+agent-shell-projects--refresh-ui project nil)
          (+agent-shell-projects--refresh-visible)
          (should-not (buffer-local-value '+agent-shell-projects--project-id source))
          (with-current-buffer sidebar
            (should-not +agent-shell-projects--view-buffer)
            (should (string-match-p "Open saved review history" (buffer-string)))
            (goto-char (point-min)) (forward-line 2) (push-button))
          (with-current-buffer (window-buffer (selected-window))
            (should (equal (plist-get artefact :id) +agent-shell-projects--artefact-id))
            (should buffer-read-only)))))))

(ert-deftest +agent-shell-projects-test-chooser-restores-archived-project ()
  (+agent-shell-projects-test--isolated
    (+agent-shell-projects-set-archived project t)
    (let ((+agent-shell-projects-mode t))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt choices &rest _)
                   (if (string-prefix-p "Project " prompt)
                       (progn
                         (should (assoc "[Restore archived project]" choices))
                         "[Restore archived project]")
                     (caar choices)))))
        (should (eq project (+agent-shell-projects--choose-project)))
        (should-not (plist-get project :archived))))))

(provide 'agent-shell-projects-manage-test)
;;; agent-shell-projects-manage-test.el ends here
