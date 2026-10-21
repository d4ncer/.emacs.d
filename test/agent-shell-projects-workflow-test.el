;;; agent-shell-projects-workflow-test.el --- Workflow audit -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise transitions between independently tested review features.

;;; Code:

(require 'agent-shell-projects-test)

(ert-deftest +agent-shell-projects-test-workflow-historical-explanation-context ()
  (+agent-shell-projects-test--isolated
    (+agent-shell-projects-test--agent
      (let* ((root (+agent-shell-projects-test--repository directory))
             (proposal (+agent-shell-projects-enrol
                        project (+agent-shell-projects-test--file
                                 directory "proposal.md" "Original proposal")))
             (proposal-version (plist-get proposal :current))
             (_ (+agent-shell-projects--set project :proposal
                                            (plist-get proposal :id)))
             (code (+agent-shell-projects-test--enrol-code project root))
             (old (+agent-shell-projects-version code (plist-get code :current)))
             (_ (+agent-shell-projects-associate project shell))
             (+agent-shell-projects-send-function (lambda (&rest _) t)))
        (+agent-shell-projects-snapshot project proposal "Revised proposal")
        (+agent-shell-projects-test--file root "tracked.txt" "New code\n")
        (+agent-shell-projects-test--capture-code project code)
        (let* ((request (+agent-shell-projects-request-explanation project code old))
               (versions (plist-get request :versions)))
          (should (equal (plist-get old :id)
                         (plist-get (seq-find
                                     (lambda (v) (equal (plist-get v :artefact)
                                                        (plist-get code :id)))
                                     versions) :version)))
          (should (equal proposal-version
                         (plist-get (seq-find
                                     (lambda (v) (equal (plist-get v :artefact)
                                                        (plist-get proposal :id)))
                                     versions) :version))))))))

(ert-deftest +agent-shell-projects-test-workflow-pending-capture-request ()
  (+agent-shell-projects-test--isolated
    (+agent-shell-projects-test--agent
      (let* ((root (+agent-shell-projects-test--repository directory))
             (+agent-shell-projects-capture-delay 60)
             (_ (+agent-shell-projects-enrol-code project root))
             (_ (+agent-shell-projects-associate project shell)))
        (with-temp-buffer
          (setq +agent-shell-projects--project-id (plist-get project :id))
          (should-error
           (+agent-shell-projects-request-proposal
            (expand-file-name "proposal.md" directory))
           :type 'user-error))
        (should-not (plist-get project :requests))))))

(ert-deftest +agent-shell-projects-test-workflow-request-context-failure ()
  (+agent-shell-projects-test--isolated
    (+agent-shell-projects-test--agent
      (let ((agent (+agent-shell-projects-associate project shell))
            (+agent-shell-projects-context-function
             (lambda (&rest _) (error "Context provider failed"))))
        (should-error (+agent-shell-projects-request project "question" agent))
        (should-not (plist-get project :requests))
        (should-not (plist-get (+agent-shell-projects-load
                               (plist-get project :id) t) :requests))))))

(ert-deftest +agent-shell-projects-test-workflow-explanation-arrives-in-view ()
  (+agent-shell-projects-test--isolated
    (+agent-shell-projects-test--agent
      (let* ((root (+agent-shell-projects-test--repository directory))
             (_ (+agent-shell-projects-test--file root "tracked.txt" "Changed\n"))
             (code (+agent-shell-projects-test--enrol-code project root))
             (version (+agent-shell-projects-version code (plist-get code :current)))
             (agent (+agent-shell-projects-associate project shell))
             (+agent-shell-projects-send-function (lambda (&rest _) t))
             (+agent-shell-projects-change-hook
              '(+agent-shell-projects--refresh-ui)))
        (save-window-excursion
          (+agent-shell-projects-render-code project code version "summary")
          (let ((view (current-buffer)) (window (selected-window)))
            (should (string-match-p "No explanation" (buffer-string)))
            (+agent-shell-projects-test--explanation project code agent version)
            (+agent-shell-projects--refresh-visible)
            (should (eq window (selected-window)))
            (with-current-buffer view
              (should (string-match-p "Preserve compatibility" (buffer-string)))
              (should-not (string-match-p "No explanation" (buffer-string))))))))))

(ert-deftest +agent-shell-projects-test-workflow-existing-explanation-no-request ()
  (+agent-shell-projects-test--isolated
    (+agent-shell-projects-test--agent
      (let* ((root (+agent-shell-projects-test--repository directory))
             (_ (+agent-shell-projects-test--file root "tracked.txt" "Changed\n"))
             (code (+agent-shell-projects-test--enrol-code project root))
             (version (+agent-shell-projects-version code (plist-get code :current)))
             (agent (+agent-shell-projects-associate project shell))
             (+agent-shell-projects-send-function (lambda (&rest _) t)))
        (+agent-shell-projects-test--explanation project code agent version)
        (let ((before (length (plist-get project :requests))))
          (+agent-shell-projects-request-explanation project code version)
          (should (= before (length (plist-get project :requests)))))))))

(ert-deftest +agent-shell-projects-test-workflow-missing-subtree-history ()
  (+agent-shell-projects-test--isolated
    (let* ((file (+agent-shell-projects-test--file
                  directory "proposal.org" "* Proposal\nOriginal text\n"))
           (source (find-file-noselect file))
           (artefact (with-current-buffer source
                       (goto-char (point-min))
                       (+agent-shell-projects-enrol project file "Proposal" t)))
           (old (plist-get artefact :current)))
      (save-window-excursion
        (+agent-shell-projects-open project (plist-get artefact :id))
        (+agent-shell-projects-edit)
        (erase-buffer)
        (insert "* Other work\n")
        (save-buffer)
        (+agent-shell-projects--refresh-ui project artefact)
        (+agent-shell-projects--refresh-visible)
        (+agent-shell-projects-open project (plist-get artefact :id))
        (should (plist-get artefact :missing))
        (+agent-shell-projects-open project (plist-get artefact :id) old)
        (should (string-match-p "Original text" (buffer-string)))))))

(ert-deftest +agent-shell-projects-test-workflow-diff-freezes-review-baseline ()
  (+agent-shell-projects-test--isolated
    (let* ((root (+agent-shell-projects-test--repository directory))
           (_ (+agent-shell-projects-test--file root "tracked.txt" "First change\n"))
           (code (+agent-shell-projects-test--enrol-code project root))
           (first (plist-get code :current)))
      (+agent-shell-projects-acknowledge
       project (list (list (plist-get code :id) first 0)))
      (+agent-shell-projects-test--file root "tracked.txt" "Second change\n")
      (let ((second (+agent-shell-projects-test--capture-code project code)))
        (save-window-excursion
          (+agent-shell-projects-render-code project code second "diff" "reviewed")
          (+agent-shell-projects-acknowledge
           project (list (list (plist-get code :id) (plist-get second :id) 0)))
          (+agent-shell-projects-test--expand)
          (goto-char (point-min))
          (should (search-forward "-First change" nil t))
          (set-mark (- (point) (length "First change")))
          (let* ((transient-mark-mode t) (mark-active t)
                 (selection (+agent-shell-projects--selection)))
            (should (equal (concat "version:" first)
                           (plist-get (plist-get selection :context) :comparison))))
          (deactivate-mark)
          (+agent-shell-projects-current)
          (+agent-shell-projects-test--captured code)
          (should (equal (concat "version:" (plist-get second :id))
                         +agent-shell-projects--comparison)))))))

(provide 'agent-shell-projects-workflow-test)
;;; agent-shell-projects-workflow-test.el ends here
