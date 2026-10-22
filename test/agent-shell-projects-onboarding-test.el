;;; agent-shell-projects-onboarding-test.el --- Intent to implementation -*- lexical-binding: t; -*-

;;; Commentary:
;; Check project creation and the handoff through existing review primitives.

;;; Code:

(require 'agent-shell-projects-test)

(ert-deftest +agent-shell-projects-test-onboarding-new-project-entry ()
  (+agent-shell-projects-test--isolated
    (let ((default-directory directory))
      (save-window-excursion
        (cl-letf (((symbol-function '+agent-shell-projects-mode) #'ignore)
                  ((symbol-function 'completing-read)
                   (lambda (&rest _) "A new intent")))
          (+agent-shell-projects))
        (setq project (+agent-shell-projects--project))
        (should (equal "A new intent" (plist-get project :name)))
        (should (string-match-p "Project intent" (buffer-name)))
        (should (string-match-p "start conversation" header-line-format))
        (should-not (plist-get project :agents))
        (should-not (plist-get project :requests))
        (call-interactively (key-binding (kbd "C-c C-k")))
        (cl-letf (((symbol-function '+agent-shell-projects-mode) #'ignore)
                  ((symbol-function 'completing-read)
                   (lambda (&rest _)
                     (format "%s [%s]" (plist-get project :name)
                             (substring (plist-get project :id) 0 6)))))
          (+agent-shell-projects))
        (should (derived-mode-p '+agent-shell-projects-home-mode))))))

(ert-deftest +agent-shell-projects-test-onboarding-intent-and-org-proposal ()
  (+agent-shell-projects-test--isolated
    (+agent-shell-projects-test--agent
      (let* ((agent (+agent-shell-projects-associate project shell))
             (accepted nil)
             (+agent-shell-projects-send-function (lambda (&rest _) accepted))
             (file (expand-file-name "plans.org" directory))
             (prompts 0) exploration proposal)
        (save-window-excursion
          (cl-letf (((symbol-function '+agent-shell-projects-mode) #'ignore))
            (+agent-shell-projects-explore project)
            (should-error (call-interactively (key-binding (kbd "C-c C-c")))
                          :type 'user-error)
            (should-not (plist-get project :requests))
            (insert "Make review easier.\nHelp me discover the success criteria.")
            (call-interactively (key-binding (kbd "C-c C-c"))))
          (should (eq shell (current-buffer)))
          (should (equal (plist-get project :id) +agent-shell-projects--project-id))
          (setq exploration (car (plist-get project :requests)))
          (should (equal "exploration" (plist-get exploration :kind)))
          (should (equal "queued" (plist-get exploration :status)))
          (should-not (plist-get project :artefacts))
          (should (string-match-p
                   "constraints and priorities"
                   (+agent-shell-projects-content
                    (list :blob (plist-get exploration :prompt)))))
          (setq accepted t)
          (+agent-shell-projects-flush project)
          (cl-letf (((symbol-function 'read-file-name)
                     (lambda (&rest _) (cl-incf prompts) file)))
            (setq proposal (+agent-shell-projects-request-proposal)))
          (should (zerop prompts))
          (should (equal (plist-get exploration :id) (plist-get proposal :parent)))
          (+agent-shell-projects-apply-operation
           (list :id "onboarding-proposal" :project (plist-get project :id)
                 :agent (plist-get agent :id) :op "draft"
                 :request (plist-get proposal :id) :body "# Agreed proposal\nOpen question"))
          (should (string-match-p "save proposal" header-line-format))
          (call-interactively (key-binding (kbd "C-c C-k")))
          (pop-to-buffer shell)
          (should (eq proposal (+agent-shell-projects-request-proposal)))
          (goto-char (point-max)) (insert "\nUser correction.")
          (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) file))
                    ((symbol-function '+agent-shell-projects-choose-org-heading)
                     (lambda (_) "Proposal")))
            (call-interactively (key-binding (kbd "C-c C-c"))))
          (let ((artefact (+agent-shell-projects-artefact
                           project (plist-get project :proposal))))
            (should (equal "org-heading" (plist-get artefact :kind)))
            (should (plist-get artefact :org-id))
            (should (equal "captured" (plist-get proposal :status)))
            (should (string-match-p "User correction" (buffer-string)))))))))

(ert-deftest +agent-shell-projects-test-onboarding-implementation-capture-and-cancel ()
  (+agent-shell-projects-test--isolated
    (+agent-shell-projects-test--agent
      (let* ((root (+agent-shell-projects-test--repository directory))
             (proposal (+agent-shell-projects-enrol
                        project (+agent-shell-projects-test--file
                                 directory "proposal.md" "Agreed plan")))
             (_ (+agent-shell-projects--set project :proposal (plist-get proposal :id)))
             (_ (+agent-shell-projects-associate project shell))
             (accepted nil)
             (+agent-shell-projects-send-function (lambda (&rest _) accepted))
             code)
        (save-window-excursion
          (pop-to-buffer shell)
          (cl-letf (((symbol-function '+agent-shell-projects-mode) #'ignore))
            (let ((+agent-shell-projects-capture-delay 60))
              (setq code (+agent-shell-projects-start-implementation root "HEAD")))
            (should-not (plist-get project :requests))
            (+agent-shell-projects-cancel-capture root)
            (should-not (plist-get project :requests))
            (+agent-shell-projects-start-implementation)
            (+agent-shell-projects-test--captured code)
            (should (= 1 (length (plist-get project :requests))))
            (let* ((request (car (plist-get project :requests)))
                   (version (plist-get code :current))
                   (prompt (+agent-shell-projects-content
                            (list :blob (plist-get request :prompt)))))
              (should (equal "implementation" (plist-get request :kind)))
              (should (equal "queued" (plist-get request :status)))
              (should (string-match-p version prompt))
              (+agent-shell-projects-start-implementation)
              (+agent-shell-projects-test--captured code)
              (should (= 1 (length (plist-get project :requests))))
              (+agent-shell-projects-snapshot project proposal "Later proposal")
              (setq accepted t)
              (+agent-shell-projects-flush project)
              (should (equal "sent" (plist-get request :status)))
              (should (string-match-p "Agreed plan" prompt))
              (should-not (string-match-p "Later proposal" prompt)))))))))

(ert-deftest +agent-shell-projects-test-onboarding-real-acp-end-to-end ()
  (+agent-shell-projects-test--isolated
    (let* ((agent-shell-show-welcome-message nil)
           (shell-maker-prompt-before-killing-buffer nil)
           (agent-shell-show-config-icons nil)
           (agent-shell-prefer-viewport-interaction nil)
           (agent-shell-mcp-servers nil)
           (agent-shell-transcript-file-path-function nil)
           (+agent-shell-projects-agent-policy 'default)
           (+agent-shell-projects-agent-config
            (agent-shell-make-agent-config
             :identifier 'onboarding-fixture :mode-line-name "Fixture"
             :buffer-name "Onboarding Fixture" :shell-prompt "Fixture> "
             :shell-prompt-regexp "Fixture> "
             :client-maker
             (lambda (buffer)
               (acp-make-client :context-buffer buffer
                                :command (executable-find "python3")
                                :command-params (list +agent-shell-projects-test--fixture)))))
           (+agent-shell-projects-change-hook
            '(+agent-shell-projects--refresh-ui +agent-shell-projects--updated-code))
           (root (+agent-shell-projects-test--repository directory))
           (destination (expand-file-name "proposal.md" directory))
           author proposal code)
      (+agent-shell-projects--set project :root root)
      (unwind-protect
          (save-window-excursion
            (cl-letf (((symbol-function '+agent-shell-projects-mode) #'ignore))
              (+agent-shell-projects-explore project)
              (insert "Make a small implementation we can review.")
              (call-interactively (key-binding (kbd "C-c C-c")))
              (setq author (current-buffer))
              (+agent-shell-projects-test--wait-for
               (lambda () (and (eq (agent-shell-status :shell-buffer author) 'ready)
                               (equal "sent" (plist-get (car (plist-get project :requests)) :status)))))
              (agent-shell-insert :text "Success means the change is easy to validate."
                                  :shell-buffer author :submit t :no-focus t)
              (+agent-shell-projects-test--wait-for
               (lambda () (with-current-buffer author
                            (string-match-p "Clarification recorded" (buffer-string)))))
              (with-current-buffer author
                (setq proposal (+agent-shell-projects-request-proposal)))
              (+agent-shell-projects-test--wait-for
               (lambda () (equal "ready" (plist-get proposal :status))))
              (should (string-match-p "easy to validate" (buffer-string)))
              (goto-char (point-max)) (insert "\nUser-reviewed proposal.")
              (cl-letf (((symbol-function 'read-file-name)
                         (lambda (&rest _) destination)))
                (call-interactively (key-binding (kbd "C-c C-c"))))
              (should (plist-get project :proposal))
              (setq code (+agent-shell-projects-start-implementation root "HEAD"))
              (+agent-shell-projects-test--wait-for
               (lambda () (> (length (plist-get code :history)) 1)))
              (+agent-shell-projects-review-code)
              (let ((request (+agent-shell-projects-request-explanation)))
                (+agent-shell-projects-test--wait-for
                 (lambda () (equal "ready" (plist-get request :status)))))
              (+agent-shell-projects--refresh-visible)
              (should (string-match-p "Implement the agreed proposal" (buffer-string)))
              (+agent-shell-projects-render-code
               project code (+agent-shell-projects-version code (plist-get code :current)) "diff")
              (+agent-shell-projects-test--expand)
              (goto-char (point-min)) (search-forward "+Before")
              (set-mark (- (point) (length "Before")))
              (let* ((transient-mark-mode t) (mark-active t)
                     (selection (+agent-shell-projects--selection))
                     (version (plist-get code :current))
                     (annotation (+agent-shell-projects-annotate
                                  project code (+agent-shell-projects-version code version)
                                  (plist-get selection :start) (plist-get selection :end)
                                  "Change Before to After" "user"
                                  (plist-get selection :context))))
                (+agent-shell-projects-submit)
                (+agent-shell-projects-test--wait-for
                 (lambda () (and (plist-get annotation :replies)
                                 (not (equal version (plist-get code :current))))))
                (should (equal version (plist-get code :ack)))
                (should-not (plist-get annotation :resolved))
                (should (equal "After\n"
                               (with-temp-buffer
                                 (insert-file-contents (expand-file-name "implementation.txt" root))
                                 (buffer-string)))))))
        (when (buffer-live-p author) (kill-buffer author))))))

(provide 'agent-shell-projects-onboarding-test)
;;; agent-shell-projects-onboarding-test.el ends here
