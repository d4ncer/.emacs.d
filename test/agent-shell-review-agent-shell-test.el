;;; agent-shell-review-agent-shell-test.el --- Origin adapter tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Verify exact-buffer capture and fix delivery without starting a shell.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'agent-shell-review)
(require 'agent-shell-review-agent-shell nil t)

(ert-deftest agent-shell-review-agent-shell-test-exact-origin-and-context ()
  "Capture the invoking shell even when another project shell exists."
  (let* ((root temporary-file-directory)
         (first (generate-new-buffer " *review-origin-first*"))
         (second (generate-new-buffer " *review-origin-second*"))
         (transcript (make-temp-file "review-transcript-")))
    (unwind-protect
        (progn
          (with-temp-file transcript (insert "## User\nOriginal instruction"))
          (with-current-buffer first
            (setq major-mode 'agent-shell-mode)
            (setq-local agent-shell--transcript-file transcript))
          (with-current-buffer second (setq major-mode 'agent-shell-mode))
          (cl-letf (((symbol-function 'agent-shell-buffers)
                     (lambda () (list first second)))
                    ((symbol-function 'agent-shell-cwd)
                     (lambda () root)))
            (with-current-buffer first
              (let ((origin (agent-shell-review-agent-shell-origin root)))
                (should (eq (plist-get origin :buffer) first))
                (should (string-match-p "Original instruction"
                                        (plist-get origin :context)))))))
      (kill-buffer first)
      (kill-buffer second)
      (delete-file transcript))))

(ert-deftest agent-shell-review-agent-shell-test-source-selection ()
  "Choose among matching shells from source and allow no origin."
  (let* ((root temporary-file-directory)
         (first (generate-new-buffer " *review-match-one*"))
         (second (generate-new-buffer " *review-match-two*"))
         (matches (list first second))
         (choices 0))
    (unwind-protect
        (progn
          (with-current-buffer first (setq major-mode 'agent-shell-mode))
          (with-current-buffer second (setq major-mode 'agent-shell-mode))
          (cl-letf (((symbol-function 'agent-shell-buffers)
                   (lambda () matches))
                  ((symbol-function 'agent-shell-cwd)
                   (lambda () root))
                  ((symbol-function 'completing-read)
                   (lambda (_prompt _items &rest _args)
                     (cl-incf choices)
                     (buffer-name second))))
          (with-temp-buffer
            (should (eq (plist-get
                         (agent-shell-review-agent-shell-origin root) :buffer)
                        second))
            (should (= choices 1))
            (setq matches nil)
            (should-not (agent-shell-review-agent-shell-origin root)))))
      (kill-buffer first)
      (kill-buffer second))))

(ert-deftest agent-shell-review-agent-shell-test-send-readiness ()
  "Never insert into an unready, busy, or closed origin buffer."
  (let ((buffer (generate-new-buffer " *review-send-origin*"))
        (ready nil)
        (busy nil)
        (sent nil))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-session-id)
                   (lambda (&rest _args) ready))
                  ((symbol-function 'shell-maker-busy)
                   (lambda () busy))
                  ((symbol-function 'agent-shell-insert)
                   (lambda (&rest args)
                     (push args sent)
                     '((:buffer . inserted)))))
          (with-current-buffer buffer (setq major-mode 'agent-shell-mode))
          (should-error (agent-shell-review-agent-shell-send buffer "Fix")
                        :type 'user-error)
          (setq ready "session-1" busy t)
          (should-error (agent-shell-review-agent-shell-send buffer "Fix")
                        :type 'user-error)
          (should-not sent)
          (setq busy nil)
          (should (agent-shell-review-agent-shell-send buffer "Fix"))
          (should (eq (plist-get (car sent) :shell-buffer) buffer))
          (should (plist-get (car sent) :submit))
          (should (plist-get (car sent) :no-focus))
          (kill-buffer buffer)
          (should-error (agent-shell-review-agent-shell-send buffer "Fix")
                        :type 'user-error))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest agent-shell-review-agent-shell-test-no-reviewer-shell-buffer ()
  "The review keeps only the existing implementation shell in shell lists."
  (let* ((origin (generate-new-buffer " *review-main-agent*"))
         (root temporary-file-directory)
         (agent-shell-review-origin-provider
          #'agent-shell-review-agent-shell-origin)
         (sent nil)
         (callback nil)
         (run nil))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-review--project-root)
                   (lambda () root))
                  ((symbol-function 'agent-shell-review-git-snapshot)
                   (lambda (&rest _args)
                     '(:root "/tmp" :base "main" :diff "change")))
                  ((symbol-function 'agent-shell-review-spec-resolve)
                   (lambda (&rest _args)
                     '(:kind conversation :source "session"
                             :text "Original criteria")))
                  ((symbol-function 'agent-shell-review-acp-create)
                   (lambda (_root _file event)
                     (setq callback event)
                     (make-agent-shell-review-acp--transport :closed t)))
                  ((symbol-function 'agent-shell-review-acp-start)
                   (lambda (_transport) (funcall callback '(:type ready))))
                  ((symbol-function 'agent-shell-review-acp-send)
                   (lambda (&rest _args) t))
                  ((symbol-function 'agent-shell-review-acp-close) #'ignore)
                  ((symbol-function 'agent-shell-session-id)
                   (lambda (&rest _args) "main-session"))
                  ((symbol-function 'shell-maker-busy) (lambda () nil))
                  ((symbol-function 'agent-shell-insert)
                   (lambda (&rest args)
                     (push args sent)
                     '((:buffer . inserted)))))
          (with-current-buffer origin
            (setq major-mode 'agent-shell-mode)
            (insert "## User\nImplement this")
            (setq run (agent-shell-review)))
          (should (eq (plist-get (agent-shell-review--run-origin run) :buffer)
                      origin))
          (should (equal (cl-remove-if-not
                          (lambda (buffer)
                            (with-current-buffer buffer
                              (derived-mode-p 'agent-shell-mode)))
                          (buffer-list))
                         (list origin)))
          (funcall (agent-shell-review--run-send-fixes run) "Fix selected")
          (should (eq (plist-get (car sent) :shell-buffer) origin)))
      (when-let* ((sidebar (and run
                               (agent-shell-review--run-sidebar-buffer run))))
        (when (buffer-live-p sidebar) (kill-buffer sidebar)))
      (kill-buffer origin))))

(provide 'agent-shell-review-agent-shell-test)
;;; agent-shell-review-agent-shell-test.el ends here
