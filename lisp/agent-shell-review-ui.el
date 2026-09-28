;;; agent-shell-review-ui.el --- Sidebar for agent reviews -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:
;; Display review progress, questions, and findings in a compact right sidebar.
;; Loaded by `agent-shell-review' after its coordinator functions are defined.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar-local agent-shell-review-ui--refresh-timer nil
  "Timer that refreshes visible staleness for this sidebar.")

(defvar-local agent-shell-review-ui--observed-stale nil
  "Staleness state last rendered in this sidebar.")

(defun agent-shell-review-ui--stop-refresh ()
  "Stop this sidebar's staleness timer before it is killed."
  (when (timerp agent-shell-review-ui--refresh-timer)
    (cancel-timer agent-shell-review-ui--refresh-timer)
    (setq agent-shell-review-ui--refresh-timer nil))
  (when (and agent-shell-review--current-run
             (eq (agent-shell-review--run-sidebar-buffer
                  agent-shell-review--current-run)
                 (current-buffer)))
    (agent-shell-review--cancel agent-shell-review--current-run)))

(defvar agent-shell-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'agent-shell-review-next-item)
    (define-key map (kbd "p") #'agent-shell-review-previous-item)
    (define-key map (kbd "RET") #'agent-shell-review-visit-source)
    (define-key map (kbd "TAB") #'agent-shell-review-toggle-details)
    (define-key map (kbd "a") #'agent-shell-review-answer)
    (define-key map (kbd "C-c C-c") #'agent-shell-review-submit-answers)
    (define-key map (kbd "m") #'agent-shell-review-mark)
    (define-key map (kbd "u") #'agent-shell-review-unmark)
    (define-key map (kbd "S") #'agent-shell-review-send-marked)
    (define-key map (kbd "g") #'agent-shell-review-rerun)
    (define-key map (kbd "v") #'agent-shell-review-open-reviewer)
    (define-key map (kbd "f") #'agent-shell-review-select-spec)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `agent-shell-review-mode'.")

(define-derived-mode agent-shell-review-mode special-mode "Agent Review"
  "Major mode for review questions and prioritized findings."
  (setq-local truncate-lines nil
              word-wrap t)
  (add-hook 'kill-buffer-hook #'agent-shell-review-ui--stop-refresh nil t))

(defun agent-shell-review-ui--buffer (run)
  "Create or return RUN's sidebar buffer."
  (or (and (buffer-live-p (agent-shell-review--run-sidebar-buffer run))
           (agent-shell-review--run-sidebar-buffer run))
      (let ((buffer (get-buffer-create
                     (format "*Agent Review: %s*"
                             (file-name-nondirectory
                              (directory-file-name
                               (agent-shell-review--run-project run)))))))
        (setf (agent-shell-review--run-sidebar-buffer run) buffer)
        (with-current-buffer buffer
          (unless (eq agent-shell-review--current-run run)
            (agent-shell-review-ui--stop-refresh))
          (agent-shell-review-mode)
          (setq-local agent-shell-review--current-run run))
        buffer)))

(defun agent-shell-review-ui-show (run)
  "Display RUN in a right sidebar without selecting it.
Return the sidebar window."
  (let* ((buffer (agent-shell-review-ui--buffer run))
         (selected (selected-window))
         (window
          (display-buffer-in-side-window
           buffer `((side . right) (slot . 0)
                    (window-width . ,(max 12 (/ (frame-width) 3)))))))
    (with-current-buffer buffer
      (setq-local agent-shell-review--current-run run)
      (unless (timerp agent-shell-review-ui--refresh-timer)
        (setq-local agent-shell-review-ui--refresh-timer
                    (run-at-time 5 5 #'agent-shell-review-ui--refresh buffer))))
    (agent-shell-review-ui-render run)
    (when (window-live-p selected)
      (select-window selected))
    window))

(defun agent-shell-review-ui--items (run)
  "Return visible items for RUN."
  (plist-get (or (agent-shell-review--run-result run)
                 (agent-shell-review--run-stale-result run))
             :items))

(defun agent-shell-review-ui--kind (run)
  "Return visible result kind for RUN."
  (plist-get (or (agent-shell-review--run-result run)
                 (agent-shell-review--run-stale-result run))
             :kind))

(defun agent-shell-review-ui--stale-p (run)
  "Return non-nil if RUN shows findings for an older changeset."
  (and (or (agent-shell-review--run-stale-result run)
           (let ((snapshot (agent-shell-review--run-snapshot run)))
             (and snapshot (agent-shell-review--run-result run)
                  (not (equal (plist-get snapshot :fingerprint)
                              (agent-shell-review-git-current-fingerprint
                               snapshot))))))
       t))

(defun agent-shell-review-ui--refresh (buffer)
  "Refresh visible staleness in sidebar BUFFER when it changes."
  (when (and (buffer-live-p buffer) (get-buffer-window buffer t))
    (with-current-buffer buffer
      (when-let* ((run agent-shell-review--current-run)
                  ((memq (agent-shell-review--run-status run)
                         '(questions findings clear))))
        (unless (eq (agent-shell-review-ui--stale-p run)
                    agent-shell-review-ui--observed-stale)
          (agent-shell-review-ui-render run))))))

(defun agent-shell-review-ui--row (run item index)
  "Insert compact ITEM row INDEX for RUN."
  (let* ((kind (agent-shell-review-ui--kind run))
         (marked (member index (agent-shell-review--run-marks run)))
         (answered (assoc (plist-get item :question)
                          (agent-shell-review--run-answers run)))
         (label
          (pcase kind
            ('questions
             (format "%d. %s%s" (1+ index)
                     (plist-get item :question)
                     (if answered "  ✓" "")))
            ('findings
             (format "%s %s %s"
                     (if marked "[x]" "[ ]")
                     (plist-get item :priority)
                     (plist-get item :title)))
            (_ ""))))
    (insert (propertize label 'agent-shell-review-item index
                        'mouse-face 'highlight)
            "\n")
    (when (member index (agent-shell-review--run-expanded run))
      (let ((detail-start (point)))
        (pcase kind
          ('questions
           (insert (format "  Criterion: %s\n  Why: %s\n"
                           (plist-get item :criterion)
                           (plist-get item :why)))
           (when answered
             (insert (format "  Answer: %s\n" (cdr answered)))))
          ('findings
           (insert (format "  %s%s\n  Evidence: %s\n  Requirement: %s\n  Fix: %s\n"
                           (or (plist-get item :file) "General")
                           (if-let* ((line (plist-get item :line)))
                               (format ":%d" line) "")
                           (plist-get item :evidence)
                           (plist-get item :requirement)
                           (plist-get item :suggestion)))))
        (add-text-properties detail-start (point)
                             (list 'agent-shell-review-item index))))))

(defun agent-shell-review-ui-render (run)
  "Render RUN's current status and review items."
  (when-let* ((buffer (agent-shell-review--run-sidebar-buffer run))
              ((buffer-live-p buffer))
              ((eq (buffer-local-value 'agent-shell-review--current-run
                                       buffer) run)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (status (agent-shell-review--run-status run))
            (kind (agent-shell-review-ui--kind run))
            (stale (agent-shell-review-ui--stale-p run))
            (old-point (point))
            (old-column (current-column))
            (item-index (or (get-text-property (point)
                                               'agent-shell-review-item)
                            (get-text-property (max (point-min) (1- (point)))
                                               'agent-shell-review-item))))
        (setq-local agent-shell-review--current-run run)
        (setq-local agent-shell-review-ui--observed-stale stale)
        (erase-buffer)
        (insert (format "Review: %s\n\n" status))
        (when stale
          (insert "Previous findings are stale; review the new result.\n\n"))
        (pcase status
          ('collecting (insert "Collecting changes…\n"))
          ('discovering (insert "Finding requirements…\n"))
          ('starting (insert "Starting a fresh reviewer…\n"))
          ('reviewing (insert "Reviewer is working…\n"))
          ('questions (insert "Answer each question before submitting.\n"))
          ('findings nil)
          ('clear (insert "No actionable correctness findings.\n"))
          ('error
           (insert (format "%s\n"
                           (or (agent-shell-review--run-error-message run)
                               "Review failed")))))
        (when (memq kind '(questions findings))
          (insert "\n")
          (cl-loop for item in (agent-shell-review-ui--items run)
                   for index from 0
                   do (agent-shell-review-ui--row run item index)))
        (if-let* ((row (and item-index
                            (text-property-any
                             (point-min) (point-max)
                             'agent-shell-review-item item-index))))
            (progn
              (goto-char row)
              (move-to-column old-column))
          (goto-char (min old-point (point-max))))
        (force-mode-line-update t)))))

(defun agent-shell-review-ui--run ()
  "Return the review run in the current sidebar."
  (or agent-shell-review--current-run
      (user-error "No review is shown here")))

(defun agent-shell-review-ui--index ()
  "Return the review item index at point."
  (or (get-text-property (point) 'agent-shell-review-item)
      (get-text-property (max (point-min) (1- (point)))
                         'agent-shell-review-item)
      (user-error "Move to a review item first")))

(defun agent-shell-review-ui--item ()
  "Return the current item in the sidebar."
  (nth (agent-shell-review-ui--index)
       (agent-shell-review-ui--items (agent-shell-review-ui--run))))

(defun agent-shell-review-next-item ()
  "Move to the next review item."
  (interactive)
  (let ((next (next-single-property-change
               (point) 'agent-shell-review-item)))
    (while (and next
                (not (get-text-property next 'agent-shell-review-item)))
      (setq next (next-single-property-change
                  next 'agent-shell-review-item)))
    (if next (goto-char next) (user-error "No next item"))))

(defun agent-shell-review-previous-item ()
  "Move to the previous review item."
  (interactive)
  (let ((previous (previous-single-property-change
                   (point) 'agent-shell-review-item)))
    (while (and previous
                (not (get-text-property previous 'agent-shell-review-item)))
      (setq previous (previous-single-property-change
                      previous 'agent-shell-review-item)))
    (if previous (goto-char previous) (user-error "No previous item"))))

(defun agent-shell-review-toggle-details ()
  "Expand or collapse the review item at point."
  (interactive)
  (let* ((run (agent-shell-review-ui--run))
         (index (agent-shell-review-ui--index))
         (expanded (agent-shell-review--run-expanded run)))
    (setf (agent-shell-review--run-expanded run)
          (if (member index expanded)
              (delete index expanded)
            (cons index expanded)))
    (agent-shell-review-ui-render run)))

(defun agent-shell-review-visit-source ()
  "Visit the source location for the finding at point."
  (interactive)
  (let* ((run (agent-shell-review-ui--run))
         (item (agent-shell-review-ui--item))
         (file (plist-get item :file)))
    (unless file (user-error "This review item has no source location"))
    (find-file-other-window (expand-file-name file
                                              (agent-shell-review--run-project
                                               run)))
    (when-let* ((line (plist-get item :line)))
      (goto-char (point-min))
      (forward-line (1- line)))))

(defun agent-shell-review-answer ()
  "Answer the review question at point."
  (interactive)
  (let* ((run (agent-shell-review-ui--run))
         (item (agent-shell-review-ui--item))
         (question (plist-get item :question)))
    (unless (and (eq (agent-shell-review--run-status run) 'questions)
                 question)
      (user-error "This item is not a current question"))
    (let* ((prior (assoc question (agent-shell-review--run-answers run)))
           (answer (read-string (format "Answer %s (or unknown): " question)
                                (cdr prior))))
      (when (string-empty-p (string-trim answer))
        (user-error "Enter an answer or type unknown"))
      (setf (agent-shell-review--run-answers run)
            (cons (cons question answer)
                  (assoc-delete-all question
                                    (agent-shell-review--run-answers run))))
      (agent-shell-review-ui-render run))))

(defun agent-shell-review-submit-answers ()
  "Send all answered questions to the same reviewer session."
  (interactive)
  (let* ((run (agent-shell-review-ui--run))
         (items (plist-get (agent-shell-review--run-result run) :items))
         (answers
          (mapcar (lambda (item)
                    (assoc (plist-get item :question)
                           (agent-shell-review--run-answers run)))
                  items)))
    (unless (eq (agent-shell-review--run-status run) 'questions)
      (user-error "Review is not waiting for answers"))
    (unless (and answers (cl-every (lambda (pair)
                                     (and pair (not (string-empty-p
                                                     (string-trim
                                                      (cdr pair))))))
                                   answers))
      (user-error "Answer every question or enter unknown"))
    (agent-shell-review--submit-answers run answers)))

(defun agent-shell-review-mark ()
  "Mark the finding at point for a later fix request."
  (interactive)
  (let* ((run (agent-shell-review-ui--run))
         (index (agent-shell-review-ui--index)))
    (unless (eq (agent-shell-review--run-status run) 'findings)
      (user-error "Only current findings can be marked"))
    (cl-pushnew index (agent-shell-review--run-marks run))
    (agent-shell-review-ui-render run)))

(defun agent-shell-review-unmark ()
  "Remove the mark from the finding at point."
  (interactive)
  (let* ((run (agent-shell-review-ui--run))
         (index (agent-shell-review-ui--index)))
    (setf (agent-shell-review--run-marks run)
          (delete index (agent-shell-review--run-marks run)))
    (agent-shell-review-ui-render run)))

(defun agent-shell-review-ui--fix-prompt (run findings)
  "Build one fix request for RUN's selected FINDINGS."
  (let* ((requirements (agent-shell-review--run-requirements run))
         (source (plist-get requirements :source)))
    (concat
     "Please fix these marked review findings, then tell me what changed.\n\n"
     (if (eq (plist-get requirements :kind) 'file)
         (format "Original requirements file: [%s](<%s>)\nRead this file before fixing."
                 (file-name-nondirectory source) source)
       (format "Original requirements (%s):\n%s"
               source (plist-get requirements :text)))
     "\n\nClarified criteria:\n"
     (if-let* ((answers (agent-shell-review--run-clarifications run)))
         (mapconcat (lambda (answer)
                      (format "Q: %s\nA: %s" (car answer) (cdr answer)))
                    answers "\n")
       "None")
     "\n\nMarked findings:\n"
     (mapconcat
      (lambda (item)
        (format "%s %s%s — %s\nEvidence: %s\nRequirement: %s\nSuggested fix: %s"
                (plist-get item :priority)
                (or (plist-get item :file) "General")
                (if-let* ((line (plist-get item :line)))
                    (format ":%d" line) "")
                (plist-get item :title)
                (plist-get item :evidence)
                (plist-get item :requirement)
                (plist-get item :suggestion)))
      findings "\n\n"))))

(defun agent-shell-review-send-marked ()
  "Send marked findings as one request to the implementation agent."
  (interactive)
  (let* ((run (agent-shell-review-ui--run))
         (marks (sort (copy-sequence (agent-shell-review--run-marks run)) #'<))
         (items (plist-get (agent-shell-review--run-result run) :items)))
    (unless (eq (agent-shell-review--run-status run) 'findings)
      (user-error "No current findings to send"))
    (unless marks (user-error "Mark findings with m first"))
    (let ((prompt (agent-shell-review-ui--fix-prompt
                   run (mapcar (lambda (index) (nth index items)) marks))))
      (setf (agent-shell-review--run-fix-prompt run) prompt)
      (if-let* ((send (agent-shell-review--run-send-fixes run)))
          (condition-case err
              (progn
                (unless (funcall send prompt)
                  (user-error "Implementation session did not accept the prompt"))
                (message "Sent %d review finding%s"
                         (length marks) (if (= (length marks) 1) "" "s")))
            (error
             (kill-new prompt)
             (user-error "%s; fix prompt copied"
                         (error-message-string err))))
        (let ((buffer
               (get-buffer-create
                (format "*Agent Review Fixes: %s*"
                        (file-name-nondirectory
                         (directory-file-name
                          (agent-shell-review--run-project run)))))))
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert prompt)
              (special-mode)))
          (pop-to-buffer buffer))))))

(defun agent-shell-review-rerun ()
  "Capture a new snapshot and start a fresh reviewer session."
  (interactive)
  (let* ((run (agent-shell-review-ui--run))
         (requirements (agent-shell-review--run-requirements run))
         (spec (and (eq (plist-get requirements :kind) 'file)
                    (plist-get requirements :source))))
    (agent-shell-review--cancel run)
    (agent-shell-review--begin
     (agent-shell-review--run-project run)
     (agent-shell-review--run-origin run)
     spec
     (agent-shell-review--run-clarifications run)
     (or (and (eq (plist-get (agent-shell-review--run-result run) :kind)
                  'findings)
              (agent-shell-review--run-result run))
         (agent-shell-review--run-stale-result run))
     (and (eq (plist-get requirements :kind) 'entered)
          requirements))))

(defun agent-shell-review-select-spec ()
  "Select a requirements file and retry the review."
  (interactive)
  (agent-shell-review '(4)))

(defun agent-shell-review-open-reviewer ()
  "Show the reviewer's raw response and ACP diagnostics."
  (interactive)
  (let* ((run (agent-shell-review-ui--run))
         (buffer
          (get-buffer-create
           (format "*Agent Review Diagnostics: %s*"
                   (file-name-nondirectory
                    (directory-file-name
                     (agent-shell-review--run-project run)))))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Raw reviewer answer:\n"
                (or (agent-shell-review--run-text run) "")
                "\n\nACP diagnostics:\n"
                (if-let* ((transport (agent-shell-review--run-transport run)))
                    (agent-shell-review-acp-diagnostics transport)
                  "No reviewer transport")
                "\n\nError:\n"
                (or (agent-shell-review--run-error-message run) "None"))
        (special-mode)))
    (pop-to-buffer buffer)))

(provide 'agent-shell-review-ui)
;;; agent-shell-review-ui.el ends here
