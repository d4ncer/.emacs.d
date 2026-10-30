;;; +agent-shell-pstack-panels.el --- The pstack panels view -*- lexical-binding: t; -*-

;;; Commentary:
;; `+agent-shell-pstack-panels' lists every pstack panel as a magit section,
;; the most urgent first, under a line counting them by status.  Each panel
;; folds into its members, its workspace and its follow-up, and below the
;; panels come the branch-mode delegate records no live panel holds.  Every
;; section has a dim line naming the keys that act on it, and ? lists them
;; all.  A refresh keeps each window on the section and column it was on,
;; and keeps what was folded.  This library loads magit-section, so it
;; loads only when the view opens; `+agent-shell-pstack-panel' holds the
;; panels themselves.

;;; Code:

(require 'magit-section)
(require '+agent-shell-pstack-panel)

(declare-function +agent-shell-arena-remote-transcript-buffer
                  "+agent-shell-arena-remote")
(declare-function +agent-shell-arena-remote-arena "+agent-shell-arena-remote")
(declare-function +agent-shell-arena-remote-polled "+agent-shell-arena-remote")
(declare-function +agent-shell-arena-remote-phase "+agent-shell-arena-remote")
(declare-function +agent-shell-arena-remote-runner-phase
                  "+agent-shell-arena-remote")
(declare-function +agent-shell-arena-remote-describe
                  "+agent-shell-arena-remote")

;;;; Faces

(defface +agent-shell-pstack-panels-input '((t :inherit warning :weight bold))
  "Face for a panel whose delivered follow-up you have not read."
  :group '+agent-shell-pstack-panel)

(defface +agent-shell-pstack-panels-error '((t :inherit error))
  "Face for a panel or member that ended in an error."
  :group '+agent-shell-pstack-panel)

(defface +agent-shell-pstack-panels-host '((t :inherit font-lock-constant-face))
  "Face for a panel waiting on its host."
  :group '+agent-shell-pstack-panel)

(defface +agent-shell-pstack-panels-judging '((t :inherit font-lock-type-face))
  "Face for an arena whose judge is scoring."
  :group '+agent-shell-pstack-panel)

(defface +agent-shell-pstack-panels-running
  '((t :inherit font-lock-keyword-face))
  "Face for a running panel or member."
  :group '+agent-shell-pstack-panel)

(defface +agent-shell-pstack-panels-queued '((t :inherit font-lock-doc-face))
  "Face for a panel or member waiting for its turn."
  :group '+agent-shell-pstack-panel)

(defface +agent-shell-pstack-panels-cancelled '((t :inherit shadow))
  "Face for a cancelled panel."
  :group '+agent-shell-pstack-panel)

(defface +agent-shell-pstack-panels-done '((t :inherit success))
  "Face for a finished panel or member."
  :group '+agent-shell-pstack-panel)

(defface +agent-shell-pstack-panels-hint '((t :inherit shadow))
  "Face for the line naming the keys that act on a section."
  :group '+agent-shell-pstack-panel)

;;;; Status

(defconst +agent-shell-pstack-panels--statuses
  '((input "needs input" +agent-shell-pstack-panels-input)
    (error "error" +agent-shell-pstack-panels-error)
    (host "on host" +agent-shell-pstack-panels-host)
    (judging "judging" +agent-shell-pstack-panels-judging)
    (running "running" +agent-shell-pstack-panels-running)
    (queued "finishing" +agent-shell-pstack-panels-queued)
    (cancelled "cancelled" +agent-shell-pstack-panels-cancelled)
    (done "done" +agent-shell-pstack-panels-done))
  "Panel statuses, most urgent first, each (STATUS LABEL FACE).
A finishing panel has ended but its follow-up has not reached its lead.")

(defun +agent-shell-pstack-panels--finished-p (panel)
  "Return whether PANEL has ended, including as a rejected request."
  (or (+agent-shell-pstack-panel-rejection panel)
      (eq (+agent-shell-pstack-panel-phase panel) 'finished)))

(defun +agent-shell-pstack-panels--member-error (member)
  "Return the kind of error MEMBER ended with, as a string, or nil."
  (pcase (+agent-shell-pstack-panel-member-state member)
    ('timeout "timeout")
    ('error (if-let* ((kind (plist-get (+agent-shell-pstack-panel-member-failure
                                        member)
                                       :kind)))
                (format "%s" kind)
              "error"))
    (_ (when (eq (+agent-shell-pstack-panel-member-capture member) 'failed)
         "capture failed"))))

(defun +agent-shell-pstack-panels--error (panel)
  "Return the kind of error PANEL ended with, as a string, or nil."
  (if (+agent-shell-pstack-panel-rejection panel)
      "rejected"
    (seq-some #'+agent-shell-pstack-panels--member-error
              (+agent-shell-pstack-panel-members panel))))

(defun +agent-shell-pstack-panels--status (panel)
  "Return PANEL's status, a key of `+agent-shell-pstack-panels--statuses'."
  (cond ((+agent-shell-pstack-panel-rejection panel) 'error)
        ((not (+agent-shell-pstack-panels--finished-p panel))
         (pcase (+agent-shell-pstack-panel-phase panel)
           ((or 'submitting 'remote 'fetching) 'host)
           ('judging 'judging)
           (_ 'running)))
        ((+agent-shell-pstack-panel-cancelled panel) 'cancelled)
        ((not (+agent-shell-pstack-panel-delivered panel)) 'queued)
        ((+agent-shell-pstack-panel-unread-p panel) 'input)
        ((+agent-shell-pstack-panels--error panel) 'error)
        (t 'done)))

(defun +agent-shell-pstack-panels--badge (panel)
  "Return PANEL's status badge, padded and in its status's face."
  (pcase-let* ((status (+agent-shell-pstack-panels--status panel))
               (`(,label ,face) (alist-get status
                                           +agent-shell-pstack-panels--statuses))
               (phase (+agent-shell-pstack-panel-phase panel))
               (detail (pcase status
                         ('error (+agent-shell-pstack-panels--error panel))
                         ('running (unless (eq phase 'running)
                                     (symbol-name phase)))
                         ('host (when-let* ((remote
                                             (+agent-shell-pstack-panel-remote
                                              panel))
                                            (there
                                             (+agent-shell-arena-remote-runner-phase
                                              remote)))
                                  (format "%s" there))))))
    (concat (propertize (format "[%s%s]" label
                                (if detail (concat ": " detail) ""))
                        'font-lock-face face)
            " ")))

(defun +agent-shell-pstack-panels--member-badge (member)
  "Return MEMBER's state, padded and in its face."
  (let* ((state (+agent-shell-pstack-panel-member-state member))
         (label (pcase state
                  ('error (let ((kind (+agent-shell-pstack-panels--member-error
                                       member)))
                            (if (equal kind "error") "error"
                              (format "error: %s" kind))))
                  ('running (if (timerp (+agent-shell-pstack-panel-member-retry
                                         member))
                                "retrying"
                              "running"))
                  (_ (symbol-name state))))
         (face (pcase state
                 ((or 'error 'timeout) '+agent-shell-pstack-panels-error)
                 ('running '+agent-shell-pstack-panels-running)
                 ('done '+agent-shell-pstack-panels-done)
                 (_ '+agent-shell-pstack-panels-queued))))
    (concat (propertize label 'font-lock-face face)
            (make-string (max 1 (- 10 (length label))) ?\s))))

(defun +agent-shell-pstack-panels--age (time)
  "Return how long ago TIME was, briefly."
  (let ((seconds (max 0 (round (- (float-time) time)))))
    (cond ((< seconds 60) (format "%ds" seconds))
          ((< seconds 3600) (format "%dm" (/ seconds 60)))
          ((< seconds 86400) (format "%dh" (/ seconds 3600)))
          (t (format "%dd" (/ seconds 86400))))))

(defun +agent-shell-pstack-panels--sorted ()
  "Return every panel, the most urgent status first, then oldest first."
  (let ((rank (lambda (panel)
                (seq-position (mapcar #'car +agent-shell-pstack-panels--statuses)
                              (+agent-shell-pstack-panels--status panel)))))
    (sort (+agent-shell-pstack-panel-list)
          :key (lambda (panel)
                 (cons (funcall rank panel)
                       (+agent-shell-pstack-panel-created panel)))
          :lessp (lambda (a b)
                   (or (< (car a) (car b))
                       (and (= (car a) (car b)) (< (cdr a) (cdr b))))))))

(defun +agent-shell-pstack-panels--summary (panels)
  "Return the line counting PANELS by status."
  (concat
   (propertize "pstack panels" 'font-lock-face 'magit-section-heading)
   "  "
   (if panels
       (mapconcat
        #'identity
        (seq-keep (pcase-lambda (`(,status ,label ,face))
                    (let ((count (seq-count
                                  (lambda (panel)
                                    (eq status (+agent-shell-pstack-panels--status
                                                panel)))
                                  panels)))
                      (when (> count 0)
                        (propertize (format "%d %s" count label)
                                    'font-lock-face face))))
                  +agent-shell-pstack-panels--statuses)
        ", ")
     "none")
   "\n"))

;;;; Mode

(defvar-keymap +agent-shell-pstack-panels-mode-map
  :doc "Keymap for `+agent-shell-pstack-panels-mode'."
  :parent magit-section-mode-map
  "RET" #'+agent-shell-pstack-panels-visit
  "x" #'+agent-shell-pstack-panels-clean
  "c" #'+agent-shell-pstack-panels-cancel
  "D" #'+agent-shell-pstack-panels-clear-delivered
  "l" #'+agent-shell-pstack-panel-log
  "g" #'revert-buffer
  "?" #'+agent-shell-pstack-panels-keys)

(define-derived-mode +agent-shell-pstack-panels-mode magit-section-mode
  "pstack panels"
  "List pstack panels, their members, workspaces and follow-ups.
Each section names the keys that act on it, and
\\<+agent-shell-pstack-panels-mode-map>\\[+agent-shell-pstack-panels-keys] \
lists them all.

\\{+agent-shell-pstack-panels-mode-map}"
  (setq-local revert-buffer-function
              (lambda (&rest _) (+agent-shell-pstack-panels--render)))
  ;; Shown again, it redraws, since what you have read may have changed.
  (add-hook 'window-buffer-change-functions
            #'+agent-shell-pstack-panels--shown nil t))

(defun +agent-shell-pstack-panels--shown (window)
  "Redraw the panels view WINDOW now shows."
  (with-current-buffer (window-buffer window)
    (+agent-shell-pstack-panels--render)))

;;;; Sections

(defun +agent-shell-pstack-panels--up (section type)
  "Return SECTION or its nearest ancestor of TYPE, or nil."
  (while (and section (not (eq (oref section type) type)))
    (setq section (oref section parent)))
  section)

(defun +agent-shell-pstack-panels--panel (&optional section)
  "Return the panel SECTION, by default the one at point, belongs to."
  (when-let* ((section (+agent-shell-pstack-panels--up
                        (or section (magit-current-section)) 'pstack-panel)))
    (+agent-shell-pstack-panel--find (oref section value))))

(defun +agent-shell-pstack-panels--record (&optional section)
  "Return the manifest of the record SECTION, by default point's, shows."
  (when-let* ((section (+agent-shell-pstack-panels--up
                        (or section (magit-current-section)) 'pstack-record)))
    (seq-find (lambda (manifest)
                (equal (oref section value)
                       (+agent-shell-arena-git-manifest-id manifest)))
              (+agent-shell-pstack-panel--interrupted))))

(defun +agent-shell-pstack-panels--lead ()
  "Return the lead of the request or panel at point, or nil."
  (if-let* ((section (+agent-shell-pstack-panels--up (magit-current-section)
                                                     'pstack-request)))
      (oref section value)
    (when-let* ((panel (+agent-shell-pstack-panels--panel)))
      (+agent-shell-pstack-panel-lead panel))))

(defun +agent-shell-pstack-panels--member (section)
  "Return the member SECTION shows, or nil."
  (when (eq (oref section type) 'pstack-member)
    (+agent-shell-pstack-panel--member (+agent-shell-pstack-panels--panel section)
                                       (oref section value))))

(defun +agent-shell-pstack-panels--target (section)
  "Return (WHAT . TARGET), what RET on SECTION visits, or nil.
TARGET is as `+agent-shell-pstack-panels--visit-target' takes it."
  (pcase (oref section type)
    ('pstack-request
     (when (buffer-live-p (oref section value))
       (cons "lead" (oref section value))))
    ('pstack-panel
     (let ((panel (+agent-shell-pstack-panels--panel section)))
       (when (buffer-live-p (+agent-shell-pstack-panel-lead panel))
         (cons "lead" (cons 'pstack-lead panel)))))
    ('pstack-member
     (let* ((panel (+agent-shell-pstack-panels--panel section))
            (member (+agent-shell-pstack-panels--member section))
            (shell (+agent-shell-pstack-panel-member-shell member))
            (report (+agent-shell-pstack-panel-member-report member)))
       (cond ((and (eq 'delegate (+agent-shell-pstack-panel-member-kind member))
                   (+agent-shell-pstack-panel-remote panel))
              (cons "transcript" (cons 'pstack-transcript
                                       (+agent-shell-pstack-panel-remote
                                        panel))))
             ((and (buffer-live-p shell)
                   (not (+agent-shell-pstack-panel--settled-p member)))
              (cons "shell" shell))
             (report (cons "report" report))
             ((buffer-live-p shell) (cons "shell" shell)))))
    ('pstack-workspace
     (when-let* ((panel (+agent-shell-pstack-panels--panel section))
                 (manifest (+agent-shell-pstack-panel-arena panel))
                 (directory (+agent-shell-arena-git-manifest-directory manifest))
                 ((file-exists-p directory))
                 (folder (if (eq 'branch (+agent-shell-pstack-panel--isolation
                                          panel))
                             (+agent-shell-arena-git-manifest-toplevel manifest)
                           directory))
                 ((file-directory-p folder)))
       (cons "folder" folder)))
    ('pstack-followup
     (cons "follow-up" (cons 'pstack-followup
                             (+agent-shell-pstack-panels--panel section))))
    ('pstack-record
     (when-let* ((manifest (+agent-shell-pstack-panels--record section)))
       (cons "checkout" (+agent-shell-arena-git-manifest-toplevel manifest))))))

(defun +agent-shell-pstack-panels--cleanable-p (panel)
  "Return whether x cleans finished PANEL's workspace now."
  (when-let* (((eq (+agent-shell-pstack-panel-phase panel) 'finished))
              (manifest (+agent-shell-pstack-panel-arena panel))
              (directory (+agent-shell-arena-git-manifest-directory manifest)))
    (file-exists-p directory)))

(defun +agent-shell-pstack-panels--actions (section &optional all)
  "Return the keys that act on SECTION, each (KEY . WHAT), most useful first.
A panel's keys act anywhere in it, but only its own section and its
workspace's list them, unless ALL."
  (let* ((type (oref section type))
         (panel (and (or all (memq type '(pstack-panel pstack-workspace)))
                     (+agent-shell-pstack-panels--panel section))))
    (append
     (when-let* ((target (+agent-shell-pstack-panels--target section)))
       (list (cons "RET" (concat "visit " (car target)))))
     (when panel
       (append
        (unless (or (+agent-shell-pstack-panels--finished-p panel)
                    (eq type 'pstack-workspace))
          (list (cons "c" "cancel the panel")))
        (when (+agent-shell-pstack-panels--cleanable-p panel)
          (list (cons "x" "clean up its workspace")))))
     (when (eq type 'pstack-record)
       (list (cons "x" (if (memq (+agent-shell-arena-git-delegate-restore
                                  (+agent-shell-arena-git-manifest-delegate
                                   (+agent-shell-pstack-panels--record
                                    section)))
                                 '(restored unneeded))
                           "forget its record"
                         "switch the checkout back"))))
     (when (or (eq type 'pstack-request)
               (and (eq type 'pstack-panel) panel))
       (list (cons "l" "request log")))
     (when (eq type 'pstack-panels)
       (list (cons "g" "refresh") (cons "D" "clear delivered panels"))))))

(defconst +agent-shell-pstack-panels--general-keys
  '(("TAB" . "fold or unfold the section")
    ("n p" . "next or previous section")
    ("M-n M-p" . "next or previous section at the same level")
    ("^" . "parent section")
    ("g" . "refresh")
    ("D" . "clear delivered panels")
    ("l" . "request log of the lead here")
    ("q" . "quit"))
  "Keys that work anywhere in the panels view.")

;;;; Rendering

(defun +agent-shell-pstack-panels--hint (indent section)
  "Insert the dim line, indented by INDENT, of the keys acting on SECTION."
  (insert (make-string indent ?\s)
          (propertize
           (mapconcat (pcase-lambda (`(,key . ,what)) (format "%s %s" key what))
                      (append (+agent-shell-pstack-panels--actions section)
                              '(("?" . "all keys")))
                      "  ")
           'font-lock-face '+agent-shell-pstack-panels-hint)
          "\n"))

(defun +agent-shell-pstack-panels--line (indent target &rest parts)
  "Insert PARTS as one line indented by INDENT, visiting TARGET when non-nil."
  (let ((text (concat (make-string indent ?\s) (apply #'concat parts) "\n")))
    (insert (if target
                (propertize text '+agent-shell-pstack-panels-target target)
              text))))

(defun +agent-shell-pstack-panels--lines (indent text &optional face)
  "Insert each line of TEXT indented by INDENT, in FACE."
  (dolist (line (split-string (string-trim-right text) "\n"))
    (insert (make-string indent ?\s)
            (if face (propertize line 'font-lock-face face) line)
            "\n")))

(defun +agent-shell-pstack-panels--return (manifest)
  "Describe returning the checkout branch-mode MANIFEST's delegate took."
  (let* ((delegate (+agent-shell-arena-git-manifest-delegate manifest))
         (name (+agent-shell-arena-git-origin-name
                (+agent-shell-arena-git-delegate-origin delegate))))
    (if (memq (+agent-shell-arena-git-delegate-restore delegate)
              '(restored unneeded))
        ;; Its record goes once it is back, unless removing it failed.
        (format "back on %s%s" name
                (if (file-exists-p (+agent-shell-arena-git-manifest-directory
                                    manifest))
                    "  (x forgets its record)"
                  ""))
      (format "%s(x switches it back to %s)"
              (if-let* ((note (+agent-shell-arena-git-delegate-note delegate)))
                  (concat note "  ")
                "")
              name))))

(defun +agent-shell-pstack-panels--last-request (lead)
  "Return LEAD's latest turn if it had a block but started no panel."
  (when-let* ((log (gethash lead +agent-shell-pstack-panel--logs))
              (entry (seq-find (lambda (entry)
                                 (eq (plist-get entry :kind) 'turn-complete))
                               (ring-elements log)))
              ((plist-get entry :block))
              ((not (memq (plist-get entry :outcome) '(started interrupted)))))
    entry))

(defun +agent-shell-pstack-panels--member-note (panel member)
  "Return the note on MEMBER of PANEL's heading line."
  (let ((capture (+agent-shell-pstack-panel-member-capture member))
        (result (+agent-shell-pstack-panel-member-result member))
        (report (+agent-shell-pstack-panel-member-report member))
        (error (+agent-shell-pstack-panel-member-error member)))
    (cond ((and (eq 'delegate (+agent-shell-pstack-panel-member-kind member))
                (+agent-shell-pstack-panel-remote panel))
           (let ((remote (+agent-shell-pstack-panel-remote panel)))
             (format "transcript (%s)"
                     (if-let* ((polled (+agent-shell-arena-remote-polled remote)))
                         (format "updated %s ago"
                                 (+agent-shell-pstack-panels--age polled))
                       (format "%s" (+agent-shell-arena-remote-phase remote))))))
          (capture
           (string-join (delq nil (list (symbol-name capture)
                                        (plist-get result :commit)
                                        (plist-get result :error)))
                        " "))
          (report (abbreviate-file-name report))
          (error (truncate-string-to-width
                  (car (split-string error "\n")) 80 nil nil "..."))
          (t (+agent-shell-pstack-panels--name
              (+agent-shell-pstack-panel-member-shell member))))))

(defun +agent-shell-pstack-panels--insert-member (panel member)
  "Insert the section of MEMBER of PANEL."
  (let* ((role (+agent-shell-pstack-panel-member-role member))
         (shell (+agent-shell-pstack-panel-member-shell member))
         (report (+agent-shell-pstack-panel-member-report member))
         (result (+agent-shell-pstack-panel-member-result member))
         (error (+agent-shell-pstack-panel-member-error member))
         (state (+agent-shell-pstack-panel-member-state member))
         (manifest (+agent-shell-pstack-panel-arena panel)))
    (magit-insert-section section (pstack-member role)
      (magit-insert-heading
        (format "  %-16s %-12s " role
                (+agent-shell-pstack-panel-member-agent member))
        (+agent-shell-pstack-panels--member-badge member)
        (+agent-shell-pstack-panels--member-note panel member))
      (+agent-shell-pstack-panels--hint 4 section)
      (when (and manifest
                 (eq 'delegate (+agent-shell-pstack-panel-member-kind member)))
        (let ((commits (+agent-shell-arena-git-delegate-commits
                        (+agent-shell-arena-git-manifest-delegate manifest))))
          (+agent-shell-pstack-panels--line
           4 nil "Branch: " (+agent-shell-arena-git-delegate-branch manifest)
           (if commits
               (format ", %d commit%s" (length commits)
                       (if (cdr commits) "s" ""))
             ""))))
      (when-let* ((checkout (plist-get result :checkout)))
        (+agent-shell-pstack-panels--line
         4 checkout "Checkout: " (abbreviate-file-name checkout)))
      ;; A heading noting nothing else names the report already.
      (when (and report (or (+agent-shell-pstack-panel-member-capture member)
                            (+agent-shell-pstack-panel-remote panel)))
        (+agent-shell-pstack-panels--line
         4 report "Report: " (abbreviate-file-name report)))
      ;; A heading noting nothing else names the shell already.
      (when (and (buffer-live-p shell)
                 (or report error (plist-get result :checkout)))
        (+agent-shell-pstack-panels--line 4 shell "Shell: " (buffer-name shell)))
      (when (and (eq state 'running)
                 (plist-get (+agent-shell-pstack-panel-member-background member)
                            :asked))
        (+agent-shell-pstack-panels--line
         4 nil "Asked to wait for the commands it left running"))
      (when-let* ((survivors (plist-get result :survivors)))
        (+agent-shell-pstack-panels--line
         4 nil (format "%d process%s outlived its stop: %s"
                       (length survivors) (if (cdr survivors) "es" "")
                       (mapconcat #'number-to-string survivors " "))))
      (when error
        (+agent-shell-pstack-panels--lines
         4 error (unless (eq state 'done) '+agent-shell-pstack-panels-error))))))

(defun +agent-shell-pstack-panels--insert-workspace (panel)
  "Insert the section of PANEL's workspace, arena or checkout, if it has one."
  (when-let* ((manifest (+agent-shell-pstack-panel-arena panel)))
    (let ((directory (+agent-shell-arena-git-manifest-directory manifest))
          (finished (eq (+agent-shell-pstack-panel-phase panel) 'finished))
          (remote (+agent-shell-pstack-panel-remote panel)))
      (magit-insert-section section (pstack-workspace)
        (magit-insert-heading
          (pcase (cond ((and directory (not (file-exists-p directory))) 'gone)
                       ((and remote (+agent-shell-pstack-panel--delegate-p panel))
                        'remote)
                       (t (+agent-shell-pstack-panel--isolation panel)))
            ('gone
             (concat (if (+agent-shell-pstack-panel--isolation panel)
                         "  Workspace: " "  Arena: ")
                     (abbreviate-file-name directory) " (gone)"))
            ('remote
             (concat "  Workspace on host: "
                     (if-let* ((arena (+agent-shell-arena-remote-arena remote)))
                         (format ".../arenas/%s/%s" arena
                                 (+agent-shell-pstack-panel-member-role
                                  (car (+agent-shell-pstack-panel-members
                                        panel))))
                       "not made yet")))
            ('branch
             (concat "  Isolation: "
                     (if finished
                         (+agent-shell-pstack-panels--return manifest)
                       "your checkout, on the delegate's branch; do not edit \
or commit there until this finishes")))
            (isolation
             (concat (if isolation "  Workspace: " "  Arena: ")
                     (if directory (abbreviate-file-name directory)
                       "not made yet")))))
        (+agent-shell-pstack-panels--hint 4 section)
        (when remote
          (+agent-shell-pstack-panels--line
           4 nil "Host: " (+agent-shell-arena-remote-describe remote t)))))))

(defun +agent-shell-pstack-panels--insert-followup (panel)
  "Insert the folded section of PANEL's follow-up, once it has one."
  (when-let* ((followup (+agent-shell-pstack-panel-followup panel)))
    (magit-insert-section section (pstack-followup nil t)
      (magit-insert-heading
        "  Follow-up: "
        (if-let* ((at (+agent-shell-pstack-panel-delivered-at panel)))
            (format "delivered %s ago%s"
                    (+agent-shell-pstack-panels--age at)
                    (if (+agent-shell-pstack-panel-unread-p panel)
                        (propertize ", unread" 'font-lock-face
                                    '+agent-shell-pstack-panels-input)
                      ""))
          (if (+agent-shell-pstack-panel-delivered panel) "delivered"
            "waiting for the lead")))
      (+agent-shell-pstack-panels--hint 4 section)
      (let ((lines (split-string (string-trim-right followup) "\n")))
        (+agent-shell-pstack-panels--lines
         4 (string-join (seq-take lines 8) "\n"))
        (when (nthcdr 8 lines)
          (+agent-shell-pstack-panels--line
           4 nil (format "... %d more lines" (- (length lines) 8))))))))

(defun +agent-shell-pstack-panels--insert-panel (panel)
  "Insert the section of PANEL."
  (magit-insert-section section (pstack-panel (+agent-shell-pstack-panel-id
                                               panel))
    (magit-insert-heading
      (+agent-shell-pstack-panels--badge panel)
      (format "%s  " (or (+agent-shell-pstack-panel-shape panel) "-"))
      (propertize (+agent-shell-pstack-panel-id panel) 'font-lock-face 'bold)
      "  " (+agent-shell-pstack-panels--name
            (+agent-shell-pstack-panel-lead panel))
      "  " (+agent-shell-pstack-panels--age
            (+agent-shell-pstack-panel-created panel))
      (when (buffer-live-p (+agent-shell-pstack-panel-lead panel))
        (format " · %s"
                (+agent-shell-pstack-panel-display-status
                 (+agent-shell-pstack-panel-lead panel)))))
    (+agent-shell-pstack-panels--hint 2 section)
    (when-let* ((rejection (+agent-shell-pstack-panel-rejection panel)))
      (+agent-shell-pstack-panels--lines 2 rejection
                                         '+agent-shell-pstack-panels-error))
    (when-let* ((reason (+agent-shell-pstack-panel-cancelled panel)))
      (+agent-shell-pstack-panels--line 2 nil "Cancelled: " reason))
    (dolist (member (+agent-shell-pstack-panel-members panel))
      (+agent-shell-pstack-panels--insert-member panel member))
    (+agent-shell-pstack-panels--insert-workspace panel)
    (+agent-shell-pstack-panels--insert-followup panel)
    (insert "\n")))

(defun +agent-shell-pstack-panels--insert-records ()
  "Insert the section of branch-mode delegate records no panel holds."
  (when-let* ((manifests (+agent-shell-pstack-panel--interrupted)))
    (magit-insert-section (pstack-records)
      (magit-insert-heading "Records on disk with no live panel")
      (dolist (manifest manifests)
        (let ((toplevel (+agent-shell-arena-git-manifest-toplevel manifest)))
          (magit-insert-section section
            (pstack-record (+agent-shell-arena-git-manifest-id manifest))
            (magit-insert-heading
              "  "
              (propertize (+agent-shell-arena-git-manifest-id manifest)
                          'font-lock-face 'bold)
              (if (+agent-shell-arena-git-delegate-restore
                   (+agent-shell-arena-git-manifest-delegate manifest))
                  "  delegate  not returned"
                "  delegate  interrupted"))
            (+agent-shell-pstack-panels--hint 4 section)
            (+agent-shell-pstack-panels--line
             4 nil "Isolation: " (abbreviate-file-name toplevel) " is on "
             (+agent-shell-arena-git-delegate-branch manifest)
             "  " (+agent-shell-pstack-panels--return manifest))))))))

(defun +agent-shell-pstack-panels--insert-requests ()
  "Insert a section per lead whose latest block started no panel."
  (maphash
   (lambda (lead _log)
     (when-let* ((entry (+agent-shell-pstack-panels--last-request lead)))
       (magit-insert-section section (pstack-request lead)
         (magit-insert-heading
           (+agent-shell-pstack-panels--name lead) "  Last request: "
           (propertize (+agent-shell-pstack-panel--log-outcome-text entry)
                       'font-lock-face '+agent-shell-pstack-panels-error))
         (+agent-shell-pstack-panels--hint 2 section)
         (insert "\n"))))
   +agent-shell-pstack-panel--logs))

;;;; Stable point

(defun +agent-shell-pstack-panels--position (position)
  "Return (SECTION LINE CHAR) locating POSITION by its section, or nil."
  (when magit-root-section
    (save-excursion
      (goto-char position)
      (when-let* ((section (magit-current-section)))
        (cons section (magit-section-get-relative-position section))))))

(defun +agent-shell-pstack-panels--go (position)
  "Move to where POSITION was, or near it, and return point."
  (pcase-let ((`(,section ,line ,char) position))
    (unless (and section (magit-section-goto-successor section line char))
      (goto-char (point-min))))
  (point))

(defun +agent-shell-pstack-panels--positions ()
  "Return where point and each window on this buffer are, by section."
  (cons (+agent-shell-pstack-panels--position (point))
        (mapcar (lambda (window)
                  (list window
                        (+agent-shell-pstack-panels--position
                         (window-point window))
                        (+agent-shell-pstack-panels--position
                         (window-start window))))
                (get-buffer-window-list nil nil t))))

(defun +agent-shell-pstack-panels--restore (positions)
  "Put point and each window back where POSITIONS say they were."
  (pcase-let ((`(,here . ,windows) positions))
    (pcase-dolist (`(,window ,point ,start) windows)
      (when (window-live-p window)
        (let ((point (save-excursion (+agent-shell-pstack-panels--go point)))
              (start (save-excursion (+agent-shell-pstack-panels--go start)
                                     (line-beginning-position))))
          (set-window-start window start t)
          (set-window-point window point))))
    (+agent-shell-pstack-panels--go here)))

(defun +agent-shell-pstack-panels--render ()
  "Redraw the current panels buffer, keeping point's section and folds."
  (let ((inhibit-read-only t)
        (positions (+agent-shell-pstack-panels--positions))
        (panels (+agent-shell-pstack-panels--sorted)))
    (erase-buffer)
    (magit-insert-section section (pstack-panels)
      (insert (+agent-shell-pstack-panels--summary panels))
      (+agent-shell-pstack-panels--hint 0 section)
      (insert "\n")
      (unless panels
        (insert "No panels yet.  A pstack lead requests one by ending its \
message with a pstack-panel block.\n\n"))
      (+agent-shell-pstack-panels--insert-requests)
      (mapc #'+agent-shell-pstack-panels--insert-panel panels)
      (+agent-shell-pstack-panels--insert-records))
    (+agent-shell-pstack-panels--restore positions)))

;;;; Commands

;;;###autoload
(defun +agent-shell-pstack-panels ()
  "Show pstack panels, their members and reports."
  (interactive)
  (let ((buffer (get-buffer-create +agent-shell-pstack-panels--buffer)))
    (with-current-buffer buffer
      (unless (derived-mode-p '+agent-shell-pstack-panels-mode)
        (+agent-shell-pstack-panels-mode))
      (+agent-shell-pstack-panels--render))
    (pop-to-buffer buffer)))

(defun +agent-shell-pstack-panels--followup-buffer (panel)
  "Return a read-only buffer showing PANEL's follow-up."
  (let ((buffer (get-buffer-create
                 (format "*pstack follow-up %s*"
                         (+agent-shell-pstack-panel-id panel)))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (+agent-shell-pstack-panel-followup panel)))
      (special-mode)
      (goto-char (point-min)))
    buffer))

(defun +agent-shell-pstack-panels--visit-target (target)
  "Visit TARGET: a file or folder, a buffer, or a tagged panel or record."
  (pcase target
    ((pred stringp) (find-file-other-window target))
    (`(pstack-transcript . ,remote)
     (pop-to-buffer (+agent-shell-arena-remote-transcript-buffer remote)))
    (`(pstack-lead . ,panel)
     (setf (+agent-shell-pstack-panel-seen panel) t)
     (pop-to-buffer (+agent-shell-pstack-panel-lead panel)))
    (`(pstack-followup . ,panel)
     (setf (+agent-shell-pstack-panel-seen panel)
           (+agent-shell-pstack-panel-delivered panel))
     (pop-to-buffer (+agent-shell-pstack-panels--followup-buffer panel)))
    ((pred buffer-live-p) (pop-to-buffer target))
    (_ (user-error "Nothing to visit here"))))

(defun +agent-shell-pstack-panels-visit ()
  "Visit what the line or section at point is about.
A line naming a report, checkout or shell visits that.  Otherwise a
panel visits its lead, a member its live shell, transcript or report, a
workspace its folder, a follow-up its full text, and a record the
checkout it left."
  (interactive)
  (let* ((section (magit-current-section))
         (target (or (get-text-property (point)
                                        '+agent-shell-pstack-panels-target)
                     (cdr (and section
                               (+agent-shell-pstack-panels--target section))))))
    (+agent-shell-pstack-panels--visit-target target)
    (+agent-shell-pstack-panels--refresh)))

(defun +agent-shell-pstack-panels-keys ()
  "Show every key that acts on the section at point, and the view's others."
  (interactive)
  (let ((actions (+agent-shell-pstack-panels--actions (magit-current-section)
                                                      t)))
    (with-help-window "*pstack panels keys*"
      (with-current-buffer standard-output
        (insert "Here:\n")
        (if actions
            (pcase-dolist (`(,key . ,what) actions)
              (insert (format "  %-8s %s\n" key what)))
          (insert "  nothing acts on this section alone\n"))
        (insert "\nAnywhere:\n")
        (pcase-dolist (`(,key . ,what) +agent-shell-pstack-panels--general-keys)
          (insert (format "  %-8s %s\n" key what)))))))

(defun +agent-shell-pstack-panels-cancel ()
  "Cancel the unfinished panel at point, after confirmation.
An arena captures what its candidates made and skips its judge."
  (interactive)
  (let ((panel (+agent-shell-pstack-panels--panel)))
    (cond
     ((null panel) (user-error "No panel here"))
     ((+agent-shell-pstack-panel-followup panel)
      (user-error "Panel %s has finished" (+agent-shell-pstack-panel-id panel)))
     ((yes-or-no-p (format "Cancel panel %s? "
                           (+agent-shell-pstack-panel-id panel)))
      (+agent-shell-pstack-panel-cancel panel "You cancelled the panel.")))))

(defun +agent-shell-pstack-panels-clean ()
  "Clean up the finished arena or delegate at point, or the record there.
Ask for confirmation first."
  (interactive)
  (let ((panel (+agent-shell-pstack-panels--panel))
        (manifest (+agent-shell-pstack-panels--record)))
    (cond
     (manifest (+agent-shell-pstack-arena--clean manifest))
     ((not (and panel (+agent-shell-pstack-panel-arena panel)))
      (user-error "No arena here"))
     ((not (eq (+agent-shell-pstack-panel-phase panel) 'finished))
      (user-error "Arena %s is still %s" (+agent-shell-pstack-panel-id panel)
                  (+agent-shell-pstack-panel-phase panel)))
     ((not (+agent-shell-pstack-panels--cleanable-p panel))
      (user-error "Panel %s left nothing to clean up"
                  (+agent-shell-pstack-panel-id panel)))
     (t (+agent-shell-pstack-arena--clean
         (+agent-shell-pstack-panel-arena panel))))))

(defun +agent-shell-pstack-panels--holding (manifest)
  "Describe briefly what MANIFEST's arena still holds."
  (let ((delegate (+agent-shell-arena-git-manifest-delegate manifest))
        (toplevel (abbreviate-file-name
                   (+agent-shell-arena-git-manifest-toplevel manifest))))
    (cond
     ((+agent-shell-pstack-arena--remote-p manifest)
      (format "its job and worktree on its host, and branch %s"
              (+agent-shell-arena-git-delegate-branch manifest)))
     ((null delegate)
      (format "its worktrees, checkouts and result refs in %s" toplevel))
     ((eq 'branch (+agent-shell-arena-git-delegate-isolation delegate))
      (if (memq (+agent-shell-arena-git-delegate-restore delegate)
                '(restored unneeded))
          "its record"
        (format "%s, on %s" toplevel
                (+agent-shell-arena-git-delegate-branch manifest))))
     (t (format "its worktree from %s, and branch %s" toplevel
                (+agent-shell-arena-git-delegate-branch manifest))))))

(defun +agent-shell-pstack-panels--forget (panel)
  "Forget PANEL, leaving its reports on disk."
  (remhash (+agent-shell-pstack-panel-id panel) +agent-shell-pstack-panel--panels))

(defun +agent-shell-pstack-panels-clear-delivered ()
  "Forget every finished panel whose follow-up was delivered.
Those holding nothing to clean go at once.  Those that still hold a
workspace, checkout or branch record are listed in one question, and
are cleaned up as x would and then forgotten, or else kept."
  (interactive)
  (let* ((delivered (seq-filter
                     (lambda (panel)
                       (and (+agent-shell-pstack-panel-delivered panel)
                            (+agent-shell-pstack-panels--finished-p panel)))
                     (+agent-shell-pstack-panel-list)))
         (held (seq-filter #'+agent-shell-pstack-panels--cleanable-p delivered))
         (free (seq-difference delivered held #'eq))
         (ids (lambda (panels)
                (mapconcat #'+agent-shell-pstack-panel-id panels ", "))))
    (unless delivered
      (user-error "No delivered panels to clear"))
    (mapc #'+agent-shell-pstack-panels--forget free)
    (+agent-shell-pstack-panels--refresh)
    (cond
     ((null held)
      (message "Forgot %d delivered panel%s" (length free)
               (if (cdr free) "s" "")))
     ((yes-or-no-p
       (format "%s%d delivered panel%s still hold%s work:\n%s\nClean %s up \
as x would, then forget %s? "
               (if free (format "Forgot %d delivered panel%s.  "
                                (length free) (if (cdr free) "s" ""))
                 "")
               (length held) (if (cdr held) "s" "") (if (cdr held) "" "s")
               (mapconcat (lambda (panel)
                            (format "  %s: %s"
                                    (+agent-shell-pstack-panel-id panel)
                                    (+agent-shell-pstack-panels--holding
                                     (+agent-shell-pstack-panel-arena panel))))
                          held "\n")
               (if (cdr held) "them" "it") (if (cdr held) "them" "it")))
      (dolist (panel held)
        (let ((manifest (+agent-shell-pstack-panel-arena panel)))
          (+agent-shell-pstack-arena--cleanup
           manifest
           (lambda (error)
             (if (not error)
                 (+agent-shell-pstack-panels--forget panel)
               ;; Keep it, holding its arena again, so x can retry.
               (unless (+agent-shell-pstack-panel-arena panel)
                 (setf (+agent-shell-pstack-panel-arena panel) manifest)))
             (+agent-shell-pstack-panels--refresh))))))
     (t (message "Kept %s, which still hold%s work; x on each cleans it up"
                 (funcall ids held) (if (cdr held) "" "s"))))))

(provide '+agent-shell-pstack-panels)
;;; +agent-shell-pstack-panels.el ends here
