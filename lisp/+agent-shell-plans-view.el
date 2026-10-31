;;; +agent-shell-plans-view.el --- Agent plans sections -*- lexical-binding: t; -*-

;;; Commentary:
;; One folded card per live shell with a plan, ordered by activity, with
;; a bounded entry preview and an expandable remainder.  Follow the panels
;; view's layout and retain section identity, folds and window positions.

;;; Code:

(require '+agent-shell-plans)
(require 'magit-section)
(require 'subr-x)

(defconst +agent-shell-plans--preview-entries 5
  "Number of entries before the folded remainder of a plan.")

(defface +agent-shell-plans-working '((t :inherit font-lock-keyword-face))
  "Face for work in progress." :group '+agent-shell-plans)
(defface +agent-shell-plans-done '((t :inherit success))
  "Face for completed work." :group '+agent-shell-plans)
(defface +agent-shell-plans-pending '((t :inherit font-lock-doc-face))
  "Face for pending work." :group '+agent-shell-plans)
(defface +agent-shell-plans-dim '((t :inherit shadow))
  "Face for rules and supporting detail." :group '+agent-shell-plans)
(defface +agent-shell-plans-key '((t :inherit help-key-binding))
  "Face for keys in the Actions block." :group '+agent-shell-plans)

(defconst +agent-shell-plans--states
  '(("in_progress" "● WORKING" +agent-shell-plans-working)
    ("completed" "✓ DONE" +agent-shell-plans-done)
    ("pending" "◌ PENDING" +agent-shell-plans-pending)))

(defun +agent-shell-plans--state (entries)
  "Return the aggregate state of ENTRIES."
  (cond ((seq-some (lambda (entry)
                     (equal (map-elt entry 'status) "in_progress")) entries)
         "in_progress")
        ((seq-every-p (lambda (entry)
                        (equal (map-elt entry 'status) "completed")) entries)
         "completed")
        (t "pending")))

(defun +agent-shell-plans--badge (status)
  "Return STATUS's glyph and word in its theme face."
  (pcase-let ((`(,label ,face)
               (or (cdr (assoc status +agent-shell-plans--states))
                   (cdr (assoc "pending" +agent-shell-plans--states)))))
    (propertize label 'font-lock-face face)))

(defun +agent-shell-plans--text (entry)
  "Return ENTRY's content or Codex step as one line."
  (string-join (split-string (or (map-elt entry 'content)
                               (map-elt entry 'step) "") "[\n\r\t ]+" t)
               " "))

(defun +agent-shell-plans--sorted ()
  "Return shells with plans, working first, then most recently updated."
  (let (shells)
    (maphash (lambda (shell plan)
               (when (and (buffer-live-p shell)
                          (with-current-buffer shell
                            (derived-mode-p 'agent-shell-mode))
                          (plist-get plan :entries))
                 (push shell shells)))
             +agent-shell-plans--plans)
    (sort shells
          (lambda (a b)
            (let* ((a (gethash a +agent-shell-plans--plans))
                   (b (gethash b +agent-shell-plans--plans))
                   (rank (lambda (plan)
                           (pcase (+agent-shell-plans--state
                                   (plist-get plan :entries))
                             ("in_progress" 0) ("pending" 1) (_ 2))))
                   (ra (funcall rank a)) (rb (funcall rank b)))
              (if (= ra rb) (> (plist-get a :updated) (plist-get b :updated))
                (< ra rb)))))))

(defvar-keymap +agent-shell-plans-mode-map
  :parent magit-section-mode-map
  "RET" #'+agent-shell-plans-visit
  "n" #'magit-section-forward
  "p" #'magit-section-backward
  "C-n" #'magit-section-forward
  "C-p" #'magit-section-backward
  "TAB" #'magit-section-toggle
  "g" #'revert-buffer
  "q" #'quit-window
  "?" #'+agent-shell-plans-keys)

(define-derived-mode +agent-shell-plans-mode magit-section-mode "Agent plans"
  "Browse live agent plans, folding their entries with TAB."
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function
              (lambda (&rest _) (+agent-shell-plans--render)))
  (add-hook 'window-buffer-change-functions #'+agent-shell-plans--shown nil t))

(defun +agent-shell-plans--shown (window)
  "Redraw the plans view WINDOW shows."
  (with-current-buffer (window-buffer window)
    (when (derived-mode-p '+agent-shell-plans-mode)
      (+agent-shell-plans--render))))

(defconst +agent-shell-plans--actions
  '(("Act" ("RET" "Visit" "the shell at point" +agent-shell-plans-visit))
    ("Navigate"
     ("n / p" "Next / previous" "item" magit-section-forward)
     ("TAB" "Fold / unfold" "the item at point" magit-section-toggle))
    ("Manage"
     ("g" "Refresh" "the whole view" revert-buffer))))

(defun +agent-shell-plans--insert-actions ()
  "Insert the view's single Actions block."
  (magit-insert-section (agent-plan-actions)
    (magit-insert-heading "Actions")
    (pcase-dolist (`(,group . ,rows) +agent-shell-plans--actions)
      (insert group "\n")
      (pcase-dolist (`(,key ,label ,what ,command) rows)
        (insert (propertize
                 (concat "  "
                         (propertize (format "%-6s" key)
                                     'font-lock-face '+agent-shell-plans-key)
                         (format " %-17s " label)
                         (propertize what 'font-lock-face
                                     '+agent-shell-plans-dim) "\n")
                 '+agent-shell-plans-action command))))
    (insert (propertize "?" 'font-lock-face '+agent-shell-plans-key)
            (propertize " keys here · " 'font-lock-face '+agent-shell-plans-dim)
            (propertize "q" 'font-lock-face '+agent-shell-plans-key)
            (propertize " quit" 'font-lock-face '+agent-shell-plans-dim) "\n")))

(defun +agent-shell-plans--summary (shells)
  "Return the line counting SHELLS by state, then the shells with no plan."
  (let ((without (seq-count
                  (lambda (buffer)
                    (and (with-current-buffer buffer
                           (derived-mode-p 'agent-shell-mode))
                         (not (memq buffer shells))))
                  (buffer-list))))
    (concat
     (propertize "agent plans" 'font-lock-face 'magit-section-heading)
     (propertize " · " 'font-lock-face '+agent-shell-plans-dim)
     (string-join
      (append
       (seq-keep
        (pcase-lambda (`(,status ,label ,face))
          (let ((count (seq-count
                        (lambda (shell)
                          (equal status (+agent-shell-plans--state
                                         (plist-get (gethash
                                                     shell
                                                     +agent-shell-plans--plans)
                                                    :entries))))
                        shells)))
            (when (> count 0)
              (propertize (format "%d %s" count
                                  (downcase (substring label 2)))
                          'font-lock-face face))))
        +agent-shell-plans--states)
       (when (> without 0)
         (list (propertize (format "%d without a plan" without)
                           'font-lock-face '+agent-shell-plans-dim))))
      ", "))))

(defun +agent-shell-plans--insert-entry (entry)
  "Insert ENTRY on one indented line."
  (insert "    " (substring (+agent-shell-plans--badge (map-elt entry 'status))
                           0 1)
          " " (+agent-shell-plans--text entry) "\n"))

(defun +agent-shell-plans--insert-card (shell)
  "Insert SHELL's plan as a folded card after a dim rule."
  (let* ((entries (plist-get (gethash shell +agent-shell-plans--plans) :entries))
         (done (seq-count (lambda (entry)
                            (equal (map-elt entry 'status) "completed")) entries))
         (current (seq-find (lambda (entry)
                              (equal (map-elt entry 'status) "in_progress"))
                            entries)))
    (insert (propertize (make-string 60 ?─)
                        'font-lock-face '+agent-shell-plans-dim) "\n")
    (magit-insert-section (agent-plan shell t)
      (magit-insert-heading
        (+agent-shell-plans--badge (+agent-shell-plans--state entries))
        "  " (buffer-name shell) (format " · %d/%d" done (length entries))
        (if current
            (concat " · " (truncate-string-to-width
                           (+agent-shell-plans--text current) 70 nil nil "…"))
          ""))
      (mapc #'+agent-shell-plans--insert-entry
            (seq-take entries +agent-shell-plans--preview-entries))
      (when-let* ((rest (nthcdr +agent-shell-plans--preview-entries entries)))
        (magit-insert-section (agent-plan-more shell t)
          (magit-insert-heading
            (propertize (format "    … %d more" (length rest))
                        'font-lock-face '+agent-shell-plans-dim))
          (mapc #'+agent-shell-plans--insert-entry rest))))))

(defun +agent-shell-plans--position (position)
  "Return POSITION as a section and its relative line and column."
  (when magit-root-section
    (save-excursion
      (goto-char position)
      (when-let* ((section (magit-current-section)))
        (cons section (magit-section-get-relative-position section))))))

(defun +agent-shell-plans--go (position)
  "Restore POSITION in the newly rendered sections."
  (pcase-let ((`(,section ,line ,column) position))
    (unless (and section (magit-section-goto-successor section line column))
      (goto-char (point-min))))
  (point))

(defun +agent-shell-plans--render ()
  "Render plans, keeping each window's place and Magit's section folds."
  (let ((inhibit-read-only t)
        (here (+agent-shell-plans--position (point)))
        (windows (mapcar
                  (lambda (window)
                    (list window
                          (+agent-shell-plans--position (window-point window))
                          (+agent-shell-plans--position (window-start window))))
                  (get-buffer-window-list nil nil t)))
        (shells (+agent-shell-plans--sorted)))
    (unless (derived-mode-p '+agent-shell-plans-mode)
      (error "Not in the plans view: %s" (buffer-name)))
    (erase-buffer)
    (magit-insert-section (agent-plans)
      (insert (+agent-shell-plans--summary shells) "\n\n")
      (+agent-shell-plans--insert-actions)
      (insert "\n")
      (unless shells (insert "No agent plans yet.\n"))
      (mapc #'+agent-shell-plans--insert-card shells))
    (pcase-dolist (`(,window ,point ,start) windows)
      (when (window-live-p window)
        (let ((point (save-excursion (+agent-shell-plans--go point)))
              (start (save-excursion (+agent-shell-plans--go start)
                                     (line-beginning-position))))
          (set-window-start window start t)
          (set-window-point window point))))
    (+agent-shell-plans--go here)))

(defun +agent-shell-plans--open ()
  "Open the plans view, preserving its previous place."
  (let ((buffer (get-buffer-create +agent-shell-plans--buffer)))
    (with-current-buffer buffer
      (unless (derived-mode-p '+agent-shell-plans-mode)
        (+agent-shell-plans-mode))
      (+agent-shell-plans--render))
    (pop-to-buffer buffer)))

(defun +agent-shell-plans--shell ()
  "Return the shell of the card at point, including its folded entries."
  (let ((section (magit-current-section)))
    (while (and section (not (eq (oref section type) 'agent-plan)))
      (setq section (oref section parent)))
    (and section (oref section value))))

(defun +agent-shell-plans-visit ()
  "Visit the shell at point, or run the Actions row at point."
  (interactive)
  (if-let* ((action (get-text-property (point) '+agent-shell-plans-action))
            ((not (eq action #'+agent-shell-plans-visit))))
      (call-interactively action)
    (let ((shell (or (+agent-shell-plans--shell)
                     (and action
                          (car (+agent-shell-plans--sorted))))))
      (unless (buffer-live-p shell) (user-error "No live shell here"))
      (pop-to-buffer shell))))

(defun +agent-shell-plans-keys ()
  "List keys acting on the item at point and across the view."
  (interactive)
  (let ((shell (+agent-shell-plans--shell))
        (action (get-text-property (point) '+agent-shell-plans-action)))
    (with-help-window "*agent plans keys*"
      (princ (cond (shell "RET     Visit this shell\n")
                   (action "RET     Run this action\n") (t "")))
      (princ "TAB     Fold or unfold this item\nn / p   Next or previous item\n")
      (princ "C-n C-p Next or previous item\nM-n M-p Next or previous sibling\n")
      (princ "^       Parent section\ng       Refresh the whole view\n")
      (princ "q       Quit the view\n?       Keys here\n"))))

(provide '+agent-shell-plans-view)
;;; +agent-shell-plans-view.el ends here
