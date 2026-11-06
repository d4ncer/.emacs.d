;;; +agent-shell-plans-view.el --- Agent plans sections -*- lexical-binding: t; -*-

;;; Commentary:
;; One row per plan document in `+agent-shell-plans-directories', newest
;; commit first, then one folded card per live shell with a plan, ordered
;; by activity, with a bounded entry preview and an expandable remainder.
;; Follow the panels view's layout and retain section identity, folds and
;; window positions.

;;; Code:

(require '+agent-shell-plans)
(require 'magit-section)
(require 'subr-x)
(require 'vc-git)

(defconst +agent-shell-plans--preview-entries 5
  "Number of entries before the folded remainder of a plan.")

(defface +agent-shell-plans-working '((t :inherit font-lock-keyword-face))
  "Face for work in progress." :group '+agent-shell-plans)
(defface +agent-shell-plans-done '((t :inherit success))
  "Face for completed work." :group '+agent-shell-plans)
(defface +agent-shell-plans-pending '((t :inherit font-lock-doc-face))
  "Face for pending work." :group '+agent-shell-plans)
(defface +agent-shell-plans-needs-you '((t :inherit warning))
  "Face for a proposed plan that waits for your answer."
  :group '+agent-shell-plans)
(defface +agent-shell-plans-dim '((t :inherit shadow))
  "Face for rules and supporting detail." :group '+agent-shell-plans)
(defface +agent-shell-plans-key '((t :inherit help-key-binding))
  "Face for keys in the Actions block." :group '+agent-shell-plans)

(defface +agent-shell-plans-copies '((t :inherit warning))
  "Face for a topic that also holds numbered plan copies."
  :group '+agent-shell-plans)

(defconst +agent-shell-plans--copy-regexp "\\`plan\\.v[0-9]+\\.md\\'"
  "Names of numbered plan copies, which are not listed as documents.")

(defvar +agent-shell-plans--document-cache (make-hash-table :test #'equal)
  "Plan document files mapped to the times they were read at and details.")

(defconst +agent-shell-plans--states
  '(("needs_you" "◆ NEEDS YOU" +agent-shell-plans-needs-you)
    ("in_progress" "● WORKING" +agent-shell-plans-working)
    ("completed" "✓ DONE" +agent-shell-plans-done)
    ("pending" "◌ PENDING" +agent-shell-plans-pending)
    ("approved" "✓ APPROVED" +agent-shell-plans-done)
    ("revised" "↺ REVISING" +agent-shell-plans-pending)
    ("cancelled" "✗ DECLINED" +agent-shell-plans-dim))
  "Card states, most urgent first, with their badge and face.")

(defun +agent-shell-plans--card-state (plan)
  "Return the state of PLAN's card.
A proposal waiting for an answer comes first, then the entries' state,
then how the proposal was answered."
  (let ((proposal (plist-get plan :proposal))
        (entries (plist-get plan :entries)))
    (cond ((and proposal (not (plist-get proposal :answer))) "needs_you")
          (entries (+agent-shell-plans--state entries))
          (t (symbol-name (plist-get proposal :answer))))))

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

(defun +agent-shell-plans--sorted ()
  "Return shells with plans, working first, then most recently updated."
  (let (shells)
    (maphash (lambda (shell plan)
               (when (and (buffer-live-p shell)
                          (with-current-buffer shell
                            (derived-mode-p 'agent-shell-mode))
                          (or (plist-get plan :entries)
                              (plist-get plan :proposal)))
                 (push shell shells)))
             +agent-shell-plans--plans)
    (sort shells
          (lambda (a b)
            (let* ((a (gethash a +agent-shell-plans--plans))
                   (b (gethash b +agent-shell-plans--plans))
                   (rank (lambda (plan)
                           (pcase (+agent-shell-plans--card-state plan)
                             ("needs_you" 0) ("in_progress" 1) ("pending" 2)
                             ("revised" 3) (_ 4))))
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
  "TAB" #'magit-section-cycle
  "g" #'revert-buffer
  "q" #'quit-window
  "?" #'+agent-shell-plans-keys)

(define-derived-mode +agent-shell-plans-mode magit-section-mode "Agent plans"
  "Browse plan documents and live agent plans; TAB cycles their folds."
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function
              (lambda (&rest _)
                (clrhash +agent-shell-plans--document-cache)
                (+agent-shell-plans--render)))
  (add-hook 'window-buffer-change-functions #'+agent-shell-plans--shown nil t))

(defun +agent-shell-plans--shown (window)
  "Redraw the plans view WINDOW shows."
  (with-current-buffer (window-buffer window)
    (when (derived-mode-p '+agent-shell-plans-mode)
      (+agent-shell-plans--render))))

(defconst +agent-shell-plans--actions
  '(("Act" ("RET" "Visit" "the document or shell at point"
            +agent-shell-plans-visit))
    ("Navigate"
     ("n / p" "Next / previous" "item" magit-section-forward)
     ("TAB" "Cycle" "folded, children, everything"
      magit-section-cycle))
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
                          (equal status (+agent-shell-plans--card-state
                                         (gethash shell
                                                  +agent-shell-plans--plans))))
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

(defun +agent-shell-plans--title (text)
  "Return the first non-blank line of markdown TEXT without its markup."
  (let ((line (seq-find (lambda (line) (not (string-blank-p line)))
                        (split-string text "\n"))))
    (string-trim (replace-regexp-in-string "\\`[#*>\t -]+\\|[*`_]+" ""
                                           (or line "")))))

(defun +agent-shell-plans--insert-proposal (shell proposal)
  "Insert SHELL's PROPOSAL folded under its first line."
  (magit-insert-section (agent-plan-proposal shell t)
    (magit-insert-heading
      "    " (propertize "Proposed plan" 'font-lock-face
                         '+agent-shell-plans-dim)
      "  " (truncate-string-to-width
            (+agent-shell-plans--title (plist-get proposal :text))
            70 nil nil "…"))
    (dolist (line (split-string (plist-get proposal :text) "\n"))
      (insert (if (string-blank-p line) "" (concat "      " line)) "\n"))))

(defun +agent-shell-plans--insert-card (shell)
  "Insert SHELL's plan as a folded card after a dim rule."
  (let* ((plan (gethash shell +agent-shell-plans--plans))
         (entries (plist-get plan :entries))
         (proposal (plist-get plan :proposal)))
    (insert (propertize (make-string 60 ?─)
                        'font-lock-face '+agent-shell-plans-dim) "\n")
    (magit-insert-section (agent-plan shell t)
      (magit-insert-heading
        (+agent-shell-plans--badge (+agent-shell-plans--card-state plan))
        "  " (buffer-name shell) " · "
        (or (+agent-shell-plans-progress shell) "proposed plan"))
      (when proposal (+agent-shell-plans--insert-proposal shell proposal))
      (mapc #'+agent-shell-plans--insert-entry
            (seq-take entries +agent-shell-plans--preview-entries))
      (when-let* ((rest (nthcdr +agent-shell-plans--preview-entries entries)))
        (magit-insert-section (agent-plan-more shell t)
          (magit-insert-heading
            (propertize (format "    … %d more" (length rest))
                        'font-lock-face '+agent-shell-plans-dim))
          (mapc #'+agent-shell-plans--insert-entry rest))))))

(defun +agent-shell-plans--document-files ()
  "Return the plan documents in `+agent-shell-plans-directories'.
Each is a plist: :file; :name, its topic and file name; and :copies,
the numbered plan copies its topic folder also holds."
  (let (documents)
    (dolist (directory +agent-shell-plans-directories)
      (when (file-directory-p directory)
        (dolist (topic (directory-files directory t "\\`[^.]"))
          (when (file-directory-p topic)
            (let* ((names (directory-files
                           topic nil "\\`[^.].*\\.\\(?:md\\|org\\)\\'"))
                   (copies (seq-filter
                            (lambda (name)
                              (string-match-p +agent-shell-plans--copy-regexp
                                              name))
                            names)))
              (dolist (name (seq-difference names copies))
                (let ((file (expand-file-name name topic)))
                  (when (file-regular-p file)
                    (push (list :file file
                                :name (concat (file-name-nondirectory topic)
                                              "/" name)
                                :copies copies)
                          documents)))))))))
    (nreverse documents)))

(defun +agent-shell-plans--document-title (file)
  "Return the title of FILE, whose text is in the current buffer.
That is its first Markdown heading, or its Org title or first headline,
else FILE's name."
  (goto-char (point-min))
  (let ((case-fold-search t))
    (or (seq-some
         (lambda (regexp)
           (and (re-search-forward regexp nil t)
                (let ((title (string-trim (match-string-no-properties 1))))
                  (and (not (string-empty-p title)) title))))
         (if (string-suffix-p ".org" file)
             '("^#\\+title:[ \t]*\\(.*\\)" "^\\*+[ \t]+\\(.*\\)")
           '("^#+[ \t]+\\(.*\\)")))
        (file-name-nondirectory file))))

(defun +agent-shell-plans--document-description ()
  "Return the current buffer's summary, or else its first prose paragraph.
A summary is a paragraph that starts \"Summary:\", or \"#+summary:\" in
Org.  The fallback skips headings, Org keywords, tables, code, lists and
metadata lines such as \"Status: draft\".  Return nil when there is none."
  (let ((case-fold-search t)
        (clean (lambda (text)
                 (string-trim
                  (replace-regexp-in-string
                   "[ \t\n]+" " "
                   (replace-regexp-in-string "\\*\\*" "" text))))))
    (goto-char (point-min))
    (if (re-search-forward "^\\(?:#\\+\\)?summary:[ \t]*" nil t)
        (let ((start (point)))
          (funcall clean
                   (buffer-substring-no-properties
                    start (if (re-search-forward "\n[ \t]*\n" nil t)
                              (match-beginning 0)
                            (point-max)))))
      (let (paragraph)
        (while (and (not paragraph) (not (eobp)))
          (skip-chars-forward " \t\n")
          (let ((start (point)))
            (if (re-search-forward "\n[ \t]*\n" nil 'move)
                (goto-char (match-beginning 0))
              (goto-char (point-max)))
            (let ((text (string-trim
                         (buffer-substring-no-properties start (point))))
                  (case-fold-search nil))
              (unless (or (string-empty-p text)
                          (string-match-p
                           (concat "\\`\\(?:#\\|\\*+ \\||\\|```\\|<\\|[-+] \\|"
                                   "[0-9]+\\. \\|:\\|\\(?:Status\\|Date\\|"
                                   "Author\\|Owner\\|Updated\\):\\)")
                           text))
                (setq paragraph (funcall clean text))))))
        paragraph))))

(defun +agent-shell-plans--checkboxes ()
  "Return the current buffer's checked and total task items as a plist."
  (goto-char (point-min))
  (let ((done 0) (total 0))
    (while (re-search-forward "^[ \t]*- \\[\\([ xX]\\)\\]" nil t)
      (cl-incf total)
      (unless (equal (match-string 1) " ") (cl-incf done)))
    (list :done done :total total)))

(defun +agent-shell-plans--head-log (root)
  "Return the HEAD reflog of the Git repository or worktree at ROOT."
  (let ((dotgit (expand-file-name ".git" root)))
    (if (file-directory-p dotgit)
        (expand-file-name "logs/HEAD" dotgit)
      (with-temp-buffer
        (ignore-errors (insert-file-contents dotgit))
        (when (looking-at "gitdir: *\\(.+\\)$")
          (expand-file-name "logs/HEAD"
                            (expand-file-name (match-string 1) root)))))))

(defun +agent-shell-plans--last-commit (file root)
  "Return FILE's last commit in the repository at ROOT as (HASH . TIME).
Return nil when FILE has no commit."
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory (expand-file-name root))))
      (when (eq 0 (process-file "git" nil t nil "log" "-1" "--format=%h %ct"
                                "--" (file-relative-name file)))
        (goto-char (point-min))
        (when (looking-at "\\([0-9a-f]+\\) \\([0-9]+\\)")
          (cons (match-string 1) (string-to-number (match-string 2))))))))

(defun +agent-shell-plans--document-details (file)
  "Return FILE's :title, :description, :done, :total, :commit, :modified.
Reuse them while FILE and its repository's HEAD reflog are unchanged."
  (let* ((root (vc-git-root file))
         (log (and root (+agent-shell-plans--head-log root)))
         (key (mapcar (lambda (path)
                        (and path (file-attribute-modification-time
                                   (file-attributes path))))
                      (list file log)))
         (cached (gethash file +agent-shell-plans--document-cache)))
    (if (and cached (equal key (car cached)))
        (cdr cached)
      (let ((details
             (with-temp-buffer
               (insert-file-contents file)
               (append (list :title (+agent-shell-plans--document-title file)
                             :description
                             (+agent-shell-plans--document-description)
                             :commit (and root (+agent-shell-plans--last-commit
                                                file root))
                             :modified (float-time (car key)))
                       (+agent-shell-plans--checkboxes)))))
        (puthash file (cons key details) +agent-shell-plans--document-cache)
        details))))

(defun +agent-shell-plans--documents ()
  "Return plan documents grouped into projects, newest project first.
A project is one topic folder: a plist of :topic, its folder; :name;
:repository, its repository's folder name; :copies; :time, its newest
document's commit or modification time; and :documents, its main
document (README, then design, plan, rationale) first, then newest
first."
  (let ((projects (make-hash-table :test #'equal)))
    (dolist (document (+agent-shell-plans--document-files))
      (let* ((file (plist-get document :file))
             (document (append document
                               (+agent-shell-plans--document-details file)))
             (topic (file-name-directory file)))
        (push document (gethash topic projects))))
    (let (result)
      (maphash
       (lambda (topic documents)
         (let* ((time (lambda (document)
                        (or (cdr (plist-get document :commit))
                            (plist-get document :modified))))
                (rank (lambda (document)
                        (let ((base (downcase (file-name-base
                                               (plist-get document :file)))))
                          (or (seq-position '("readme" "design" "plan" "rationale")
                                            base)
                              4))))
                (sorted (sort documents
                              (lambda (a b)
                                (let ((ra (funcall rank a))
                                      (rb (funcall rank b)))
                                  (if (= ra rb)
                                      (> (funcall time a) (funcall time b))
                                    (< ra rb))))))
                (root (vc-git-root topic)))
           (push (list :topic topic
                       :name (file-name-nondirectory
                              (directory-file-name topic))
                       :repository (and root (file-name-nondirectory
                                              (directory-file-name root)))
                       :copies (plist-get (car documents) :copies)
                       :time (seq-max (mapcar time documents))
                       :documents sorted)
                 result)))
       projects)
      (sort result :key (lambda (project) (plist-get project :time))
            :reverse t))))

(defun +agent-shell-plans--age (time)
  "Return how long ago TIME was, briefly."
  (let ((seconds (max 0 (round (- (float-time) time)))))
    (cond ((< seconds 60) (format "%ds ago" seconds))
          ((< seconds 3600) (format "%dm ago" (/ seconds 60)))
          ((< seconds 86400) (format "%dh ago" (/ seconds 3600)))
          (t (format "%dd ago" (/ seconds 86400))))))

(defun +agent-shell-plans--insert-document (document width)
  "Insert DOCUMENT as a folded one-line row no wider than WIDTH.
Its description and file are folded beneath."
  (let* ((done (plist-get document :done))
         (total (plist-get document :total))
         (commit (plist-get document :commit))
         (description (plist-get document :description))
         (dim '+agent-shell-plans-dim)
         (facts (concat
                 (when (> total 0)
                   (concat (propertize " · " 'font-lock-face dim)
                           (propertize (format "%d of %d done" done total)
                                       'font-lock-face
                                       (if (= done total)
                                           '+agent-shell-plans-done
                                         'default))))
                 (propertize
                  (concat " · " (if commit
                                    (+agent-shell-plans--age (cdr commit))
                                  "not committed"))
                  'font-lock-face dim))))
    (magit-insert-section (agent-plan-document (plist-get document :file) t)
      (magit-insert-heading
        "  " (truncate-string-to-width
              (plist-get document :title)
              (max 10 (- width 2 (string-width facts))) nil nil "…")
        facts)
      (when description
        (insert (replace-regexp-in-string
                 "^" "      " (string-fill description (- width 6)))
                "\n"))
      (insert "      " (propertize "File  " 'font-lock-face dim)
              (abbreviate-file-name (plist-get document :file)) "\n"))))

(defun +agent-shell-plans--project-name (name)
  "Return topic folder NAME as a project name, such as \"Retire playroom\"."
  (let ((words (replace-regexp-in-string "[-_]+" " " name)))
    (concat (upcase (substring words 0 1)) (substring words 1))))

(defun +agent-shell-plans--insert-documents (projects)
  "Insert the Documents section with one card per project in PROJECTS.
A card's heading names the project; its main document's description is a
folded excerpt; its documents follow, each folded."
  (when projects
    (let* ((window (get-buffer-window (current-buffer)))
           (width (min 100 (if window (window-body-width window) 80)))
           (labels (seq-uniq (mapcar (lambda (project)
                                       (plist-get project :repository))
                                     projects)))
           (dim '+agent-shell-plans-dim))
      (magit-insert-section (agent-plan-documents)
        (magit-insert-heading "Documents")
        (dolist (project projects)
          (let* ((documents (plist-get project :documents))
                 (copies (plist-get project :copies))
                 (description (plist-get (car documents) :description)))
            (insert (propertize (make-string (min 60 width) ?─)
                                'font-lock-face dim)
                    "\n")
            (magit-insert-section (agent-plan-project (plist-get project :topic))
              (magit-insert-heading
                (propertize (+agent-shell-plans--project-name
                             (plist-get project :name))
                            'font-lock-face 'magit-section-heading)
                (when copies
                  (concat "  " (propertize
                                (format "⚠ %d %s" (length copies)
                                        (if (cdr copies) "COPIES" "COPY"))
                                'font-lock-face '+agent-shell-plans-copies)))
                (propertize
                 (concat (format " · %d %s" (length documents)
                                 (if (cdr documents) "docs" "doc"))
                         (when (cdr labels)
                           (concat " · " (plist-get project :repository)))
                         " · " (+agent-shell-plans--age
                                (plist-get project :time)))
                 'font-lock-face dim))
              (when (or description copies)
                (magit-insert-section (agent-plan-about
                                       (plist-get project :topic) t)
                  (magit-insert-heading
                    "    " (propertize
                            (truncate-string-to-width
                             (or description "About") (- width 4) nil nil "…")
                            'font-lock-face dim))
                  (when description
                    (insert (replace-regexp-in-string
                             "^" "      " (string-fill description (- width 6)))
                            "\n"))
                  (when copies
                    (insert "      " (propertize "Copies  " 'font-lock-face dim)
                            (string-join copies ", ") "\n"))))
              (dolist (document documents)
                (+agent-shell-plans--insert-document document width)))))
        (insert "\n")))))

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
    (let ((documents (+agent-shell-plans--documents)))
      (erase-buffer)
      (magit-insert-section (agent-plans)
        (insert (+agent-shell-plans--summary shells) "\n\n")
        (+agent-shell-plans--insert-actions)
        (insert "\n")
        (+agent-shell-plans--insert-documents documents)
        (unless shells (insert "No agent plans yet.\n"))
        (mapc #'+agent-shell-plans--insert-card shells)))
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

(defun +agent-shell-plans--goto (shell)
  "Move to SHELL's card and unfold it, or stay put when it has none."
  (when-let* ((card (seq-find (lambda (section)
                                (and (eq (oref section type) 'agent-plan)
                                     (eq (oref section value) shell)))
                              (oref magit-root-section children))))
    (goto-char (oref card start))
    (magit-section-show card)))

(defun +agent-shell-plans--enclosing (type)
  "Return the value of the TYPE section at or around point."
  (let ((section (magit-current-section)))
    (while (and section (not (eq (oref section type) type)))
      (setq section (oref section parent)))
    (and section (oref section value))))

(defun +agent-shell-plans--shell ()
  "Return the shell of the card at point, including its folded entries."
  (+agent-shell-plans--enclosing 'agent-plan))

(defun +agent-shell-plans-visit ()
  "Visit the document or shell at point, or run the Actions row at point."
  (interactive)
  (if-let* ((action (get-text-property (point) '+agent-shell-plans-action))
            ((not (eq action #'+agent-shell-plans-visit))))
      (call-interactively action)
    (if-let* ((file (+agent-shell-plans--enclosing 'agent-plan-document)))
        (pop-to-buffer (find-file-noselect file))
      (let ((shell (or (+agent-shell-plans--shell)
                       (and action
                            (car (+agent-shell-plans--sorted))))))
        (unless (buffer-live-p shell) (user-error "No live shell here"))
        (pop-to-buffer shell)))))

(defun +agent-shell-plans-keys ()
  "List keys acting on the item at point and across the view."
  (interactive)
  (let ((document (+agent-shell-plans--enclosing 'agent-plan-document))
        (shell (+agent-shell-plans--shell))
        (action (get-text-property (point) '+agent-shell-plans-action)))
    (with-help-window "*agent plans keys*"
      (princ (cond (document "RET     Visit this document\n")
                   (shell "RET     Visit this shell\n")
                   (action "RET     Run this action\n") (t "")))
      (princ "TAB     Cycle folded, children shown, everything shown\nn / p   Next or previous item\n")
      (princ "C-n C-p Next or previous item\nM-n M-p Next or previous sibling\n")
      (princ "^       Parent section\ng       Refresh the whole view\n")
      (princ "q       Quit the view\n?       Keys here\n"))))

(provide '+agent-shell-plans-view)
;;; +agent-shell-plans-view.el ends here
