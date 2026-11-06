;;; +agent-shell-pstack-walkthrough.el --- Code links in pstack result reports -*- lexical-binding: t; -*-

;;; Commentary:
;; A delegate's or arena candidate's report ends with a walkthrough whose
;; claims cite path:line in its result commit.  When the panel finishes,
;; `+agent-shell-pstack-panel--write-results' records each result beside
;; its report as ROLE.result.json.  Visiting such a report turns on
;; `+agent-shell-pstack-walkthrough-mode', which makes each path:line or
;; path:line-line whose path is in the commit's tree a link.  Following
;; one opens the file at that line in a checkout of the commit: the role's
;; worktree or the lead's checkout when its HEAD is the commit, otherwise a
;; detached review worktree under the panel's report folder, so the file
;; gets its usual major mode and LSP.  See docs/AGENT-SHELL-PSTACK.md.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require '+agent-shell-arena-git)
(require '+agent-shell-pstack-panel)

(defgroup +agent-shell-pstack-walkthrough nil
  "Code links in pstack result reports."
  :group '+agent-shell-pstack-panel)

(defcustom +agent-shell-pstack-walkthrough-opens-file
  (locate-user-emacs-file "var/pstack-walkthrough-opens.eld")
  "File that gains one line per followed walkthrough link.
Each line is a list (TIME PANEL ROLE PATH), TIME in ISO 8601."
  :type 'file)

(defface +agent-shell-pstack-walkthrough-link '((t :inherit link))
  "Face for a walkthrough link into a result commit.")

(defvar-keymap +agent-shell-pstack-walkthrough-link-map
  :doc "Keymap on walkthrough links."
  :parent button-map
  "RET" #'push-button
  "<return>" #'push-button
  "<mouse-1>" #'push-button)

(define-button-type '+agent-shell-pstack-walkthrough-link
  'face '+agent-shell-pstack-walkthrough-link
  'mouse-face 'highlight
  'follow-link t
  'keymap +agent-shell-pstack-walkthrough-link-map
  'action #'+agent-shell-pstack-walkthrough--push
  'help-echo "RET or click: open at this line in the result commit")

(defconst +agent-shell-pstack-walkthrough--link-regexp
  (rx (or line-start (not (any alnum "_./+@~-")))
      (group (any alnum "_.+@~-") (* (any alnum "_./+@~-")))
      ":" (group (+ digit)) (? "-" (+ digit)))
  "Regexp matching a path:line or path:line-line citation.")

(defvar +agent-shell-pstack-walkthrough--trees (make-hash-table :test #'equal)
  "Map a commit to the set of paths in its tree, a hash table.")

(defvar-local +agent-shell-pstack-walkthrough--result nil
  "The result record of the report this buffer visits.")

;;;; Result records

(defun +agent-shell-pstack-walkthrough--result-file (report)
  "Return the result record file beside REPORT, when REPORT is a panel's."
  (when (and (string-suffix-p ".md" report)
             (file-in-directory-p report +agent-shell-pstack-panel-directory))
    (let ((file (concat (file-name-sans-extension report) ".result.json")))
      (when (file-regular-p file) file))))

(defun +agent-shell-pstack-walkthrough--read (file)
  "Return the result record in FILE as a plist, or nil if it is not one."
  (when-let* ((json (ignore-errors
                      (with-temp-buffer
                        (let ((coding-system-for-read 'utf-8-unix))
                          (insert-file-contents file))
                        (json-parse-buffer :object-type 'plist
                                           :null-object nil
                                           :false-object nil))))
              ((seq-every-p (lambda (key) (stringp (plist-get json key)))
                            '(:panel :role :git_dir :toplevel :commit))))
    json))

(defun +agent-shell-pstack-walkthrough--git (result &rest args)
  "Run git ARGS on RESULT's repository; return the output, or nil on failure."
  (let* ((git-dir (plist-get result :git_dir))
         (toplevel (plist-get result :toplevel)))
    (when (file-directory-p git-dir)
      (apply #'+agent-shell-arena-git--read
             (if (file-directory-p toplevel) toplevel git-dir)
             "--git-dir" git-dir args))))

(defun +agent-shell-pstack-walkthrough--present-p (result)
  "Return whether RESULT's repository holds its commit."
  (and (+agent-shell-pstack-walkthrough--git
        result "cat-file" "-e"
        (concat (plist-get result :commit) "^{commit}"))
       t))

(defun +agent-shell-pstack-walkthrough--review-ref (result)
  "Return the ref a review of RESULT fetches its commit into."
  (format "refs/pstack/review/%s/%s"
          (plist-get result :panel) (plist-get result :role)))

(defun +agent-shell-pstack-walkthrough--fetch (result)
  "Fetch RESULT's commit from its results bundle, if it has one.
Return whether the repository then holds the commit."
  (let ((bundle (plist-get result :bundle))
        (ref (plist-get result :bundle_ref))
        (review (+agent-shell-pstack-walkthrough--review-ref result)))
    (when (and bundle ref (file-regular-p bundle))
      (+agent-shell-pstack-walkthrough--git
       result "fetch" "-q" "--no-tags" "--no-write-fetch-head" bundle
       (format "+%s:%s" ref review))
      (equal (plist-get result :commit)
             (string-trim
              (or (+agent-shell-pstack-walkthrough--git
                   result "rev-parse" "--verify" "-q" review)
                  ""))))))

(defun +agent-shell-pstack-walkthrough--ensure (result)
  "Make sure RESULT's repository holds its commit, or signal a user error."
  (unless (or (+agent-shell-pstack-walkthrough--present-p result)
              (+agent-shell-pstack-walkthrough--fetch result))
    (user-error "Panel %s, role %s: result %s is gone: its refs were \
deleted and no results bundle holds it"
                (plist-get result :panel) (plist-get result :role)
                (plist-get result :commit))))

(defun +agent-shell-pstack-walkthrough--paths (result)
  "Return the paths in RESULT's commit as a hash table, or nil."
  (let ((commit (plist-get result :commit)))
    (or (gethash commit +agent-shell-pstack-walkthrough--trees)
        (when-let* (((or (+agent-shell-pstack-walkthrough--present-p result)
                         (+agent-shell-pstack-walkthrough--fetch result)))
                    (output (+agent-shell-pstack-walkthrough--git
                             result "ls-tree" "-r" "-z" "--name-only"
                             commit)))
          (let ((paths (make-hash-table :test #'equal)))
            (dolist (path (split-string output "\0" t))
              (puthash path t paths))
            (puthash commit paths +agent-shell-pstack-walkthrough--trees))))))

;;;; Links

(defun +agent-shell-pstack-walkthrough--clear ()
  "Remove this buffer's walkthrough links."
  (remove-overlays (point-min) (point-max) '+agent-shell-pstack-walkthrough t))

(defun +agent-shell-pstack-walkthrough--linkify (result)
  "Make each citation of a path in RESULT's commit a link."
  (when-let* ((paths (+agent-shell-pstack-walkthrough--paths result)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward +agent-shell-pstack-walkthrough--link-regexp
                                nil t)
        (let ((path (string-remove-prefix "./" (match-string-no-properties 1))))
          (when (gethash path paths)
            (make-button (match-beginning 1) (match-end 0)
                         :type '+agent-shell-pstack-walkthrough-link
                         '+agent-shell-pstack-walkthrough t
                         '+agent-shell-pstack-walkthrough-link
                         (list result path
                               (string-to-number (match-string 2)))))
          (goto-char (match-end 0)))))))

(define-minor-mode +agent-shell-pstack-walkthrough-mode
  "Make each path:line in a pstack result report a link into its commit.
The report's ROLE.result.json names the commit.  Only paths in the
commit's tree become links; RET or a click on one runs
`+agent-shell-pstack-walkthrough-follow'."
  :lighter nil
  (+agent-shell-pstack-walkthrough--clear)
  (setq +agent-shell-pstack-walkthrough--result nil)
  (when +agent-shell-pstack-walkthrough-mode
    (if-let* ((file (and buffer-file-name
                         (+agent-shell-pstack-walkthrough--result-file
                          buffer-file-name)))
              (result (+agent-shell-pstack-walkthrough--read file)))
        (progn
          (setq +agent-shell-pstack-walkthrough--result result)
          (+agent-shell-pstack-walkthrough--linkify result))
      (setq +agent-shell-pstack-walkthrough-mode nil))))

(defun +agent-shell-pstack-walkthrough-maybe-enable ()
  "Turn on walkthrough links if this buffer visits a result report."
  (when (and buffer-file-name
             (+agent-shell-pstack-walkthrough--result-file buffer-file-name))
    (+agent-shell-pstack-walkthrough-mode 1)))

;;;###autoload
(define-minor-mode +agent-shell-pstack-walkthrough-links-mode
  "Turn on walkthrough links in every pstack result report you visit."
  :global t
  :group '+agent-shell-pstack-walkthrough
  (if +agent-shell-pstack-walkthrough-links-mode
      (add-hook 'find-file-hook #'+agent-shell-pstack-walkthrough-maybe-enable)
    (remove-hook 'find-file-hook
                 #'+agent-shell-pstack-walkthrough-maybe-enable)))

;;;; Following

(defun +agent-shell-pstack-walkthrough--review-directory (panel role)
  "Return the review worktree of PANEL's ROLE."
  (file-name-as-directory
   (expand-file-name (format "%s/review/%s" panel role)
                     +agent-shell-pstack-panel-directory)))

(defun +agent-shell-pstack-walkthrough--at-commit-p (directory commit path)
  "Return whether DIRECTORY is a checkout at COMMIT that holds PATH."
  (and directory
       (file-exists-p (expand-file-name path directory))
       (let ((read (lambda (&rest args)
                     (string-trim
                      (or (apply #'+agent-shell-arena-git--read directory args)
                          "")))))
         (and (equal (file-truename (file-name-as-directory directory))
                     (file-truename (file-name-as-directory
                                     (funcall read "rev-parse"
                                              "--show-toplevel"))))
              (equal commit (funcall read "rev-parse" "--verify" "-q"
                                     "HEAD"))))))

(defun +agent-shell-pstack-walkthrough--checkout (result path)
  "Return an existing checkout at RESULT's commit holding PATH, or nil.
That is the role's worktree, the lead's checkout or a review worktree."
  (seq-find (lambda (directory)
              (+agent-shell-pstack-walkthrough--at-commit-p
               directory (plist-get result :commit) path))
            (list (plist-get result :worktree) (plist-get result :toplevel)
                  (+agent-shell-pstack-walkthrough--review-directory
                   (plist-get result :panel) (plist-get result :role)))))

(defun +agent-shell-pstack-walkthrough--log (result path)
  "Append a line recording that RESULT's PATH was opened."
  (let ((file +agent-shell-pstack-walkthrough-opens-file)
        (coding-system-for-write 'utf-8-unix))
    (make-directory (file-name-directory file) t)
    (write-region (concat (prin1-to-string
                           (list (format-time-string "%FT%T%z")
                                 (plist-get result :panel)
                                 (plist-get result :role) path))
                          "\n")
                  nil file t 'silent)))

(defun +agent-shell-pstack-walkthrough--open (result directory path line
                                                     window)
  "Visit PATH at LINE in DIRECTORY, RESULT's checkout, from WINDOW."
  (when (window-live-p window) (select-window window))
  (find-file-other-window (expand-file-name path directory))
  (goto-char (point-min))
  (forward-line (1- line))
  (+agent-shell-pstack-walkthrough--log result path)
  (current-buffer))

(defun +agent-shell-pstack-walkthrough-follow (result path line
                                                      &optional callback)
  "Open PATH at LINE in a checkout of RESULT's commit.
Use the role's worktree or the lead's checkout when its HEAD is the
commit.  Otherwise add a detached review worktree at the commit under
the panel's report folder, first fetching the commit from the results
bundle into refs/pstack/review/PANEL/ROLE when the repository lacks it.
CALLBACK, when given, is called with the file's buffer once it shows."
  (let ((window (selected-window))
        (callback (or callback #'ignore)))
    (if-let* ((directory (+agent-shell-pstack-walkthrough--checkout
                          result path)))
        (funcall callback (+agent-shell-pstack-walkthrough--open
                           result directory path line window))
      (+agent-shell-pstack-walkthrough--ensure result)
      (let* ((directory (+agent-shell-pstack-walkthrough--review-directory
                         (plist-get result :panel) (plist-get result :role)))
             (git-dir (plist-get result :git_dir)))
        (when (file-exists-p directory)
          (user-error "%s is not a checkout of %s; remove it and try again"
                      (abbreviate-file-name directory)
                      (plist-get result :commit)))
        (make-directory (file-name-directory (directory-file-name directory))
                        t)
        (message "Checking out %s for review..."
                 (substring (plist-get result :commit) 0 12))
        (+agent-shell-arena-git--run
         (list "git" "--git-dir" git-dir "-c" "core.hooksPath=/dev/null"
               "-c" "core.fsmonitor=false" "worktree" "add" "-q" "--detach"
               (directory-file-name directory) (plist-get result :commit))
         git-dir nil
         (lambda (error)
           (message "Could not check out %s's %s result: %s"
                    (plist-get result :panel) (plist-get result :role) error))
         (lambda (_)
           (funcall callback (+agent-shell-pstack-walkthrough--open
                              result directory path line window))))))))

(defun +agent-shell-pstack-walkthrough--push (button)
  "Follow the walkthrough link BUTTON."
  (apply #'+agent-shell-pstack-walkthrough-follow
         (button-get button '+agent-shell-pstack-walkthrough-link)))

;;;; Cleanup

(defun +agent-shell-pstack-walkthrough--results (panel)
  "Return the result records under PANEL's report folder."
  (let ((directory (expand-file-name panel
                                     +agent-shell-pstack-panel-directory)))
    (when (file-directory-p directory)
      (seq-keep #'+agent-shell-pstack-walkthrough--read
                (directory-files directory t "\\.result\\.json\\'")))))

(defun +agent-shell-pstack-walkthrough-reviews-p (panel)
  "Return whether PANEL has a review worktree or review ref."
  (or (file-directory-p
       (expand-file-name (concat panel "/review")
                         +agent-shell-pstack-panel-directory))
      (seq-some (lambda (result)
                  (+agent-shell-pstack-walkthrough--refs result))
                (+agent-shell-pstack-walkthrough--results panel))))

(defun +agent-shell-pstack-walkthrough--refs (result)
  "Return the review refs of RESULT's panel in RESULT's repository."
  (split-string (or (+agent-shell-pstack-walkthrough--git
                     result "for-each-ref" "--format=%(refname)"
                     (format "refs/pstack/review/%s/"
                             (plist-get result :panel)))
                    "")
                "\n" t))

(defun +agent-shell-pstack-walkthrough-clean (panel callback)
  "Remove PANEL's review worktrees and its refs/pstack/review/PANEL/ refs.
A review worktree with changes stays, as `git worktree remove' leaves
it.  Call CALLBACK with a list of messages naming what stayed."
  (let* ((results (+agent-shell-pstack-walkthrough--results panel))
         (root (expand-file-name (concat panel "/review/")
                                 +agent-shell-pstack-panel-directory))
         (reviews (and (file-directory-p root)
                       (directory-files root t "\\`[^.]")))
         kept)
    (cl-labels
        ((next (reviews)
           (if (null reviews)
               (refs)
             (let* ((directory (car reviews))
                    (result (seq-find
                             (lambda (result)
                               (equal (plist-get result :role)
                                      (file-name-nondirectory directory)))
                             results))
                    (keep (lambda (why)
                            (push (format "%s (%s)"
                                          (abbreviate-file-name directory) why)
                                  kept)
                            (next (cdr reviews)))))
               (if (not (and result
                             (file-directory-p (plist-get result :git_dir))))
                   (funcall keep "no repository recorded")
                 (+agent-shell-arena-git--run
                  (list "git" "--git-dir" (plist-get result :git_dir)
                        "worktree" "remove" directory)
                  (plist-get result :git_dir) nil keep
                  (lambda (_) (next (cdr reviews))))))))
         (refs ()
           (dolist (result results)
             (dolist (ref (+agent-shell-pstack-walkthrough--refs result))
               (+agent-shell-pstack-walkthrough--git
                result "update-ref" "-d" ref)))
           (when (and (file-directory-p root)
                      (null (directory-files root nil "\\`[^.]")))
             (delete-directory root))
           (funcall callback (nreverse kept))))
      (next reviews))))

(provide '+agent-shell-pstack-walkthrough)
;;; +agent-shell-pstack-walkthrough.el ends here
