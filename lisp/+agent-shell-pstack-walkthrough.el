;;; +agent-shell-pstack-walkthrough.el --- Code links in pstack result reports -*- lexical-binding: t; -*-

;;; Commentary:
;; A delegate's or arena candidate's report ends with a walkthrough whose
;; claims cite path:line in its result commit.  When the panel finishes,
;; `+agent-shell-pstack-panel--write-results' records each result beside
;; its report as ROLE.result.json.  Visiting such a report turns on
;; `+agent-shell-pstack-walkthrough-mode', which makes each path:line or
;; path:line-line whose path is in the commit's tree a link.  Following
;; one opens the file at that line in the lead's checkout when its copy
;; matches the commit's, else in the role's worktree when it is at the
;; commit.  Otherwise it offers to switch the lead's checkout to the
;; result, recording where it was in
;; `+agent-shell-pstack-walkthrough-review-file', and
;; `+agent-shell-pstack-walkthrough-return' goes back.  One result is
;; reviewed at a time per checkout.  See docs/AGENT-SHELL-PSTACK.md.

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

(defcustom +agent-shell-pstack-walkthrough-review-file
  (locate-user-emacs-file "var/pstack-walkthrough-review.eld")
  "File recording each checkout switched to a result for review.
It holds a list of plists (:checkout DIR :origin ORIGIN :panel PANEL
:role ROLE :commit COMMIT): DIR is the checkout's true name, and ORIGIN,
\(branch . NAME) or (detached . COMMIT), is where it was before its
first switch."
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

;;;; Reviewing in the lead's checkout

(defun +agent-shell-pstack-walkthrough--identical-p (directory commit path)
  "Return whether PATH in DIRECTORY matches COMMIT's, in index and work tree."
  (and directory
       (file-regular-p (expand-file-name path directory))
       (seq-every-p
        (lambda (cached)
          (apply #'+agent-shell-arena-git--read directory
                 "--literal-pathspecs" "diff" "--quiet" "--no-ext-diff"
                 "--no-textconv"
                 (append cached (list commit "--" path))))
        '(nil ("--cached")))))

(defun +agent-shell-pstack-walkthrough--read-in (directory &rest args)
  "Return the trimmed output of git ARGS in DIRECTORY, or nil on failure."
  (when-let* ((output (apply #'+agent-shell-arena-git--read directory args)))
    (string-trim output)))

(defun +agent-shell-pstack-walkthrough--key (directory)
  "Return the true name of the checkout at DIRECTORY, the key of its review."
  (file-truename (file-name-as-directory directory)))

(defun +agent-shell-pstack-walkthrough--reviews ()
  "Return the recorded reviews; see `+agent-shell-pstack-walkthrough-review-file'."
  (let ((file +agent-shell-pstack-walkthrough-review-file))
    (when (file-readable-p file)
      (seq-filter (lambda (review) (stringp (plist-get review :checkout)))
                  (ignore-errors
                    (with-temp-buffer
                      (let ((coding-system-for-read 'utf-8-unix))
                        (insert-file-contents file))
                      (read (current-buffer))))))))

(defun +agent-shell-pstack-walkthrough--review (checkout)
  "Return the recorded review of CHECKOUT, a true name, or nil."
  (seq-find (lambda (review) (equal checkout (plist-get review :checkout)))
            (+agent-shell-pstack-walkthrough--reviews)))

(defun +agent-shell-pstack-walkthrough--put-review (checkout review)
  "Record REVIEW for CHECKOUT, a true name, or forget its review when nil."
  (let ((file +agent-shell-pstack-walkthrough-review-file)
        (reviews (append (and review (list review))
                         (seq-remove (lambda (old)
                                       (equal checkout
                                              (plist-get old :checkout)))
                                     (+agent-shell-pstack-walkthrough--reviews))))
        (coding-system-for-write 'utf-8-unix)
        (print-length nil)
        (print-level nil))
    (if (null reviews)
        (when (file-exists-p file) (delete-file file))
      (make-directory (file-name-directory file) t)
      (write-region (concat (prin1-to-string reviews) "\n") nil file nil
                    'silent))))

(defun +agent-shell-pstack-walkthrough-reviews ()
  "Return each checkout switched to a result, as a plist.
It has the keys `+agent-shell-pstack-walkthrough-review-file' records,
:head, the commit the checkout is at now, and :place, what it is on, as
`+agent-shell-pstack-walkthrough--place' returns; both are nil when the
checkout is gone."
  (mapcar (lambda (review)
            (let ((checkout (plist-get review :checkout)))
              (if (file-directory-p checkout)
                  (append (list :head (+agent-shell-pstack-walkthrough--read-in
                                       checkout "rev-parse" "-q" "--verify"
                                       "HEAD")
                                :place (+agent-shell-pstack-walkthrough--place
                                        checkout))
                          review)
                review)))
          (+agent-shell-pstack-walkthrough--reviews)))

(defun +agent-shell-pstack-walkthrough--place (directory)
  "Return what DIRECTORY's checkout is on: (branch . NAME) or (detached . OID)."
  (if-let* ((branch (+agent-shell-pstack-walkthrough--read-in
                     directory "symbolic-ref" "-q" "--short" "HEAD")))
      (cons 'branch branch)
    (cons 'detached (+agent-shell-pstack-walkthrough--read-in
                     directory "rev-parse" "-q" "--verify" "HEAD"))))

(defun +agent-shell-pstack-walkthrough--short (commit)
  "Return COMMIT abbreviated to seven characters."
  (substring commit 0 (min 7 (length commit))))

(defun +agent-shell-pstack-walkthrough-place-name (place)
  "Return a name for PLACE, a (branch . NAME) or (detached . OID)."
  (pcase place
    (`(branch . ,name) name)
    (`(detached . ,(and oid (pred stringp)))
     (format "commit %s" (+agent-shell-pstack-walkthrough--short oid)))
    (_ "no commit")))

(defun +agent-shell-pstack-walkthrough-describe (review)
  "Return a name for the result REVIEW, a result or review record, shows."
  (format "panel %s's %s result %s" (plist-get review :panel)
          (plist-get review :role)
          (+agent-shell-pstack-walkthrough--short (plist-get review :commit))))

(defconst +agent-shell-pstack-walkthrough--operations
  '(("rebase-merge" . "a rebase")
    ("rebase-apply" . "a rebase or am")
    ("MERGE_HEAD" . "a merge")
    ("CHERRY_PICK_HEAD" . "a cherry-pick")
    ("REVERT_HEAD" . "a revert")
    ("sequencer" . "a cherry-pick or revert sequence"))
  "Each (GIT-PATH . OPERATION) whose presence means OPERATION is in progress.")

(defun +agent-shell-pstack-walkthrough--check (directory)
  "Signal a user error saying why DIRECTORY's checkout cannot switch now.
It cannot while a git operation is in progress or tracked files have
uncommitted changes."
  (let ((name (abbreviate-file-name directory)))
    (pcase-dolist (`(,path . ,operation)
                   +agent-shell-pstack-walkthrough--operations)
      (when-let* ((file (+agent-shell-pstack-walkthrough--read-in
                         directory "rev-parse" "--git-path" path))
                  ((file-exists-p (expand-file-name file directory))))
        (user-error "%s has %s in progress; finish or abort it first"
                    name operation)))
    (let ((output (+agent-shell-arena-git--read
                   directory "status" "--porcelain=v1" "-z"
                   "--untracked-files=no"))
          paths)
      (unless output
        (user-error "Could not read the status of %s" name))
      (let ((entries (split-string output "\0" t)))
        (while entries
          (let ((entry (pop entries)))
            (push (substring entry 3) paths)
            (when (memq (aref entry 0) '(?R ?C)) (pop entries)))))
      (when paths
        (user-error "%s has uncommitted changes to tracked files: %s; \
commit or stash them first"
                    name (+agent-shell-arena-git-describe-paths
                          (nreverse paths)))))))

(defun +agent-shell-pstack-walkthrough--branch-at (directory commit panel)
  "Return a local branch at COMMIT that no worktree has checked out, or nil.
Look in DIRECTORY's repository, preferring PANEL's delegate branch."
  (let* ((output (+agent-shell-arena-git--read
                  directory "for-each-ref" "--points-at" commit
                  "--format=%(worktreepath)%00%(refname:short)" "refs/heads/"))
         (free (seq-keep (lambda (line)
                           (pcase (split-string line "\0")
                             (`("" ,branch) branch)))
                         (split-string (or output "") "\n" t))))
    (or (car (member (concat "pstack/delegate/" panel) free))
        (car free))))

(defun +agent-shell-pstack-walkthrough--revert (directory)
  "Revert each unmodified buffer whose file under DIRECTORY changed on disk."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and buffer-file-name
                 (not (buffer-modified-p))
                 (file-exists-p buffer-file-name)
                 (not (verify-visited-file-modtime buffer))
                 (file-in-directory-p buffer-file-name directory))
        (revert-buffer t t t)))))

(defun +agent-shell-pstack-walkthrough--switch (directory place fail done)
  "Switch DIRECTORY's checkout to PLACE, then call DONE, or FAIL with a message.
PLACE is (branch . NAME) or (detached . OID).  Unmodified buffers whose
files the switch changed are then reverted."
  (+agent-shell-arena-git--run
   (append (list "git" "-c" "core.hooksPath=/dev/null"
                 "-c" "core.fsmonitor=false" "switch" "-q")
           (pcase place
             (`(branch . ,name) (list name))
             (`(detached . ,oid) (list "--detach" oid))))
   directory nil
   (lambda (error)
     (message "Could not switch %s to %s: %s" (abbreviate-file-name directory)
              (+agent-shell-pstack-walkthrough-place-name place) error)
     (funcall fail error))
   (lambda (_)
     (+agent-shell-pstack-walkthrough--revert directory)
     (funcall done))))

(defun +agent-shell-pstack-walkthrough--offer (result done)
  "Offer to switch RESULT's lead checkout to its commit, and call DONE there.
Record where the checkout was before its first switch, so
`+agent-shell-pstack-walkthrough-return' can go back.  Switch to a
local branch at the commit when one is free, else detach."
  (let* ((toplevel (plist-get result :toplevel))
         (commit (plist-get result :commit))
         (name (abbreviate-file-name toplevel)))
    (unless (file-directory-p toplevel)
      (user-error "The checkout %s is gone" name))
    (if (equal commit (+agent-shell-pstack-walkthrough--read-in
                       toplevel "rev-parse" "-q" "--verify" "HEAD"))
        (funcall done)
      (+agent-shell-pstack-walkthrough--check toplevel)
      (let* ((checkout (+agent-shell-pstack-walkthrough--key toplevel))
             (place (+agent-shell-pstack-walkthrough--place toplevel))
             (previous (+agent-shell-pstack-walkthrough--review checkout))
             (branch (+agent-shell-pstack-walkthrough--branch-at
                      toplevel commit (plist-get result :panel)))
             (what (+agent-shell-pstack-walkthrough-describe result))
             (origin (if previous (plist-get previous :origin) place)))
        (when (yes-or-no-p
               (format "Switch %s from %s to %s (%s)? " name
                       (+agent-shell-pstack-walkthrough-place-name place)
                       what (if branch (concat "branch " branch) "detached")))
          (+agent-shell-pstack-walkthrough--put-review
           checkout (list :checkout checkout :origin origin
                          :panel (plist-get result :panel)
                          :role (plist-get result :role) :commit commit))
          (+agent-shell-pstack-panels--refresh)
          (message "Switching %s to %s..." name what)
          (+agent-shell-pstack-walkthrough--switch
           toplevel (if branch (cons 'branch branch) (cons 'detached commit))
           (lambda (_)
             (+agent-shell-pstack-walkthrough--put-review checkout previous)
             (+agent-shell-pstack-panels--refresh))
           (lambda ()
             (+agent-shell-pstack-panels--refresh)
             (message "%s now shows %s; %s returns it to %s" name what
                      (substitute-command-keys
                       "\\[+agent-shell-pstack-walkthrough-return]")
                      (+agent-shell-pstack-walkthrough-place-name origin))
             (funcall done))))))))

(defun +agent-shell-pstack-walkthrough-follow (result path line
                                                      &optional callback)
  "Open PATH at LINE as it is in RESULT's commit.
Open it in the lead's checkout when its copy there, in the index and the
work tree, matches the commit's.  Else open it in the role's worktree
when its HEAD is the commit.  Otherwise offer to switch the lead's
checkout to the commit; `+agent-shell-pstack-walkthrough-return' brings
it back.  A commit the repository lacks is first fetched from the
results bundle into refs/pstack/review/PANEL/ROLE.  CALLBACK, when
given, is called with the file's buffer once it shows."
  (let* ((window (selected-window))
         (callback (or callback #'ignore))
         (commit (plist-get result :commit))
         (toplevel (plist-get result :toplevel))
         (worktree (plist-get result :worktree))
         (open (lambda (directory)
                 (funcall callback (+agent-shell-pstack-walkthrough--open
                                    result directory path line window)))))
    (+agent-shell-pstack-walkthrough--ensure result)
    (cond
     ((+agent-shell-pstack-walkthrough--identical-p toplevel commit path)
      (funcall open toplevel))
     ((+agent-shell-pstack-walkthrough--at-commit-p worktree commit path)
      (funcall open worktree))
     (t (+agent-shell-pstack-walkthrough--offer
         result (lambda () (funcall open toplevel)))))))

(defun +agent-shell-pstack-walkthrough--choose ()
  "Return the checkout to return: this buffer's when it is under review.
Otherwise ask which reviewed checkout, or return nil when there is none."
  (let* ((reviews (+agent-shell-pstack-walkthrough--reviews))
         (toplevel (and (file-directory-p default-directory)
                        (+agent-shell-pstack-walkthrough--read-in
                         default-directory "rev-parse" "--show-toplevel")))
         (own (and toplevel (+agent-shell-pstack-walkthrough--key toplevel))))
    (cond
     ((null reviews) nil)
     ((and own (+agent-shell-pstack-walkthrough--review own)) own)
     (t (let ((choices
               (mapcar (lambda (review)
                         (cons (abbreviate-file-name
                                (plist-get review :checkout))
                               (plist-get review :checkout)))
                       reviews)))
          (cdr (assoc (completing-read "Return which checkout? " choices
                                       nil t nil nil (caar choices))
                      choices)))))))

;;;###autoload
(defun +agent-shell-pstack-walkthrough-return (checkout)
  "Switch CHECKOUT back to where it was before it was switched to a result.
Then forget its review.  Refuse while a git operation is in progress or
tracked files have uncommitted changes.  Interactively, CHECKOUT is the
current buffer's checkout when it is under review, else one you choose."
  (interactive (list (+agent-shell-pstack-walkthrough--choose)))
  (if-let* ((review (and checkout
                         (+agent-shell-pstack-walkthrough--review checkout))))
      (let* ((origin (plist-get review :origin))
             (where (+agent-shell-pstack-walkthrough-place-name origin))
             (name (abbreviate-file-name checkout))
             (forget (lambda (format-string &rest args)
                       (+agent-shell-pstack-walkthrough--put-review
                        checkout nil)
                       (+agent-shell-pstack-panels--refresh)
                       (apply #'message format-string args))))
        (cond
         ((not (file-directory-p checkout))
          (funcall forget "%s is gone; forgot its review" name))
         ((equal origin (+agent-shell-pstack-walkthrough--place checkout))
          (funcall forget "%s is already on %s; forgot its review" name where))
         (t
          (+agent-shell-pstack-walkthrough--check checkout)
          (unless (+agent-shell-pstack-walkthrough--read-in
                   checkout "rev-parse" "-q" "--verify"
                   (pcase origin
                     (`(branch . ,branch) (concat "refs/heads/" branch))
                     (`(detached . ,oid) (concat oid "^{commit}"))))
            (user-error "%s cannot return to %s, which no longer exists"
                        name where))
          (+agent-shell-pstack-walkthrough--switch
           checkout origin #'ignore
           (lambda ()
             (funcall forget "Switched %s back to %s" name where))))))
    (message "No checkout is switched to a walkthrough result")))

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
  "Return whether PANEL's results have refs/pstack/review/PANEL/ refs."
  (seq-some #'+agent-shell-pstack-walkthrough--refs
            (+agent-shell-pstack-walkthrough--results panel)))

(defun +agent-shell-pstack-walkthrough--refs (result)
  "Return the review refs of RESULT's panel in RESULT's repository."
  (split-string (or (+agent-shell-pstack-walkthrough--git
                     result "for-each-ref" "--format=%(refname)"
                     (format "refs/pstack/review/%s/"
                             (plist-get result :panel)))
                    "")
                "\n" t))

(defun +agent-shell-pstack-walkthrough-clean (panel)
  "Delete the refs/pstack/review/PANEL/ refs walkthrough links fetched."
  (dolist (result (+agent-shell-pstack-walkthrough--results panel))
    (dolist (ref (+agent-shell-pstack-walkthrough--refs result))
      (+agent-shell-pstack-walkthrough--git result "update-ref" "-d" ref))))

(provide '+agent-shell-pstack-walkthrough)
;;; +agent-shell-pstack-walkthrough.el ends here
