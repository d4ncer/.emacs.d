;;; elpaca-apply-lock.el --- Move installed packages to their locked commits -*- lexical-binding: t; -*-

;;; Commentary:
;; Run from this repository right before restarting Emacs:
;;
;;   emacs -Q --batch -l scripts/elpaca-apply-lock.el PACKAGE...
;;
;; Elpaca does not move an existing checkout when elpaca-lock.eld changes.
;; This checks out each PACKAGE's locked commit in elpaca/sources, fetching
;; it from origin when missing, then relinks, regenerates autoloads and
;; byte-compiles its build as Elpaca's build steps do.  It refuses to change
;; anything if a named source has local changes.  It deletes lisp/**/*.elc
;; so local libraries recompile against the new builds.  Set
;; ELPACA_APPLY_CONFIG to apply to another configuration directory.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst +apply-config
  (file-name-as-directory
   (expand-file-name (or (getenv "ELPACA_APPLY_CONFIG")
                         (file-name-directory
                          (directory-file-name
                           (file-name-directory load-file-name)))))))

(defvar elpaca-directory (expand-file-name "elpaca/" +apply-config))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(add-to-list 'load-path (expand-file-name "elpaca/" elpaca-builds-directory))
(require 'elpaca)
(require 'elpaca-git)
(require 'bytecomp)

(defun +apply-git (directory &rest arguments)
  "Run git ARGUMENTS in DIRECTORY; return (STATUS . OUTPUT)."
  (with-temp-buffer
    (cons (apply #'process-file "git" nil t nil "-C" directory arguments)
          (string-trim (buffer-string)))))

(defun +apply-git! (directory &rest arguments)
  "Run git ARGUMENTS in DIRECTORY and return its output, or signal."
  (pcase-let ((`(,status . ,output) (apply #'+apply-git directory arguments)))
    (unless (eql status 0)
      (error "git %s failed in %s: %s"
             (string-join arguments " ") directory output))
    output))

(defun +apply-has-commit-p (source ref)
  "Return non-nil if SOURCE has commit REF."
  (eql 0 (car (+apply-git source "cat-file" "-e" (concat ref "^{commit}")))))

(defun +apply-version (file)
  "Return the Version header of FILE, or \"?\"."
  (if (not (file-exists-p file)) "?"
    (with-temp-buffer
      (insert-file-contents file)
      (if (re-search-forward "^;+ *Version: *\\(.+\\)$" nil t)
          (match-string 1) "?"))))

(defun +apply-plan (lock name)
  "Return the plist for package NAME locked in LOCK."
  (let* ((recipe (plist-get (cdr (or (assq (intern name) lock)
                                     (error "%s is not in the lock" name)))
                            :recipe))
         (package (plist-get recipe :package))
         (source (file-name-as-directory (elpaca-git-repo-dir recipe))))
    (unless (file-directory-p source)
      (error "%s has no source at %s" name source))
    (list :name name :recipe recipe :ref (plist-get recipe :ref)
          :source source :old (+apply-git! source "rev-parse" "HEAD")
          :build (file-name-as-directory
                  (expand-file-name package elpaca-builds-directory))
          :main (expand-file-name (concat package ".el") source))))

(defun +apply-checkout (plan)
  "Check out PLAN's locked commit, fast-forwarding its branch if possible."
  (let ((source (plist-get plan :source))
        (ref (plist-get plan :ref))
        (old (plist-get plan :old)))
    (unless (+apply-has-commit-p source ref)
      (+apply-git source "fetch" "--quiet" "origin")
      (unless (+apply-has-commit-p source ref)
        (+apply-git! source "fetch" "--quiet" "origin" ref)))
    (cond
     ((equal old ref))
     ((and (eql 0 (car (+apply-git source "symbolic-ref" "-q" "HEAD")))
           (eql 0 (car (+apply-git source "merge-base" "--is-ancestor"
                                   old ref))))
      (+apply-git! source "merge" "--quiet" "--ff-only" ref))
     (t (+apply-git! source "checkout" "--quiet" "--detach" ref)))
    (unless (equal (+apply-git! source "rev-parse" "HEAD") ref)
      (error "%s is not at %s after checkout" source ref))))

(defun +apply-build (plan)
  "Relink, autoload and byte-compile PLAN's build; return failed files."
  (let* ((build (plist-get plan :build))
         (e (elpaca<--create :recipe (plist-get plan :recipe)
                             :source-dir (plist-get plan :source)
                             :build-dir build))
         failed)
    (when (file-exists-p build) (delete-directory build t))
    (make-directory build t)
    (pcase-dolist (`(,file . ,link) (elpaca--files e))
      (when (file-exists-p file)
        (make-directory (file-name-directory link) t)
        (make-symbolic-link file link t)))
    (elpaca-generate-autoloads (file-name-nondirectory
                                (directory-file-name build))
                               build)
    (dolist (file (directory-files-recursively build "\\.el\\'"))
      (unless (condition-case err (byte-compile-file file)
                (error (message "%s: %s" file (error-message-string err))
                       nil))
        (push file failed)))
    failed))

(let* ((names (or command-line-args-left
                  (error "Usage: emacs -Q --batch -l %s PACKAGE..."
                         load-file-name)))
       (lock (with-temp-buffer
               (insert-file-contents
                (expand-file-name "elpaca-lock.eld" +apply-config))
               (read (current-buffer))))
       (plans (mapcar (apply-partially #'+apply-plan lock) names))
       (dirty (seq-filter (lambda (plan)
                            (not (string-empty-p
                                  (+apply-git! (plist-get plan :source)
                                               "status" "--porcelain"))))
                          plans))
       (versions (mapcar (lambda (plan) (+apply-version (plist-get plan :main)))
                         plans))
       failed)
  (setq command-line-args-left nil)
  (when dirty
    (error "Local changes in %s; nothing changed"
           (mapconcat (lambda (plan) (plist-get plan :source)) dirty ", ")))
  (mapc #'+apply-checkout plans)
  (setq load-prefer-newer t
        byte-compile-warnings nil
        load-path (append (directory-files elpaca-builds-directory t "\\`[^.]")
                          load-path))
  (dolist (plan plans)
    (setq failed (append failed (+apply-build plan))))
  (dolist (file (directory-files-recursively
                 (expand-file-name "lisp/" +apply-config) "\\.elc\\'"))
    (delete-file file))
  (cl-mapc
   (lambda (plan version)
     (let ((source (plist-get plan :source))
           (old (plist-get plan :old))
           (ref (plist-get plan :ref)))
       (princ (format "%s %s -> %s, version %s -> %s, %s commits\n"
                      (plist-get plan :name) (substring old 0 7)
                      (substring ref 0 7) version
                      (+apply-version (plist-get plan :main))
                      (if (+apply-has-commit-p source old)
                          (+apply-git! source "rev-list" "--count"
                                       (concat old ".." ref))
                        "?")))
       (unless (equal old ref)
         (princ (concat (+apply-git! source "log" "--oneline" "-5"
                                     (concat old ".." ref))
                        "\n")))))
   plans versions)
  (princ (format "Deleted lisp/ bytecode in %s; it recompiles after restart.\n"
                 +apply-config))
  (when failed
    (princ (format "Byte-compilation failed:\n  %s\n"
                   (string-join failed "\n  ")))
    (kill-emacs 1)))

;;; elpaca-apply-lock.el ends here
