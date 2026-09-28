;;; agent-shell-review-spec.el --- Discover review criteria -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:
;; Resolve requirements from a selected file, a shell transcript reference,
;; candidate spec directories, or the implementation conversation.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup agent-shell-review-spec nil
  "Requirement discovery for agent review."
  :group 'tools)

(defcustom agent-shell-review-spec-search-roots
  '("~/.codex" "~/.claude")
  "Additional directories searched for project-related Markdown specs."
  :type '(repeat directory))

(defcustom agent-shell-review-spec-max-bytes 262144
  "Largest requirements source in bytes."
  :type 'integer)

(defconst agent-shell-review-spec--project-dirs
  '("docs/superpowers/specs" "docs/specs" "specs" "docs/plans")
  "Project directories likely to contain requirements.")

(defun agent-shell-review-spec--read (file)
  "Read FILE without truncation, rejecting unreadable or large sources."
  (unless (and (file-regular-p file) (file-readable-p file))
    (user-error "Requirements file is not readable: %s" file))
  (when (> (file-attribute-size (file-attributes file))
           agent-shell-review-spec-max-bytes)
    (user-error "Requirements file exceeds %d bytes: %s"
                agent-shell-review-spec-max-bytes file))
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun agent-shell-review-spec--referenced-paths (text)
  "Return Markdown paths referenced in TEXT, in occurrence order."
  (let (paths)
    (dolist (pattern '("`\\([^`\n]+\\.md\\)`"
                       "(\\([^)\n]+\\.md\\))"
                       "\\(?:~?/\\|\\.\\.?/\\|[[:alnum:]_.-]+/\\)[^[:space:]<>\"'`)]*\\.md"))
      (let ((start 0))
        (while (string-match pattern text start)
          (let ((path (or (match-string 1 text) (match-string 0 text))))
            (unless (member path paths)
              (push path paths)))
          (setq start (match-end 0)))))
    (nreverse paths)))

(defun agent-shell-review-spec--candidate-files (directory depth)
  "Find spec-like Markdown files in DIRECTORY up to DEPTH levels."
  (when (and (> depth 0) (file-directory-p directory)
             (file-readable-p directory))
    (let (found)
      (dolist (path (directory-files directory t directory-files-no-dot-files-regexp
                                     t))
        (let ((name (file-name-nondirectory path)))
          (cond
           ((and (file-directory-p path)
                 (not (file-symlink-p path))
                 (not (member name '("cache" "plugins" "node_modules"
                                     ".git" "eln-cache" "sessions" "tmp"))))
            (setq found
                  (nconc found (agent-shell-review-spec--candidate-files
                                path (1- depth)))))
           ((and (file-regular-p path)
                 (string-match-p "\\.md\\'" name)
                 (string-match-p "\\(spec\\|design\\|plan\\)"
                                 (downcase path)))
            (push path found)))))
      found)))

(defun agent-shell-review-spec--referenced-file (root transcript)
  "Choose a credible requirements file referenced in TRANSCRIPT for ROOT."
  (let* ((paths
          (delete-dups
           (cl-loop for reference in
                    (agent-shell-review-spec--referenced-paths transcript)
                    for path = (expand-file-name reference root)
                    when (and (file-regular-p path)
                              (file-readable-p path))
                    collect path)))
         (specs
          (cl-remove-if-not
           (lambda (path)
             (let ((name (downcase (file-name-nondirectory path))))
               (and (not (equal name "readme.md"))
                    (or (string-match-p
                         "\\(spec\\|design\\|plan\\|requirement\\|proposal\\)"
                         name)
                        (string-match-p
                         "/\\(specs?\\|plans?\\|designs?\\|requirements?\\)/"
                         (downcase (file-name-directory path)))))))
           paths))
         (choices
          (or specs
              (cl-remove-if
               (lambda (path)
                 (string-match-p "\\`readme\\.md\\'"
                                 (downcase (file-name-nondirectory path))))
               paths))))
    (pcase (length choices)
      (0 nil)
      (1 (car choices))
      (_ (completing-read "Referenced review spec: "
                          (sort choices #'string<) nil t)))))

(defun agent-shell-review-spec--select-candidate (root)
  "Return a discovered spec path for ROOT, prompting on ambiguity."
  (let* ((project-files
          (delete-dups
           (cl-mapcan
            (lambda (dir)
              (agent-shell-review-spec--candidate-files
               (expand-file-name dir root) 5))
            agent-shell-review-spec--project-dirs)))
         (global-files
          (delete-dups
           (cl-mapcan
            (lambda (dir)
              (agent-shell-review-spec--candidate-files
               (expand-file-name dir) 5))
            agent-shell-review-spec-search-roots)))
         (project-name (file-name-nondirectory
                        (directory-file-name root)))
         (related-global
          (cl-remove-if-not
           (lambda (path)
             (string-match-p (regexp-quote project-name) path))
           global-files))
         (candidates (or project-files related-global global-files)))
    (pcase (length candidates)
      (0 nil)
      (1 (car candidates))
      (_ (completing-read "Review requirements: "
                          (sort candidates #'string<) nil t)))))

(defun agent-shell-review-spec--user-context (transcript)
  "Extract user instructions from TRANSCRIPT, or use plain text."
  (let ((start 0)
        sections)
    (while (string-match "^## User\\b[^\n]*\n" transcript start)
      (let* ((body-start (match-end 0))
             (next (string-match "^## \\(?:User\\|Agent\\)\\b"
                                 transcript body-start))
             (section (string-trim
                       (substring transcript body-start (or next
                                                            (length transcript))))))
        (unless (string-empty-p section)
          (push section sections))
        (setq start (or next (length transcript)))
        (when (= start (length transcript))
          (setq start (1+ start)))))
    (if sections
        (string-join (nreverse sections) "\n\n")
      (string-trim transcript))))

(defun agent-shell-review-spec--prompt-source ()
  "Prompt for a requirements file or entered text."
  (condition-case nil
      (let ((choice (completing-read "Requirements source: "
                                     '("File" "Text") nil t)))
        (pcase choice
          ("File" (let ((file (read-file-name "Requirements file: "
                                             nil nil t)))
                    (unless (and file (not (string-empty-p file)))
                      (user-error "Requirements file was not selected"))
                    (list :kind 'file :source file
                          :text (agent-shell-review-spec--read file))))
          ("Text" (let ((text (read-from-minibuffer
                                "Requirements text: ")))
                    (when (string-empty-p (string-trim (or text "")))
                      (user-error "Requirements text is empty"))
                    (when (> (string-bytes text)
                             agent-shell-review-spec-max-bytes)
                      (user-error "Entered requirements exceed %d bytes"
                                  agent-shell-review-spec-max-bytes))
                    (list :kind 'entered :source "entered requirements"
                          :text text)))
          (_ (user-error "Requirements source was not selected"))))
    (quit (user-error "Requirements source was cancelled"))))

(defun agent-shell-review-spec-resolve (root origin-text
                                             &optional explicit-file)
  "Find requirements for ROOT using ORIGIN-TEXT.
EXPLICIT-FILE, when non-nil, always takes precedence."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (file
          (condition-case nil
              (or (and explicit-file (expand-file-name explicit-file))
                  (agent-shell-review-spec--referenced-file
                   root (or origin-text ""))
                  (agent-shell-review-spec--select-candidate root))
            (quit (user-error "Requirements selection was cancelled")))))
    (cond
     (file
      (list :kind 'file :source file
            :text (agent-shell-review-spec--read file)))
     ((and origin-text
           (not (string-empty-p
                 (agent-shell-review-spec--user-context origin-text))))
      (let ((text (agent-shell-review-spec--user-context origin-text)))
        (when (> (string-bytes text) agent-shell-review-spec-max-bytes)
          (user-error "Conversation requirements exceed %d bytes"
                      agent-shell-review-spec-max-bytes))
        (list :kind 'conversation :source "implementation conversation"
              :text text)))
     (t (agent-shell-review-spec--prompt-source)))))

(provide 'agent-shell-review-spec)
;;; agent-shell-review-spec.el ends here
