;;; agent-shell-review-protocol.el --- Reviewer prompts and results -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:
;; Define one JSON response schema for questions, findings, and clear reviews.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defun agent-shell-review-protocol-prompt (snapshot requirements clarifications)
  "Build a review request from SNAPSHOT, REQUIREMENTS, and CLARIFICATIONS."
  (format
   (concat
    "Review this changeset for correctness against the original requirements. "
    "Do not edit files or make implementation changes. If any success "
    "criterion is uncertain, ask clarifying questions instead of making "
    "assumptions or reporting a clear result.\n\n"
    "Return exactly one JSON object, optionally in a json code fence. "
    "Use one of these schemas:\n"
    "{\"kind\":\"questions\",\"items\":[{\"question\":\"...\","
    "\"criterion\":\"...\",\"why\":\"...\"}]}\n"
    "{\"kind\":\"findings\",\"items\":[{\"priority\":\"P0|P1|P2|P3\","
    "\"file\":\"relative/path\",\"line\":1,\"title\":\"...\","
    "\"evidence\":\"...\",\"requirement\":\"...\","
    "\"suggestion\":\"...\"}]}\n"
    "{\"kind\":\"clear\",\"items\":[]}\n"
    "For a nonlocal finding, use null for both file and line. "
    "Use P0 for immediate blockers, P1 for significant correctness issues, "
    "P2 for moderate issues, and P3 for minor issues. "
    "Only report evidence grounded in the diff and requirements.\n\n"
    "Requirements source: %s (%s)\n%s\n\n"
    "Clarified criteria:\n%s\n\n"
    "Changeset base: %s\nChangeset:\n%s")
   (plist-get requirements :source)
   (plist-get requirements :kind)
   (plist-get requirements :text)
   (if clarifications
       (mapconcat (lambda (answer)
                    (format "Q: %s\nA: %s" (car answer) (cdr answer)))
                  clarifications "\n")
     "None")
   (plist-get snapshot :base)
   (plist-get snapshot :diff)))

(defun agent-shell-review-protocol--json-text (text)
  "Remove an optional JSON fence from TEXT."
  (let ((trimmed (string-trim text)))
    (if (string-prefix-p "```" trimmed)
        (let ((lines (split-string trimmed "\n")))
          (unless (and (member (car lines) '("```" "```json"))
                       (> (length lines) 2)
                       (equal (car (last lines)) "```"))
            (user-error "Malformed reviewer JSON fence"))
          (string-join (butlast (cdr lines)) "\n"))
      trimmed)))

(defun agent-shell-review-protocol--field (item key)
  "Return nonempty string KEY from ITEM or signal an error."
  (let ((value (alist-get (intern key) item)))
    (unless (and (stringp value) (not (string-empty-p
                                       (string-trim value))))
      (user-error "Reviewer result lacks %s" key))
    value))

(defun agent-shell-review-protocol--question (item)
  "Validate a question ITEM and return its plist."
  (list :question (agent-shell-review-protocol--field item "question")
        :criterion (agent-shell-review-protocol--field item "criterion")
        :why (agent-shell-review-protocol--field item "why")))

(defun agent-shell-review-protocol--safe-file (file root)
  "Validate FILE as a project-relative path under ROOT."
  (when file
    (unless (and (stringp file)
                 (not (string-empty-p file))
                 (not (file-name-absolute-p file))
                 (not (string-prefix-p "~" file))
                 (not (member ".." (split-string file "/" t)))
                 (string-prefix-p
                  (file-name-as-directory (expand-file-name root))
                  (expand-file-name file root)))
      (user-error "Unsafe reviewer file path: %s" file)))
  file)

(defun agent-shell-review-protocol--finding (item root)
  "Validate finding ITEM under ROOT and return its plist."
  (let* ((priority (agent-shell-review-protocol--field item "priority"))
         (file (alist-get 'file item))
         (line (alist-get 'line item)))
    (when (eq file :null) (setq file nil))
    (when (eq line :null) (setq line nil))
    (unless (member priority '("P0" "P1" "P2" "P3"))
      (user-error "Invalid review priority: %s" priority))
    (agent-shell-review-protocol--safe-file file root)
    (unless (or (and (null file) (null line))
                (and file (integerp line) (> line 0)))
      (user-error "Reviewer finding needs a positive line for its file"))
    (list :priority priority :file file :line line
          :title (agent-shell-review-protocol--field item "title")
          :evidence (agent-shell-review-protocol--field item "evidence")
          :requirement (agent-shell-review-protocol--field item "requirement")
          :suggestion (agent-shell-review-protocol--field item "suggestion"))))

(defun agent-shell-review-protocol-parse (text root)
  "Parse review TEXT for ROOT into a validated result plist."
  (let* ((data
          (condition-case err
              (json-parse-string
               (agent-shell-review-protocol--json-text text)
               :object-type 'alist :array-type 'list :null-object :null
               :false-object :false)
            (json-parse-error
             (user-error "Invalid reviewer JSON: %s"
                         (error-message-string err)))))
         (kind (alist-get 'kind data))
         (items (alist-get 'items data)))
    (unless (and (listp data) (listp items)
                 (assoc 'items data))
      (user-error "Reviewer result needs an items array"))
    (pcase kind
      ("questions"
       (unless items (user-error "Question result has no questions"))
       (list :kind 'questions
             :items (mapcar #'agent-shell-review-protocol--question items)))
      ("findings"
       (unless items (user-error "Finding result has no findings"))
       (list :kind 'findings
             :items (mapcar (lambda (item)
                              (agent-shell-review-protocol--finding item root))
                            items)))
      ("clear"
       (when items (user-error "Clear result contains items"))
       (list :kind 'clear :items nil))
      (_ (user-error "Unknown reviewer result kind: %s" kind)))))

(provide 'agent-shell-review-protocol)
;;; agent-shell-review-protocol.el ends here
