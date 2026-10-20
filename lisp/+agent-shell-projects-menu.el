;;; +agent-shell-projects-menu.el --- Project actions -*- lexical-binding: t; -*-

;;; Commentary:
;; Project-wide operations live here; contextual actions use Embark.

;;; Code:

(require 'transient)
(require '+agent-shell-projects-core)
(defvar-local +agent-shell-projects--project-id nil)
(defvar +agent-shell-projects-home-mode-map)
(defvar +agent-shell-projects-sidebar-mode-map)
(defvar +agent-shell-projects-artefact-mode-map)
(declare-function +agent-shell-projects "+agent-shell-projects")
(declare-function +agent-shell-projects-home "+agent-shell-projects")
(declare-function +agent-shell-projects-rename "+agent-shell-projects")
(declare-function +agent-shell-projects-refresh "+agent-shell-projects")
(declare-function +agent-shell-projects-enrol-file "+agent-shell-projects")
(declare-function +agent-shell-projects-submit "+agent-shell-projects-agent")
(declare-function +agent-shell-projects-mark-reviewed "+agent-shell-projects-agent")
(declare-function +agent-shell-projects-choose-author "+agent-shell-projects-agent")
(declare-function +agent-shell-projects-extract "+agent-shell-projects-agent")
(declare-function +agent-shell-projects-capture-outcome "+agent-shell-projects-agent")
(declare-function +agent-shell-projects-cancel-request "+agent-shell-projects-agent")
(autoload '+agent-shell-projects-add-code "+agent-shell-projects-code")
(autoload '+agent-shell-projects-review-code "+agent-shell-projects-review")
(autoload '+agent-shell-projects-request-explanation "+agent-shell-projects-review")
(autoload '+agent-shell-projects-request-proposal "+agent-shell-projects-review")
(autoload '+agent-shell-projects-set-proposal "+agent-shell-projects-review")
(autoload '+agent-shell-projects-cancel-capture "+agent-shell-projects-jobs")

(transient-define-prefix +agent-shell-projects-menu ()
  "Actions for the current authoring project."
  [["Project"
    ("h" "Home" +agent-shell-projects-home)
    ("o" "Open project" +agent-shell-projects)
    ("a" "Choose author" +agent-shell-projects-choose-author)
    ("R" "Rename project" +agent-shell-projects-rename)
    ("g" "Refresh" +agent-shell-projects-refresh)]
   ["Artefacts"
    ("f" "Enrol document" +agent-shell-projects-enrol-file)
    ("w" "Enrol Git workspace" +agent-shell-projects-add-code)
    ("x" "Extract prose" +agent-shell-projects-extract)
    ("p" "Choose proposal" +agent-shell-projects-set-proposal)
    ("P" "Reconstruct proposal" +agent-shell-projects-request-proposal)]
   ["Review"
    ("s" "Submit feedback" +agent-shell-projects-submit)
    ("m" "Mark reviewed" +agent-shell-projects-mark-reviewed)
    ("c" "Review code" +agent-shell-projects-review-code)
    ("G" "Generate explanations" +agent-shell-projects-request-explanation)
    ("C" "Cancel capture" +agent-shell-projects-cancel-capture)
    ("r" "Capture outcome" +agent-shell-projects-capture-outcome)
    ("k" "Cancel pending request" +agent-shell-projects-cancel-request)]]
  (interactive)
  (unless +agent-shell-projects--project-id (+agent-shell-projects))
  (transient-setup '+agent-shell-projects-menu))

(define-key +agent-shell-projects-home-mode-map (kbd "?") #'+agent-shell-projects-menu)
(define-key +agent-shell-projects-sidebar-mode-map (kbd "?") #'+agent-shell-projects-menu)
(define-key +agent-shell-projects-artefact-mode-map (kbd "C-c C-m") #'+agent-shell-projects-menu)

(provide '+agent-shell-projects-menu)
;;; +agent-shell-projects-menu.el ends here
