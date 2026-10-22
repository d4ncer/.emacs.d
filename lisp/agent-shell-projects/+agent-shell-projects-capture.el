;;; +agent-shell-projects-capture.el --- Headless workspace capture -*- lexical-binding: t; -*-

;;; Commentary:
;; Enrol workspaces and schedule captures without loading review UI libraries.

;;; Code:

(require '+agent-shell-projects-core)
(require '+agent-shell-projects-jobs)

(defun +agent-shell-projects-capture-code (project artefact &optional actor callback)
  "Schedule PROJECT ARTEFACT capture, optionally calling CALLBACK with a version.
Return the last completed version, or nil while the first capture is pending."
  (+agent-shell-projects-schedule-capture project artefact actor callback)
  (when (plist-get artefact :current)
    (+agent-shell-projects-version artefact (plist-get artefact :current))))

(defun +agent-shell-projects-enrol-code (project root &optional base name)
  "Register ROOT in PROJECT and schedule its first capture against BASE."
  (let* ((root (or (locate-dominating-file root ".git")
                   (user-error "Not a Git workspace: %s" root)))
         (artefact (list :id (+agent-shell-projects--id)
                         :name (or name "Implementation") :kind "code"
                         :path (file-truename root) :base (or base "HEAD")
                         :history nil :current nil :ack nil :event 0 :ack-event 0)))
    (unless (plist-get project :author)
      (+agent-shell-projects--set project :root (plist-get artefact :path)))
    (+agent-shell-projects--set project :artefacts
                               (append (plist-get project :artefacts) (list artefact)))
    (+agent-shell-projects--changed project nil "user")
    (+agent-shell-projects-capture-code project artefact)
    artefact))

(provide '+agent-shell-projects-capture)
;;; +agent-shell-projects-capture.el ends here
