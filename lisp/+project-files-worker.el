;;; +project-files-worker.el --- Relocated worker entry -*- lexical-binding: t; -*-

;;; Commentary:
;; Existing Emacs sessions can still launch the worker through this old path.

;;; Code:

(load (expand-file-name "project-files/+project-files-worker.el"
                        (file-name-directory load-file-name)) nil t)

(provide '+project-files-worker)
;;; +project-files-worker.el ends here
