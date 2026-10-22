;;; +project-files.el --- Relocated finder entry -*- lexical-binding: t; -*-

;;; Commentary:
;; Preserve the old load path for external callers and existing configurations.

;;; Code:

(load (expand-file-name "project-files/+project-files.el"
                        (file-name-directory load-file-name)) nil t)

(provide '+project-files)
;;; +project-files.el ends here
