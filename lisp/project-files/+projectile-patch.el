;;; +projectile-patch.el --- el-patch fixes to Projectile -*- lexical-binding: t; -*-

;;; Commentary:
;; Required by +project-files, after Projectile.

;;; Code:

(require 'el-patch)
(require 'projectile)

(defvar +project-files-mode)

(el-patch-defun projectile--watch-directories (project files)
  (el-patch-concat
    "Return the directories of PROJECT to watch, derived from cached FILES.
The result contains the project root and the directory chain of every
cached file, as absolute names with trailing slashes.  Directories with
no cached files below them (e.g. empty directories) are not included,
so files that later appear in them go unnoticed until the next full
re-index."
    (el-patch-add
      "\nWith `+project-files-mode', stop at one directory past
`projectile-watch-max-directories'."))
  (let ((dirs (make-hash-table :test 'equal)))
    (puthash (file-name-as-directory (expand-file-name project)) t dirs)
    (el-patch-wrap 2 0
      (catch 'limit
        (dolist (file files)
          (dolist (dir (projectile--directory-ancestors file))
            (puthash (expand-file-name dir project) t dirs)
            (el-patch-add
              (when (and +project-files-mode
                         (> (hash-table-count dirs)
                            projectile-watch-max-directories))
                (throw 'limit nil)))))))
    (hash-table-keys dirs)))

(provide '+projectile-patch)
;;; +projectile-patch.el ends here
