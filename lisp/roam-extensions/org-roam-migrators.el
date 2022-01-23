;;; org-roam-migrators.el --- <enter description here>  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'vulpea)
(require 'org-roam-dailies)

;;;###autoload
(defun org-roam-migrator/dailies ()
  "Migrate dailies to have a category & tag set to 'daily'."
  (interactive)
  (dolist (file (org-roam-dailies--list-files))
    (if-let* ((id (vulpea-db-get-id-by-file file))
              (note (vulpea-db-get-by-id id)))
        (vulpea-utils-with-note note
          (vulpea-buffer-tags-set "daily")
          (org-set-property "CATEGORY" "daily")
          (save-buffer)))))

(provide 'org-roam-migrators)

;;; org-roam-migrators.el ends here
