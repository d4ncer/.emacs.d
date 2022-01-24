;;; org-roam-migrators.el --- Custom migration scripts for org-roam  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'vulpea)
(require 'org-roam-dailies)

;;;###autoload
(defun orm/dailies ()
  "Migrate dailies to have a category & tag set to 'daily'."
  (interactive)
  (dolist (file (org-roam-dailies--list-files))
    (if-let* ((id (vulpea-db-get-id-by-file file))
              (note (vulpea-db-get-by-id id)))
        (vulpea-utils-with-note note
          (vulpea-buffer-tags-set "daily")
          (goto-char (point-min))
          (org-delete-property "CATEGORY")
          (save-buffer)))))

;;;###autoload
(defun orm/ca-notes ()
  "Migrate notes to check for CA or not."
  (interactive)
  (dolist (file (org-roam-list-files))
    (message "processing %s" file)
    (if-let* ((id (vulpea-db-get-id-by-file file))
              (note (vulpea-db-get-by-id id)))
        (vulpea-utils-with-note note
          (goto-char (point-min))
          (when-let ((untagged (not (-contains-p (vulpea-buffer-tags-get) "ca")))
                     (is-ca (y-or-n-p (format "Is %s a CA note?" (vulpea-buffer-title-get)))))
            (vulpea-buffer-tags-add "ca")
            (save-buffer))))))

(provide 'org-roam-migrators)

;;; org-roam-migrators.el ends here
