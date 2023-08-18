;;; rk-org-scratchpad.el --- Scratchpad for org-related experiments  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(defun rk-ql-dailies ()
  (interactive)
  (org-ql-search (f-files rk-org--roam-dailies-dir) '(heading "Food") :narrow nil))

(defun my-org-copy-subtree-contents ()
  "Get the content text of the subtree at point and add it to the `kill-ring'.
Excludes the heading and any child subtrees."
  (interactive)
  (substring-no-properties
   (org-agenda-get-some-entry-text
    (point-marker)
    most-positive-fixnum)))

(defun extract-food-headers ()
  "Extracts the contents of the 'Food' header from every Org file in DIRECTORY."
  (interactive)
  (let ((buffer (generate-new-buffer "*Food Headers*")))
    (mapc (lambda (file)
            (with-current-buffer (find-file-noselect file)
              (org-map-entries
               (lambda ()
                 (when (string= (org-no-properties (org-get-heading)) "Food")
                   (save-excursion
                     (let ((content (buffer-substring-no-properties (point) (org-entry-end-position))))
                       (with-current-buffer buffer
                         (insert content)))))))
              (kill-buffer)))
          (directory-files rk-org--roam-dailies-dir t "\\.org$"))
    (switch-to-buffer-other-window buffer)
    (org-mode)))

(provide 'rk-org-scratchpad)

;;; rk-org-scratchpad.el ends here
