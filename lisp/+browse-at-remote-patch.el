;;; +browse-at-remote-patch.el --- el-patch fixes to browse-at-remote -*- lexical-binding: t; -*-

;;; Commentary:
;; Required once `browse-at-remote' loads.

;;; Code:

(require 'el-patch)
(require 'browse-at-remote)

(defvar git-timemachine-revision)

(el-patch-defun browse-at-remote--file-url (filename &optional start end)
  (el-patch-concat
    "Return the URL to browse FILENAME from lines START to END. "
    (el-patch-add
      "\nIn `git-timemachine-mode', link the revision the buffer shows."))
  (let* ((remote-ref (browse-at-remote--remote-ref filename))
         (remote (car remote-ref))
         (ref (el-patch-swap
                (cdr remote-ref)
                (if (bound-and-true-p git-timemachine-mode)
                    (car git-timemachine-revision)
                  (cdr remote-ref))))
         (relname (f-relative filename (f-expand (vc-git-root filename))))
         (target-repo (browse-at-remote--get-url-from-remote remote))
         (remote-type (browse-at-remote--get-remote-type (plist-get target-repo :unresolved-host)))
         (repo-url (plist-get target-repo :url))
         (url-formatter (browse-at-remote--get-formatter 'region-url remote-type))
         (start-line (when start (line-number-at-pos start)))
         (end-line (when end (line-number-at-pos end))))
    (unless url-formatter
      (error (format "Origin repo parsing failed: %s" repo-url)))

    (funcall url-formatter repo-url ref relname
             (if start-line start-line)
             (if (and end-line (not (equal start-line end-line))) end-line))))

(provide '+browse-at-remote-patch)
;;; +browse-at-remote-patch.el ends here
