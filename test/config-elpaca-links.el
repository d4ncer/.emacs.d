;;; config-elpaca-links.el --- Check package build links -*- lexical-binding: t; -*-

;;; Commentary:
;; Elpaca links each built file to its source.  A build made in a checkout
;; that shared the package folder can record that checkout's own paths,
;; which break once it goes.  The gate fails on each such link.

;;; Code:

(require 'subr-x)

(defun +config-elpaca-bad-links (root)
  "Return a line for each bad link under ROOT's elpaca/builds/, sorted.
A link is bad when it does not resolve, or when its target lies outside
elpaca/sources/ of the directory ROOT's elpaca resolves to, the one
Elpaca builds from."
  (let* ((elpaca (file-name-as-directory
                  (file-truename (expand-file-name "elpaca/" root))))
         (builds (expand-file-name "builds/" elpaca))
         (sources (expand-file-name "sources/" elpaca))
         bad)
    (when (file-directory-p builds)
      (dolist (link (directory-files-recursively builds ""))
        (when-let* ((target (file-symlink-p link)))
          (let ((path (expand-file-name target (file-name-directory link)))
                (name (file-relative-name link elpaca)))
            (cond ((not (file-exists-p path))
                   (push (format "%s -> %s does not resolve" name target) bad))
                  ((not (string-prefix-p sources path))
                   (push (format "%s -> %s is outside %s" name target
                                 (abbreviate-file-name sources))
                         bad)))))))
    (sort bad #'string<)))

(provide 'config-elpaca-links)
;;; config-elpaca-links.el ends here
