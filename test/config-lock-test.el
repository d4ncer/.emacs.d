;;; config-lock-test.el --- Locked recipe verification -*- lexical-binding: t; -*-

;;; Commentary:
;; Resolve recipes offline and verify every recorded commit exists locally.

;;; Code:

(require 'config-test-support)
(require 'elpaca)
(require 'elpaca-git)

(ert-deftest +config-lock-resolves-all-recorded-commits ()
  (let* ((elpaca-lock-file (expand-file-name "elpaca-lock.eld" +config-test-root))
         (elpaca-menu-lock-file--cache nil)
         (entries (with-temp-buffer
                    (insert-file-contents elpaca-lock-file) (read (current-buffer)))))
    (should-not (assq 'treesit-auto entries))
    (should-not (assq 'wgrep entries))
    (dolist (entry entries)
      (let* ((id (car entry))
             (locked (plist-get (plist-get (cdr entry) :recipe) :ref))
             (recipe (elpaca-recipe id))
             (source (elpaca-git-repo-dir recipe)))
        (should (string-match-p "\\`[[:xdigit:]]\\{40\\}\\'" locked))
        (should (equal locked (plist-get recipe :ref)))
        (should (zerop (process-file "git" nil nil nil "-C" source
                                    "cat-file" "-e" (concat locked "^{commit}"))))))))

(ert-deftest +config-lock-bootstrap-matches-lock ()
  (let ((locked (with-temp-buffer
                  (insert-file-contents (expand-file-name "elpaca-lock.eld" +config-test-root))
                  (plist-get (plist-get (cdr (assq 'elpaca (read (current-buffer))))
                                        :recipe) :ref))))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "elpaca-bootstrap.el" +config-test-root))
      (goto-char (point-min))
      (should (re-search-forward ":ref \"\\([[:xdigit:]]\\{40\\}\\)\"" nil t))
      (should (equal (match-string 1) locked)))))

(provide 'config-lock-test)
;;; config-lock-test.el ends here
