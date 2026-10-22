;;; config-modeline-test.el --- Cached mode line regressions -*- lexical-binding: t; -*-

;;; Commentary:
;; Assert data freshness and absence of IO in rendering accessors.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dired)
(require '+modeline-data)
(defvar vulpea-ui--current-note nil)

(ert-deftest +config-modeline-dired-count-and-render-without-io ()
  (let ((directory (make-temp-file "config-dired-" t)))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "one" directory) nil 'silent)
          (write-region "" nil (expand-file-name "two" directory) nil 'silent)
          (make-directory (expand-file-name "child" directory))
          (with-current-buffer (dired-noselect directory "-al")
            (+modeline-dired-refresh)
            (should (equal (+modeline-dired-file-count) "3 items"))
            (cl-letf (((symbol-function 'file-system-info)
                       (lambda (&rest _) (ert-fail "IO during render")))
                      ((symbol-function 'dired-get-filename)
                       (lambda (&rest _) (ert-fail "Scanning during render"))))
              (dotimes (_ 10)
                (+modeline-dired-directory)
                (+modeline-dired-file-count)
                (+modeline-dired-free-space)))
            (+modeline-data--cancel)
            (kill-buffer)))
      (delete-directory directory t))))

(ert-deftest +config-modeline-remote-space-never-probes ()
  (with-temp-buffer
    (setq default-directory "/ssh:unreachable:/tmp/")
    (cl-letf (((symbol-function 'file-system-info)
               (lambda (&rest _) (ert-fail "Remote space probe"))))
      (+modeline-dired--update-space)
      (should-not (+modeline-dired-free-space)))))

(ert-deftest +config-modeline-note-refresh-and-timer-cleanup ()
  (let ((+modeline-data-mode t) (calls 0))
    (with-temp-buffer
      (setq major-mode 'vulpea-ui-sidebar-mode)
      (setq-local vulpea-ui--current-note 'fixture)
      (cl-letf (((symbol-function 'vulpea-note-title) (lambda (_) "Fixture"))
                ((symbol-function 'vulpea-note-tags) (lambda (_) '("tag")))
                ((symbol-function 'vulpea-note-id) (lambda (_) "id"))
                ((symbol-function 'vulpea-db-query-by-links-some)
                 (lambda (_) (cl-incf calls) '(one two))))
        (+modeline-vulpea-refresh)
        (should (= (plist-get +modeline-vulpea-data :backlinks) 2))
        (dotimes (_ 10) (plist-get +modeline-vulpea-data :title))
        (should (= calls 1))
        (+modeline-vulpea--note-changed nil nil 'set (current-buffer))
        (let ((previous +modeline-data--timer))
          (+modeline-vulpea--db-changed)
          (should-not (eq previous +modeline-data--timer)))
        (+modeline-data--cancel)
        (should-not +modeline-data--timer)))))

(ert-deftest +config-modeline-watches-real-note-assignment ()
  (unwind-protect
      (progn
        (+modeline-data-mode 1)
        (+modeline-data-mode 1)
        (should (= 1 (cl-count #'+modeline-vulpea--note-changed
                              (get-variable-watchers 'vulpea-ui--current-note))))
        (with-temp-buffer
          (setq major-mode 'vulpea-ui-sidebar-mode)
          (setq-local vulpea-ui--current-note 'changed-note)
          (should (timerp +modeline-data--timer))))
    (+modeline-data-mode -1))
  (should-not (memq #'+modeline-vulpea--note-changed
                    (get-variable-watchers 'vulpea-ui--current-note))))

(provide 'config-modeline-test)
;;; config-modeline-test.el ends here
