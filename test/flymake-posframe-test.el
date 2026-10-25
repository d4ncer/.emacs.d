;;; flymake-posframe-test.el --- Diagnostic popup lifecycle -*- lexical-binding: t; -*-

;;; Commentary:
;; Drive `+flymake-posframe-display' through simulated commands with the
;; child frame stubbed out.

;;; Code:

(require 'config-test-support)
(require 'flymake)
(require '+flymake-posframe)

(defmacro +flymake-posframe-test--with-commands (&rest body)
  "Run BODY with posframe stubbed; BODY sees `shows', `hides' and `command'.
`command' runs the command hooks around moving point to a position."
  (declare (indent 0))
  `(let ((shows 0) (hides 0) (visible nil))
     (cl-letf (((symbol-function 'posframe-show)
                (lambda (&rest _) (cl-incf shows) (setq visible t)))
               ((symbol-function 'posframe-hide)
                (lambda (&rest _) (cl-incf hides) (setq visible nil)))
               ((symbol-function '+flymake-posframe--visible-p)
                (lambda () visible))
               ((symbol-function 'display-graphic-p) (lambda (&rest _) t))
               ((symbol-function 'flymake-diagnostic-text) (lambda (_) "msg"))
               ((symbol-function '+flymake-posframe--prefix)
                (lambda (_) '(nil . nil))))
       (cl-flet ((command (position &optional buffer)
                   (run-hooks 'pre-command-hook)
                   (when buffer (set-buffer buffer))
                   (goto-char position)
                   (run-hooks 'post-command-hook)))
         (unwind-protect (progn ,@body)
           (remove-hook 'post-command-hook '+flymake-posframe--check))))))

(ert-deftest +flymake-posframe-test-stays-up-within-a-diagnostic ()
  "Moving within one diagnostic keeps the popup; leaving it hides it."
  (with-temp-buffer
    (insert "ok bad bad ok")
    (put-text-property 4 11 'flymake-diagnostic 'diag)
    (setq-local flymake-mode t)
    (+flymake-posframe-test--with-commands
      (+flymake-posframe-mode 1)
      (command 5)
      (command 6)
      (command 8)
      (should (= shows 1))
      (should (= hides 0))
      (command 1)
      (should (= hides 1)))))

(ert-deftest +flymake-posframe-test-hides-on-leaving-the-buffer ()
  "Switching to another buffer hides the popup."
  (let ((source (generate-new-buffer " *posframe-source*")))
    (unwind-protect
        (+flymake-posframe-test--with-commands
          (with-temp-buffer
            (let ((other (current-buffer)))
              (with-current-buffer source
                (insert "bad")
                (put-text-property 1 4 'flymake-diagnostic 'diag)
                (setq-local flymake-mode t)
                (+flymake-posframe-mode 1)
                (command 2)
                (should (= shows 1))
                (command 1 other))
              (should (= hides 1)))))
      (kill-buffer source))))

(provide 'flymake-posframe-test)
;;; flymake-posframe-test.el ends here
