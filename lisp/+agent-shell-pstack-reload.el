;;; +agent-shell-pstack-reload.el --- Reload live pstack code -*- lexical-binding: t; -*-

;;; Commentary:
;; Reload only active libraries, adopting changed defaults that are still in use.

;;; Code:

(require 'cl-lib)

(defconst +agent-shell-pstack-reload--libraries
  '(+agent-shell-sandbox +agent-shell-drive +agent-shell-arena-git
    +agent-shell-egress +agent-shell-pstack +agent-shell-pstack-panel
    +agent-shell-arena-remote +agent-shell-pstack-arena-trace)
  "Pstack libraries in dependency order.")

(defun +agent-shell-pstack-reload--minor-mode-p (symbol)
  "Return non-nil if SYMBOL is a minor mode variable."
  (or (memq symbol minor-mode-list)
      (eq (get symbol 'custom-set) 'custom-set-minor-mode)))

(defun +agent-shell-pstack-reload--default (symbol)
  "Return SYMBOL's evaluated standard value in a list, or nil on error."
  (when-let* ((standard (get symbol 'standard-value)))
    (condition-case nil
        (list (eval (car standard) t))
      (error nil))))

;;;###autoload
(defun +agent-shell-pstack-reload ()
  "Reload the already loaded pstack libraries in dependency order.
Adopt changed option defaults only where the live value still equals the
old default.  Preserve minor modes and report panels dropped by the schema
guard.  Options whose standard values cannot be evaluated are left alone."
  (interactive)
  (let (libraries options modes reset
        (dropped 0)
        (load-prefer-newer t))
    (dolist (feature +agent-shell-pstack-reload--libraries)
      (when (featurep feature)
        (when-let* ((history
                     (cl-find-if
                      (lambda (entry)
                        (member (cons 'provide feature) (cdr entry)))
                      load-history)))
          (push (cons feature history) libraries)
          (dolist (symbol (cdr history))
            (when (and (symbolp symbol) (boundp symbol))
              (when (and (memq symbol global-minor-modes)
                         (default-value symbol))
                (cl-pushnew symbol modes))
              (unless (+agent-shell-pstack-reload--minor-mode-p symbol)
                (when-let* ((default
                             (+agent-shell-pstack-reload--default symbol)))
                  (push (cons symbol (car default)) options))))))))
    (setq libraries (nreverse libraries)
          options (nreverse options))
    (unwind-protect
        (let ((original-message (symbol-function 'message)))
          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest arguments)
                       (let ((text (and (stringp format-string)
                                        (apply #'format-message
                                               format-string arguments))))
                         (if (and text
                                  (string-match
                                   "\\`Dropped \\([0-9]+\\) pstack panels?\\b"
                                   text))
                             (progn
                               (cl-incf dropped
                                        (string-to-number
                                         (match-string 1 text)))
                               text)
                           (apply original-message format-string arguments))))))
            (dolist (library libraries)
              (load (file-name-sans-extension (cadr library)) nil t))
            (dolist (option options)
              (let ((symbol (car option))
                    (old (cdr option)))
                (when (and (boundp symbol)
                           (not (+agent-shell-pstack-reload--minor-mode-p
                                 symbol))
                           (equal (default-value symbol) old))
                  (when-let* ((new (+agent-shell-pstack-reload--default symbol)))
                    (unless (equal old (car new))
                      (custom-reevaluate-setting symbol)
                      (push symbol reset))))))))
      (dolist (mode modes)
        (unless (default-value mode)
          (funcall mode 1))))
    (message "pstack: reloaded %d libraries (%s); options reset: %s; \
schema guard dropped %d pstack panels%s"
             (length libraries)
             (if libraries (mapconcat (lambda (entry) (symbol-name (car entry)))
                                      libraries ", ") "none")
             (if reset (mapconcat #'symbol-name (nreverse reset) ", ") "none")
             dropped
             (if (> dropped 0) "; kill their member shells by hand" ""))))

(provide '+agent-shell-pstack-reload)
;;; +agent-shell-pstack-reload.el ends here
