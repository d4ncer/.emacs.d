;;; agent-shell-review-agent-shell.el --- Review origin adapter -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") (agent-shell "0"))

;;; Commentary:
;; Capture an implementation agent-shell and send fixes back to that buffer.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'agent-shell)

(defun agent-shell-review-agent-shell--context (buffer)
  "Return readable conversation text from agent-shell BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((file (and (boundp 'agent-shell--transcript-file)
                       agent-shell--transcript-file)))
        (cond
         ((and file (file-readable-p file))
          (with-temp-buffer
            (insert-file-contents file)
            (buffer-string)))
         ((not (string-empty-p (string-trim (buffer-string))))
          (buffer-string)))))))

(defun agent-shell-review-agent-shell--matching (root)
  "Return agent-shell buffers whose project is ROOT."
  (cl-remove-if-not
   (lambda (buffer)
     (and (buffer-live-p buffer)
          (with-current-buffer buffer
            (and (derived-mode-p 'agent-shell-mode)
                 (when-let* ((cwd (agent-shell-cwd)))
                   (equal (file-truename root)
                          (file-truename cwd)))))))
   (agent-shell-buffers)))

(defun agent-shell-review-agent-shell--choose (root)
  "Choose a project shell for ROOT, if there is one."
  (let ((matches (agent-shell-review-agent-shell--matching root)))
    (pcase (length matches)
      (0 nil)
      (1 (car matches))
      (_ (get-buffer
          (completing-read "Implementation shell: "
                           (mapcar #'buffer-name matches) nil t))))))

(defun agent-shell-review-agent-shell-origin (root)
  "Return context and a send callback for the exact shell in ROOT."
  (when-let* ((buffer (if (derived-mode-p 'agent-shell-mode)
                         (current-buffer)
                       (agent-shell-review-agent-shell--choose root))))
    (list :buffer buffer
          :context (agent-shell-review-agent-shell--context buffer)
          :send (lambda (prompt)
                  (agent-shell-review-agent-shell-send buffer prompt)))))

(defun agent-shell-review-agent-shell-send (buffer prompt)
  "Send PROMPT to captured implementation BUFFER when it is ready."
  (unless (buffer-live-p buffer)
    (user-error "Original implementation session has closed"))
  (with-current-buffer buffer
    (unless (derived-mode-p 'agent-shell-mode)
      (user-error "Original implementation buffer is no longer an agent shell"))
    (unless (agent-shell-session-id :shell-buffer buffer)
      (user-error "Original implementation session is not ready"))
    (when (shell-maker-busy)
      (user-error "Original implementation session is busy")))
  (or (with-current-buffer buffer
        (agent-shell-insert :text prompt :submit t :no-focus t
                            :shell-buffer buffer))
      (user-error "Original implementation session did not accept the prompt")))

(provide 'agent-shell-review-agent-shell)
;;; agent-shell-review-agent-shell.el ends here
