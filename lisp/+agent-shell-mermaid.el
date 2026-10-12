;;; +agent-shell-mermaid.el --- Inline Mermaid diagrams -*- lexical-binding: t; -*-

;; Author: rk
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (agent-shell "0.66.1"))

;;; Commentary:
;; Render complete Mermaid fences asynchronously using mmdc.  Text properties
;; survive agent-shell's temporary rendering buffers and viewport copies.
;; Run `+agent-shell-mermaid-refresh' to render an existing conversation.
;; Press RET or click a diagram to open its image in a file buffer.

;;; Code:

(require 'agent-shell-markdown)
(require 'map)
(require 'subr-x)

(defgroup +agent-shell-mermaid nil
  "Inline Mermaid diagrams in agent-shell."
  :group 'agent-shell)

(defcustom +agent-shell-mermaid-program "mmdc"
  "Mermaid CLI executable, or its absolute path."
  :type 'string)

(defcustom +agent-shell-mermaid-cache-directory
  (locate-user-emacs-file "cache/mermaid/")
  "Directory for generated diagram images."
  :type 'directory)

(defcustom +agent-shell-mermaid-timeout 30
  "Seconds allowed for each Mermaid render."
  :type 'number)

(defvar +agent-shell-mermaid--jobs (make-hash-table :test #'equal)
  "Render processes indexed by cache key.")

(defvar +agent-shell-mermaid--failures (make-hash-table :test #'equal)
  "Failed cache keys, suppressed until an explicit refresh.")

(defvar +agent-shell-mermaid-mode)

(defun +agent-shell-mermaid--theme ()
  "Return the Mermaid theme matching the selected frame."
  (if (eq (frame-parameter nil 'background-mode) 'dark) "dark" "default"))

(defun +agent-shell-mermaid--key (source)
  "Return a cache key for SOURCE and the frame's theme."
  (secure-hash 'sha256
               (concat (+agent-shell-mermaid--theme) "\0" source)))

(defun +agent-shell-mermaid--file (key)
  "Return the image file for KEY."
  (expand-file-name (concat key ".png")
                    +agent-shell-mermaid-cache-directory))

(defun +agent-shell-mermaid--display (start end file)
  "Display FILE over START to END without changing the source text."
  (let* ((window (get-buffer-window (current-buffer) t))
         (width (max 100 (- (window-body-width window t) 40)))
         (image (create-image file 'png nil :max-width width :scale 0.5))
         (open (lambda ()
                 (interactive)
                 (agent-shell-markdown-visit-file :file file)))
         (keymap (make-sparse-keymap)))
    (define-key keymap (kbd "RET") open)
    (define-key keymap [mouse-1] open)
    (with-silent-modifications
      (put-text-property start end 'display image)
      (put-text-property start end 'keymap keymap)
      (put-text-property start end 'pointer 'hand)
      (put-text-property start end 'help-echo
                         "RET or click to open Mermaid diagram"))))

(defun +agent-shell-mermaid--update-buffers (key file)
  "Display FILE in every surviving copy of the diagram identified by KEY."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (let ((pos (point-min)))
          (while (setq pos (text-property-any
                            pos (point-max) '+agent-shell-mermaid-key key))
            (let ((end (next-single-property-change
                        pos '+agent-shell-mermaid-key nil (point-max))))
              (+agent-shell-mermaid--display pos end file)
              (setq pos end))))))))

(defun +agent-shell-mermaid--request (source key)
  "Start an asynchronous render of SOURCE for KEY unless already pending."
  (unless (or (gethash key +agent-shell-mermaid--jobs)
              (gethash key +agent-shell-mermaid--failures))
    (let ((program (executable-find +agent-shell-mermaid-program))
          (output (+agent-shell-mermaid--file key)))
      (if (not program)
          (progn
            (puthash key t +agent-shell-mermaid--failures)
            (message "Mermaid: cannot find %s" +agent-shell-mermaid-program))
        (make-directory +agent-shell-mermaid-cache-directory t)
        (let ((input (make-temp-file "agent-shell-mermaid-" nil ".mmd"))
              (log (generate-new-buffer " *mermaid-render*"))
              (default-directory +agent-shell-mermaid-cache-directory)
              timer proc)
          (with-temp-file input (insert source))
          (condition-case err
              (progn
                (setq proc
                      (make-process
                       :name "agent-shell-mermaid" :buffer log :noquery t
                       :connection-type 'pipe
                       :command (list program "-i" input "-o" output
                                      "-t" (+agent-shell-mermaid--theme)
                                      "-b" "transparent" "-w" "1200"
                                      "-s" "2" "--quiet")
                       :sentinel
                       (lambda (process _event)
                         (when (memq (process-status process) '(exit signal))
                           (when timer (cancel-timer timer))
                           (remhash key +agent-shell-mermaid--jobs)
                           (unwind-protect
                               (if (and (zerop (process-exit-status process))
                                        (file-exists-p output))
                                   (when +agent-shell-mermaid-mode
                                     (+agent-shell-mermaid--update-buffers
                                      key output))
                                 (puthash key t +agent-shell-mermaid--failures)
                                 (when (file-exists-p output)
                                   (delete-file output))
                                 (when +agent-shell-mermaid-mode
                                   (let ((details
                                          (with-current-buffer log
                                            (buffer-string))))
                                     (with-current-buffer
                                         (get-buffer-create "*Mermaid errors*")
                                       (goto-char (point-max))
                                       (insert source "\n\n" details "\n"))
                                     (message
                                      "Mermaid render failed; see *Mermaid errors*"))))
                             (delete-file input)
                             (kill-buffer log))))))
                (puthash key proc +agent-shell-mermaid--jobs)
                (setq timer
                      (run-at-time +agent-shell-mermaid-timeout nil
                                   (lambda ()
                                     (when (process-live-p proc)
                                       (with-current-buffer log
                                         (goto-char (point-max))
                                         (insert "\nMermaid rendering timed out.\n"))
                                       (delete-process proc))))))
            (error
             (delete-file input)
             (kill-buffer log)
             (puthash key t +agent-shell-mermaid--failures)
             (message "Mermaid: %s" (error-message-string err)))))))))

(defun +agent-shell-mermaid--claim (start end source markdown)
  "Claim START to END for SOURCE, preserving original MARKDOWN for copying."
  (let* ((source (string-trim source))
         (key (+agent-shell-mermaid--key source))
         (file (+agent-shell-mermaid--file key)))
    (with-silent-modifications
      (add-text-properties
       start end
       `(+agent-shell-mermaid-source ,source
         +agent-shell-mermaid-key ,key
         agent-shell-markdown-source ,markdown
         agent-shell-markdown-frozen t
         rear-nonsticky (agent-shell-markdown-frozen
                         +agent-shell-mermaid-source
                         +agent-shell-mermaid-key
                         display keymap pointer help-echo))))
    (if (file-exists-p file)
        (+agent-shell-mermaid--display start end file)
      (+agent-shell-mermaid--request source key))))

(defun +agent-shell-mermaid--render-hook (context)
  "Claim complete Mermaid blocks from agent-shell's CONTEXT."
  (when +agent-shell-mermaid-mode
    (dolist (block (map-elt context :source-blocks))
      (when (and (equal (map-elt block :language) "mermaid")
                 (map-elt block :complete))
        (let ((start (map-nested-elt block '(:block :start)))
              (end (map-nested-elt block '(:block :end))))
          (unless (get-text-property start '+agent-shell-mermaid-source)
            ;; Keep the final newline outside the image's display span.
            (when (eq (char-before end) ?\n) (setq end (1- end)))
            (+agent-shell-mermaid--claim
             start end (map-elt block :body)
             (buffer-substring-no-properties start end)))))))
  nil)

(defun +agent-shell-mermaid--claim-existing ()
  "Claim Mermaid blocks whose fences have already been styled away."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^mermaid ⧉" nil t)
      (let* ((start (match-beginning 0))
             (body (next-single-property-change
                    start 'agent-shell-markdown-source-block-body
                    nil (point-max))))
        (when (and (get-text-property
                    start 'agent-shell-markdown-source-block-rendered)
                   (< body (point-max))
                   (get-text-property body
                                      'agent-shell-markdown-source-block-body)
                   (not (get-text-property start '+agent-shell-mermaid-source)))
          (let ((end (next-single-property-change
                      body 'agent-shell-markdown-source-block-body
                      nil (point-max))))
            (+agent-shell-mermaid--claim
             start end (buffer-substring-no-properties body end)
             (get-text-property body 'agent-shell-markdown-source))))))))

(defun +agent-shell-mermaid-refresh (&optional recompile)
  "Render Mermaid diagrams in the current buffer, retrying failures.
With prefix argument RECOMPILE, regenerate cached images too."
  (interactive "P")
  (unless +agent-shell-mermaid-mode
    (user-error "Enable +agent-shell-mermaid-mode first"))
  (save-restriction
    (widen)
    (let ((pos (point-min)))
      (while (< pos (point-max))
        (let* ((source (get-text-property pos '+agent-shell-mermaid-source))
               (end (next-single-property-change
                     pos '+agent-shell-mermaid-source nil (point-max))))
          (when source
            (let* ((key (+agent-shell-mermaid--key source))
                   (file (+agent-shell-mermaid--file key)))
              (remhash key +agent-shell-mermaid--failures)
              (when (and recompile (file-exists-p file)) (delete-file file))
              (with-silent-modifications
                (put-text-property pos end '+agent-shell-mermaid-key key))
              (if (file-exists-p file)
                  (+agent-shell-mermaid--display pos end file)
                (+agent-shell-mermaid--request source key))))
          (setq pos end))))
    (+agent-shell-mermaid--claim-existing)
    (+agent-shell-mermaid--render-hook (agent-shell-markdown-context))))

;;;###autoload
(define-minor-mode +agent-shell-mermaid-mode
  "Toggle inline Mermaid rendering in agent-shell and its viewports."
  :global t :group '+agent-shell-mermaid
  (if +agent-shell-mermaid-mode
      (progn
        (add-hook 'agent-shell-markdown-render-functions
                  #'+agent-shell-mermaid--render-hook)
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (when (derived-mode-p 'agent-shell-mode
                                  'agent-shell-viewport-view-mode)
              (+agent-shell-mermaid-refresh)))))
    (remove-hook 'agent-shell-markdown-render-functions
                 #'+agent-shell-mermaid--render-hook)
    (maphash (lambda (_key process) (delete-process process))
             +agent-shell-mermaid--jobs)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (save-restriction
          (widen)
          (let ((pos (point-min)))
            (while (< pos (point-max))
              (let ((end (next-single-property-change
                          pos '+agent-shell-mermaid-source nil (point-max))))
                (when (get-text-property pos '+agent-shell-mermaid-source)
                  (with-silent-modifications
                    (remove-text-properties
                     pos end '(display nil keymap nil pointer nil
                               help-echo nil))))
                (setq pos end)))))))))

(provide '+agent-shell-mermaid)
;;; +agent-shell-mermaid.el ends here
