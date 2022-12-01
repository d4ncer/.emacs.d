;;; rk-flymake.el --- <enter description here>  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'definers)
(require 'dash)

(use-package flymake
  :general
  (:keymaps 'flymake-diagnostics-buffer-mode-map :states '(normal emacs)
            "C-g" #'kill-current-buffer
            "j" #'rk-flymake--diag-next-error
            "k" #'rk-flymake--diag-prev-error
            "RET" #'rk-flymake--goto-diag)
  :preface
  (defun rk-flymake--diag-next-error ()
    (interactive)
    (next-line)
    (flymake-show-diagnostic (point) t))
  (defun rk-flymake--diag-prev-error ()
    (interactive)
    (previous-line)
    (flymake-show-diagnostic (point) t))
  (defun rk-flymake--goto-diag ()
    (interactive)
    (flymake-goto-diagnostic (point))
    (kill-buffer (flymake--diagnostics-buffer-name)))
  (defun rk-flymake--setup ()
    (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake)
    (flymake-mode 1))
  (defun rk-flymake--disable-bc ()
    (remove-hook 'flymake-diagnostic-functions #'elisp-flymake-byte-compile t))

  (defun rk-flymake--toggle-buffer-list ()
    (interactive)
    (if-let* ((buffer (flymake--diagnostics-buffer-name))
              (window (get-buffer-window buffer)))
        (kill-buffer buffer)
      (flymake-show-buffer-diagnostics)
      (switch-to-buffer-other-window buffer)
      (face-remap-add-relative 'header-line
                               :foreground nano-light-salient
                               :background nano-light-background)))

  :init
  (add-hook 'prog-mode-hook #'rk-flymake--setup)
  (rk-leader-def
    "el" '(rk-flymake--toggle-buffer-list :wk "list buffer errors"))

  :config
  (add-hook 'emacs-lisp-mode-hook #'rk-flymake--disable-bc)

  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flymake diagnostics" (one-or-more anything) "*" eos)
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (slot . 1)
                 (reusable-frames . visible)
                 (side . bottom)
                 (window-height . 0.2))))

(use-package flymake
  :after nano-modeline
  :preface
  (defun rk-modeline--flymake-count (type)
    "TYPE is either `:error', `:warning', or `:note'."
    (let ((count 0))
      (dolist (d (flymake-diagnostics))
        (when (= (flymake--severity type)
                 (flymake--severity (flymake-diagnostic-type d)))
          (cl-incf count)))
      count))

  (defun rk-modeline--maybe-suffix-s (count)
    (if (equal count 1) "" "s"))

  (defun rk-modeline--has-diag (count)
    (> count 0))

  (defun rk-modeline--flymake-counts ()
    (let ((errors (rk-modeline--flymake-count :error))
          (warnings (rk-modeline--flymake-count :warning))
          (notes (rk-modeline--flymake-count :note)))
      (cond
       ((and (rk-modeline--has-diag errors) (rk-modeline--has-diag warnings))
        (format "✖ (%s error%s, %s warn%s)"
                errors
                (rk-modeline--maybe-suffix-s errors)
                warnings
                (rk-modeline--maybe-suffix-s warnings)))
       ((rk-modeline--has-diag errors)
        (format "✖ (%s error%s)" errors (rk-modeline--maybe-suffix-s errors)))
       ((rk-modeline--has-diag warnings)
        (format "! (%s warning%s)" warnings (rk-modeline--maybe-suffix-s warnings)))
       ((rk-modeline--has-diag notes)
        (format "? (%s note%s)" notes (rk-modeline--maybe-suffix-s notes)))
       (t
        "✔"))))

  (defun rk-modeline--prog-mode ()
    (let ((icon (plist-get (cdr (assoc 'prog-mode nano-modeline-mode-formats)) :icon))
          ;; We take into account the case of narrowed buffers
          (buffer-name (cond
                        ((and (derived-mode-p 'org-mode)
                              (buffer-narrowed-p)
                              (buffer-base-buffer))
                         (format"%s [%s]" (buffer-base-buffer)
                                (org-link-display-format
                                 (substring-no-properties (or (org-get-heading 'no-tags)
                                                              "-")))))
                        ((and (buffer-narrowed-p)
                              (buffer-base-buffer))
                         (format"%s [narrow]" (buffer-base-buffer)))
                        (t
                         (format-mode-line "%b"))))

          (mode-name   (nano-modeline-mode-name))
          (branch      (nano-modeline-vc-branch))
          (position    (format-mode-line "%l:%c"))
          (flymake-counts (rk-modeline--flymake-counts)))
      (nano-modeline-render icon
                            buffer-name
                            (if branch (concat "(" branch ")") "")
                            (concat " " flymake-counts " " position))))

  :custom
  (nano-modeline-mode-formats
   (--map-when (eq (car it) 'prog-mode)
               '(prog-mode :mode-p nano-modeline-prog-mode-p
                           :format rk-modeline--prog-mode
                           :icon "")
               nano-modeline-mode-formats)))

(use-package flymake-posframe
  :straight '(flymake-posframe :type git :host github
                               :repo "Ladicle/flymake-posframe")
  :after flymake
  :hook (flymake-mode . flymake-posframe-mode))

(provide 'rk-flymake)

;;; rk-flymake.el ends here
