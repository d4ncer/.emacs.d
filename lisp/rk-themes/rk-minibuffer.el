;;; rk-minibuffer.el --- <enter description here>  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package)
  (require 'nano-base-colors)
  (require 'nano-theme))

(with-eval-after-load 'mini-frame
  ;; Miniframe at the bottom for a nicer display
  (setq mini-frame-show-parameters
        `((left . 0.5)
          (top . 1.0)
          (width . 1.0)
          (height . 8)
          (left-fringe . 0)
          (right-fringe . 0)
          (child-frame-border-width . 16)
          (internal-border-width . 0)
          (foreground-color . ,nano-color-foreground)
          (background-color . ,nano-color-subtle)))


  (with-eval-after-load 'selectrum
    (setq selectrum-max-window-height 6)
    (set-face 'selectrum-current-candidate 'nano-face-strong)
    (set-face-attribute 'selectrum-current-candidate nil
                        :extend t
                        :foreground nano-color-background
                        :background nano-color-faded))

  (setq mini-frame-ignore-commands
        '("edebug-eval-expression" debugger-eval-expression))

  (setq mini-frame-internal-border-color nano-color-subtle)
  ;; (setq mini-frame-resize 'grow-only) ;; -> buggy as of 01/05/2021
  (setq mini-frame-resize 'not-set)
  ;; (setq mini-frame-resize nil)
  ;; (add-hook 'minibuffer-setup-hook
  ;;           (lambda ()
  ;;             (overlay-put (make-overlay (point-min) (+ (point-min) 1))
  ;;                          'before-string
  ;;                          (propertize "\n" 'face `(:extend t
  ;;                                                           :height .5)))))
  (mini-frame-mode 1))

(provide 'rk-minibuffer)

;;; rk-minibuffer.el ends here
