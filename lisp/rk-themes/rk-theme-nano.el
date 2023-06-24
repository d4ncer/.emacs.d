;;; rk-theme-nano.el --- Nano theme config  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'rk-colors)

(defconst rk-font-family "JetBrains Mono")

;; Nano stuff

(use-package nano-theme
  :straight '(nano-theme :type git :host github
                         :repo "rougier/nano-theme")
  :preface
  (defun rk-themes-build-theme ()
    ;; Add internal border
    (setq default-frame-alist
          (append (list
                   '(min-height . 1)  '(height . 45)
                   '(min-width  . 1)  '(width  . 81)
                   '(vertical-scroll-bars . nil)
                   '(internal-border-width . 24)
                   '(left-fringe . 0)
                   '(right-fringe . 0)
                   '(tool-bar-lines . 0)
                   '(menu-bar-lines . 0))))
    ;; Vertical window divider
    (setq window-divider-default-right-width 24)
    (setq window-divider-default-places 'right-only)
    (window-divider-mode 1)

    ;; Set up theme
    (nano-light)

    ;; Nicer glyphs for continuation and wrap
    (set-display-table-slot standard-display-table
                            'truncation (make-glyph-code ?… 'nano-faded))
    (set-display-table-slot standard-display-table
                            'wrap (make-glyph-code ?- 'nano-faded))

    ;; Override faces
    (set-face-attribute 'default nil
                        :weight 'regular
                        :family rk-font-family
                        :height 190)

    (set-face-attribute 'mode-line nil
                        :height 10
                        :background "#bbb"
                        :box nil)

    (set-face-attribute 'mode-line-inactive nil
                        :height 10
                        :background "#ddd"
                        :box nil)

    (set-face-attribute 'bold nil
                        :weight 'bold)

    (set-face-attribute 'italic nil
                        :slant 'italic)

    (set-face-attribute 'highlight nil
                        :background nano-light-subtle)

    (set-face-attribute 'font-lock-keyword-face nil
                        :weight 'light
                        :inherit 'default)

    (set-face-attribute 'font-lock-builtin-face nil
                        :weight 'light
                        :inherit 'default)

    (set-face-attribute 'font-lock-variable-name-face nil
                        :inherit 'default)

    (set-face-attribute 'font-lock-function-name-face nil
                        :weight 'semi-bold
                        :inherit 'default)

    (set-face-attribute 'font-lock-constant-face nil
                        :inherit 'default)

    (set-face-attribute 'font-lock-type-face nil
                        :inherit 'default)

    (set-face-attribute 'font-lock-string-face nil
                        :weight 'light
                        :inherit 'default)

    (set-face-attribute 'font-lock-comment-face nil
                        :weight 'semi-bold
                        :inherit 'default)

    (set-face-attribute 'font-lock-doc-face nil
                        :weight 'semi-bold
                        :inherit 'default)

    (with-eval-after-load 'consult
      (set-face-attribute 'consult-file nil :inherit nil
                          :weight 'semi-bold))

    (with-eval-after-load 'marginalia
      (dolist (face '(marginalia-key
                      marginalia-number
                      marginalia-file-owner
                      marginalia-file-priv-read
                      marginalia-file-priv-write
                      marginalia-file-priv-exec
                      marginalia-file-priv-link
                      marginalia-file-priv-dir
                      marginalia-file-priv-no
                      marginalia-file-priv-other
                      marginalia-file-priv-rare))
        (set-face-attribute face nil
                            :inherit nil
                            :weight 'light)))

    (with-eval-after-load 'magit
      (set-face-attribute 'magit-process-ng nil
                          :foreground rk-colors-red)

      (set-face-attribute 'magit-process-ok nil
                          :foreground rk-colors-green)

      (set-face-attribute 'magit-section-heading nil
                          :weight 'semi-bold)

      (set-face-attribute 'magit-branch-local nil
                          :foreground rk-colors-blue
                          :weight 'semi-bold)

      (set-face-attribute 'magit-branch-remote nil
                          :foreground rk-colors-green
                          :weight 'semi-bold)

      (set-face-attribute 'magit-branch-current nil
                          :foreground rk-colors-blue
                          :weight 'semi-bold
                          :box 1)

      (set-face-attribute 'magit-tag nil
                          :foreground rk-colors-yellow
                          :weight 'semi-bold)

      (set-face-attribute 'magit-diff-context nil
                          :background nano-light-background
                          :weight 'regular)

      (set-face-attribute 'magit-diff-removed nil
                          :foreground rk-colors-red
                          :weight 'regular)

      (set-face-attribute 'magit-diff-added nil
                          :foreground rk-colors-green
                          :weight 'regular)

      (set-face-attribute 'magit-diff-removed-highlight nil
                          :foreground rk-colors-red
                          :weight 'regular)

      (set-face-attribute 'magit-diff-added-highlight nil
                          :foreground rk-colors-green
                          :weight 'regular))

    (with-eval-after-load 'highlight-thing
      (set-face-attribute 'highlight-thing nil
                          :foreground nano-light-salient
                          :background nano-light-background
                          :weight 'semi-bold))

    (with-eval-after-load 'org
      (set-face-attribute 'org-block nil
                          :inherit 'default)
      (set-face-attribute 'org-level-1 nil
                          :inherit nil
                          :weight 'bold
                          :height 200)
      (set-face-attribute 'org-verbatim nil
                          :foreground nano-light-salient
                          :weight 'medium))

    (with-eval-after-load 'corfu
      (set-face-attribute 'corfu-border nil
                          :background nano-light-salient)

      (set-face-attribute 'corfu-default nil
                          :background nano-light-background))

    (with-eval-after-load 'tree-sitter-hl
      (set-face-attribute 'tree-sitter-hl-face:function.call nil
                          :inherit nil
                          :weight 'semi-bold
                          :underline nil
                          :foreground nano-light-foreground
                          :slant 'normal)
      (set-face-attribute 'tree-sitter-hl-face:property nil
                          :slant 'normal)
      (set-face-attribute 'tree-sitter-hl-face:tag nil
                          :weight 'medium)
      (set-face-attribute 'tree-sitter-hl-face:attribute nil
                          :foreground nano-light-faded))

    (with-eval-after-load 'flyspell
      (set-face-attribute 'flyspell-incorrect nil
                          :foreground nano-light-critical)
      (set-face-attribute 'flyspell-duplicate nil
                          :foreground nano-light-critical))

    (with-eval-after-load 'flymake-posframe
      (set-face-attribute 'flymake-posframe-background-face nil
                          :background nano-light-background)
      (set-face-attribute 'flymake-posframe-foreground-face nil
                          :foreground nano-light-foreground))

    (with-eval-after-load 'lsp-ui-peek
      (set-face-attribute 'lsp-ui-peek-header nil
                          :background nano-light-salient)
      (set-face-attribute 'lsp-ui-peek-footer nil
                          :background nano-light-salient)
      (set-face-attribute 'lsp-ui-peek-peek nil
                          :background nano-light-background)
      (set-face-attribute 'lsp-ui-peek-list nil
                          :background nano-light-background)
      (set-face-attribute 'lsp-ui-peek-filename nil
                          :foreground nano-light-faded
                          :weight 'semi-bold)
      (set-face-attribute 'lsp-ui-peek-selection nil
                          :background nano-light-faded
                          :foreground nano-light-background)
      (set-face-attribute 'lsp-ui-peek-highlight nil
                          :box nil
                          :background nano-light-subtle)))
  :config
  (rk-themes-build-theme))

(use-package nano-modeline
  :straight '(nano-modeline :type git :host github
                            :repo "rougier/nano-modeline" :branch "simpler")
  :after (nano-theme)
  :preface
  (defun rk-modeline-org-buffer-name (&optional name)
    (propertize
     (cond (name
            name)
           ((buffer-narrowed-p)
            (format "%s [%s]" (or (buffer-base-buffer) (buffer-name))
                    (org-link-display-format
                     (substring-no-properties
                      (or (org-get-heading 'no-tags) "-")))))
           ((f-equal? (file-name-directory (buffer-file-name))
                      rk-org--roam-dir)
            (format "(roam) %s" (car (last (s-split "-" (buffer-file-name))))))
           (t
            (buffer-name)))
     'face (nano-modeline-face 'name)))
  (defun rk-modeline-org-mode ()
    (funcall nano-modeline-position
             '((nano-modeline-buffer-status) " "
               (rk-modeline-org-buffer-name) " "
               (nano-modeline-git-info))
             '((nano-modeline-cursor-position)
               (nano-modeline-window-dedicated))))
  :init
  (setq-default mode-line-format "")
  :config
  (add-hook 'text-mode-hook #'nano-modeline-text-mode)
  (add-hook 'org-mode-hook #'rk-modeline-org-mode))

(provide 'rk-theme-nano)

;;; rk-theme-nano.el ends here
