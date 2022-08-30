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

    (with-eval-after-load 'company
      (set-face-attribute 'company-tooltip-annotation-selection nil
                          :inherit 'company-tooltip-selection))

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
                            :repo "rougier/nano-modeline")
  :after (nano-theme)
  :custom
  (nano-modeline-prefix-padding t)
  :config
  (nano-modeline-mode))

(use-package flycheck
  :straight t
  :after nano-modeline
  :preface
  (defun rk-flycheck--custom-mode-line-status-text (&optional status)
    (pcase (or status flycheck-last-status-change)
      (`no-checker "Checks[-]")
      (`errored "Checks[ERROR]")
      (`finished
       (let-alist (flycheck-count-errors flycheck-current-errors)
         (cond
          ((and .error .warning)
           (format "✖ (%s error%s, %s warn%s)"
                   .error
                   (if (equal .error 1) "" "s")
                   .warning
                   (if (equal .warning 1) "" "s")))
          (.error
           (format "✖ (%s error%s)" .error (if (equal .error 1) "" "s")))

          (.warning
           (format "! (%s warning%s)" .warning (if (equal .warning 1) "" "s")))
          (t
           "✔"))))
      (`interrupted "? (interrupted)")
      (`suspicious "? (suspicious)")
      (`running "···")
      (_
       "")))

  (defun rk-nano-modeline--default-mode (&optional icon)
    (let ((icon (or icon
                    (plist-get (cdr (assoc 'text-mode nano-modeline-mode-formats)) :icon)))
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
          (flyc        (rk-flycheck--custom-mode-line-status-text)))
      (nano-modeline-render icon
                            buffer-name
                            (if branch (concat "(" branch ")") "")
                            (concat " " flyc " " position))))

  :config
  (advice-add 'nano-modeline-default-mode :override #'rk-nano-modeline--default-mode))

(use-package nano-minibuffer
  :straight '(nano-minibuffer :type git :host github
                              :repo "rougier/nano-minibuffer"))

(provide 'rk-theme-nano)

;;; rk-theme-nano.el ends here
