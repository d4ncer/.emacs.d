;;; rk-theme-nano.el --- <enter description here>  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package)
  (require 'straight))

(defconst rk-theme-cursor-yellow "#f1c40f")
(defconst rk-theme-cursor-blue "#3498db")
(defconst rk-theme-cursor-green "#2ecc71")
(defconst rk-theme-cursor-purple "#9b59b6")

;; Solarized light for text
(defconst rk-theme-nano-yellow "#b58900")
(defconst rk-theme-nano-orange "#cb4b16")
(defconst rk-theme-nano-red "#dc322f")
(defconst rk-theme-nano-magenta "#d33682")
(defconst rk-theme-nano-violet "#6c71c4")
(defconst rk-theme-nano-blue "#268bd2")
(defconst rk-theme-nano-cyan "#2aa198")
(defconst rk-theme-nano-green "#859900")

;; Offwhites for highlights
(defconst rk-theme-nano-light-red "#ffe0e0")

;; Default fg/bg
(defconst rk-theme-nano-offblack "#333")
(defconst rk-theme-nano-offwhite "#fdfdfd")
(defconst rk-theme-nano-offwhite-dark "#fbf1d4")

;; Greys
(defconst rk-theme-nano-solarized-b03 "#002b36")
(defconst rk-theme-nano-solarized-b02 "#073642")
(defconst rk-theme-nano-solarized-b01 "#586e75")
(defconst rk-theme-nano-solarized-b00 "#657b83")
(defconst rk-theme-nano-solarized-b0 "#839496")
(defconst rk-theme-nano-solarized-b1 "#93a1a1")
(defconst rk-theme-nano-solarized-b2 "#eee8d5")
(defconst rk-theme-nano-solarized-b3 "#fdf6e3")
(defconst rk-theme-nano-dark-grey "#747369")
(defconst rk-theme-nano-light-grey "#e8e6df")

(defconst rk-theme-font-family "JetBrains Mono")

;; Nano stuff

(straight-use-package
 '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))

(require 'nano-base-colors)
(require 'nano-faces)
(require 'nano-theme)
(setq nano-font-size 18)
(setq nano-font-family-monospaced "JetBrains Mono")

(nano-faces)
(nano-theme)

(require 'nano-theme-light)
(require 'nano-layout)

(require 'nano-modeline)
(require 'rk-minibuffer)

(set-face-attribute 'default nil
                    :weight 'regular)

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
                      :foreground rk-theme-nano-red)

  (set-face-attribute 'magit-process-ok nil
                      :foreground rk-theme-nano-green))

(with-eval-after-load 'highlight-thing
  (set-face-attribute 'highlight-thing nil
                      :foreground nano-color-salient
                      :background nano-color-background
                      :weight 'normal))

(provide 'rk-theme-nano)

;;; rk-theme-nano.el ends here