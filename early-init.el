;;; early-init.el --- early Emacs init file -*- lexical-binding: t; -*-

;;; Commentary:

;; This file is loaded by the editor early in the bootstrap process, before
;; resources like GUI frames are initialised. It is generally used to tune
;; critical aspects of the presentation or behaviour of the editor.

;;; Code:


(eval-and-compile
  (add-to-list 'load-path (file-name-concat user-emacs-directory "lisp/")))

(setq package-enable-at-startup nil)

;; Set up personal settings

(setq user-full-name "Raghuvir Kasturi")
(setq user-mail-address "raghuvir.kasturi@gmail.com")

;;; Configure use-package

(setq use-package-verbose init-file-debug)
(setq use-package-always-defer t)
(setq use-package-enable-imenu-support t)
(setq use-package-minimum-reported-time 0.01)
(setq use-package-hook-name-suffix nil)

(unless init-file-debug
  (add-hook 'after-init-hook
            (lambda ()
              ;; Enable this when the configuration reaches a steady state; it
              ;; will make it much easier to read the macro-expanded output of
              ;; use-package calls.
              (setq use-package-expand-minimally t))))

(require '+load-incrementally)
(+load-incrementally-setup-use-package-keywords)

;;; Customise as much UI as possible early in init sequence.

;; Theme + fonts

(require '+theme)
(setq +theme-light 'modus-operandi)

(set-face-attribute 'default nil :family "JetBrainsMono Nerd Font")
(set-face-attribute 'default nil :height 190)
(set-face-attribute 'variable-pitch nil :family "Helvetica Neue")

(+theme-update)

;; Make the window-borders appear as padding instead. Not sure if this is really
;; usable yet, but it sure looks pretty.

(defun +sync-frame-parameters (&optional in-early-init)
  (modify-all-frames-parameters `((vertical-scroll-bars . nil)
                                  (right-divider-width . 24)
                                  (internal-border-width . 24)))

  ;; Themes aren't initialised until after early-init, so we can't access the
  ;; background colour yet.
  (unless in-early-init
    (let ((bg (face-attribute 'default :background)))
      (dolist (face '(fringe
                      window-divider
                      window-divider-first-pixel
                      window-divider-last-pixel))
        (face-spec-reset-face face)
        (set-face-foreground face bg)))))

(+sync-frame-parameters t)
(add-hook '+theme-changed-hook #'+sync-frame-parameters)
;; The minibuffer doesn't pick up the fringe parameters unless we sync again.
(add-hook 'after-init-hook #'+sync-frame-parameters)

;; Disable unneeded UI clutter

;; Take a cue from Doom's playbook and avoid calling the functions which can
;; trigger window-system redraws; instead, modify the frame parameters directly.

(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq menu-bar-mode nil)
(setq tool-bar-mode nil)
(setq scroll-bar-mode nil)
(setq frame-resize-pixelwise t)
(setq frame-inhibit-implied-resize t)

;; Mode & header
(setq-default mode-line-format "")
(setq-default header-line-format "")

;; Title bar
(when (eq system-type 'darwin)
  (setq frame-title-format nil)
  (setq ns-use-native-fullscreen nil)
  (when (executable-find "gls")
    (setq insert-directory-program "gls"))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . light)))

;;; Customise native compilation

;; Silence unactionable warnings.

(let ((ncomp-warn-level (if init-file-debug
                            t
                          'silent)))
  (setq native-comp-async-report-warnings-errors ncomp-warn-level)
  (setq native-comp-warning-on-missing-source ncomp-warn-level))

;; If native comp breaks, double check these paths
(setenv "LIBRARY_PATH" "/opt/homebrew/opt/gcc/lib/gcc/14:/opt/homebrew/opt/libgccjit/lib/gcc/14:/opt/homebrew/opt/gcc/lib/gcc/14/gcc/aarch64-apple-darwin23/14")

;; Always prompt for "y" or "n", rather than "yes" or "no".
(setq use-short-answers t)

;; For safety, don't treat space as a "y".
(define-key y-or-n-p-map (kbd "SPC") nil)

(setq inhibit-x-resources t)
(setq inhibit-startup-screen t)

(set-language-environment "UTF-8")

;; Banish the customisation interface to the shadow realm.
(setq custom-file (make-temp-file "emacs-custom-"))

;; Local Variables:
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
