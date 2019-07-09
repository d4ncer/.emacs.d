;;; rk-leader-keys.el --- Grab-bag for configuring general prefixed keys.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'definers)
(require 'subr-x)
(require 'rk-misc-utils)

(autoload 'org-narrow-to-subtree "org")

;;; Misc functions

;;; Key definers

(use-package rk-delete-current-buffer-and-file
  :commands (rk-delete-current-buffer-and-file)
  :preface
  (progn
    (autoload 'projectile-invalidate-cache "projectile")
    (autoload 'projectile-project-p "projectile")

    (defun rk-leader-keys--invalidate-cache (_path)
      (when (and (featurep 'projectile) (projectile-project-p))
        (call-interactively #'projectile-invalidate-cache))))

  :config
  (add-hook 'rk-delete-current-buffer-and-file-functions #'rk-leader-keys--invalidate-cache))

(use-package which-key
  :straight t
  :preface
  (progn
    (autoload 'which-key-mode "which-key")
    (autoload 'which-key-add-key-based-replacements "which-key")

    (defun rk-leader-keys-set-up-which-key-buffer (&rest _)
      (when-let (buf (get-buffer which-key-buffer-name))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (setq-local mode-line-format nil)
            (setq-local header-line-format nil)
            (force-mode-line-update))))))
  :config
  (progn
    (general-setq which-key-popup-type 'side-window
                  which-key-side-window-location 'right)
    (general-setq which-key-special-keys nil)
    (general-setq which-key-use-C-h-commands t)
    (general-setq which-key-echo-keystrokes 0.02)
    (general-setq which-key-max-description-length 32)
    (general-setq which-key-sort-order 'which-key-key-order-alpha)
    (general-setq which-key-idle-delay 0.4)
    (general-setq which-key-allow-evil-operators t)

    (advice-add 'which-key--create-buffer-and-show
                :after #'rk-leader-keys-set-up-which-key-buffer)

    (which-key-mode +1)))

(rk-leader-def
  "u"     '(universal-argument :wk "universal arg.")
  "SPC"   '(execute-extended-command :wk "M-x")
  "TAB"   '(rk-alternate-buffer :wk "last buffer")
  "|"     '(rk-toggle-window-split :wk "transpose windows")
  "!"     '(shell-command :wk "shell cmd")

  ","     '(:ignore t :wk "sparens")

  "a"     '(:ignore t :wk "applications")
  "a m"   '(:ignore t :wk "misc")
  "a m t" '(rk-insert-iso-timestamp :wk "insert ISO timestap")

  "b"     '(:ignore t :wk "buffer")
  "b a"   '(mark-whole-buffer :wk "select all")
  "b b"   '(bury-buffer :wk "bury buffer")
  "b d"   '(kill-this-buffer :wk "kill buffer")
  "b e"   '(erase-buffer :wk "erase buffer")
  "b Y"   '(rk-copy-whole-buffer-to-clipboard :wk "copy buffer")
  "b P"   '(rk-copy-clipboard-to-whole-buffer :wk "replace buffer with clipboard")

  "C"     '(compile :wk "compile")
  "c"     '(:ignore t :wk "comments")
  "c r"   '(comment-or-uncomment-region :wk "toggle region comments")

  "e"     '(:ignore t :wk "errors")

  "f"     '(:ignore t :wk "file")
  "f d"   '(dired :wk "dired")
  "f D"   '(rk-delete-current-buffer-and-file :wk "delete file & buffer")
  "f F"   '(find-file-other-window :wk "open file (new window)")
  "f R"   '(rk-rename-file-and-buffer :wk "rename file")
  "f e"   '(rk-sudo-edit :wk "edit w/ sudo")
  "f f"   '(find-file :wk "find file")
  "f m"   '(mkdir :wk "create dir")
  "f s"   '(save-buffer :wk "save buffer")
  "f S"   '(save-some-buffers :wk "save buffers")
  "f W"   '(write-file :wk "write file")
  "f v"   '(rk-reload-file :wk "reload file")
  "f y"   '(rk-copy-buffer-path :wk "copy buffer path")

  "g"     '(:ignore t :wk "goto/git")
  "g m"   '(rk-goto-messages :wk "goto *Messages*")
  "g l"   '(rk-goto--base-ledger :wk "goto ledger")

  "h"     '(:ignore t :wk "help/info")
  "h d"   '(:ignore t :wk "describe")
  "h d C" '(rk-get-face-at-point :wk "face at point")
  "h d c" '(describe-face :wk "describe face")
  "h d k" '(describe-key :wk "describe key")
  "h d m" '(describe-mode :wk "describe mode")
  "h d v" '(describe-variable :wk "describe variable")
  "h f"   '(:ignore t :wk "find")
  "h f c" '(find-face-definition :wk "find face def")
  "h f f" '(find-function :wk "find function")
  "h f l" '(find-library :wk "find library")
  "h f v" '(find-variable :wk "find variable")
  "h i"   '(info :wk "info")

  "i"     '(:ignore t :wk "info")

  "k"     '(:ignore t :wk "kill")
  "k b"   '(kill-this-buffer :wk "kill buffer")
  "k w"   '(delete-window :wk "kill window")

  "n"     '(:ignore t :wk "narrow/widen")
  "n f"   '(narrow-to-defun :wk "narrow to function")
  "n r"   '(narrow-to-region :wk "narrow to region")
  "n s"   '(org-narrow-to-subtree :wk "narrow to subtree")
  "n w"   '(widen :wk "widen")

  "o"     '(:ignore t :wk "org")

  "p"     '(:ignore t :wk "project")

  "q"     '(:ignore t :wk "quit")
  "q q"   '(save-buffers-kill-emacs :wk "quit emacs")

  "s"     '(:ignore t :wk "search")

  "t"     '(:ignore t :wk "text")

  "T"     '(:ignore t :wk "toggle")
  "T F"   '(toggle-frame-fullscreen :wk "FULLSCREEN")
  "T f"   '(toggle-frame-maximized :wk "fullscreen")

  "w"     '(:ignore t :wk "window")
  "w ="   '(balance-windows :wk "balance windows")
  "w o"   '(delete-other-windows :wk "kill other windows")
  "w q"   '(delete-window :wk "kill window")

  "x"     '(:ignore t :wk "processes")

  "y"     '(:ignore t :wk "yasnippet")

  "z"     '(:ignore t :wk "scale"))

(use-package rk-scale-font-transient-state
  :commands (rk-scale-font-transient-state/body)
  :init
  (rk-leader-def
    "zx" '(rk-scale-font-transient-state/body :wk "font scale hydra")))

(use-package rk-buffer-transient-state
  :commands (rk-buffer-transient-state/body
             rk-buffer-transient-state/next-buffer
             rk-buffer-transient-state/previous-buffer)
  :init
  (rk-leader-def
    "bn" '(rk-buffer-transient-state/next-buffer :wk "next buffer")
    "bN" '(rk-buffer-transient-state/previous-buffer :wk "prev buffer")
    "bp" '(rk-buffer-transient-state/previous-buffer :wk "prev buffer")))


(provide 'rk-leader-keys)

;;; rk-leader-keys.el ends here
