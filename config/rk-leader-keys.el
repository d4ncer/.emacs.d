;;; rk-leader-keys.el --- Grab-bag for configuring general prefixed keys.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'definers)
(require 'rk-misc-utils)

;;; Misc functions

;;; Key definers

(use-package rk-delete-current-buffer-and-file
  :commands (rk-delete-current-buffer-and-file)
  :after projectile
  :preface
  (defun rk-leader-keys--invalidate-cache (_path)
    (when (and (featurep 'projectile) (projectile-project-p))
      (call-interactively #'projectile-invalidate-cache)))

  :config
  (add-hook 'rk-delete-current-buffer-and-file-functions #'rk-leader-keys--invalidate-cache))

(use-package which-key
  :straight t
  :custom
  (which-key-special-keys nil)
  (which-key-use-C-h-commands t)
  (which-key-echo-keystrokes 0.02)
  (which-key-max-description-length 32)
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-idle-delay 0.4)
  (which-key-allow-evil-operators t)

  :preface
  (defun rk-leader-keys-set-up-which-key-buffer (&rest _)
    (when-let (buf (get-buffer which-key-buffer-name))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (setq-local mode-line-format nil)
          (force-mode-line-update t)))))

  :config
  (advice-add 'which-key--create-buffer-and-show
              :after #'rk-leader-keys-set-up-which-key-buffer)


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

    "?"     '(describe-bindings :wk "describe bindings")

    "f"     '(:ignore t :wk "file")
    "f f"   '(find-file :wk "find file")
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
    "h d f" '(describe-function :wk "describe function")
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
  (which-key-mode +1))

(use-package rk-scale-font-transient-state
  :commands (rk-general--scale-font/body)
  :init
  (rk-leader-def
    "zx" '(rk-general--scale-font/body :wk "font scale hydra")))

(provide 'rk-leader-keys)

;;; rk-leader-keys.el ends here
