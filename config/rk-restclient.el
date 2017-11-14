;;; rk-restclient.el --- Configuration for restclient-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)

(use-package restclient
  :commands (restclient-mode
             restclient-http-send-current
             restclient-http-send-current-stay-in-window)
  :config
  (progn
    (spacemacs-keys-set-leader-keys-for-major-mode 'restclient-mode
      "c" #'restclient-http-send-current
      "o" #'restclient-http-send-current-stay-in-window)

    (setq restclient-same-buffer-response-name "*restclient*")

    (with-eval-after-load 'which-key
      (with-no-warnings
        (push `((nil . ,(rx bos "restclient-http-" (group (+ nonl)))) . (nil . "\\1"))
              which-key-replacement-alist)))

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*restclient*" eos)
                   (display-buffer-reuse-window
                    display-buffer-pop-up-window)
                   (reusable-frames . visible)
                   (side            . bottom)
                   (window-height   . 0.66)))))


(provide 'rk-restclient)

;;; rk-restclient.el ends here
