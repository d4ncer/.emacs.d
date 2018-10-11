;;; rk-restclient.el --- Configuration for restclient-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'definers)

(use-package restclient
  :straight t
  :commands (restclient-mode
             restclient-http-send-current
             restclient-jump-next
             restclient-jump-prev
             restclient-http-send-current-stay-in-window)
  :general
  (:keymaps 'restclient-mode-map :states '(normal)
            "C-n" #'restclient-jump-next
            "C-p" #'restclient-jump-prev)
  :config
  (progn
    (rk-local-leader-def :keymaps 'restclient-mode-map
      "c" '(restclient-http-send-current :wk "execute current")
      "o" '(restclient-http-send-current-stay-in-window :wk "execute current (stay)"))

    (setq restclient-same-buffer-response-name "*restclient*")

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*restclient*" eos)
                   (display-buffer-reuse-window
                    display-buffer-pop-up-window)
                   (reusable-frames . visible)
                   (side            . bottom)
                   (window-height   . 0.66)))))


(provide 'rk-restclient)

;;; rk-restclient.el ends here
