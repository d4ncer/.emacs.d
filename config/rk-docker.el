;;; rk-docker.el --- Configuration for Dockerfiles.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)

(use-package dockerfile-mode
  :straight t
  :commands (dockerfile-mode
             dockerfile-build-buffer
             dockerfile-build-no-cache-buffer
             dockerfile-test-function)
  :mode (("Dockerfile\\'" . dockerfile-mode))
  :config
  (spacemacs-keys-set-leader-keys-for-major-mode 'dockerfile-mode
    "b" #'dockerfile-build-buffer
    "B" #'dockerfile-build-no-cache-buffer
    "t" #'dockerfile-test-function))

(provide 'rk-docker)

;;; rk-docker.el ends here
