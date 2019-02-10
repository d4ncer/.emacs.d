;;; rk-docker.el --- Configuration for Dockerfiles.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'definers)

(use-package dockerfile-mode
  :straight t
  :commands (dockerfile-mode
             dockerfile-build-buffer
             dockerfile-build-no-cache-buffer
             dockerfile-test-function)
  :mode (("Dockerfile\\'" . dockerfile-mode))
  :config
  (rk-local-leader-def :keymaps 'dockerfile-mode-map
    "b" '(dockerfile-build-buffer :wk "build")
    "B" '(dockerfile-build-no-cache-buffer :wk "build w/o cache")
    "t" '(dockerfile-test-function :wk "test")))

(use-package docker
  :straight t
  :init
  (rk-leader-def
    "ad" '(docker :wk "docker")))

(provide 'rk-docker)

;;; rk-docker.el ends here
