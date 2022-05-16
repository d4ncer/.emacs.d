;;; rk-java.el --- Basic Java config  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'f)
(require 'gnus)

(defvar rk-java--lombok-version "1.18.22")
(defvar rk-java--lombok-path
  (f-join gnus-home-directory (format ".m2/repository/org/projectlombok/lombok/%s/lombok-%s.jar" rk-java--lombok-version rk-java--lombok-version)))

(use-package lsp-java
  :straight t
  :after lsp
  :config
  ;; (setq lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx1G" "-Xms100m"))
  ;; (setq lsp-java-vmargs
  ;;       `("-noverify"
  ;;         "-XX:+UseParallelGC"
  ;;         "-XX:GCTimeRatio=4"
  ;;         "-XX:AdaptiveSizePolicyWeight=90"
  ;;         "-Dsun.zip.disableMemoryMapping=true"
  ;;         "-Xmx1G"
  ;;         "-Xms100m"
  ;;         ,(format "-javaagent:%s" rk-java--lombok-path)
  ;;         ,(format "-Xbootclasspath/a:%s" rk-java--lombok-path)))
  (setq lsp-java-vmargs
        `("-noverify"
          "-XX:+UseParallelGC"
          "-XX:GCTimeRatio=4"
          "-XX:AdaptiveSizePolicyWeight=90"
          "-Dsun.zip.disableMemoryMapping=true"
          "-Xmx1G"
          "-Xms100m"
          ,(format "-javaagent:%s" rk-java--lombok-path)))
  (add-hook 'java-mode-hook #'lsp))

(provide 'rk-java)

;;; rk-java.el ends here
