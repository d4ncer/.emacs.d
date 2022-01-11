;;; rk-pdf.el --- PDF stuff  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package pdf-tools
  :straight t
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)

  :custom
  (pdf-view-display-size 'fit-page)

  :init
  (use-package pdf-history
    :commands (pdf-history-minor-mode))
  (use-package pdf-occur
    :commands (pdf-occur-global-minor-mode))

  :config
  (require 'pdf-annot)
  (require 'pdf-sync)
  (require 'pdf-links)
  (require 'pdf-outline)
  (require 'pdf-history)
  (require 'pdf-cache)
  (require 'pdf-view)

  ;; Redefine a few macros as functions to work around byte compilation errors.
  (defun pdf-view-current-page (&optional window)
    (image-mode-window-get 'page window))

  (defun pdf-view-current-overlay (&optional window)
    (image-mode-window-get 'overlay window))

  (pdf-tools-install))

(provide 'rk-pdf)

;;; rk-pdf.el ends here
