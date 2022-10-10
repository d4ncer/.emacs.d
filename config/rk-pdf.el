;;; rk-pdf.el --- PDF stuff  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package pdf-tools
  :straight t
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :general
  (:keymaps 'pdf-view-mode-map :states '(normal)
            "gp" #'pdf-view-goto-page)
  :preface
  (defun rk-pdf--unset-cursor ()
    (setq-local evil-normal-state-cursor (list nil)))
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

  (add-hook 'pdf-view-mode-hook #'rk-pdf--unset-cursor)

  ;; Enable hiDPI support, but at the cost of memory! See politza/pdf-tools#51
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)

  ;; Redefine a few macros as functions to work around byte compilation errors.
  (defun pdf-view-current-page (&optional window)
    (image-mode-window-get 'page window))

  (defun pdf-view-current-overlay (&optional window)
    (image-mode-window-get 'overlay window))

  (pdf-tools-install))

(use-package pdf-tools
  :straight t
  :after evil
  :preface
  (defun rk-pdf--get-desc (file page &optional text)
    (concat "p" page (when text (concat ";quoting:" text))))
  :custom
  (org-pdftools-get-desc-function #'rk-pdf--get-desc))

(provide 'rk-pdf)

;;; rk-pdf.el ends here
