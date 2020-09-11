;;; rk-company.el --- Startup file for Emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'general)

(use-package company
  :straight t
  :commands (global-company-mode)

  :general
  (:keymaps 'company-active-map
            "C-<return>" #'company-complete-selection)

  :commands (company-select-next-or-abort
             company-select-previous-or-abort
             company-show-doc-buffer)
  :custom
  (company-tooltip-align-annotations t)
  (company-idle-delay 0)
  (company-require-match nil)

  :init
  (add-hook 'after-init-hook #'global-company-mode)

  :config
  (dolist (map (list company-active-map company-search-map company-mode-map))
    (general-def map "C-j" #'company-select-next-or-abort)
    (general-def map "C-k" #'company-select-previous-or-abort)
    (general-def map "C-h" #'company-show-doc-buffer)
    (general-def map "C-w" nil)))

(use-package company-dabbrev
  :after company
  :config
  (progn
    (setq company-dabbrev-ignore-case nil)
    (setq company-dabbrev-downcase nil)))

(use-package company-box
  :straight t
  :after company
  :hook
  (company-mode . company-box-mode)
  :preface
  (defun rk-company-box-icons--yasnippet (candidate)
    (when (get-text-property 0 'yas-annotation candidate)
      'Yasnippet))

  (defun rk-company-box-icons--elisp (candidate)
    (when (derived-mode-p 'emacs-lisp-mode)
      (let ((sym (intern candidate)))
        (cond ((fboundp sym)  'ElispFunction)
              ((boundp sym)   'ElispVariable)
              ((featurep sym) 'ElispFeature)
              ((facep sym)    'ElispFace)))))
  :config
  (general-setq company-box-show-single-candidate t
                company-box-doc-frame-parameters '((internal-border-width . 1))
                company-box-backends-colors nil
                company-box-max-candidates 50
                company-box-icons-alist 'company-box-icons-all-the-icons
                company-box-icons-functions
                '(rk-company-box-icons--yasnippet company-box-icons--lsp rk-company-box-icons--elisp company-box-icons--acphp)
                company-box-icons-all-the-icons
                `((Unknown       . ,(all-the-icons-material "find_in_page"             :height 0.8 :face 'all-the-icons-purple))
                  (Text          . ,(all-the-icons-material "text_fields"              :height 0.8 :face 'all-the-icons-green))
                  (Method        . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))
                  (Function      . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))
                  (Constructor   . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))
                  (Field         . ,(all-the-icons-material "build"                    :height 0.8 :face 'all-the-icons-blue))
                  (Variable      . ,(all-the-icons-material "adjust"                   :height 0.8 :face 'all-the-icons-blue))
                  (Class         . ,(all-the-icons-material "class"                    :height 0.8 :face 'all-the-icons-red))
                  (Interface     . ,(all-the-icons-material "settings_input_component" :height 0.8 :face 'all-the-icons-red))
                  (Module        . ,(all-the-icons-material "view_module"              :height 0.8 :face 'all-the-icons-red))
                  (Property      . ,(all-the-icons-material "build"                    :height 0.8 :face 'all-the-icons-blue))
                  (Unit          . ,(all-the-icons-material "straighten"               :height 0.8 :face 'all-the-icons-red))
                  (Value         . ,(all-the-icons-material "filter_1"                 :height 0.8 :face 'all-the-icons-red))
                  (Enum          . ,(all-the-icons-material "plus_one"                 :height 0.8 :face 'all-the-icons-red))
                  (Keyword       . ,(all-the-icons-material "filter_center_focus"      :height 0.8 :face 'all-the-icons-red))
                  (Snippet       . ,(all-the-icons-material "short_text"               :height 0.8 :face 'all-the-icons-red))
                  (Color         . ,(all-the-icons-material "color_lens"               :height 0.8 :face 'all-the-icons-red))
                  (File          . ,(all-the-icons-material "insert_drive_file"        :height 0.8 :face 'all-the-icons-red))
                  (Reference     . ,(all-the-icons-material "collections_bookmark"     :height 0.8 :face 'all-the-icons-red))
                  (Folder        . ,(all-the-icons-material "folder"                   :height 0.8 :face 'all-the-icons-red))
                  (EnumMember    . ,(all-the-icons-material "people"                   :height 0.8 :face 'all-the-icons-red))
                  (Constant      . ,(all-the-icons-material "pause_circle_filled"      :height 0.8 :face 'all-the-icons-red))
                  (Struct        . ,(all-the-icons-material "streetview"               :height 0.8 :face 'all-the-icons-red))
                  (Event         . ,(all-the-icons-material "event"                    :height 0.8 :face 'all-the-icons-red))
                  (Operator      . ,(all-the-icons-material "control_point"            :height 0.8 :face 'all-the-icons-red))
                  (TypeParameter . ,(all-the-icons-material "class"                    :height 0.8 :face 'all-the-icons-red))
                  (Yasnippet     . ,(all-the-icons-material "short_text"               :height 0.8 :face 'all-the-icons-green))
                  (ElispFunction . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))
                  (ElispVariable . ,(all-the-icons-material "check_circle"             :height 0.8 :face 'all-the-icons-blue))
                  (ElispFeature  . ,(all-the-icons-material "stars"                    :height 0.8 :face 'all-the-icons-orange))
                  (ElispFace     . ,(all-the-icons-material "format_paint"             :height 0.8 :face 'all-the-icons-pink)))))

(provide 'rk-company)

;;; rk-company.el ends here
