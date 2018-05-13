;;; rk-coffeescript.el --- Configuration for CoffeeScript.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package coffee-mode
  :straight t
  :defer t
  :mode
  (("\\.coffee\\'" . coffee-mode)
   ("\\.iced\\'" . coffee-mode)
   ("Cakefile\\'" . coffee-mode)
   ("\\.cson\\'" . coffee-mode))

  :defines (coffee-tab-width)
  :interpreter ("coffee" . coffee-mode)
  :config
  (progn
    (setq coffee-tab-width 2)
    (setq coffee-indent-like-python-mode t)))

(provide 'rk-coffeescript)

;;; rk-coffeescript.el ends here
