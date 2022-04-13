;;; rk-web-modes.el --- Major modes derived from web-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(require 'web-mode)
(require 'js)

;;;###autoload
(define-derived-mode rk-web-js-mode js-mode "JS"
  "Derived mode for editing JavaScript files.")

;;;###autoload
(define-derived-mode rk-web-html-mode web-mode "HTML"
  "Derived mode for editing HTML files."
  (setq-local web-mode-content-type "html"))

;;;###autoload
(define-derived-mode rk-web-css-mode web-mode "CSS"
  "Derived mode for editing CSS files."
  (setq-local web-mode-content-type "css"))

(provide 'rk-web-modes)

;;; rk-web-modes.el ends here
