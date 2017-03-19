;;; rk-smartparens.el --- Smartparens config.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)

(use-package smartparens
  :defer t

  :preface
  (progn
    (autoload 'thing-at-point-looking-at "thingatpt")
    (autoload 's-matches? "s")

    (defun rk-smartparens--this-command-is-eval-expression (&rest _)
      (equal this-command 'eval-expression))

    (defun rk-smartparens--org-skip-asterisk (_ mb me)
      (or (and (= (line-beginning-position) mb)
               (eq 32 (char-after (1+ mb))))
          (and (= (1+ (line-beginning-position)) me)
               (eq 32 (char-after me)))))

    (defun rk-smartparens--sp-for-eval-expression ()
      (when (eq this-command 'eval-expression)
        (smartparens-mode)))

    (defun rk-smartparens-add-space-after-sexp-insertion (id action _context)
      (when (eq action 'insert)
        (save-excursion
          (forward-char (sp-get-pair id :cl-l))
          (when (or (eq (char-syntax (following-char)) ?w)
                    (looking-at (sp--get-opening-regexp)))
            (insert " ")))))

    (defun rk-smartparens-web-mode-is-code-context (_id action _context)
      (and (eq action 'insert)
           (not (or (get-text-property (point) 'part-side)
                    (get-text-property (point) 'block-side)))))

    (defun rk-smartparens--web-mode-format-paren-after-keyword (_id action context)
      "Insert a space after some keywords."
      (when (and (equal action 'insert)
                 (equal context 'code)
                 (thing-at-point-looking-at
                  (rx symbol-start (or "=" "for" "of" "in"
                                       "if" "else" "while"
                                       "return"
                                       "yield" "yield*"
                                       "function" "function*")
                      (* space) "(")))
        (save-excursion
          (search-backward "(")
          (just-one-space))))

    (defun rk-smartparens-add-space-before-sexp-insertion (id action _context)
      (when (eq action 'insert)
        (save-excursion
          (backward-char (length id))
          (cond
           ((and (eq (preceding-char) ?$)
                 (equal id "{")))

           ((eq (char-syntax (preceding-char)) ?w)
            (just-one-space))

           ((and (looking-back (sp--get-closing-regexp) (line-beginning-position))
                 (not (eq (char-syntax (preceding-char)) ?')))
            (just-one-space)))))))

  :init
  (progn
    (add-hook 'prog-mode-hook #'smartparens-strict-mode)
    (add-hook 'text-mode-hook #'smartparens-strict-mode)
    (add-hook 'text-mode-hook #'smartparens-strict-mode)
    (add-hook 'minibuffer-setup-hook #'rk-smartparens--sp-for-eval-expression))

  :config
  (progn
    (setq sp-show-pair-delay 0.2)
    (setq sp-show-pair-from-inside t)
    (setq sp-cancel-autoskip-on-backward-movement nil)
    (setq sp-highlight-pair-overlay nil)
    (setq sp-highlight-wrap-overlay nil)
    (setq sp-highlight-wrap-tag-overlay nil)
    (setq sp-navigate-close-if-unbalanced t)
    (setq sp-message-width nil)

    (require 'smartparens-config)
    (require 'smartparens-scala)
    (require 'smartparens-rust)

    (bind-key [remap c-electric-backspace] 'sp-backward-delete-char smartparens-strict-mode-map)

    (with-eval-after-load 'evil
      (define-key (with-no-warnings evil-insert-state-map) ")" #'sp-up-sexp))

    ;; Configure global pairs.

    (sp-pair "`" "`"
             :bind "M-`")
    (sp-pair "{" "}"
             :bind "M-{"
             :pre-handlers '(rk-smartparens-add-space-before-sexp-insertion)
             :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
    (sp-pair "[" "]"
             :bind "M-["
             :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
    (sp-pair "(" ")"
             :bind "M-("
             :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
    (sp-pair "\"" "\""
             :bind "M-\""
             :pre-handlers '(:add (rk-smartparens-add-space-before-sexp-insertion)))

    ;; Configure local pairs.

    (sp-with-modes 'minibuffer-inactive-mode
      (sp-local-pair "'" nil :actions nil)
      (sp-local-pair "(" nil
                     :when (rk-smartparens--this-command-is-eval-expression)
                     :pre-handlers '(rk-smartparens-add-space-before-sexp-insertion)
                     :post-handlers '(rk-smartparens-add-space-after-sexp-insertion))
      (sp-local-pair "\"" nil
                     :when (rk-smartparens--this-command-is-eval-expression)
                     :pre-handlers '(rk-smartparens-add-space-before-sexp-insertion)
                     :post-handlers '(rk-smartparens-add-space-after-sexp-insertion)))

    (sp-with-modes sp-lisp-modes
      (sp-local-pair "(" nil
                     :pre-handlers '(rk-smartparens-add-space-before-sexp-insertion)
                     :post-handlers '(rk-smartparens-add-space-after-sexp-insertion))
      (sp-local-pair "[" nil
                     :pre-handlers '(rk-smartparens-add-space-before-sexp-insertion)
                     :post-handlers '(rk-smartparens-add-space-after-sexp-insertion))
      (sp-local-pair "\"" nil
                     :pre-handlers '(rk-smartparens-add-space-before-sexp-insertion)
                     :post-handlers '(rk-smartparens-add-space-after-sexp-insertion))
      (sp-local-pair "{" nil
                     :pre-handlers '(rk-smartparens-add-space-before-sexp-insertion)
                     :post-handlers '(rk-smartparens-add-space-after-sexp-insertion)))

    (sp-with-modes 'web-mode
      (sp-local-pair "<" nil :when '(rk-smartparens-web-mode-is-code-context)))

    (sp-with-modes 'rk-web-js-mode
      ;; Flow strict types
      (sp-local-pair "{|" "|}" :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
      (sp-local-pair "(" ")" :pre-handlers '(:add rk-smartparens--web-mode-format-paren-after-keyword)))

    (sp-with-modes 'org-mode
      (sp-local-pair "*" "*" :actions '(insert wrap) :unless '(sp-point-after-word-p sp-point-at-bol-p) :wrap "C-*" :skip-match 'rk-smartparens--org-skip-asterisk)
      (sp-local-pair "_" "_" :unless '(sp-point-after-word-p) :wrap "C-_")
      (sp-local-pair "/" "/" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "~" "~" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "=" "=" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "«" "»"))

      ;; Keybindings
      (spacemacs-keys-set-leader-keys
        ",A" #'sp-add-to-previous-sexp
        ",a" #'sp-add-to-next-sexp
        ",B" #'sp-backward-barf-sexp
        ",b" #'sp-forward-barf-sexp
        ",M" #'sp-backward-slurp-sexp
        ",m" #'sp-forward-slurp-sexp
        ",c" #'sp-convolute-sexp
        ",D" #'sp-backward-kill-sexp
        ",d" #'sp-kill-sexp
        ",e" #'sp-emit-sexp
        ",l" #'sp-end-of-sexp
        ",h" #'sp-beginning-of-sexp
        ",j" #'sp-join-sexp
        ",K" #'sp-splice-sexp-killing-backward
        ",k" #'sp-splice-sexp-killing-forward
        ",n" #'sp-next-sexp
        ",p" #'sp-previous-sexp
        ",r" #'sp-raise-sexp
        ",s" #'sp-splice-sexp-killing-around
        ",t" #'sp-transpose-sexp
        ",U" #'sp-backward-unwrap-sexp
        ",u" #'sp-unwrap-sexp
        ",w" #'sp-rewrap-sexp
        ",x" #'sp-split-sexp
        ",Y" #'sp-backward-copy-sexp
        ",y" #'sp-copy-sexp
        ",," #'sp-previous-sexp
        ",." #'sp-next-sexp
        ",<" #'sp-backward-down-sexp
        ",>" #'sp-down-sexp)

    ;; Enable modes.

    (smartparens-global-strict-mode +1)
    (show-smartparens-global-mode +1))

  :functions (sp-local-pair
              sp-pair
              sp-get-pair
              sp--get-opening-regexp
              sp--get-closing-regexp)
  :commands (smartparens-mode
             sp-up-sexp
             smartparens-strict-mode
             smartparens-global-strict-mode
             show-smartparens-global-mode))


(provide 'rk-smartparens)

;;; rk-smartparens.el ends here
