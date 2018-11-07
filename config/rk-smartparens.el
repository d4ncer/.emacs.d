;;; rk-smartparens.el --- Smartparens config.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'definers)
(require 'general)

(use-package smartparens
  :straight t
  :defer t

  :general
  (:keymaps 'prog-mode-map :states 'insert
            ")" #'sp-up-sexp)
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
    (rk-leader-def
      ",A" '(sp-add-to-previous-sexp :wk "add to previous sexp")
      ",a" '(sp-add-to-next-sexp :wk "add to next sexp")
      ",B" '(sp-backward-barf-sexp :wk "backward barf")
      ",b" '(sp-forward-barf-sexp :wk "forward barf")
      ",M" '(sp-backward-slurp-sexp :wk "backward slurp")
      ",m" '(sp-forward-slurp-sexp :wk "forward slurp")
      ",c" '(sp-convolute-sexp :wk "convolute")
      ",D" '(sp-backward-kill-sexp :wk "kill backwards")
      ",d" '(sp-kill-sexp :wk "kill")
      ",e" '(sp-emit-sexp :wk "emit")
      ",l" '(sp-end-of-sexp :wk "goto end of sexp")
      ",h" '(sp-beginning-of-sexp :wk "goto beginning of sexp")
      ",j" '(sp-join-sexp :wk "join")
      ",K" '(sp-splice-sexp-killing-backward :wk "splice killing backwards")
      ",k" '(sp-splice-sexp-killing-forward :wk "splice killing forwards")
      ",n" '(sp-next-sexp :wk "next")
      ",p" '(sp-previous-sexp :wk "prev")
      ",r" '(sp-raise-sexp :wk "raise")
      ",s" '(sp-splice-sexp-killing-around :wk "splice killing around")
      ",t" '(sp-transpose-sexp :wk "transpose")
      ",U" '(sp-backward-unwrap-sexp :wk "backward unwrap")
      ",u" '(sp-unwrap-sexp :wk "unwrap")
      ",w" '(sp-rewrap-sexp :wk "rewrap")
      ",x" '(sp-split-sexp :wk "split")
      ",Y" '(sp-backward-copy-sexp :wk "copy backwards")
      ",y" '(sp-copy-sexp :wk "copy")
      ",," '(sp-previous-sexp :wk "prev")
      ",." '(sp-next-sexp :wk "next")
      ",<" '(sp-backward-down-sexp :wk "go down backwards")
      ",>" '(sp-down-sexp :wk "go down"))

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
