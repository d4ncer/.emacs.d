;;; rk-smartparens.el --- Smartparens config.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'definers)
(require 'general)
(require 'thingatpt)
(require 's)

(use-package smartparens
  :straight t
  :demand t

  :hook ((prog-mode . smartparens-strict-mode)
         (eshell-mode . smartparens-strict-mode)
         (text-mode . smartparens-strict-mode))

  :general
  (:keymaps 'smartparens-strict-mode-map [remap c-electric-backspace] #'sp-backward-delete-char)
  (:states 'insert ")" #'sp-up-sexp)
  (:states 'normal "D" #'sp-kill-hybrid-sexp)

  :preface
  (progn
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

    (defun rk-smartparens--k&r-curlies (&rest _)
      (save-excursion
        (search-backward "{")
        (newline-and-indent))
      (save-excursion
        (newline-and-indent))
      (indent-according-to-mode))

    (defun rk-smartparens--add-space-after-sexp-insertion (id action _context)
      (when (eq action 'insert)
        (save-excursion
          (forward-char (sp-get-pair id :cl-l))
          (when (or (eq (char-syntax (following-char)) ?w)
                    (looking-at (sp--get-opening-regexp)))
            (insert " ")))))

    (defun rk-smartparens--web-mode-is-code-context (_id action _context)
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

    (defun rk-smartparens--delete-horizontal-space-for-delete (f &rest args)
      "Perform context-sensitive whitespace cleanups when deleting."
      (-let* ((line-before-pt (buffer-substring (line-beginning-position) (point)))
              (line-after-pt (buffer-substring (point) (line-end-position)))

              ((&plist :beg beg :end end :op op :cl cl) (sp-get-enclosing-sexp))
              (inside-start (when op (+ beg (length op))))
              (inside-end   (when op (- end (length cl))))
              (inside       (when op
                              (concat (buffer-substring inside-start (point))
                                      (buffer-substring (point) inside-end)))))
        (cond
         ;; Collapse horizontal space in empty pairs.
         ;;
         ;; [  |  ] -> [|]
         ;;
         ((when op (string-match-p (rx bos (+ space) eos) inside))
          (delete-region inside-start inside-end))

         ;; Delete contents for multiline pairs that were just inserted, e.g. braces.
         ;;
         ;; {
         ;;   |
         ;; }
         ;;
         ;; ->
         ;;
         ;; {|}
         ((when op (string-match-p (rx bos (* space) "\n" (* space) "\n" (* space) eos) inside))
          (delete-region inside-start inside-end))

         ;; Delete back from end of the line.
         ;;
         ;;
         ;; foo |
         ;; ->
         ;; foo|

         ;; foo      |
         ;; ->
         ;; foo |
         ((string-empty-p line-after-pt)
          (if (string-match-p (rx space space eos) line-before-pt)
              (while (looking-back (rx space space) (line-beginning-position))
                (delete-char -1))
            (funcall f args)))

         ;; Don't aggressively delete whitespace if there's a comment
         ;; following pt.
         ;;
         ;;
         ;; foo |  // bar
         ;;
         ;; ->
         ;;
         ;; foo|  // bar
         ;;
         ((string-match-p (rx (* nonl) (syntax comment-start)) line-after-pt)
          (funcall f args))

         ;; Collapse surrounding space, but preserve padding inside pairs.
         ;;
         ;; foo | bar -> foo|bar
         ;;
         ;; foo | }   -> foo| }
         ;;
         ((and (string-match-p (rx (or bol (not space)) space eos) line-before-pt)
               (string-match-p (rx bos space (or eol (not space))) line-after-pt))
          (let ((backward-only? (when inside (string-match-p (rx bos space) inside))))
            (delete-horizontal-space backward-only?)))

         ;; Delete if there is a single preceding space.
         ;;
         ;; foo |bar -> foo|bar
         ;;
         ;; but not:
         ;;
         ;; foo| bar -> foo|bar
         ;;
         ((and (string-match-p (rx (or bol (not space)) space eos) line-before-pt)
               (string-match-p (rx bos (not space)) line-after-pt))
          (delete-char -1))

         ;; Delete surrounding whitespace beyond a certain length.
         ;;
         ;; foo    |bar      -> foo |bar
         ;; foo    |    bar  -> foo | bar
         ((string-match-p (rx (+ space) eos) line-before-pt)
          (let ((has-space? (eq (char-after) ? )))
            (skip-chars-forward " ")
            (while (looking-back (rx space space) (line-beginning-position))
              (delete-char -1))
            (when has-space?
              (insert " ")
              (forward-char -1))))

         (t
          (funcall f args)))))

    (defun rk-smartparens--add-space-before-sexp-insertion (id action _context)
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
  (add-hook 'minibuffer-setup-hook #'rk-smartparens--sp-for-eval-expression)

  :config
  (progn
    (general-setq sp-show-pair-delay 0.2
                  sp-show-pair-from-inside t
                  sp-cancel-autoskip-on-backward-movement nil
                  sp-highlight-pair-overlay nil
                  sp-highlight-wrap-overlay nil
                  sp-highlight-wrap-tag-overlay nil
                  sp-navigate-close-if-unbalanced t
                  sp-message-width nil)

    (require 'smartparens-config)
    (require 'smartparens-scala)
    (require 'smartparens-rust)

    ;; Configure global pairs.

    (sp-pair "`" "`"
             :bind "M-`")
    (sp-pair "{" "}"
             :bind "M-{"
             :pre-handlers '(rk-smartparens--add-space-before-sexp-insertion)
             :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
    (sp-pair "[" "]"
             :bind "M-["
             :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
    (sp-pair "(" ")"
             :bind "M-("
             :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
    (sp-pair "\"" "\""
             :bind "M-\""
             :pre-handlers '(:add (rk-smartparens--add-space-before-sexp-insertion)))

    ;; Configure local pairs.

    (sp-with-modes 'minibuffer-inactive-mode
      (sp-local-pair "'" nil :actions nil)
      (sp-local-pair "(" nil
                     :when (rk-smartparens--this-command-is-eval-expression)
                     :pre-handlers '(rk-smartparens--add-space-before-sexp-insertion)
                     :post-handlers '(rk-smartparens--add-space-after-sexp-insertion))
      (sp-local-pair "\"" nil
                     :when (rk-smartparens--this-command-is-eval-expression)
                     :pre-handlers '(rk-smartparens--add-space-before-sexp-insertion)
                     :post-handlers '(rk-smartparens--add-space-after-sexp-insertion)))

    (sp-with-modes (cons 'eshell-mode sp-lisp-modes)
      (sp-local-pair "(" nil
                     :pre-handlers '(rk-smartparens--add-space-before-sexp-insertion)
                     :post-handlers '(rk-smartparens--add-space-after-sexp-insertion))
      (sp-local-pair "[" nil
                     :pre-handlers '(rk-smartparens--add-space-before-sexp-insertion)
                     :post-handlers '(rk-smartparens--add-space-after-sexp-insertion))
      (sp-local-pair "\"" nil
                     :pre-handlers '(rk-smartparens--add-space-before-sexp-insertion)
                     :post-handlers '(rk-smartparens--add-space-after-sexp-insertion))
      (sp-local-pair "{" nil
                     :pre-handlers '(rk-smartparens--add-space-before-sexp-insertion)
                     :post-handlers '(rk-smartparens--add-space-after-sexp-insertion)))

    (sp-with-modes 'haskell-mode
      (sp-local-pair "(" nil
                     :pre-handlers '(rk-smartparens--add-space-before-sexp-insertion)
                     :post-handlers '(rk-smartparens--add-space-after-sexp-insertion))
      (sp-local-pair "[" nil
                     :pre-handlers '(rk-smartparens--add-space-before-sexp-insertion)
                     :post-handlers '(rk-smartparens--add-space-after-sexp-insertion))
      (sp-local-pair "{" nil
                     :pre-handlers '(rk-smartparens--add-space-before-sexp-insertion)
                     :post-handlers '(rk-smartparens--add-space-after-sexp-insertion))
      (sp-local-pair "[|" "|]" :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
      (sp-local-pair "{-" "-}" :post-handlers '(("||\n[i]" "RET") ("- " "SPC")))
      (sp-local-pair "{-#" "#-}" :post-handlers '(("||\n[i]" "RET") ("-# " "SPC"))))

    (sp-with-modes 'web-mode
      (sp-local-pair "<" nil :when '(rk-smartparens--web-mode-is-code-context)))

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

    ;; Delete enclosing whitespace if necessary.

    (advice-add 'sp-backward-delete-char :around #'rk-smartparens--delete-horizontal-space-for-delete)

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
