;;; mod-languages.el --- Programming language configurations -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Raghuvir Kasturi
;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:
;; This module contains programming language configurations including:
;; - Lisp and Emacs Lisp
;; - Data formats (YAML, JSON)
;; - TypeScript/JavaScript
;; - Configuration files
;; - Markdown
;; - Elixir and Erlang
;; - Nix
;; - Tree-sitter mode remapping

;;; Code:

(eval-and-compile (require '+keybindings))

(eval-and-compile
  (require '+corelib))

;;; Lisp and Emacs Lisp

(use-package lisp-mode
  ;; General configuration for all derived lisp modes.
  :config
  (add-hook! '(lisp-data-mode-hook emacs-lisp-mode-hook)
    (add-hook 'before-save-hook #'check-parens nil t)))

(use-package elisp-mode
  :general-config (:keymaps 'emacs-lisp-mode-map "C-c RET" #'pp-macroexpand-last-sexp)

  ;; Make lambdas look like λ.
  :hook (emacs-lisp-mode-hook . prettify-symbols-mode)

  :config
  (+local-leader-set-key 'emacs-lisp-mode-map
    "e" '(nil :which-key "eval")
    "es" '(eval-last-sexp :wk "last sexp")
    "eb" (list (defun +eval-buffer ()
                 (interactive)
                 (let ((inhibit-redisplay t))
                   (call-interactively #'eval-buffer)
                   (message "Buffer evaluated" ))
                 (when pulsar-mode
                   (pulsar--create-pulse (cons (point-min) (point-max)) 'pulsar-yellow)))
               :wk "buffer"))

  :init
  (use-package checkdoc
    :custom
    (checkdoc-force-docstrings-flag nil))

  (use-package +elisp
    :general (:keymaps 'emacs-lisp-mode-map "C-c C-c" #'+elisp-eval-dwim)

    :config
    (+local-leader-set-key 'emacs-lisp-mode-map
      "ee" '(+elisp-eval-dwim :wk "dwim"))

    ;; Improve plist indentation
    :autoload +elisp--calculate-lisp-indent-a
    :init
    (advice-add #'calculate-lisp-indent :override #'+elisp--calculate-lisp-indent-a))

  :custom
  (define-advice eval-region (:around (fn &rest args) clear-visual-state)
    (unwind-protect (apply fn args)
      (when (eq evil-state 'visual)
        (evil-normal-state)))))

;;; Data formats

(use-package yaml-ts-mode
  :config
  (setq-hook! 'yaml-ts-mode-hook
    tab-width 2))

(use-package json-ts-mode)

(use-package kdl-mode
  :ensure (:host github :repo "taquangtrung/emacs-kdl-mode")
  :mode "\\.kdl\\'")

;;; TypeScript/JavaScript

(defun +deno-project-root (&optional directory)
  "Return the Deno project root containing DIRECTORY, if any."
  (locate-dominating-file
   (or directory default-directory)
   (lambda (directory)
     (or (file-exists-p (expand-file-name "deno.json" directory))
         (file-exists-p (expand-file-name "deno.jsonc" directory))))))

(defun +typescript-eglot-server (_interactive _project)
  "Select the Deno LSP for Deno projects, otherwise TypeScript LSP."
  (if (+deno-project-root)
      '("deno" "lsp")
    '("typescript-language-server" "--stdio")))

(with-eval-after-load 'eglot
  (setf (alist-get '((js-mode :language-id "javascript")
                     (js-ts-mode :language-id "javascript")
                     (tsx-ts-mode :language-id "typescriptreact")
                     (typescript-ts-mode :language-id "typescript")
                     (typescript-mode :language-id "typescript"))
                   eglot-server-programs nil nil #'equal)
        #'+typescript-eglot-server))

(use-package typescript-ts-mode
  :hook ((js-mode-hook js-ts-mode-hook tsx-ts-mode-hook
          typescript-mode-hook typescript-ts-mode-hook) . eglot-ensure)
  :config
  (pushnew! find-sibling-rules
            ;; Tests -> impl
            (list (rx (group (+? any)) (or ".test" ".integration") ".ts" eos)
                  (rx (backref 1) ".ts"))
            ;; Impl -> tests
            (list (rx (group (+? any)) ".ts" eos)
                  (rx (backref 1) ".test.ts")
                  (rx (backref 1) ".integration.ts"))))

;;; Configuration and Markup

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :custom (markdown-fontify-code-block-natively t)
  :init (setq markdown-command "multimarkdown")
  :hook (markdown-ts-mode-hook . visual-line-mode)
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

;;;###autoload
(define-derived-mode may-i-config-mode lisp-data-mode "Lisp-Data [may-i]"
  "Major-mode for may-i configuration files."
  (setcar font-lock-defaults
          (append '(lisp-el-font-lock-keywords
                    lisp-el-font-lock-keywords-1
                    lisp-el-font-lock-keywords-2)
                  (when (boundp 'elisp-semantic-font-lock-keywords)
                    '(elisp-semantic-font-lock-keywords)))))

;;;###autoload
(add-to-list 'auto-mode-alist (cons (rx "/may-i/config.lisp" eos)
                                    'may-i-config-mode))

(put 'defcontext 'lisp-indent-function 1)
(put 'with-facts 'lisp-indent-function 1)

;;; Elixir and Erlang

(use-package elixir-ts-mode
  :mode ("\\.ex\\'" "\\.exs\\'")
  :hook ((elixir-ts-mode-hook heex-ts-mode-hook) . eglot-ensure)
  :config
  ;; (with-eval-after-load 'eglot
  ;;   (let ((+elixir-ls-bin (file-name-concat user-emacs-directory "var/lsp-servers/elixir-ls/language_server.sh")))
  ;;     (add-to-list 'eglot-server-programs `((elixir-mode elixir-ts-mode heex-ts-mode) ,+elixir-ls-bin))))
  (with-eval-after-load 'eglot
    (setf (alist-get '(elixir-mode elixir-ts-mode heex-ts-mode)
                     eglot-server-programs
                     nil nil #'equal)
          (eglot-alternatives
           `(("expert" "--stdio")                        ; Project-local via PATH/Nix
             (,(expand-file-name "~/.local/bin/expert-ls") "--stdio")   ; Global fallback
             "start_lexical.sh"))))

  ;; Switching between files & tests

  (pushnew! find-sibling-rules
            ;; Impl -> tests
            (list (rx (group-n 1 (+? nonl)) "/lib/" (group-n 2 (+? any)) ".ex" eos)
                  (rx (backref 1) "/test/" (backref 2) "_test.exs"))

            ;; Tests -> impl
            (list (rx (group-n 1 (+? nonl)) "/test/" (group-n 2 (+? any)) "_test.exs" eos)
                  (rx (backref 1) "/lib/" (backref 2) ".ex"))))

(use-package +expert-compat
  :ensure nil
  :after eglot
  :demand t
  :config
  (+expert-compat-mode 1))

(use-package inf-elixir :ensure t)

(use-package erlang :ensure t
  :mode (("\\.erl\\'" . erlang-mode)
         ("\\.hrl\\'" . erlang-mode))
  :init
  (with-eval-after-load 'dired
    (pushnew! completion-ignored-extensions ".jam" ".vee" ".beam"))
  (with-eval-after-load 'dired-x
    (pushnew! dired-omit-extensions ".jam" ".vee" ".beam")))

;;; Nix

(use-package nix-ts-mode
  :ensure (:files ("nix-ts-mode.el"))
  :commands nix-ts-mode
  :mode ("\\.nix\\'" . +nix-mode)
  :preface
  (defun +nix-mode ()
    "Select Nix tree-sitter mode after ensuring its grammar is installed."
    (interactive)
    (if (treesit-ensure-installed 'nix)
        (nix-ts-mode)
      (conf-unix-mode))))

;;; Bazel

(use-package bazel :ensure t
  ;; Major modes for BUILD/WORKSPACE/MODULE/.bzl files plus bazelrc &
  ;; bazelignore, with build/test/run commands, buildifier integration
  ;; (flymake + apheleia), xref for labels and completion-at-point.
  ;;
  ;; The package registers its own `auto-mode-alist' entries via autoloads,
  ;; so only the commands need listing here.
  :commands (bazel-build
             bazel-test
             bazel-run
             bazel-coverage
             bazel-test-at-point
             bazel-compile-current-file
             bazel-buildifier
             bazel-find-build-file
             bazel-find-workspace-file
             bazel-find-module-file
             bazel-show-consuming-target
             bazel-find-project)
  :preface
  (defun +bazel-find-project (directory)
    "Return the Bazel project for DIRECTORY, if it is inside a workspace.
Cheap wrapper around `bazel-find-project' that avoids loading the package
unless a workspace marker file exists above DIRECTORY."
    (when (locate-dominating-file
           directory
           (lambda (dir)
             (seq-some (lambda (file) (file-exists-p (expand-file-name file dir)))
                       '("MODULE.bazel" "WORKSPACE.bazel" "WORKSPACE" "WORKSPACE.bzlmod"))))
      (bazel-find-project directory)))
  :init
  ;; Let project.el (and therefore eglot) treat a Bazel workspace as a project
  ;; even when it isn't the VC root.
  (add-hook 'project-find-functions #'+bazel-find-project)
  :config
  ;; Formatting on save is handled by apheleia's buildifier formatter.
  (setq bazel-buildifier-before-save nil)

  ;; `bazel-mode-map' is inherited by all the derived modes
  ;; (bazel-build-mode, bazel-workspace-mode, bazel-starlark-mode, ...).
  (+local-leader-set-key 'bazel-mode-map
    "b" '(bazel-build :wk "build")
    "t" '(bazel-test :wk "test")
    "T" '(bazel-test-at-point :wk "test at point")
    "r" '(bazel-run :wk "run")
    "c" '(bazel-coverage :wk "coverage")
    "C" '(bazel-compile-current-file :wk "compile file")
    "f" '(bazel-buildifier :wk "buildifier")
    "g" '(nil :which-key "goto")
    "gb" '(bazel-find-build-file :wk "BUILD file")
    "gw" '(bazel-find-workspace-file :wk "WORKSPACE file")
    "gm" '(bazel-find-module-file :wk "MODULE file")
    "gc" '(bazel-show-consuming-target :wk "consuming target")))

;;; Third-party tree-sitter modes

(with-eval-after-load 'treesit
  (add-to-list 'major-mode-remap-alist '(nix-mode . +nix-mode))
  ;; js2-mode is a third-party entry absent from the native mapping table.
  (add-to-list 'major-mode-remap-alist '(js2-mode . js-ts-mode)))

;; Make shell-scripts etc executable on save.
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(provide 'mod-languages)
;;; mod-languages.el ends here
