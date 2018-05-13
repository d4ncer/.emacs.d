;;; rk-scala.el --- Configuration for Scala packages.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'rk-emacs)
(require 'dash)
(require 'dash-functional)
(require 'f)
(require 's)
(require 'spacemacs-keys)
(require 'subr-x)

(autoload 'evil-define-key "evil-core")
(autoload 'projectile-project-name "projectile")

;; Use conf mode for play routes files.

(add-to-list 'auto-mode-alist (cons "/routes\\'" #'conf-unix-mode))

;; Keybindings are separated into those that are loaded before and after ensime.

(defconst rk-scala--initial-leader-keys
  '(("n s" . ensime)))

(defconst rk-scala--ensime-leader-keys
  '(("/" . ensime-search)
    ("'" . ensime-inf-switch)
    ("b c" . ensime-sbt-do-clean)
    ("b b" . ensime-sbt-do-compile)
    ("b i" . ensime-sbt-switch)
    ("b p" . ensime-sbt-do-package)
    ("b r" . ensime-sbt-do-run)
    ("c a" . ensime-typecheck-all)
    ("c c" . ensime-typecheck-current-buffer)
    ("e e" . ensime-print-errors-at-point)
    ("e l" . ensime-show-all-errors-and-warnings)
    ("e s" . ensime-stacktrace-switch)
    ("g g" . ensime-edit-definition)
    ("g i" . ensime-goto-impl)
    ("g p" . ensime-pop-find-definition-stack)
    ("g t" . ensime-goto-test)
    ("h T" . ensime-type-at-point-full-name)
    ("h h" . ensime-show-doc-for-symbol-at-point)
    ("h t" . ensime-type-at-point)
    ("h u" . ensime-show-uses-of-symbol-at-point)
    ("i i" . ensime-import-type-at-point)
    ("i p" . ensime-inspect-project-package)
    ("l"  . rk-scala-send-file)
    ("n F" . ensime-reload-open-files)
    ("n s" . ensime)
    ("p" . rk-scala-send-as-paste)
    ("r D" . ensime-undo-peek)
    ("r a" . ensime-refactor-add-type-annotation)
    ("r f" . ensime-format-source)
    ("r i" . ensime-refactor-diff-organize-imports)
    ("r l" . ensime-refactor-diff-inline-local)
    ("r m" . ensime-refactor-diff-extract-method)
    ("r r" . ensime-refactor-diff-rename)
    ("r t" . ensime-import-type-at-point)
    ("r v" . ensime-refactor-diff-extract-local)
    ("t a" . ensime-sbt-do-test-dwim)
    ("t r" . ensime-sbt-do-test-quick-dwim)
    ("t t" . ensime-sbt-do-test-only-dwim)))

(defconst rk-scala--prefix-keys
  '(("m b" . "build")
    ("m c" . "check")
    ("m d" . "debug")
    ("m e" . "errors")
    ("m g" . "goto")
    ("m h" . "docs")
    ("m i" . "inspect")
    ("m n" . "ensime")
    ("m r" . "refactor")
    ("m t" . "test")
    ("m s" . "repl")
    ("m y" . "yank")))

(use-package which-key
  :config
  (progn

    (defconst rk-scala--boring-prefixes
      '("rk-scala-"
        "ensime-sbt-do-"
        "ensime-sbt-"
        "ensime-refactor-diff-"
        "ensime-refactor-"
        "ensime-"))

    (let ((match-suffix (rx-to-string `(and bos (or ,@rk-scala--boring-prefixes) (group (+ nonl)))
                                      t)))
      (push `((nil . ,match-suffix) . (nil . "\\1"))
            which-key-replacement-alist))))

;; Load modes.

(use-package scala-mode
  :straight t
  :defer t
  :mode (("\\.scala\\'" . scala-mode))
  :interpreter
  ("scala" . scala-mode)
  :config
  (progn
    (setq scala-indent:align-forms t)
    (setq scala-indent:align-parameters t)
    (setq scala-indent:default-run-on-strategy scala-indent:operator-strategy)
    (setq scala-indent:indent-value-expression t)

    ;; KLUDGE: Don't use the auto-mode form provided by this package.
    (setq auto-mode-alist (delete '("\\.\\(scala\\|sbt\\)\\'" . scala-mode) auto-mode-alist))

    ;; KLUDGE: Scala mode exposes the face, but doesn't apply it. :/
    (font-lock-add-keywords 'scala-mode
                            `((,(rx symbol-start "var" symbol-end) 0 'scala-font-lock:var-keyword-face)))))

(use-package sbt-mode
  :commands (sbt-start sbt-command)
  :preface
  (defun rk-set-sbt-root ()
    "Sets sbt root for the given project."
    (-when-let* ((root (locate-dominating-file (buffer-file-name) "build.sbt"))
                 (sbt-root (and root
                                (sbt:find-root-impl "build.sbt" root))))
      (setq-local sbt:buffer-project-root sbt-root)))
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (progn
    (add-hook 'sbt-mode-hook #'rk-set-sbt-root)
    (substitute-key-definition 'minibuffer-complete-word
                               'self-insert-command
                               minibuffer-local-completion-map)))

(use-package ensime
  :straight (:host github :repo "ensime/ensime-emacs"
                   :branch "2.0")
  :defer t
  :commands (ensime)

  :preface
  (progn
    (autoload 'comint-send-eof "comint")
    (autoload 'comint-send-string "comint")
    (autoload 'ensime-inf-load-file "ensime-inf")

    ;; Define commands for working with the repl.

    (defun rk-scala-switch-to-source ()
      (interactive)
      (when-let* ((buf (--first (with-current-buffer it
                                 (equal major-mode 'scala-mode))
                               (buffer-list)))
                  (win (display-buffer buf)))
        (select-window win)))

    (defun rk-scala-send-as-paste (beg end)
      (interactive (if (region-active-p)
                       (list (region-beginning) (region-end))
                     (list (point-min) (point-max))))
      (-let* ((buf ensime-inf-buffer-name)
              (str (substring-no-properties (buffer-substring beg end))))
        (with-current-buffer buf
          (comint-send-string buf (format ":paste -raw\n%s" str))
          (comint-send-eof))
        (message "Buffer pasted into REPL.")
        (display-buffer buf)))

    (defun rk-scala-send-file (file)
      "Quickly load current file in the Ensime repl."
      (interactive (list (buffer-file-name)))
      (save-buffer)
      (ensime-inf-load-file file)))

  :init
  (progn
    (-each rk-scala--prefix-keys
      (-lambda ((prefix . name))
        (spacemacs-keys-declare-prefix-for-mode 'scala-mode prefix name)))

    (-each rk-scala--initial-leader-keys
      (-lambda ((key . command))
        (spacemacs-keys-set-leader-keys-for-major-mode 'scala-mode key command))))

  :config
  (progn
    (-each rk-scala--ensime-leader-keys
      (-lambda ((key . command))
        (spacemacs-keys-set-leader-keys-for-major-mode 'scala-mode key command)))

    (dolist (state '(normal insert))
      (eval `(evil-define-key ',state ensime-mode-map (kbd "C-c C-l") 'rk-scala-send-as-paste)))

    (dolist (state '(normal insert))
      (eval `(evil-define-key ',state ensime-inf-mode-map (kbd "C-c C-z") 'rk-scala-switch-to-source)))

    (dolist (state '(normal insert))
      (eval `(evil-define-key ',state ensime-mode-map
               (kbd "M-.") 'ensime-edit-definition
               (kbd "M-,") 'ensime-pop-find-definition-stack
               (kbd "C-c C-z") 'ensime-inf-switch)))

    (evil-define-key 'normal ensime-popup-buffer-map
      (kbd "q") 'ensime-popup-buffer-quit-function)

    (evil-define-key 'normal ensime-mode-map
      (kbd "K") 'ensime-inspect-type-at-point)

    (evil-define-key 'normal ensime-inspector-mode-map
      (kbd "M-.") 'ensime-inspector-browse-source
      (kbd "K") 'ensime-inspector-browse-doc
      (kbd "q") 'ensime-popup-buffer-quit-function
      (kbd ",") 'ensime-inspector-backward-page
      (kbd ".") 'ensime-inspector-forward-page
      (kbd "^") 'ensime-inspector-backward-page)

    (evil-define-key 'normal ensime-refactor-info-map
      (kbd "q") 'rk-scala/ensime-refactor-cancel
      (kbd "c") 'rk-scala/ensime-refactor-accept
      (kbd "RET") 'rk-scala/ensime-refactor-accept)

    (evil-define-key 'normal ensime-compile-result-map
      (kbd "g") 'ensime-show-all-errors-and-warnings
      (kbd "TAB") 'forward-button
      (kbd "<backtab>") 'backward-button
      (kbd "M-n") 'forward-button
      (kbd "M-p") 'backward-button
      (kbd "n") 'forward-button
      (kbd "N") 'backward-button))

  :preface
  (progn
    (defun rk-scala--delete-existing-ensime-process-buffer (&rest _)
      (let ((bufname (format "*ENSIME-%s*" (projectile-project-name))))
        (when (get-buffer bufname)
          (kill-buffer bufname))))

    (defun rk-scala--display-ensime-process-buffer-on-error (&rest _)
      (let* ((bufname (format "*ENSIME-%s*" (projectile-project-name)))
             (buf (get-buffer-create bufname)))
        (when-let (proc (get-buffer-process buf))
          (set-process-sentinel proc
                                (lambda (p _)
                                  (when (and (/= 0 (process-exit-status p))
                                             (buffer-live-p buf))
                                    (display-buffer buf)))))))

    ;; Use compilation mode for ensime config generation buffers.

    (autoload 'ensime--refresh-config-sentinel "ensime-config")

    (defconst rk-scala--ensime-gen-config-buffer "*ensime-gen-config*")

    (defun rk-scala--refresh-config-sbt (project-root task on-success-fn)
      (let ((default-directory (file-name-as-directory project-root))
            (compilation-buffer-name-function (lambda (_) rk-scala--ensime-gen-config-buffer)))
        (unless (executable-find ensime-sbt-command) (error "SBT command not found"))

        (compile (format "%s -Dsbt.log.noformat=true %s" ensime-sbt-command task))

        (with-current-buffer (get-buffer rk-scala--ensime-gen-config-buffer)
          (display-buffer (current-buffer))
          (set-process-sentinel (get-buffer-process (current-buffer))
                                (lambda (process event)
                                  (ensime--refresh-config-sentinel process event on-success-fn))))
        (message "Updating ENSIME config..."))))

  :config
  (progn
    (setq ensime-startup-notification nil)
    (setq ensime-auto-generate-config t)
    (setq ensime-implicit-gutter-icons nil)
    (setq ensime-sem-high-enabled-p t)
    (setq ensime-sem-high-faces
          `((deprecated . (:underline ,rk-theme-base-orange))))
    (setq ensime-sbt-perform-on-save "compile")
    (setq ensime-startup-dirname (f-join rk-emacs-cache-directory "ensime"))

    (advice-add 'ensime :before #'rk-scala--delete-existing-ensime-process-buffer)
    (advice-add 'ensime :after #'rk-scala--display-ensime-process-buffer-on-error)

    (defalias 'ensime--refresh-config-sbt #'rk-scala--refresh-config-sbt)

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*ENSIME-" (+? nonl) "*" eos)
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (side            . bottom)
                   (slot            . 0)
                   (window-height   . 0.2)))

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*sbt*" (+ nonl) eos)
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (side            . bottom)
                   (slot            . 0)
                   (window-height   . 0.2)))

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*Scala REPL*" eos)
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (side            . bottom)
                   (slot            . 0)
                   (window-height   . 0.2)))

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*ensime-gen-config*" eos)
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (side            . bottom)
                   (slot            . 0)
                   (window-height   . 0.2)))))

(use-package ensime-company
  :after ensime
  :preface
  (progn
    (autoload 'company-mode "company")
    (autoload 'yas-minor-mode-on "yasnippet"))

  :config
  ;; HACK: Prevent ensime from clobbering company settings.
  (with-eval-after-load 'ensime-company
    (defun ensime-company-enable ()
      (set (make-local-variable 'company-backends) '(ensime-company))
      (company-mode)
      (yas-minor-mode-on)
      (set (make-local-variable 'company-idle-delay) 0))))

(use-package flycheck
  :defer t
  :preface
  (defun rk-scala--disable-flycheck-scala ()
    (when (boundp 'flycheck-disabled-checkers)
      (push 'scala flycheck-disabled-checkers)))

  :config
  (add-hook 'ensime-mode-hook #'rk-scala--disable-flycheck-scala))

;; Snippet utilities


;; Slick table mapping template

(defun rk-scala-yasnippet-slick-star-fields (attrs)
  (let ((names (--map (plist-get it :name) (rk-scala-yasnippet--parse-attrs attrs))))
    (s-join ", " names)))

(defun rk-scala-yasnippet-slick-column-defs (attrs)
  (let ((defs (-map 'scala-yasnippet--slick-attr-to-def (rk-scala-yasnippet--parse-attrs attrs)))
        (indent (current-indentation)))
    (s-join (concat "\n" (s-repeat indent " ")) defs)))

(defun rk-scala-yasnippet--slick-attr-to-def (attr)
  (-let [(&plist :name name :type type) attr]
    (format "def %s = column[%s](\"%s\")" name type name)))

(defun rk-scala-yasnippet--parse-attrs (attrs)
  (let ((ctor-args (s-split (rx (* space) "," (* space)) attrs)))
    (--map (-let [(_ name type) (s-match (rx (group (*? nonl))
                                             (* space) ":" (* space)
                                             (group (* nonl)))
                                         (s-trim it))]
             (list :name (or name "x") :type (or type "T")))
           ctor-args)))

;; Test fixtures

(defun rk-scala-yasnippet-test-fixture-name ()
  (or (ignore-errors (f-filename (f-no-ext (buffer-file-name))))
      "TestFixture"))


(provide 'rk-scala)

;;; rk-scala.el ends here
