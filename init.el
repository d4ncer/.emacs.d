;;; init.el --- Startup file for Emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(when (< emacs-major-version 30)
  (user-error "Emacs 30 required"))

(require 'use-package)

(eval-and-compile
  (defvar +lisp-dir (file-name-concat user-emacs-directory "lisp/"))
  (add-to-list 'load-path +lisp-dir)
  (require '+corelib)
  (require '+load-incrementally))

(defvar org-directory "~/org/")
(defvar org-roam-directory "~/org/roam/")
(defvar org-default-notes-file "~/org/notes.org")

(defvar +ligatures-dir (file-name-concat user-emacs-directory "ligatures/"))

(load-file (file-name-concat user-emacs-directory "elpaca-bootstrap.el"))

(elpaca elpaca-use-package
	(elpaca-use-package-mode))

;; Make sure I don't accidentally start loading super-expensive packages on startup.

(defconst +expensive-packages '(org org-roam org-agenda forge))

(add-transient-hook! 'after-init-hook
  (when-let* ((loaded (seq-filter #'featurep +expensive-packages)))
    (warn "The following package(s) were loaded eagerly, rather than deferred: %S" loaded)))

;;; Extra UI & session lifecycle hooks

;; These hooks are cribbed from Doom--they're a great pattern for deferred
;; loading.

;; The first set are *transient* hooks; they are run only once in the Emacs
;; session, the first time a particular action is performed by the user.

(defvar +first-input-hook nil
  "Transient hook before first user input.")

(defvar +first-file-hook nil
  "Transient hook before first interactively opened file.")

(defvar +first-buffer-hook nil
  "Transient hook before first interactively opened buffer.")

;; These are set up to run just once by other hooks.

(+run-hook-once '+first-buffer-hook '(+switch-buffer-hook find-file-hook))
(+run-hook-once '+first-file-hook '(find-file-hook dired-initial-position-hook))
(+run-hook-once '+first-input-hook '(pre-command-hook))

;; The remaining hooks are executed routinely throughout the session.

(defvar +switch-buffer-hook nil
  "Hooks run after changing the current buffer.")

(defvar +switch-frame-hook nil
  "Hooks run after changing focused frame.")

(defvar +switch-window-hook nil
  "Hooks run after changing focused window.")

(defun +run-switch-buffer-hooks-h (&optional _)
  (let ((gc-cons-threshold most-positive-fixnum)
        (inhibit-redisplay t))
    (run-hooks '+switch-buffer-hook)))

(defun +run-switch-window-or-frame-hooks-h (&optional _)
  (let ((gc-cons-threshold most-positive-fixnum)
        (inhibit-redisplay t))
    (unless (equal (old-selected-frame) (selected-frame))
      (run-hooks '+switch-frame-hook))
    (unless (or (minibufferp)
                (equal (old-selected-window) (minibuffer-window)))
      (run-hooks '+switch-window-hook))))

(add-transient-hook! 'after-init-hook
  (add-hook 'window-selection-change-functions #'+run-switch-window-or-frame-hooks-h)
  (add-hook 'window-buffer-change-functions #'+run-switch-buffer-hooks-h)
  (add-hook 'server-visit-hook #'+run-switch-buffer-hooks-h))

;; Adapt the escape key customisation from Doom.

(defvar +default-minibuffer-maps
  '(minibuffer-local-map
    minibuffer-local-ns-map
    minibuffer-local-completion-map
    minibuffer-local-must-match-map
    minibuffer-local-isearch-map
    read-expression-map))

(with-eval-after-load 'general
  (general-define-key :keymaps +default-minibuffer-maps
                      "S-v" #'yank))

(defvar +escape-hook nil
  "Hook functions run until success when ESC is pressed.")

(defun +escape (&optional interactive)
  "Quit things, abort things, and finish things.
Runs `+escape-hook'."
  (interactive (list 'interactive))
  (let ((inhibit-quit t)
        (in-minibuffer? (minibuffer-window-active-p (minibuffer-window))))
    (cond
     (in-minibuffer?
      (when interactive (setq this-command 'abort-recursive-edit))
      (abort-recursive-edit))

     ;; Run all escape hooks. If any returns non-nil, then stop there.
     ((run-hook-with-args-until-success '+escape-hook))

     ;; Don't abort keyboard macros.
     ((or defining-kbd-macro executing-kbd-macro))

     ;; Fall back to keyboard-quit.
     (t
      (unwind-protect (keyboard-quit)
        (when interactive
          (setq this-command 'keyboard-quit)))))))

(global-set-key [remap keyboard-quit] #'+escape)
(global-set-key [remap abort-recursive-edit] #'+escape)
(with-eval-after-load 'general
  (general-define-key :keymaps +default-minibuffer-maps [escape] #'+escape))
(with-eval-after-load 'eldoc
  (eldoc-add-command '+escape))

;;; Leader key

(use-package general :ensure (:wait t) :demand t
  ;; General provides a featureful key binding system. It makes defining leader
  ;; key bindings much easier.
  :init
  (general-auto-unbind-keys)
  (general-unbind :states '(normal motion) "SPC")
  :config
  (require '+window)
  (require '+roam)
  (require '+edit-cmds)

  (general-define-key
   :states '(normal motion)
   :prefix "SPC"
   :prefix-command '+leader-key

   "SPC" '(execute-extended-command :wk "M-x")
   "RET" '(gptel-menu :wk "LLM...")
   "." '(other-window-prefix :wk "in other window...")
   "!" '(async-shell-command :wk "shell command")
   "|" '(rotate-layout :wk "rotate window layout")
   "-" '(window-toggle-side-windows :wk "side windows")
   ":" '(pp-eval-expression :wk "eval")
   ";" '(ielm :wk "REPL")
   "d" '(dirvish :wk "dir editor")
   "i" '(consult-imenu :wk "imenu")
   "r" #'vertico-repeat
   "s" '(save-buffer :wk "save buf")
   "S" '(save-some-buffers :wk "save some bufs...")
   "u" '(universal-argument :wk "C-u")
   "x" '(execute-extended-command :wk "M-x")
   "K" 'man
   "T" (list (defun +goto-mode-template-file ()
               (interactive)
               (let* ((modes (nreverse (parent-mode-list major-mode)))
                      (mode (completing-read "Snippets table for mode: " modes nil t))
                      (filename (format "%s.eld" (string-remove-suffix "-mode" mode))))
                 (find-file (file-name-concat user-emacs-directory "templates" filename))))
             :wk "edit templates...")


   "'" (general-predicate-dispatch #'poporg-dwim

         ;; Exit indirect edit session if active

         (bound-and-true-p poporg-mode) #'poporg-edit-exit
         (bound-and-true-p edit-indirect--overlay) #'edit-indirect-commit
         (bound-and-true-p org-src-mode) #'org-edit-src-exit

         ;; Otherwise, open indirect-edit buffer

         (and (derived-mode-p 'prog-mode)
              ;; Are we in a string or comment? See: `parse-partial-sexp'
              (or (nth 3 (syntax-ppss)) (nth 4 (syntax-ppss))))
         #'poporg-dwim

         (and (derived-mode-p 'prog-mode) (region-active-p)) #'edit-indirect-region
         (equal (buffer-name) "*Edit Formulas*") #'org-table-fedit-finish
         (derived-mode-p 'org-mode) #'org-edit-special
         (and (derived-mode-p 'markdown-mode) (markdown-code-block-at-point-p)) 'markdown-edit-code-block)

   "/" '(consult-ripgrep :wk "search (rg)")
   "*" (list (defun +consult-ripgrep-symbol ()
               (interactive)
               (consult-ripgrep nil (format "%s" (symbol-at-point))))
             :wk "search (symbol)")

   "<tab>" (list (defun +swap-buffers ()
                   "Switch between the previous buffer and the current one."
                   (interactive)
                   (switch-to-buffer nil))
                 :wk "swap bufs")

   "p"  '(nil :wk "project")
   "p" project-prefix-map

   "h"  '(nil :wk "help")
   "h" help-map

   "," '(nil :wk "structure")
   ",n" '(puni-forward-sexp :wk "forward-sexp")
   ",p" '(puni-backward-sexp :wk "backward-sexp")
   ",<" '(puni-backward-sexp-or-up-list :wk "backward-sexp-or-up-list")
   ",c" '(puni-convolute :wk "convolute")
   ",d" '(+forward-kill-sexp :wk "kill sexp forward")
   ",D" '(+backward-kill-sexp :wk "kill sexp back")

   ",k" '(puni-splice-killing-forward :wk "splice-killing-forward")
   ",K" '(puni-splice-killing-backward :wk "splice-killing-backward")
   ;; TODO: define a killing-around variant.
   ",s" '(puni-splice-killing-backward :wk "splice-killing-backward")
   ",r" '(puni-raise :wk "raise")
   ",b" '(puni-barf-forward :wk "barf-forward")
   ",B" '(puni-barf-backward :wk "barf-backward")
   ",m" '(puni-slurp-forward :wk "slurp-forward")
   ",M" '(puni-slurp-backward :wk "slurp-backward")
   ",t" '(puni-transpose :wk "transpose")
   ",u" '(puni-splice :wk "splice")
   ",x" '(puni-split :wk "split")

   "a"  '(nil :wk "apps")
   "ac" #'quick-calc
   "aC" #'full-calc
   "ae" #'eshell
   "ar" (general-predicate-dispatch 'profiler-start
          (and (featurep 'profiler) (profiler-running-p)) #'+profiler-stop-and-report)

   "ap"  '(nil :wk "elpaca")
   "app" #'elpaca-manager
   "apl" #'elpaca-log
   "api" #'elpaca-info
   "apb" #'elpaca-browse
   "apv" #'elpaca-visit

   "b"  '(nil :wk "buffers")
   "bb" '(bury-buffer :wk "bury")
   "bd" '(bury-buffer :wk "bury")
   "bD" '(kill-current-buffer :wk "kill")
   "bl" '(ibuffer :wk "list")
   "bn" '(next-buffer :wk "next")
   "bp" '(previous-buffer :wk "prev")
   "bc" (list
         (general-predicate-dispatch #'clone-indirect-buffer
           (region-active-p) #'+clone-indirect-buffer-of-region)
         :wk "clone indirect")

   "f"  '(nil :wk "files")
   "ff" '(find-file :wk "find")
   "fF" '(find-file-other-window :wk "find (other window)")
   "fs" '(save-buffer :wk "save")
   "fR" '(rename-visited-file :wk "rename")
   "fr" '(recentf :wk "recent")
   "fw" '(write-file :wk "write copy")
   "fo" '(find-sibling-file :wk "other file")

   "fD" (list (defun +delete-file-and-buffer ()
                (interactive)
                (let ((file (buffer-file-name)))
                  (kill-buffer (current-buffer))
                  (when file
                    (delete-file file))))
              :wk "delete file & buf")

   "fy" (list (defun +copy-file-path ()
                (interactive)
                (if-let* ((file (buffer-file-name)))
                    (progn
                      (kill-new file)
                      (message "Copied to clipboard => %s" file))
                  (user-error "Buffer is not visiting a file")))
              :wk "copy (full path)")

   "fd" (list (defun +copy-file-directory ()
                (interactive)
                (if-let* ((file (buffer-file-name))
                          (dir (file-name-directory file)))
                    (progn
                      (kill-new dir)
                      (message "Copied to clipboard => %s" dir))
                  (user-error "Buffer is not visiting a file")))
              :wk "copy (dir)")

   "fv" (list (defun +revisit-file ()
                (interactive)
                (if-let* ((file (buffer-file-name)))
                    (find-alternate-file file)
                  (user-error "Buffer is not visiting a file")))
              :wk "reload")

   "n"  '(nil :wk "narrowing")
   "nf" '(narrow-to-defun :wk "defun")
   "nr" '(narrow-to-region :wk "region")
   "nw" #'widen

   "c"  '(nil :wk "code/comments")
   "cm" '(xref-find-references :wk "find refs")
   "cr" '(comment-dwim :wk "comment (dwim)")
   "cd" '(eglot-find-typeDefinition :wk "find type def")
   "cc" '(eglot-find-declaration :wk "find decl")
   "ci" '(eglot-find-implementation :wk "find impl")
   "cl" '(comment-line :wk "comment out")

   "g"  '(nil :wk "git/goto")
   "gb" '(magit-blame :wk "blame")
   "gd" '(magit-diff-buffer-file :wk "buffer diff")
   "gf" '(magit-file-dispatch :wk "file actions...")
   "gg" '(magit-status :wk "status")
   "gl" '(magit-log-buffer-file :wk "buffer log")
   "gr" '(browse-at-remote :wk "open on GitHub")
   "gt" '(git-timemachine-toggle :wk "file history")
   "gy" '(browse-at-remote-kill :wk "copy GitHub link ")

   "g?" (list (defun +goto-messages ()
                (interactive)
                (display-buffer "*Messages*"))
              :wk "messages")

   "ge" (list (defun +goto-emacs-init-file ()
                (interactive)
                (find-file (file-name-concat user-emacs-directory "init.el")))
              :wk "init file")

   "gs" (list (defun +goto-emacs-site-file ()
                (interactive)
                (find-file
                 (read-file-name "Site file: " +site-files-directory)))
              :wk "site file...")

   "gn" (list (defun +goto-nix-file ()
                (interactive)
                (project-find-file-in  "flake.nix" nil
                                       (project-current nil "~/.config/nix-configuration")))
              :wk "nix config file...")

   "L" '(gptel-menu :wk "LLM menu")
   "l" '(nil :wk "LLMs")
   "la" '(gptel-add :wk "add/remove from context")
   "lf" '(gptel-add-file :wk "add file to context")
   "ls" '(gptel-send :wk "send")
   "l?" '(gptel-menu :wk "menu")
   "ll" '(gptel :wk "open chat")
   "lw" '(gptel :wk "rewrite")

   "o"  '(nil :wk "org")
   "on" (list (defun +org-goto-notes ()
                (interactive)
                (find-file org-default-notes-file))
              :wk "notes")
   "oi" (list (defun +goto-org-roam-index ()
                (interactive)
                (find-file (file-name-concat org-roam-directory "notes/index.org")))
              :wk "roam index")
   "ot" (list (defun +goto-org-todos ()
                (interactive)
                (find-file (file-name-concat org-roam-directory "todos.org")))
              :wk "todos")
   "oa" (list (defun +org-agenda-dwim ()
                (interactive)
                (org-agenda nil "p"))
              :wk "agenda")

   "oj" '(consult-org-agenda :wk "agenda file heading...")
   "og" '(org-capture-goto-last-stored :wk "goto captured")
   "ov" '(org-tags-view :wk "search by tag")
   "ok" #'org-capture
   "ol" '(org-store-link :wk "store link")
   "of" '(+roam-node-find :wk "find (roam)")
   "os" '(org-roam-search :wk "search (roam)")
   "ow" '(timekeep-visit-node :wk "work file")

   "oc" '(nil :wk "clock")
   "occ" '(org-clock-in-last :wk "clock in (last)")
   "ocd" (list (general-predicate-dispatch #'org-clock-display
                 (not (derived-mode-p 'org-mode))
                 (defun +org-clock-display-last (&optional arg)
                   "Jump to the latest clock and display clocking info in that buffer."
                   (interactive "P")
                   (org-clock-goto arg)
                   (org-clock-display)))
               :wk "display")
   "oci" '(org-clock-in :wk "clock in")
   "oco" '(org-clock-out :wk "clock out")
   "ocr" '(org-resolve-clocks :wk "resolve")
   "ocg" '(org-clock-goto :wk "goto clock")
   "ocq" '(org-clock-cancel :wk "cancel")

   "or" '(nil :wk "roam/review")
   "ord" '(org-roam-review-list-recently-added :wk "list recent")
   "orl" '(org-roam-links :wk "linked nodes")
   "orr" '(org-roam-review :wk "review")
   "ort" '(org-roam-search-tags :wk "search by tag")

   "e"  '(nil :wk "errors")
   "el" '(consult-flymake :wk "error list")
   "en" '(next-error :wk "next error")
   "ep" '(previous-error :wk "prev error")

   "kr" '(consult-yank-pop :wk "kill-ring")

   "t"  '(nil :wk "toggles")
   "tb" '(breadcrumb-mode :wk "breadcrumbs (header)")
   "td" '(dirvish-side :wk "dirvish (side window)")
   "th" '(global-hl-line-mode :wk "highlight line")
   "tf" '(toggle-frame-maximized :wk "frame")
   "ti" '(indent-bars-mode :wk "indent bars")
   "tl" '(global-display-line-numbers-mode :wk "line numbers")
   "tm" '(toggle-input-method :wk "input method")
   "ts" '(spell-fu-mode :wk "spellchecks")
   "tr" '(read-only-mode :wk "readonly")
   "tw" '(whitespace-mode :wk "whitespace")

   "w"  '(nil :wk "windows")
   "w-" '(+split-window-vertically-dwim :wk "vsplit")
   "w/" '(+split-window-horizontally-dwim :wk "hsplit")
   "w="  '(balance-windows :wk "balance")
   "wd" '(delete-window :wk "delete")
   "wo"  '(+delete-nondedicated-windows :wk "delete others")
   "wO"  '(delete-other-windows :wk "delete (+dedicated)")
   "wq" '(delete-window :wk "delete")
   "wr" '(evil-window-rotate-downwards :wk "rotate")
   "ws" '(consult-register-load :wk "registers")
   "wS" '(window-configuration-to-register :wk "save to reg")
   "wt"  '(+toggle-window-dedication :wk "toggle dedication")
   "ww" '(other-window :wk "other")

   "z" '(global-text-scale-adjust :wk "text scaling")
   )

  ;; Support multiple SPC-u calls in sequence to chain universal-argument calls.

  (keymap-set universal-argument-map "SPC u" #'universal-argument-more))

(defmacro +local-leader-set-key (keymaps &rest general-args)
  (declare (indent 1))
  `(general-define-key :prefix "," :states '(normal motion) :keymaps ,keymaps ,@general-args))

;;; General editing

(custom-theme-set-faces 'user
                        '(region ((t (:foreground unspecified :background unspecified :inherit modus-themes-search-lazy))))
                        '(iedit-occurrence ((t (:inherit modus-themes-search-replace))))
                        ;; Dim delimiters like commas, semicolons, etc.
                        '(font-lock-delimiter-face ((t (:inherit shadow)))))

(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Since I use evil, I have no need for the usual rectangular selection
;; keybinding.
(when (equal system-type 'darwin)
  (keymap-global-set "C-x SPC" #'ns-do-show-character-palette))

(setq-default fill-column 80)
(setq ring-bell-function #'ignore)

(setq create-lockfiles nil)
(setq auto-save-include-big-deletions t)

;; Wrap words on word boundaries.
(setq-default word-wrap t)
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)

;; Tune scrolling behaviour
(setq hscroll-margin 2)
(setq hscroll-step 1)
(setq scroll-conservatively 10)
(setq scroll-margin 0)
(setq scroll-preserve-screen-position t)
(setq auto-window-vscroll nil)

(blink-cursor-mode -1)
(setq blink-matching-paren nil)
(setq x-stretch-cursor nil)
(setq delete-pair-blink-delay 0.1)

(setq use-dialog-box nil)

(setq next-error-recenter '(4))
(setq find-library-include-other-files nil)

;; Show keystrokes in minibuffer pretty much immediately.
(setq echo-keystrokes 0.02)

(use-package tooltip
  ;; Emacs' built-in tooltip system. Just disable the thing.
  :init (tooltip-mode -1))

(use-package simple
  ;; Core editing functionality.
  :custom
  (kill-do-not-save-duplicates t)
  ;; Hide commands that don't work in the current major-mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  (setq-default indent-tabs-mode nil))

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(keymap-set minibuffer-local-map "C-k" #'previous-line-or-history-element)
(keymap-set minibuffer-local-map "C-j" #'next-line-or-history-element)

(keymap-global-set "C-c e e" #'toggle-debug-on-error)

(use-package window
  ;; Window management stuff that's not in the C layer.
  :general ("M-o" #'other-window)

  ;; Prefer vertical splits--better when the Emacs GUI window is wide rather
  ;; than tall.
  :custom
  (split-width-threshold 160)
  (split-height-threshold nil))

(use-package frame
  ;; Frame management settings
  :custom
  (window-divider-default-places t)
  (window-divider-default-bottom-width 1)
  (window-divider-default-right-width 1)
  :init
  (window-divider-mode +1))

(use-package paragraphs
  ;; Emacs' core paragraph parser.
  :custom
  (sentence-end-double-space nil))

(defvar +auto-save-dir (file-name-concat user-emacs-directory "autosave/"))

(use-package files
  ;; General built-in file IO.
  :custom
  (backup-inhibited t)
  (require-final-newline t)
  (find-file-visit-truename t)
  (make-backup-files nil)
  (confirm-nonexistent-file-or-buffer nil)
  (auto-mode-case-fold nil)
  (version-control nil)
  (backup-by-copying t)
  (delete-old-versions t)
  (kept-old-versions 5)
  (kept-new-versions 5)
  (backup-directory-alist `(("." . ,+auto-save-dir)))
  (auto-save-list-file-prefix (file-name-concat +auto-save-dir ".saves-"))
  (auto-save-file-name-transforms `((".*" ,+auto-save-dir t)))

  ;; Used by `find-sibling-file' to figure out what files are related.
  (find-sibling-rules
   `(
     ;; Tests -> impl in TS
     ,(list (rx (group (+? any)) (or ".test" ".integration") ".ts" eos)
            (rx (backref 1) ".ts"))

     ;; Impl -> tests in TS
     ,(list (rx (group (+? any)) ".ts" eos)
            (rx (backref 1) ".test.ts")
            (rx (backref 1) ".integration.ts"))
     ))

  :config
  (define-advice after-find-file (:around (fn &rest args) dont-block-on-autosave-exists)
    "Prevent the editor blocking to inform you when an autosave file exists."
    (cl-letf (((symbol-function #'sit-for) #'ignore))
      (apply fn args))))

(use-package tramp
  ;; Provides remote editing support, e.g. over SSH connections.
  :after files
  :config
  (setq tramp-backup-directory-alist backup-directory-alist)
  (setq tramp-auto-save-directory (file-name-concat user-emacs-directory "tramp-autosave/")))

(use-package uniquify
  ;; Controls how buffers with conflicting names are managed.
  :custom
  (uniquify-buffer-name-style 'forward))

(use-package autorevert
  ;; Automatically revert buffers.
  ;;
  ;; This configuration is adapted from Doom; it disables the file watcher
  ;; mechanism and instead auto-reverts based on users switching windows &
  ;; buffers. This is much less resource-intensive.
  :config
  (defun +auto-revert-current-buffer-h ()
    (unless (or auto-revert-mode (active-minibuffer-window))
      (let ((auto-revert-mode t))
        (auto-revert-handler))))

  (defun +auto-revert-visible-buffers-h ()
    "Auto revert stale buffers in visible windows, if necessary."
    (dolist (buf (+visible-buffers))
      (with-current-buffer buf
	(+auto-revert-current-buffer-h))))
  :hook
  (after-save-hook . +auto-revert-visible-buffers-h)
  (+switch-buffer-hook . +auto-revert-current-buffer-h)
  (+switch-window-hook . +auto-revert-current-buffer-h)
  :config
  (add-function :after after-focus-change-function #'+auto-revert-visible-buffers-h)

  :custom
  (auto-revert-use-notify nil)
  (auto-revert-stop-on-user-input nil)
  ;; Only prompts for confirmation when buffer is unsaved.
  (revert-without-query (list ".")))

(use-package elec-pair
  ;; Automatically insert matching pairs.
  :after-call +first-file-hook +first-buffer-hook
  :init
  (electric-pair-mode +1))

(use-package lisp
  ;; Despite its name, provides many programming-language generic features.
  :general
  (:keymaps 'prog-mode-map :states 'normal "(" 'backward-sexp ")" 'forward-sexp))

(use-package puni :ensure t
  ;; Provides structured editing commands.
  :after-call +first-file-hook +first-buffer-hook
  :hook (text-mode-hook prog-mode-hook conf-mode-hook)
  :general
  (:keymaps 'puni-mode-map :states 'insert "C-w" #'puni-backward-kill-word)
  (:keymaps 'puni-mode-map :states '(visual) "C-k" #'puni-kill-active-region)
  (:keymaps 'puni-mode-map :states '(insert normal emacs)
            "C-k" #'+kill-line
            "M-(" #'puni-wrap-round
            "M-[" #'puni-wrap-square
            "M-S-{" #'puni-wrap-curly))

(use-package recentf
  ;; Maintain a list of visited files.
  :after-call recentf consult-buffer
  :defer-incrementally t
  :custom
  (recentf-max-saved-items 100)
  :config
  (recentf-mode +1))

(use-package winner
  ;; Provides undo/redo for buffer & window layout changes.
  :general-config (:keymaps 'winner-mode-map
                            "M-<" #'winner-undo
                            "M->" #'winner-redo)
  :after-call +first-file-hook +first-buffer-hook
  :init
  (winner-mode +1)
  :custom
  (winner-boring-buffers '("*Completions*" "*Compile-Log*" "*inferior-lisp*"
                           "*Fuzzy Completions*" "*Apropos*" "*Help*" "*cvs*"
                           "*Buffer List*" "*Ibuffer*" "*esh command on file*"))
  :config
  (with-eval-after-load 'evil
    (keymap-set evil-normal-state-map "C-." #'winner-redo)))

(use-package saveplace
  ;; Save buffer position when re-visiting files, even across Emacs sessions.
  :init (save-place-mode +1)

  :config
  (define-advice save-place-find-file-hook (:after-while (&rest _) recenter)
    "Recenter on cursor when loading a saved place."
    (when buffer-file-name (ignore-errors (recenter))))

  (define-advice save-place-alist-to-file (:around (fn &rest args) use-prin1-not-pp)
    "Use the faster prin1 for saving history."
    (cl-letf (((symbol-function #'pp) #'prin1))
      (apply fn args))))

(use-package diff
  ;; Support for Unix diff files.
  :custom
  (diff-default-read-only t)
  (diff-advance-after-apply-hunk t)
  (diff-font-lock-prettify t)
  (diff-font-lock-syntax 'hunk-also))

(use-package ediff
  ;; Interactive file diff & merge UI.
  :custom
  (ediff-diff-options "-w")
  (ediff-split-window-function #'split-window-horizontally)
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  (ediff-show-clashes-only t)
  :config
  (with-eval-after-load 'org
    (defun +ad-ediff-reveal-org-content-around-hunk (&rest _)
      (dolist (buf (list ediff-buffer-A ediff-buffer-B ediff-buffer-C))
        (when (and buf (buffer-live-p buf))
          (with-current-buffer buf
            (when (derived-mode-p 'org-mode)
              (org-reveal t))))))

    (advice-add 'ediff-next-difference :after #'+ad-ediff-reveal-org-content-around-hunk)
    (advice-add 'ediff-previous-difference :after #'+ad-ediff-reveal-org-content-around-hunk)))

(use-package tabify
  ;; Tab-to-space conversion
  :custom
  (tabify-regexp "^\t* [ \t]+"))

(use-package comint
  ;; Emacs' basic system for hosting interactive command interpreters.
  :custom
  (comint-prompt-read-only t)
  (comint-buffer-maximum-size 2048) ; double the default.
  )

(use-package compile
  ;; Integration for running compilers and other processes from inside Emacs.
  :custom
  (compilation-always-kill t)
  (compilation-ask-about-save nil) ; automatically save before compiling.
  (compilation-scroll-output 'first-error)
  :config
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

  ;; Automatically truncate long compilation buffers.
  (autoload 'comint-truncate-buffer "comint" nil t)
  (add-hook 'compilation-filter-hook #'comint-truncate-buffer))

;; Disable bidirectional text by default.
(setq-default bidi-display-reordering 'left-to-right)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Don't render cursors or regions in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)

(setq redisplay-skip-fontification-on-input t)

(use-package elpaca
  ;; Configure aspects of elpaca not required for initial package bootstrap.
  :general-config
  (:states 'normal :keymaps 'elpaca-manager-mode-map "/" #'elpaca-ui-search)
  (:keymaps 'elpaca-info-mode-map "q" #'quit-window))

(use-package profiler
  :config
  (defun +profiler-stop-and-report (&optional continue-p)
    "Stop the profiler and show results.

With optional prefix arg CONTINUE-P, keep profiling."
    (interactive "P")
    (let ((ran-p (profiler-running-p)))

      (unless continue-p
        (profiler-stop))
      (profiler-report)
      (when ran-p
        (if continue-p
            (message "Profiler still recording")
          (message "Profiler stopped"))))))

(use-package goto-addr
  ;; Turns URLs in the buffer into clickable buttons.
  :init
  (defun +goto-address-maybe-h ()
    (unless (derived-mode-p 'org-mode 'org-agenda-mode)
      (goto-address)
      (goto-address-mode +1)))
  :hook ((prog-mode-hook text-mode-hook conf-mode-hook magit-process-mode-hook) . +goto-address-maybe-h)

  ;; Teach evil-ret to open URLs.
  :init
  (define-advice evil-ret (:before-until (&rest _) goto-addr)
    (when-let* ((url (thing-at-point 'url)))
      (browse-url url)
      t)))

(use-package better-jumper :ensure t
  ;; Maintains a jump list so you can more easily get back to where you were if
  ;; a command takes you somewhere else.
  :after-call +first-file-hook +first-buffer-hook
  :preface
  (defun +set-jump-point ()
    (when (get-buffer-window)
      (better-jumper-set-jump))
    nil)
  :init
  (better-jumper-mode +1)

  :config
  (add-hook 'kill-buffer-hook #'+set-jump-point)
  (advice-add #'consult-imenu :before #'+set-jump-point)
  (advice-add #'org-mark-ring-push :before #'+set-jump-point)
  (add-hook 'org-open-at-point-functions #'+set-jump-point)

  :general
  (:states 'normal
           "C-." #'better-jumper-jump-forward
           "C-," #'better-jumper-jump-backward)

  :general-config
  ([remap evil-jump-forward]  #'better-jumper-jump-forward
   [remap evil-jump-backward] #'better-jumper-jump-backward
   [remap xref-pop-marker-stack] #'better-jumper-jump-backward
   [remap xref-go-back] #'better-jumper-jump-backward
   [remap pop-tag-mark] #'better-jumper-jump-backward
   [remap xref-go-forward] #'better-jumper-jump-forward))

(use-package server
  ;; Use existing Emacs instances to edit files as $EDITOR.
  :if (display-graphic-p)
  :after-call +first-input-hook +first-file-hook
  :config
  (unless (server-running-p)
    (server-start)))

(use-package so-long
  ;; Improve performance of files with very long lines.
  :hook (elpaca-after-init-hook . global-so-long-mode))

;; Teach Emacs that C-i and C-m do in fact exist.
(pcase-dolist (`(,key ,fallback . ,events)
               '(([C-i] [?\C-i] tab kp-tab)
                 ([C-m] [?\C-m] return kp-return)))
  (define-key
   input-decode-map fallback
   (lambda (&rest _args)
     (interactive)
     (if (when-let ((keys (this-single-command-raw-keys)))
           (and (display-graphic-p)
                (not (cl-loop for event in events
                              if (cl-position event keys)
                              return t))
                ;; Use FALLBACK if nothing is bound to KEY, otherwise we've
                ;; broken all pre-existing FALLBACK keybinds.
                (key-binding (vconcat (if (= 0 (length keys)) [] (cl-subseq keys 0 -1))
                                      key)
                             nil t)))
         key fallback))))

;; TODO: Why is this bound on some installations but not others? 🤔
(when (boundp 'trusted-content)
  (add-to-list 'trusted-content (file-name-concat user-emacs-directory "early-init.el"))
  (add-to-list 'trusted-content (file-name-concat user-emacs-directory "init.el"))
  (add-to-list 'trusted-content (file-name-concat +lisp-dir "lisp/")))

;; Silence "For information about GNU Emacs and the GNU system..." on startup.
(advice-add #'display-startup-echo-area-message :override #'ignore)

;; Don't tell me what key I could have used instead of M-x.
;; (advice-add #'execute-extended-command--describe-binding-msg :override #'ignore)

(use-package eshell
  ;; Emacs' built-in shell combining Emacs Lisp evaluation with Unix shell
  ;; features.
  :config
  (require '+eshell))

(use-package string-inflection :ensure t
  ;; Provides commands for cycling different string casing styles for the ident
  ;; at point, e.g. UpperCamelCase, lowerCamelCase, etc.
  :general (:states '(normal insert) "M--" #'string-inflection-all-cycle))

(use-package envrc :ensure t
  ;; Adds direnv support.
  :hook (+first-file-hook . envrc-global-mode)
  :custom
  (envrc-show-summary-in-minibuffer nil) ; very noisy.
  )

(use-package ligature :ensure t
  ;; Teach Emacs how to display ligatures when available.
  :after-call +first-buffer-hook +first-file-hook
  :config

  (defun +read-ligatures (file)
    (with-temp-buffer
      (insert-file-contents-literally (file-name-concat +ligatures-dir file))
      (read (current-buffer))))

  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'prog-mode (+read-ligatures "prog-mode.eld"))
  (ligature-set-ligatures '(text-mode org-agenda-mode) (+read-ligatures "text-mode.eld"))

  (global-ligature-mode t))

(use-package align
  ;; Emacs has an extensible mode-specific alignment system. In this age of code
  ;; formatters it's not terribly useful, but I do use `align-regexp' from
  ;; time-to-time.
  :general ("C-x a a" #'align-regexp))

(use-package rotate :ensure t
  ;; Provides a few commands for arranging windows in pre-configured
  ;; layouts--very handy.
  ;;
  ;; TODO: Define my own version that ignores side-windows when re-arranging.
  :commands (rotate-layout)
  :config
  (setq rotate-functions '(rotate:even-horizontal rotate:even-vertical)))

(use-package newcomment
  ;; Provides comment-related commands and variables to customise their
  ;; behaviour.
  :custom
  (comment-empty-lines t)
  (comment-multi-line t)
  (comment-style 'extra-line)
  :config
  (setq-default comment-column 0)
  (with-eval-after-load 'evil

    ;; Teach "J" (evil-join) to delete comment delimiters as needed to join
    ;; lines.

    ;; Taken from doom, which itself adapts solutions in this github issue:
    ;; https://github.com/emacs-evil/evil/issues/606

    (define-advice evil-join (:around (fn beg end) join-comments)
      (if-let* (((not (= (line-end-position) (point-max))))
                (cend (save-excursion (goto-char end) (line-end-position)))
                (cbeg (save-excursion
                        (goto-char beg)
                        (and (+point-in-comment-p
                              (save-excursion
                                (goto-char (line-beginning-position 2))
                                (skip-syntax-forward " \t")
                                (point)))
                             (or (comment-search-backward (line-beginning-position) t)
                                 (comment-search-forward  (line-end-position) t)
                                 (and (+point-in-comment-p beg)
                                      (stringp comment-continue)
                                      (or (search-forward comment-continue (line-end-position) t)
                                          beg)))))))
          (let* ((count (count-lines beg end))
                 (count (if (> count 1) (1- count) count))
                 (fixup-mark (make-marker)))
            (uncomment-region (line-beginning-position 2)
                              (save-excursion
                                (goto-char cend)
                                (line-end-position 0)))
            (unwind-protect
                (dotimes (_ count)
                  (join-line 1)
                  (save-match-data
                    (when (or (and comment-continue
                                   (not (string-empty-p comment-continue))
                                   (looking-at (concat "\\(\\s-*" (regexp-quote comment-continue) "\\) ")))
                              (and comment-start-skip
                                   (not (string-empty-p comment-start-skip))
                                   (looking-at (concat "\\(\\s-*" comment-start-skip "\\)"))))
                      (replace-match "" t nil nil 1)
                      (just-one-space))))
              (set-marker fixup-mark nil)))
        ;; But revert to the default we're not in a comment, where
        ;; `fill-region-as-paragraph' is too greedy.
        (funcall fn beg end)))))

(use-package proced
  ;; User-process management UI.
  :custom
  (proced-enable-color-flag t))

(use-package replace
  ;; Defines search+replace functionality, including `occur'.
  :hook
  (occur-mode-hook . hl-line-mode))

(use-package grep
  ;; Buffers showing filesystem search results. The default program is grep;
  ;; change it to ripgrep.
  :custom
  (grep-use-headings t)
  (grep-template "rg --line-number --with-filename --null --regexp <R> <F>"))

(use-package wgrep :ensure t
  ;; Adds a mode for grep-like results buffers that allows you to edit the
  ;; underlying files directly.
  ;;
  ;; TODO: Replace with built-in `grep-edit-mode' once I'm on Emacs 31.
  :commands wgrep-change-to-wgrep-mode
  :custom
  (wgrep-auto-save-buffer t))

(use-package xref
  ;; Provides the interface for navigating symbol definitions & references, etc.
  :custom
  (xref-search-program 'ripgrep))

(use-package indent
  ;; Indentation behaviour.
  :custom
  (tab-first-completion 'word-or-paren-or-punct))

;;; Visual enhancements

(use-package hideshow
  ;; Basic code folding.
  :hook (prog-mode-hook . hs-minor-mode))

(use-package page-break-lines :ensure t
  ;; Displays ^L page break characters as a horizontal rule. Useful for
  ;; demarcating sections of a file.
  :after-call +first-file-hook +first-buffer-hook
  :config
  (global-page-break-lines-mode +1))

(use-package hide-mode-line :ensure (hide-mode-line
                                     :host github
                                     :repo "hlissner/emacs-hide-mode-line")
  ;; Disable the mode-line in situations where it's not useful.
  :hook ((completion-list-mode-hook
          Man-mode-hook
          ielm-mode-hook
          calendar-mode-hook
          eshell-mode-hook
          compilation-mode-hook
          help-mode-hook
          shell-command-mode-hook
          gptel-mode-hook
          org-roam-mode-hook
          )
         . hide-mode-line-mode))

(use-package hl-todo :ensure t
  ;; Display TODO comments with special highlights.
  :hook (prog-mode-hook yaml-ts-mode-hook conf-mode-hook)
  :custom
  (hl-todo-highlight-punctuation ":")
  (hl-todo-keyword-faces
   '(("TODO" warning bold)
     ("FIXME" error bold)
     ("HACK" font-lock-constant-face bold)
     ("DEPRECATED" font-lock-doc-face bold)
     ("NOTE" success bold))))

(use-package pulsar :ensure t
  ;; Temporarily highlights the current line after performing certain operations
  :hook (+first-input-hook . pulsar-global-mode)
  :custom
  (pulsar-iterations 5)
  (pulsar-pulse-on-window-change t)
  :config
  (require '+pulsar)

  (add-hook 'next-error-hook #'pulsar-pulse-line)

  (dolist (hook '(consult-after-jump-hook
                  imenu-after-jump-hook))
    (add-hook hook #'pulsar-recenter-top)
    (add-hook hook #'pulsar-reveal-entry))

  (dolist (hook '(org-agenda-after-show-hook
                  org-follow-link-hook))
    (add-hook hook #'pulsar-recenter-center)
    (add-hook hook #'pulsar-reveal-entry))

  (define-advice flymake-goto-next-error (:after (&rest _) pulsar)
    (when pulsar-mode
      (pcase (cl-loop for o in (overlays-at (point))
                      for diag = (overlay-get o 'flymake-diagnostic)
                      when diag
                      return (flymake--severity (flymake-diagnostic-type diag)))
        (3 (pulsar-pulse-line-red))
        (2 (pulsar-pulse-line-yellow))
        (_ (pulsar-pulse-line-cyan)))))

  (delq! 'evil-goto-first-line pulsar-pulse-functions)
  (delq! 'evil-goto-line pulsar-pulse-functions)

  (define-advice evil-goto-line (:after (count) pulsar)
    "Don't pulse if moving to the first or last line via gg/G."
    (when (and pulsar-mode
               count ; nil if going to end of buffer
               (< 1 count ))
      (pulsar-pulse-line)))

  (define-advice evil-yank (:after (start end &rest _) pulsar)
    "Pulse yanked lines & regions."
    (when pulsar-mode
      (pulsar--pulse nil 'pulsar-generic start end)))

  (define-advice evil-jump-item (:after (&rest _) pulsar)
    "Pulse if jumping to a different line."
    (unless (region-active-p)
      (pulsar-pulse-line)))

  ;; Show a pulse indicating success or failure of eval-expression, eval-region,
  ;; etc.
  :config
  (define-advice eval-region (:around (fn start end &rest args) pulsar)
    "Pulse evaluated regions."
    (+with-eval-pulse start end
      (apply fn start end args)))

  (define-advice eval-last-sexp (:around (fn &rest args) pulsar)
    "Pulse evaluated expressions."
    (pcase-let ((`(,start . ,end) (or (bounds-of-thing-at-point 'sexp)
                                      (cons (ignore-errors (save-excursion
                                                             (backward-sexp)
                                                             (point)))
                                            (point)))))
      (+with-eval-pulse start end
        (apply fn args)))))

(use-package hl-line
  ;; Highlight the current line.
  :custom
  (hl-line-sticky-flag nil))

(use-package whitespace
  ;; Visualise whitespace characters.
  :config
  (delq! 'newline whitespace-style)
  (delq! 'newline-mark whitespace-style))

(use-package paren
  ;; Provides `show-paren-mode', which highlights the matched pair at point.
  :custom
  (show-paren-delay 0.1)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  (show-paren-context-when-offscreen 'overlay))

(use-package paren-face :ensure t
  ;; Adds a face for parentheses in lisps. I hijack it to dim semicolons and
  ;; other non-critical syntax elements in other langs.
  :hook (lisp-data-mode-hook c-ts-base-mode-hook)

  :config
  (setq-hook! 'c-ts-base-mode-hook
    paren-face-regexp (rx (any ";,"))))

(use-package breadcrumb :ensure t
  :custom
  (breadcrumb-idle-time 0.3))


;;; CHRIS CONFIG ABOVE

(setq gc-cons-threshold (* 800 1024))

(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))

;; Set up personal settings

(setq user-full-name "Raghuvir Kasturi")
(setq user-mail-address "raghuvir.kasturi@gmail.com")

;; Set up evil

(use-package evil :ensure t
  :init
  (evil-mode +1))

(use-package evil-escape :ensure t
  :after evil
  :custom
  (evil-escape-key-sequence "jk")
  :init
  (evil-escape-mode))



;; Set up Vertico

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  ;; :general (:keymaps 'vertico-map
  ;;                    "C-j" #'next-line-or-history-element
  ;;                    "C-k" #'previous-line-or-history-element
  ;;                    "C-<return>" #'vertico-exit-input)
  )

;; Set up general to auto unbind keys (override everything)

;; (general-auto-unbind-keys)

;; Setup paths & features

;; (require 'paths (expand-file-name "paths.el" (concat user-emacs-directory "/config")))
;; (paths-initialise)
;; (add-to-list 'custom-theme-load-path paths-themes-directory)

;; Up sub-process throughput data size
;; https://github.com/emacs-mirror/emacs/commit/cc78faee7d23dd0433ba537818a68cbd20fa52a3

;; (setq read-process-output-max (* 1024 1024))


;; Ack org-roam V2

;; (defvar org-roam-v2-ack)
;; (setq org-roam-v2-ack t)

;; Aggressively load persist

;; (use-package persist
;;   :straight t
;;   :demand t
;;   :config
;;   (setq persist--directory-location (f-join paths-cache-directory "persist")))

;; Aggressively load themes

;; (use-package rk-themes)

;; Aggressively load flyspell-lazy
;; It has to load before flyspell
(use-package flyspell-lazy
  :ensure t
  :demand t
  :commands (flyspell-lazy-mode)
  :custom
  (flyspell-lazy-idle-seconds 1)
  (flyspell-lazy-window-idle-seconds 3)
  :config
  (flyspell-lazy-mode +1))


;; Load features.

;; Base setup

;; (use-package rk-emacs)
;; (use-package rk-basic-settings)
;; (use-package rk-auto-insert)
;; (use-package rk-leader-keys)
;; (use-package rk-darwin :if (equal system-type 'darwin))

;; Editor capabilities

;; (use-package rk-evil)
;; (use-package rk-tree-sitter)
;; (use-package rk-completions)
;; (use-package rk-highlight-thing)
;; (use-package rk-auto-save)
;; (use-package rk-search)
;; (use-package rk-help)
;; (use-package rk-projectile)
;; (use-package rk-magit)
;; (use-package rk-parentheses)
;; (use-package rk-smartparens)
;; (use-package rk-restclient)
;; (use-package rk-dired)
;; (use-package rk-hl-todo)
;; (use-package rk-lsp)
;; (use-package rk-ws-butler)
;; (use-package rk-aggressive-indent)
;; (use-package rk-flymake)
;; (use-package rk-ibuffer)
;; (use-package rk-org)
;; (use-package rk-spelling)
;; (use-package rk-string)
;; (use-package rk-expand-region)
;; (use-package rk-yasnippet)
;; (use-package rk-prodigy)
;; (use-package rk-ledger)
;; (use-package rk-pdf)
;; (use-package rk-sql)
;; (use-package rk-mermaid)
;; (use-package rk-bazel)
;; (use-package rk-fmt)

;; Programming language support

;; (use-package rk-elisp)
;; (use-package rk-web-mode)
;; (use-package rk-typescript)
;; (use-package rk-graphql)
;; (use-package rk-haskell)
;; (use-package rk-go)
;; (use-package rk-markdown)
;; (use-package rk-yaml)
;; (use-package rk-docker)
;; (use-package rk-python)
;; (use-package rk-rust)
;; (use-package rk-nim)
;; (use-package rk-racket)
;; (use-package rk-protobuf)
;; (use-package rk-sh)
;; (use-package rk-hashicorp)
;; (use-package rk-nix)
;; (use-package rk-clojure)
;; (use-package rk-java)
;; (use-package rk-zig)
;; (use-package rk-c)
;; (use-package rk-elixir)

;; Setup envrc
;; (use-package rk-envrc)


;; (use-package opam-user-setup
;;   :when (f-exists-p "~/.emacs.d/opam-user-setup.el")
;;   :load-path "~/.emacs.d/opam-user-setup.el")

;;; Post init setup.

;; Load keychain after everything else to ensure env is setup

(use-package keychain-environment
  :ensure t
  :config
  (keychain-refresh-environment))

;;; Improve eval-expression

(defvar eval-expression-interactively-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map read-expression-map)
    (define-key map (kbd "<escape>") #'abort-minibuffers)
    (define-key map (kbd "C-g") #'abort-minibuffers)
    map))

(defun eval-expression-interactively--read (prompt &optional initial-contents)
  (let ((minibuffer-completing-symbol t))
    (minibuffer-with-setup-hook
        (lambda ()
          (let ((inhibit-message t))
            (emacs-lisp-mode)
            (use-local-map eval-expression-interactively-map)
            (setq font-lock-mode t)
            (funcall font-lock-function 1)))
      (read-from-minibuffer prompt initial-contents
                            eval-expression-interactively-map nil
                            'read-expression-history))))

(autoload 'pp-display-expression "pp")
(autoload 'pp-to-string "pp")

(defun eval-expression-interactively (expression &optional arg)
  "Like `eval-expression' with nicer input handling.

- Use `emacs-lisp-mode' to provide font locking and better
  integration with other packages.

- Use the `pp' library to display the output in a readable form.

EXPRESSION is a Lisp form to evaluate.

With optional prefix ARG, insert the results into the buffer at
point."
  (interactive (list (read (eval-expression-interactively--read "Eval: "))
                     current-prefix-arg))
  (if arg
      (insert (pp-to-string (eval expression lexical-binding)))
    (pp-display-expression (eval expression lexical-binding)
                           "*Pp Eval Output*")))

;; (require 'general)
;; (general-define-key :keymaps 'override :states '(normal motion visual)
;;                     "M-:" 'eval-expression-interactively)

;;; Hack

;; (general-unbind global-map "M-<return>")
;; (general-unbind org-mode-map "M-<return>")
;; (general-unbind org-capture-mode-map "M-<return>")

;;; Print overall startup time.

(unless noninteractive
  (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed)))

(use-package private-config
  :when (file-directory-p "~/private")
  :load-path "~/private")

(provide 'init)

;;; init.el ends here
