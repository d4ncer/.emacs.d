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
   ";" '(comment-dwim :wk "REPL")
   "d" '(dired :wk "dir editor")
   "i" '(consult-imenu :wk "imenu")
   "r" #'vertico-repeat
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
   "p!" '(projectile-run-shell-command-in-root :wk "shell cmd as root")
   "p&" '(projectile-run-async-shell-command-in-root :wk "async shell cmd as root")
   "pI" '(projectile-invalidate-cache :wk "invalidate cache")
   "pc" '(projectile-compile-project :wk "compile project")
   "pC" '(projectile-cleanup-known-projects :wk "cleanup known projects")
   "pr" '(projectile-replace :wk "replace (project)")
   "pt" '(projectile-test-project :wk "test (project)")
   "pu" '(projectile-run-project :wk "run (project)")
   "pp" '(projectile-switch-project :wk "switch project")
   "pf" '(projectile-find-file :wk "find file (project)")
   "pF" '(projectile-recentf :wk "find recent file (project)")
   "pd" '(projectile-find-dir :wk "find dir (project)")
   "pb" '(projectile-switch-to-buffer :wk "switch buffer (project)")
   "ps" (list (defun +find-file-in-project ()
                 (interactive)
                 (let ((projectile-switch-project-action #'projectile-find-file))
                   (projectile-switch-project)))
               :wk "find file (other project)")

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
   "ba" '(mark-whole-buffer :wk "select buffer")
   "bb" '(bury-buffer :wk "bury")
   "bd" '(bury-buffer :wk "bury")
   "bD" '(kill-current-buffer :wk "kill")
   "bl" '(ibuffer :wk "list")
   "bn" '(next-buffer :wk "next")
   "bs" '(switch-to-buffer :wk "switch buffer")
   "bp" '(previous-buffer :wk "prev")
   "bc" (list
         (general-predicate-dispatch #'clone-indirect-buffer
           (region-active-p) #'+clone-indirect-buffer-of-region)
         :wk "clone indirect")

   "f"  '(nil :wk "files")
   "ff" '(find-file :wk "find")
   "fF" '(find-file-other-window :wk "find (other window)")
   "fs" '(save-buffer :wk "save")
   "fS" '(save-some-buffers :wk "save some")
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
   "gs" '(magit-status :wk "status")
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

   "gS" (list (defun +goto-emacs-site-file ()
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

   "q" '(nil :wk "quit")
   "qq" '(save-buffers-kill-emacs :wk "quit emacs")

   "s" '(nil :wk "selection/search")
   "se" '(evil-iedit-state/iedit-mode :wk "iedit")
   ;; TODO FIX THIS FIRST - see buffer on right for help
   "ss" (list (defun +symbol-in-file ()
                (interactive)
                (let ((selection (if (use-region-p)
                           (buffer-substring-no-properties (region-beginning) (region-end))
                         (substring-no-properties (thing-at-point 'symbol)))))
                  (consult-line selection)))
              :wk "search symbol in file")

   "t"  '(nil :wk "toggles")
   "tb" '(breadcrumb-mode :wk "breadcrumbs (header)")
   "th" '(global-hl-line-mode :wk "highlight line")
   "tf" '(toggle-frame-maximized :wk "frame")
   "ti" '(indent-bars-mode :wk "indent bars")
   "tl" '(global-display-line-numbers-mode :wk "line numbers")
   "tm" '(toggle-input-method :wk "input method")
   "ts" '(spell-fu-mode :wk "spellchecks")
   "tr" '(read-only-mode :wk "readonly")
   "tw" '(whitespace-mode :wk "whitespace")

   "v" '(er/expand-region :wk "expand")

   "w"  '(nil :wk "windows")
   "w-" '(+split-window-vertically-dwim :wk "vsplit")
   "w/" '(+split-window-horizontally-dwim :wk "hsplit")
   "w="  '(balance-windows :wk "balance")
   "wd" '(delete-window :wk "delete")
   "wo"  '(+delete-nondedicated-windows :wk "delete others")
   "wO"  '(delete-other-windows :wk "delete (+dedicated)")
   "wh" '(windmove-left :wk "move left")
   "wl" '(windmove-right :wk "move right")
   "wk" '(windmove-up :wk "move up")
   "wj" '(windmove-down :wk "move down")
   "wq" '(delete-window :wk "delete")
   "wr" '(evil-window-rotate-downwards :wk "rotate")
   "ws" '(consult-register-load :wk "registers")
   "wS" '(window-configuration-to-register :wk "save to reg")
   "wt"  '(+toggle-window-dedication :wk "toggle dedication")
   "ww" '(other-window :wk "other")

   "z" '(global-text-scale-adjust :wk "text scaling"))

  ;; Support multiple SPC-u calls in sequence to chain universal-argument calls.

  (keymap-set universal-argument-map "SPC u" #'universal-argument-more))

(defmacro +local-leader-set-key (keymaps &rest general-args)
  (declare (indent 1))
  `(general-define-key :prefix "," :states '(normal motion) :keymaps ,keymaps ,@general-args))

;;; General editing

(custom-theme-set-faces 'user
                        '(region ((t (:foreground unspecified :background unspecified :inherit modus-themes-search-lazy))))
                        '(iedit-occurrence ((t (:inherit modus-themes-search-replace))))
                        ;; Set a light modeline
                        '(mode-line ((t (:height 10 :background "#bbb" :box nil))))
                        '(mode-line-inactive ((t (:height 10 :background "#ddd" :box nil))))
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
  (window-divider-default-places 'right-only)
  (window-divider-default-right-width 24)
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
  (insert-directory-program "gls")
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
  (:keymaps 'puni-mode-map :states '(visual) "M-k" #'puni-kill-active-region)
  (:keymaps 'puni-mode-map :states '(insert normal emacs)
            "M-k" #'+kill-line
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
  :disabled t
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

(use-package exec-path-from-shell :ensure t
  ;; Use the shell to get some environment vars; necessary when the window
  ;; system runs Emacs under a very different process environment.
  ;;
  ;; Also, turns out we need this for direnv to work right in compilation buffers.
  :after-call +first-buffer-hook +first-file-hook
  :if (memq system-type '(darwin x))
  :demand t
  :config
  (pushnew! exec-path-from-shell-variables
            "SSH_AUTH_SOCK"
            "SSH_AGENT_PID")

  ;; Speed up by using a non-interactive shell.
  (delq! "-i" exec-path-from-shell-arguments)

  (exec-path-from-shell-initialize))

(use-package helpful :ensure t
  :general
  (:keymaps 'emacs-lisp-mode-map :states '(normal)
            "K" #'helpful-at-point))

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

;;; Spell-checking

(use-package ispell
  ;; Built-in spellchecker. I don't actually use it directly, but other packages
  ;; reference its configuration.
  :custom
  (ispell-dictionary "en_AU")
  (ispell-personal-dictionary (file-name-concat org-directory "aspell.en.pws"))
  :config
  (unless (executable-find "aspell")
    (warn "Could not find aspell program; spell checking will not work"))
  (ispell-set-spellchecker-params))

(use-package spell-fu :ensure t
  ;; A more lightweight spell-checker than the built-in.
  :hook (text-mode-hook prog-mode-hook conf-mode-hook)
  :general
  (:states '(normal motion)
           "zn" #'spell-fu-goto-next-error
           "zp" #'spell-fu-goto-previous-error
           "zg" #'spell-fu-word-add
           "zx" #'spell-fu-word-remove)

  :config
  (add-hook! 'spell-fu-mode-hook
    (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "en_AU"))
    (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "fr")))

  (setq-hook! 'org-mode-hook
    spell-fu-faces-exclude '(org-meta-line org-link org-code org-block
                             org-block-begin-line org-block-end-line
                             org-footnote org-tag org-modern-tag org-verbatim)))

(use-package flyspell-correct :ensure t
  ;; Provides a nicer command for working with spelling corrections.
  :after spell-fu
  :general
  (:states 'normal "z SPC" #'flyspell-correct-at-point))

;;; Dired & dirvish

(use-package dired
  ;; Emacs' built-in file management interface.
  :hook
  (dired-mode-hook . dired-hide-details-mode)
  (dired-mode-hook . hl-line-mode)
  :custom
  (dired-garbage-files-regexp (rx (or ".log" ".toc" ".dvi" ".bak" ".orig" ".rej" ".aux" ".DS_Store")
                                  eos))
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-kill-when-opening-new-dired-buffer t)
  (delete-by-moving-to-trash t)
  (dired-use-ls-dired t)
  (dired-dwim-target t)
  (dired-auto-revert-buffer 'dired-directory-changed-p)
  (dired-listing-switches
   "--almost-all --human-readable --group-directories-first --no-group"))

(use-package dired-aux
  :custom
  (dired-create-destination-dirs 'ask)
  (dired-vc-rename-file t))

(use-package nerd-icons :ensure t
  :disabled t
  ;; Icon set used by various packages.
  :autoload nerd-icons-codicon nerd-icons-faicon)

(use-package nerd-icons-dired :ensure t
  :disabled t
  ;; Show icons in dired.
  :hook dired-mode-hook)

(use-package wdired
  ;; Makes dired buffers directly editable; the changes are interpreted into
  ;; operations on the corresponding file names.
  :general
  (:keymaps 'dired-mode-map "C-c C-e" #'wdired-change-to-wdired-mode))

(use-package dirvish :ensure t
  :disabled t
  ;; Wrapper around `dired' that provides better UX.
  :hook (+first-input-hook . dirvish-override-dired-mode)

  :general
  (:keymaps '(dirvish-mode-map dired-mode-map) :states 'normal
            "q" #'dirvish-quit)
  (:keymaps 'dirvish-mode-map :states 'normal
            "<tab>" #'dirvish-layout-toggle)
  :custom
  (dirvish-reuse-session nil)
  (dirvish-attributes '(file-size subtree-state))
  (dirvish-mode-line-format '(:left (sort file-time symlink) :right (omit yank index)))
  (dirvish-subtree-always-show-state t)
  (dirvish-hide-details '(dirvish dirvish-side))
  (dirvish-hide-cursor '(dirvish dirvish-side))

  :config
  (when (featurep 'nerd-icons)
    (setq dirvish-path-separators (list
                                   (format "  %s " (nerd-icons-codicon "nf-cod-home"))
                                   (format "  %s " (nerd-icons-codicon "nf-cod-root_folder"))
                                   (format " %s " (nerd-icons-faicon "nf-fa-angle_right")))))


  :config
  (dirvish-peek-mode +1))

(use-package diredfl :ensure t
  ;; Add extra font-lock to dired/dirvish file listings.
  :hook ((dired-mode-hook dirvish-directory-view-mode-hook) . diredfl-mode))

(use-package evil :ensure t
  ;; Evil is a better vim emulation implementation than the one that
  ;; ships with Emacs.
  :demand t

  :config
  (defun +find-refs-at-point ()
    (interactive)
    (if-let ((sym (thing-at-point 'symbol)))
        (xref-find-references sym)
      (call-interactively #'xref-find-references)))

  (defun +select-non-empty-line ()
    (interactive)
    (back-to-indentation)
    (set-mark (point))
    (end-of-line)
    (backward-char 1))
  
  :general-config
  (:states 'emacs "ESC ESC" #'evil-normal-state)
  (:states '(normal motion)
           "gd" #'xref-find-definitions
           "gD" #'xref-find-definitions-other-window
           "gb" #'xref-go-back
           "R" #'+find-refs-at-point
           "C-l" #'+select-non-empty-line
           "C-j" #'evil-scroll-page-down
           "C-k" #'evil-scroll-page-up)
  (:states '(insert normal emacs)
           "M-." #'xref-find-definitions
           "C-x RET" #'insert-char)
  ;; `comment-indent-new-line' is a nicer default--it inserts comment delimiters
  ;; for you when you do a newline in a comment. However, it breaks
  ;; electric-pair's special newline padding functionality, so only call it if
  ;; we're actually on a comment.
  (:states 'insert "RET"
           (general-predicate-dispatch #'newline-and-indent
             (nth 4 (syntax-ppss)) ; at a comment?
             #'comment-indent-new-line))
  :custom
  (evil-symbol-word-search t)
  (evil-undo-system 'undo-redo)
  (evil-v$-excludes-newline t)
  (evil-want-C-g-bindings)
  (evil-want-C-u-delete nil)
  (evil-want-C-u-scroll t)
  (evil-want-C-w-delete t)
  (evil-want-Y-yank-to-eol t)
  (evil-want-abbrev-expand-on-insert-exit nil)
  (evil-want-integration t)
  (evil-want-keybinding nil)

  ;; Cursor customisation
  :config
  (setq evil-motion-state-cursor '("#9b59b6" box)
        evil-visual-state-cursor '("#eee8d5" (hbar . 2))
        evil-normal-state-cursor '("#f1c40f" box)
        evil-insert-state-cursor '("#2ecc71" (bar . 2))
        evil-emacs-state-cursor  '("#3498db" hbar))

  :config
  ;; Keep shift-width in sync if mode changes.
  (setq-hook! 'after-change-major-mode
    evil-shift-width tab-width)

  :config
  (add-hook '+escape-hook
            (defun +evil-disable-ex-highlights-h ()
              (when (evil-ex-hl-active-p 'evil-ex-search)
                (evil-ex-nohighlight)
                t)))

  :init
  (evil-mode +1)

  ;; Use more natural Emacs/readline keybindings in ex.
  :general-config
  (:keymaps '(evil-ex-completion-map evil-ex-search-keymap)
            "C-a" #'evil-beginning-of-line
            "C-b" #'evil-backward-char
            "C-f" #'evil-forward-char)

  :config
  (defun +delete-backward-word-no-kill (arg)
    "Like `backward-kill-word', but doesn't affect the kill-ring."
    (interactive "p")
    (let ((kill-ring nil) (kill-ring-yank-pointer nil))
      (ignore-errors (backward-kill-word arg))))

  :general-config
  (:keymaps +default-minibuffer-maps
            "C-a"    #'move-beginning-of-line
            "C-r"    #'evil-paste-from-register
            "C-u"    #'evil-delete-back-to-indentation
            "C-v"    #'yank
            "C-h"    #'+delete-backward-word-no-kill))

(use-package evil-escape :ensure t
  :after evil
  :custom
  (evil-escape-key-sequence "jk")
  :init
  (evil-escape-mode))

(use-package vundo :ensure (vundo :host github :repo "casouri/vundo")
  ;; Visualise the Emacs undo history.
  :general ("C-x u" #'vundo)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

(use-package evil-collection :ensure t
  ;; Community-managed collection of evil keybindings; makes evil behave more
  ;; consistently across many modes.
  :custom
  ;; Ensure we do not overwrite the global leader key binding.
  (evil-collection-key-blacklist '("SPC" "S-SPC"))

  ;; Org-mode derives from outline-mode; disable the outline bindings to prevent
  ;; conflicts.
  (evil-collection-outline-enable-in-minor-mode-p nil)

  ;; Be a bit smarter about the evil-collection load sequence; in particular,
  ;; set up bindings in hooks first time we activate a major-mode. This makes
  ;; key binding setup more performant and more predictable.
  :init
  (with-eval-after-load 'evil
    (require '+evil-collection)
    (+evil-collection-defer-install-to-mode-activation))
  :config
  (+evil-collection-init 'comint)

  ;; Fix leader keybindings that get clobbered by evil-collection.

  (define-advice evil-collection-magit-init (:after (&rest _) bind-leader)
    (general-define-key :keymaps (append evil-collection-magit-maps
                                         evil-collection-magit-section-maps)
                        :states '(normal)
                        "SPC" #'+leader-key)))

(use-package evil-surround :ensure t
  ;; Evil-surround makes the S key work as an operator to surround an
  ;; object with, e.g., matched parentheses.
  :hook ((text-mode-hook prog-mode-hook) . evil-surround-mode)
  ;; Use lowercase 's' for surround instead of 'S'.
  :general (:states '(visual) :keymaps 'evil-surround-mode-map "s" #'evil-surround-region)
  :custom
  (evil-surround-pairs-alist '((?\( . ("(" . ")"))
                               (?\) . ("(" . ")"))
                               (?\[ . ("[" . "]"))
                               (?\] . ("[" . "]"))
                               (?\{ . ("{" . "}"))
                               (?\} . ("{" . "}"))
                               (?# . ("#{" . "}"))
                               (?> . ("<" . ">"))
                               (?f . evil-surround-function)
                               (?t . evil-surround-read-tag)
                               (?< . evil-surround-read-tag)))

  :config
  (add-hook! 'emacs-lisp-mode-hook
    (make-local-variable 'evil-surround-pairs-alist)
    (alist-set! evil-surround-pairs-alist ?` '("`" . "'"))
    (alist-set! evil-surround-pairs-alist ?' '("`" . "'"))
    (alist-set! evil-surround-pairs-alist ?f #'evil-surround-prefix-function)))

(use-package evil-multiedit :ensure t
  ;; Evil-compatible multiple cursors.
  :after evil
  :disabled t
  :config
  (evil-multiedit-default-keybinds)

  :init
  (defun +multiedit ()
    (interactive)
    (evil-normal-state)
    (unless (eolp)
      (forward-char -1))
    (evil-multiedit-match-all))

  :general
  (:states 'visual
           "E" (general-predicate-dispatch #'evil-multiedit-match-all
                 (equal last-command 'evil-visual-char) #'+multiedit))

  :general-config
  (:keymaps 'evil-multiedit-mode-map
   :states 'normal
   "Y" (defun +evil-multiedit-copy ()
         (interactive)
         (when-let* ((str (iedit-current-occurrence-string)))
           (kill-new str)
           (message "Copied to kill ring")))
   "<tab>" #'iedit-toggle-selection
   "n" #'evil-multiedit-next
   "N" #'evil-multiedit-prev
   "S" #'evil-multiedit--change-line))

(use-package iedit :ensure t)

(use-package evil-iedit-state :ensure t
  :after evil
  :commands (evil-iedit-state evil-iedit-state/iedit-mode)
  :init
  (require 'iedit))

(use-package expand-region :ensure t
  :custom
  (expand-region-contract-fast-key "V")
  (expand-region-reset-fast-key "r"))

;;; Navigation

(use-package avy :ensure t
  ;; Jump to things or execute other actions by typing a few letters.
  :general ("M-g" #'avy-goto-char-timer)

  ;; Customise the action keys to make actions a bit more vimmy.
  :config
  (require '+avy)
  :custom
  (avy-dispatch-alist '((?x . avy-action-kill-stay)
                        (?d . avy-action-kill-move)
                        (?c . +avy-action-change-move)
                        (?t . avy-action-teleport)
                        (?v . avy-action-mark)
                        (?y . avy-action-copy)
                        (?p . avy-action-yank)
                        (?P . avy-action-yank-line)
                        (?i . avy-action-ispell)
                        (?K . +avy-action-evil-lookup)
                        (? . avy-action-zap-to-char)))

  ;; Integrate avy with pulsar for better visual feedback

  :config
  (with-eval-after-load 'pulsar
    (define-advice avy-process (:filter-return (result) pulse-red-on-no-matches)
      (when (eq t result)
        (when pulsar-mode
          (pulsar-pulse-line-red)))
      result)

    (defun +avy-pulse-for-move (&rest _)
      (when pulsar-mode
        (pulsar-pulse-line)))

    (advice-add #'avy-action-goto :after #'+avy-pulse-for-move)

    (defun +avy-pulse-for-change (&rest _)
      (when pulsar-mode
        (pulsar-pulse-line-magenta)))

    (advice-add #'avy-action-kill-move :after #'+avy-pulse-for-change)
    (advice-add #'+avy-action-change-move :after #'+avy-pulse-for-change)

    (defun +avy-pulse-for-change-elsewhere (fn pt)
      (+with-clean-up-in-starting-buffer-and-window (funcall fn pt)
        (when pulsar-mode
          (goto-char pt)
          (pulsar-pulse-line-magenta))))

    (advice-add #'avy-action-kill-stay :around #'+avy-pulse-for-change-elsewhere)

    (defun +avy-pulse-for-action-elsewhere (fn pt)
      (+with-clean-up-in-starting-buffer-and-window (funcall fn pt)
        (when pulsar-mode
          (goto-char pt)
          (pulsar-pulse-line-green))))

    (advice-add #'+avy-action-evil-lookup :around #'+avy-pulse-for-action-elsewhere)
    (advice-add #'avy-action-copy :around #'+avy-pulse-for-action-elsewhere)
    (advice-add #'avy-action-ispell :around #'+avy-pulse-for-action-elsewhere))

  ;; KLUDGE: Pre-configure indentation for dynamically-loaded macro. Ensures
  ;; Apheleia applies correct indentation if I touch this file without avy being
  ;; loaded in the editing session.
  :init
  (function-put '+with-clean-up-in-starting-buffer-and-window 'lisp-indent-function 1))

;; Use +/- to mark syntactic elements with tree-sitter. However, if I don't have
;; a selection, make - call avy.
;; (general-define-key :states '(normal motion)
;;                     "-" (general-predicate-dispatch #'avy-goto-char-timer
;;                           (region-active-p) #'expreg-contract)
;;                     "+" #'+expreg-expand-dwim)

(use-package ace-window :ensure t
  ;; Jump to specific windows
  :general ("M-o" #'ace-window))

;;; Completion

(use-package vertico :ensure t
  ;; Vertico provides a better completion UI than the built-in default.
  :hook +first-input-hook
  :custom
  (vertico-preselect 'no-prompt)
  (vertico-cycle t)
  :general-config (:keymaps 'vertico-map
                            "C-<return>" #'vertico-exit-input
                            "C-j" #'next-line-or-history-element
                            "C-k" #'previous-line-or-history-element
                            "RET" #'vertico-directory-enter
                            "DEL" #'vertico-directory-delete-char
                            "C-l" #'vertico-insert
                            "C-h" #'vertico-directory-delete-word
                            "M-l" #'vertico-insert
                            "M-h" #'vertico-directory-delete-word
                            "M-P" #'vertico-repeat-previous
                            "M-N" #'vertico-repeat-next)
  :init
  (vertico-mode +1)

  (use-package vertico-directory
    ;; Extension that teaches vertico how to operate on filename
    ;; components in a more ergonomic way.
    :demand t
    :hook (rfn-eshadow-update-overlay-hook . vertico-directory-tidy))

  (use-package vertico-repeat
    ;; Quickly restore the previous vertico command you ran.
    :hook (minibuffer-setup-hook . vertico-repeat-save)
    :config
    (with-eval-after-load 'savehist
      (add-to-list 'savehist-additional-variables 'vertico-repeat-history))))

(use-package marginalia :ensure t
  ;; Marginalia shows extra information alongside minibuffer items
  ;; during completion.
  :hook +first-input-hook
  :general
  (:keymaps 'minibuffer-local-map "M-A" #'marginalia-cycle))

(use-package minibuffer
  ;; Customise minibuffer completion behaviour.
  ;;
  ;; The configuration that determines which style to use is rather subtle; see
  ;; § 5.4.1:
  ;; https://protesilaos.com/emacs/dotemacs#h:14b09958-279e-4069-81e3-5a16c9b69892
  ;;
  ;; Briefly, use the following approach:
  ;;
  ;; 1. Prefer explicit and prefix matches first, falling back to orderless
  ;; matching last.
  ;;
  ;; 2. Override this behaviour explicitly for a few select types of completion.

  :custom
  ;; To determine a completion style when entering text in the minibuffer,
  ;; consult `completion-category-overrides' according to the type of thing
  ;; we're trying to complete. Fall back to `completion-styles' if there are no
  ;; specific style set for that type.
  ;;
  ;; Completion strategies are tried in order until a match is found. Putting
  ;; orderless last means more precise approaches are tried first.
  ;;
  ;; See `completion-styles-alist' for the behaviour of specific completion
  ;; styles.

  (completion-category-overrides
   '((file (styles . (basic partial-completion orderless)))
     (bookmark (styles . (basic substring)))
     (library (styles . (basic substring)))
     (imenu (styles . (basic substring orderless)))
     (kill-ring (styles . (emacs22 orderless)))
     (eglot (styles . (emacs22 substring orderless)))))

  (completion-styles '(orderless basic))

  ;; Disable any out-of-the-box defaults.
  (completion-category-defaults nil)

  :init
  (use-package orderless :ensure t
    ;; Orderless allows you to filter completion candidates by typing
    ;; space-separated terms in any order.
    :after-call +first-input-hook))

(use-package savehist
  ;; Persists Emacs completion history. Used by vertico.
  :init (savehist-mode +1)
  :custom
  (savehist-autosave-interval nil) ; on exit
  (history-delete-duplicates t)
  :config
  (pushnew! savehist-additional-variables
            'kill-ring
            'register-alist
            'mark-ring 'global-mark-ring
            'search-ring 'regexp-search-ring)

  (setq-hook! 'savehist-save-hook
    ;; Reduce size of savehist's cache by dropping text properties.
    kill-ring (mapcar #'substring-no-properties (cl-remove-if-not #'stringp kill-ring))
    register-alist (cl-loop for (reg . item) in register-alist
                            if (stringp item)
                            collect (cons reg (substring-no-properties item))
                            else collect (cons reg item))

    ;; Avoid attempts to save unprintable registers, e.g. window configurations.
    register-alist (seq-filter #'savehist-printable register-alist)))

(setq enable-recursive-minibuffers t)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq completion-ignore-case t)

(use-package crm
  ;; Provides a variant of completing-read that allows users to enter multiple
  ;; values, separated by a delimiter.
  :config
  (define-advice completing-read-multiple (:filter-args (args) crm-indicator)
    "Display the separator during `completing-read-multiple'."
    (let ((sans-brackets
           (replace-regexp-in-string (rx (or (and bos "[" (*? any) "]*")
                                             (and "[" (*? any) "]*" eos)))
                                     ""
                                     crm-separator)))
      (cons (format "[CRM %s] %s" (propertize sans-brackets 'face 'error) (car args))
            (cdr args)))))

(use-package corfu :ensure t
  ;; Corfu provides in-buffer completions as you type.
  :hook (+first-input-hook . global-corfu-mode)
  :general-config (:keymaps 'corfu-map
                            "RET" #'corfu-send
                            "<escape>" #'corfu-reset
                            "C-n" #'corfu-next
                            "C-p" #'corfu-previous)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.24)
  (corfu-quit-no-match t)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-count 16)
  (corfu-max-width 120)
  (corfu-on-exact-match nil)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  (tab-always-indent 'complete)
  (corfu-popupinfo-delay '(1.0 . 0.5))
  (global-corfu-modes '((not org-mode help-mode) t))
  :init
  (setq-hook! 'eshell-mode-hook corfu-auto nil)
  :config
  (corfu-popupinfo-mode +1)
  (add-hook 'evil-insert-state-exit-hook #'corfu-quit)

  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package nerd-icons-corfu :ensure t
  :disabled t
  ;; Adds icons to corfu popups.
  :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape :ensure t
  ;; Adds useful functionality for `completion-at-point-functions'.
  :init
  (add-hook! 'prog-mode-hook
    (add-hook 'completion-at-point-functions #'cape-file -10 t))
  (add-hook! 'org-mode-hook
    (add-hook 'completion-at-point-functions #'cape-elisp-block 0 t))

  (advice-add #'comint-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive))

(use-package which-key
  ;; which-key displays a UI popup of available key commands as you type.
  :demand t
  :init
  (which-key-mode +1)
  :custom
  (which-key-prefix-prefix "…")
  (which-key-idle-delay 0.4)
  (which-key-sort-order #'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1)
  (which-key-min-display-lines 6)
  (which-key-side-window-slot -10)
  :config
  (which-key-setup-side-window-bottom)
  (setq-hook! 'which-key-init-buffer-hook line-spacing 3))

(use-package consult :ensure t
  ;; Consult provides commands for common tasks that leverage the Emacs
  ;; completion system. It composes well with the above packages.
  :general
  ([remap bookmark-jump]                 #'consult-bookmark
   [remap evil-show-marks]               #'consult-mark
   [remap evil-show-registers]           #'consult-register
   [remap goto-line]                     #'consult-goto-line
   [remap imenu]                         #'consult-imenu
   [remap Info-search]                   #'consult-info
   [remap locate]                        #'consult-locate
   [remap load-theme]                    #'consult-theme
   [remap recentf-open-files]            #'consult-recent-file
   [remap switch-to-buffer]              #'consult-buffer
   [remap switch-to-buffer-other-window] #'consult-buffer-other-window
   [remap switch-to-buffer-other-frame]  #'consult-buffer-other-frame
   [remap yank-pop]                      #'consult-yank-pop)

  :general
  (:states '(motion normal)
           "C-'" #'consult-imenu-multi
           "/" #'consult-line)

  :custom
  ;; Use Consult to select xref locations with preview
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-narrow-key "<")

  ;; Optimise for responsive input.
  (consult-async-min-input 2)
  (consult-async-refresh-delay  0.15)
  (consult-async-input-throttle 0.2)
  (consult-async-input-debounce 0.1)
  (consult-fd-args
   '((if (executable-find "fdfind" 'remote) "fdfind" "fd")
     "--color=never"
     ;; https://github.com/sharkdp/fd/issues/839
     "--full-path --absolute-path"
     "--hidden --exclude .git"))

  :config
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key "C-SPC")
  (consult-customize
   consult-theme
   :preview-key (list "C-SPC" :debounce 0.5 'any))

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5))

(use-package embark :ensure t
  ;; Embark provides a UI for performing contextual actions on selected items
  ;; within completing-read.
  :general
  (:states '(normal emacs motion)
           "C-." #'embark-act
           "C-t" #'embark-dwim)
  (:keymaps +default-minibuffer-maps
            "C-." #'embark-act
            "C-c C-e" #'embark-export
            "C-c C-c" #'embark-collect))

(use-package embark-consult :ensure t
  ;; Integrate embark with consult
  :after (:any consult embark)
  :demand t
  :hook (embark-collect-mode-hook . consult-preview-at-point-mode))

(pushnew! completion-ignored-extensions
          ".DS_Store"
          ".eln"
          ".drv"
          ".direnv/"
          ".git/")

(use-package minibuf-eldef
  ;; Set how the default option for empty input is displayed in the minibuffer.
  :hook (after-init . minibuffer-electric-default-mode)
  :custom
  (minibuffer-default-prompt-format " [%s]"))

(use-package dabbrev
  ;; Dynamically complete using identifier-like words entered in this or other
  ;; buffers.
  :custom
  (dabbrev-abbrev-skip-leading-regexp "[$*/=~']")
  (dabbrev-upcase-means-case-search t)
  :config
  (pushnew! dabbrev-ignored-buffer-modes
            'docview-mode 'pdf-view-mode))

;;; VC & magit

(use-package transient :ensure t
  ;; Lib for showing pop-up menus of key commands, often with switches to modify
  ;; behaviour.
  ;;
  ;; Magit depends on a more recent version of transient than the one that ships
  ;; with Emacs.
  :custom
  (transient-display-buffer-action '(display-buffer-below-selected))
  :general-config
  (:keymaps 'transient-map [escape] #'transient-quit-one))

(use-package magit :ensure t
  ;; Magit is the definitive UX for working with git.
  :config
  ;; Set initial evil state depending on whether the line is empty or not. Empty
  ;; line = new commit message, whereas non-empty means we're editing an
  ;; existing one.
  (add-hook! 'git-commit-mode-hook
    (when (and (bolp) (eolp))
      (evil-insert-state)))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-bury-buffer-function #'magit-restore-window-configuration))

(use-package git-timemachine :ensure t
  :general-config
  (:states 'normal
   :keymaps 'git-timemachine-mode-map
   "C-p" #'git-timemachine-show-previous-revision
   "C-n" #'git-timemachine-show-next-revision
   "gb"  #'git-timemachine-blame
   "gtc" #'git-timemachine-show-commit)

  :config
  ;; git-timemachine uses `delay-mode-hooks', which can suppress font-lock.
  (add-hook 'git-timemachine-mode-hook #'font-lock-mode)
  ;; Ensure evil keymaps are applied
  (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)

  ;; Show information in header-line for better visibility.
  :custom
  (git-timemachine-show-minibuffer-details t)
  :config
  (define-advice git-timemachine--show-minibuffer-details (:override (revision) use-header-line)
    "Show revision details in the header-line, instead of the minibuffer."
    (let* ((date-relative (nth 3 revision))
           (date-full (nth 4 revision))
           (author (if git-timemachine-show-author (concat (nth 6 revision) ": ") ""))
           (sha-or-subject (if (eq git-timemachine-minibuffer-detail 'commit) (car revision) (nth 5 revision))))
      (setq header-line-format
            (format "%s%s [%s (%s)]"
                    (propertize author 'face 'git-timemachine-minibuffer-author-face)
                    (propertize sha-or-subject 'face 'git-timemachine-minibuffer-detail-face)
                    date-full date-relative)))))

(use-package browse-at-remote :ensure t
  :custom
  (browse-at-remote-add-line-number-if-no-region-selected nil)

  :config
  (define-advice browse-at-remote--get-local-branch (:after-until () const-main)
    "Return 'main' in detached state."
    "main")

  ;; Integrate browse-at-remote with git-timemachine
  :config
  (define-advice browse-at-remote-get-url (:around (fn &rest args) git-timemachine-integration)
    "Allow `browse-at-remote' commands in git-timemachine buffers to open that
file in your browser at the visited revision."
    (if (bound-and-true-p git-timemachine-mode)
        (let* ((start-line (line-number-at-pos (min (region-beginning) (region-end))))
               (end-line (line-number-at-pos (max (region-beginning) (region-end))))
               (remote-ref (browse-at-remote--remote-ref buffer-file-name))
               (remote (car remote-ref))
               (ref (car git-timemachine-revision))
               (relname
                (file-relative-name
                 buffer-file-name (expand-file-name (vc-git-root buffer-file-name))))
               (target-repo (browse-at-remote--get-url-from-remote remote))
               (remote-type (browse-at-remote--get-remote-type target-repo))
               (repo-url (cdr target-repo))
               (url-formatter (browse-at-remote--get-formatter 'region-url remote-type)))
          (unless url-formatter
            (error (format "Origin repo parsing failed: %s" repo-url)))
          (funcall url-formatter repo-url ref relname
                   (if start-line start-line)
                   (if (and end-line (not (equal start-line end-line))) end-line)))
      (apply fn args))))

(use-package forge :ensure t
  ;; Teach magit how to work with pull requests on GitHub and other git hosting
  ;; services.
  :after-call magit-status ; avoids compilation until first use

  :preface
  (setq forge-database-file (file-name-concat user-emacs-directory "forge/forge-database.sqlite"))
  :general
  (:keymaps 'magit-mode-map [remap magit-browse-thing] #'forge-browse)
  (:keymaps 'magit-remote-section-map [remap magit-browse-thing] #'forge-browse-remote)
  (:keymaps 'magit-branch-section-map [remap magit-browse-thing] #'forge-browse-branch)
  :general-config
  (:keymaps 'forge-topic-list-mode-map :states 'normal "q" #'kill-current-buffer))

(use-package vc
  :custom
  ;; Don't prompt when following links to files that are under version control.
  (vc-follow-symlinks t)
  ;; I literally only ever use Git these days.
  (vc-handled-backends '(Git))
  :config
  (pushnew! vc-directory-exclusion-list
            "node_modules"
            "cdk.out"
            "target"
            ".direnv"))

;;; Modeline

(use-package nano-modeline :ensure (:host github :repo "rougier/nano-modeline" :branch "rewrite")
  :demand t
  :init
  (defun +modeline-org-buffer-name (&optional name)
    (propertize
     (cond (name
            name)
           ((buffer-narrowed-p)
            (format "%s [%s]" (or (buffer-base-buffer) (buffer-name))
                    (org-link-display-format
                     (substring-no-properties
                      (or (org-get-heading 'no-tags) "-")))))
           ((file-equal-p (file-name-directory (buffer-file-name))
                      +org--roam-dir)
            (format "(roam) %s" (car (last (s-split "-" (buffer-file-name))))))
           (t
            (buffer-name)))
     'face (nano-modeline-face 'name)))

  ;; TODO this isn't wired up (yet)
  (defun +modeline-org-mode ()
    (nano-modeline (cons
                    '(nano-modeline-element-buffer-status
                      nano-modeline-element-space
                      +modeline-org-buffer-name
                      nano-modeline-element-space
                      nano-modeline-element-buffer-vc-mode)
                    '(nano-modeline-element-buffer-position
                      nano-modeline-element-window-status
                      nano-modeline-element-space))))
  :config
  (nano-modeline nil nil t)
  (setq-default mode-line-format ""))

;;; projects

(use-package projectile :ensure t
  :custom
  (projectile-indexing-method 'alien)
  (projectile-completion-system 'default)
  (projectile-switch-project-action #'+switch-project-action)
  (projectile-enable-caching t)
  (projectile-globally-ignored-files '("TAGS" ".DS_Store"))
  (projectile-globally-ignored-file-suffixes '("meta" "jsbundle" "gz" "zip" "tar" "elc"))
  (projectile-globally-ignored-directoriess '(".bzr"
                                              ".ensime_cache"
                                              ".eunit"
                                              ".fslckout"
                                              ".g8"
                                              ".git"
                                              ".hg"
                                              ".idea"
                                              ".stack-work"
                                              ".svn"
                                              "build"
                                              "dist"
                                              "node_modules"
                                              "vendor"
                                              "straight/repos"
                                              "target"))
  :config
  (autoload 'magit-status "magit")
  (defun +switch-project-action ()
    (let ((project-root (projectile-acquire-root)))
      (if (file-exists-p (expand-file-name ".git" project-root))
          (magit-status project-root)
        (dired project-root))))
  (projectile-mode +1))

;;; CHRIS CONFIG ABOVE

(use-package highlight-thing :ensure t
  :hook (prog-mode-hook . highlight-thing-mode)
  :custom
  (highlight-thing-what-thing 'symbol)
  (highlight-thing-delay-seconds 0.2)
  (highlight-thing-limit-to-defun nil)
  (highlight-thing-case-sensitive-p t)
  :config
  (defun +face-ancestors (face)
    "List all faces that FACE transitively inherits from."
    (let (result)
      (while (and face (not (equal face 'unspecified)))
        (setq result (cons face result))
        (setq face (face-attribute face :inherit)))
      (nreverse result)))

  (defun +should-highlight-p (res)
    (unless (or (bound-and-true-p lsp-ui-mode)
                (bound-and-true-p tide-mode))
      (when res
        (let ((excluded-faces '(font-lock-string-face
                                font-lock-keyword-face
                                font-lock-comment-face
                                font-lock-preprocessor-face
                                font-lock-builtin-face))
              (faces (seq-mapcat #'+face-ancestors (face-at-point nil t))))
          (null (seq-intersection faces excluded-faces))))))
  (advice-add 'highlight-thing-should-highlight-p :filter-return
              #'+should-highlight-p))

(use-package gptel :ensure t
  ;; Provides LLM integrations.
  :hook (gptel-mode-hook . visual-line-mode)
  :general (:states 'visual "RET" #'gptel-rewrite)
  :init
  (defun +gptel-send ()
    (interactive)
    (unless (region-active-p)
      (goto-char (line-end-position)))
    (gptel-send)
    (evil-normal-state))
  (defun +gptel-go-to-insert ()
    "Go to end of buffer and enter evil insert state."
    (interactive)
    (goto-char (point-max))
    (backward-char)
    (evil-insert-state))

  :general
  (:keymaps 'gptel-mode-map :states '(normal insert)
            "C-c C-s" #'+gptel-send
            "A" #'+gptel-go-to-insert)
  :custom
  (gptel-model 'claude-3-7-sonnet-20250219)
  (gptel-default-mode 'org-mode)
  :config
  (alist-set! gptel-prompt-prefix-alist 'org-mode "* ")
  (setq-hook! 'gptel-mode-hook
    org-pretty-entities-include-sub-superscripts nil)

  (setq gptel-backend
        (gptel-make-anthropic "Claude"
          :stream t
          :key (lambda ()
                 (auth-source-pick-first-password :host "api.anthropic.com"))))

  (add-hook 'gptel-mode-hook 'evil-insert-state)

  ;; Prevent transient from creating extra windows due to conflicts with custom
  ;; display-buffer rules. See:
  ;; https://github.com/magit/transient/discussions/358
  (setq transient-display-buffer-action
        '(display-buffer-below-selected
          (dedicated . t)
          (inhibit-same-window . t)))

  ;; Pulse the part of the buffer being sent to the LLM.

  (define-advice gptel-send (:after (&optional show-transient) pulse)
    (when (bound-and-true-p pulsar-mode)
      (unless show-transient
        (let ((pulsar-region-face 'pulsar-green))
          (cond ((region-active-p)
                 (pulsar-pulse-region))
                ((and gptel-mode (org-at-heading-p))
                 (pulsar-pulse-line-green))
                (t
                 (pulsar--pulse nil 'pulsar-green (point-min) (point)))))))))

;;; OLD CONFIG BELOW

(setq gc-cons-threshold (* 800 1024))

(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))

;; Set up personal settings

(setq user-full-name "Raghuvir Kasturi")
(setq user-mail-address "raghuvir.kasturi@gmail.com")

;; Set up Vertico


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
