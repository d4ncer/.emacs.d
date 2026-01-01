;;; mod-keybindings.el --- Leader key bindings -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Raghuvir Kasturi
;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:
;; This module contains the main leader key (SPC) bindings and local leader
;; macro definition. The leader key provides access to most Emacs functionality
;; through a mnemonic, hierarchical menu system powered by General.el.

;;; Code:

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
   :states '(normal motion insert)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
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
         (and (derived-mode-p 'markdown-mode)
              (fboundp 'markdown-code-block-at-point-p)
              (markdown-code-block-at-point-p))
         'markdown-edit-code-block)

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

   "pi"  '(nil :wk "issues (beads)")
   "pii" '(beads :wk "list issues")
   "pic" '(beads-create-issue :wk "create issue")

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
   "oi" (list (defun +goto-org-index ()
                (interactive)
                (find-file (file-name-concat org-directory "index.org")))
              :wk "index")
   "ot" (list (defun +goto-org-todos ()
                (interactive)
                (find-file (file-name-concat org-directory "todos.org")))
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
   "of" '(org-node-find :wk "find (node)")
   "os" '(org-node-grep :wk "search (node)")
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

   "or" '(nil :wk "node")
   "ord" (list (defun +org-node-daily ()
                 (interactive)
                 (let* ((date (format-time-string "%Y-%m-%d"))
                        (title (format "Daily: %s" date))
                        (filename (format "%s-daily.org" date)))
                   (find-file (file-name-concat org-directory "daily" filename))
                   (when (= (buffer-size) 0)
                     (insert (format "#+TITLE: %s\n#+DATE: %s\n\n* Daily Notes\n\n" title date)))))
               :wk "daily journal")
   "orl" '(org-node-extract-subtree :wk "extract subtree")
   "orr" '(org-node-reset :wk "reset cache")
   "ort" '(org-node-tag-grep :wk "search by tag")

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
   "sp" (list (defun +symbol-in-project ()
                "Search for occurrences of the symbol or sub-string at point in the project."
                (interactive)
                (let ((selection (if (use-region-p)
                                     (buffer-substring-no-properties (region-beginning) (region-end))
                                   (substring-no-properties (thing-at-point 'symbol)))))
                  (consult-ripgrep (project-root (project-current)) selection)))
              :wk "search symbol in project")

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

(provide 'mod-keybindings)
;;; mod-keybindings.el ends here
